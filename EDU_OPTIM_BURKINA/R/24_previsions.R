# ==========================================================================
# 24_previsions.R — Prévisions des effectifs et des besoins
# ==========================================================================
# Séries courtes (10 années) : on privilégie des modèles parcimonieux et on
# les COMPARE par backtesting (estimation jusqu'à N-3, test sur les 3 dernières).

METHODES_PREVISION <- c(
  lineaire = "Tendance linéaire", log_lineaire = "Tendance exponentielle (log-linéaire)",
  holt = "Lissage de Holt amorti", ets = "Modèle ETS (sélection automatique)",
  arima = "ARIMA (sélection automatique)", demographique = "Projection démographique × TBS")

conv_fc <- function(f, futur) data.frame(
  annee = futur, prevision = as.numeric(f$mean), bas80 = as.numeric(f$lower[, 1]),
  haut80 = as.numeric(f$upper[, 1]), bas95 = as.numeric(f$lower[, 2]), haut95 = as.numeric(f$upper[, 2]))

#' Prévision d'une série annuelle par une méthode.
#' @param y valeurs ; @param annees années ; @param h horizon (années)
#' @param pop population d'âge scolaire (méthode démographique)
#' @param mult_croissance multiplicateur de la croissance (scénarios)
prevoir_serie <- function(y, annees, h, methode = "holt", pop = NULL, mult_croissance = 1) {
  ok <- !is.na(y); y <- y[ok]; annees <- annees[ok]
  if (!is.null(pop)) pop <- pop[ok]
  futur <- max(annees) + seq_len(h)
  vide <- data.frame(annee = futur, prevision = NA_real_, bas80 = NA_real_, haut80 = NA_real_,
                     bas95 = NA_real_, haut95 = NA_real_)
  if (length(y) < 4 || h < 1) return(vide)
  ts_y <- stats::ts(y, start = min(annees))
  res <- tryCatch(switch(methode,
    lineaire = {
      m <- stats::lm(y ~ annees)
      p80 <- stats::predict(m, data.frame(annees = futur), interval = "prediction", level = 0.8)
      p95 <- stats::predict(m, data.frame(annees = futur), interval = "prediction", level = 0.95)
      data.frame(annee = futur, prevision = p80[, 1], bas80 = p80[, 2], haut80 = p80[, 3], bas95 = p95[, 2], haut95 = p95[, 3])
    },
    log_lineaire = {
      m <- stats::lm(log(pmax(y, 1)) ~ annees)
      p80 <- exp(stats::predict(m, data.frame(annees = futur), interval = "prediction", level = 0.8))
      p95 <- exp(stats::predict(m, data.frame(annees = futur), interval = "prediction", level = 0.95))
      data.frame(annee = futur, prevision = p80[, 1], bas80 = p80[, 2], haut80 = p80[, 3], bas95 = p95[, 2], haut95 = p95[, 3])
    },
    holt = conv_fc(forecast::holt(ts_y, h = h, damped = TRUE, level = c(80, 95)), futur),
    ets = conv_fc(forecast::forecast(forecast::ets(ts_y), h = h, level = c(80, 95)), futur),
    arima = conv_fc(forecast::forecast(forecast::auto.arima(ts_y, stepwise = TRUE, approximation = TRUE,
                                                            max.p = 2, max.q = 2, max.d = 1),
                                       h = h, level = c(80, 95)), futur),
    demographique = {
      if (is.null(pop) || all(is.na(pop))) stop("population absente")
      n <- length(pop); k <- max(1, n - 5)
      g <- (pop[n] / pop[k])^(1 / (n - k)) - 1
      pop_f <- pop[n] * (1 + g * mult_croissance)^seq_len(h)
      tbs <- y / pop
      mt <- stats::lm(tbs ~ annees)
      tbs_f <- pmax(pmin(stats::predict(mt, data.frame(annees = futur)), max(tbs) * 1.25), 0)
      sig <- stats::sd(stats::residuals(mt)) * sqrt(seq_len(h))
      data.frame(annee = futur, prevision = pop_f * tbs_f, bas80 = pop_f * pmax(0, tbs_f - 1.28 * sig),
                 haut80 = pop_f * (tbs_f + 1.28 * sig), bas95 = pop_f * pmax(0, tbs_f - 1.96 * sig),
                 haut95 = pop_f * (tbs_f + 1.96 * sig))
    }, stop("Méthode inconnue : ", methode)), error = function(e) vide)
  res[, -1] <- lapply(res[, -1], function(z) pmax(0, z))
  if (methode != "demographique" && mult_croissance != 1) {
    # Scénario : on amplifie l'écart entre la projection et la dernière valeur
    base <- y[length(y)]
    res[, -1] <- lapply(res[, -1], function(z) pmax(0, base + (z - base) * mult_croissance))
  }
  res
}

#' Backtesting : apprentissage jusqu'à `fin_apprentissage`, test sur la suite.
backtester <- function(y, annees, pop = NULL, fin_apprentissage = max(annees) - 3,
                       methodes = names(METHODES_PREVISION)) {
  app <- annees <= fin_apprentissage
  if (sum(app & !is.na(y)) < 4 || !any(!app & !is.na(y))) return(NULL)
  do.call(rbind, lapply(methodes, function(m) {
    f <- prevoir_serie(y[app], annees[app], h = sum(!app), methode = m, pop = if (!is.null(pop)) pop[app])
    obs <- y[!app][seq_len(nrow(f))]
    e <- f$prevision - obs
    data.frame(methode = m, libelle = METHODES_PREVISION[[m]],
               mae = mean(abs(e), na.rm = TRUE), rmse = sqrt(mean(e^2, na.rm = TRUE)),
               mape = 100 * mean(abs(e / obs), na.rm = TRUE),
               couverture95 = mean(obs >= f$bas95 & obs <= f$haut95, na.rm = TRUE))
  }))
}

#' Backtesting sur toutes les séries d'un panel (provinces d'un cycle).
backtester_panel <- function(p, variable = "eff_t", methodes = names(METHODES_PREVISION)) {
  res <- lapply(split(p, p$code_geo), function(s) {
    s <- s[order(s$annee_debut), ]
    b <- backtester(s[[variable]], s$annee_debut, s$pop_age_scolaire, methodes = methodes)
    if (!is.null(b)) b$code_geo <- s$code_geo[1]
    b
  })
  r <- do.call(rbind, res)
  if (is.null(r)) return(NULL)
  r <- r[is.finite(r$mape), ]
  agg <- stats::aggregate(cbind(mae, rmse, mape, couverture95) ~ methode + libelle, r, mean)
  agg[order(agg$mape), ]
}

#' Projette les effectifs de toutes les provinces d'un cycle à l'horizon.
projeter_panel <- function(p, horizon, methode = "holt", mult_croissance = 1) {
  derniere <- max(p$annee_debut)
  h <- max(1, horizon - derniere)
  do.call(rbind, lapply(split(p, p$code_geo), function(s) {
    s <- s[order(s$annee_debut), ]
    f <- prevoir_serie(s$eff_t, s$annee_debut, h, methode, s$pop_age_scolaire, mult_croissance)
    f$code_geo <- s$code_geo[1]; f$cycle <- as.character(s$cycle[1])
    f$eff_actuel <- s$eff_t[nrow(s)]
    f
  }))
}

#' Besoins à l'horizon, à stock constant (salles, enseignants, équipements).
besoins_futurs <- function(proj, actuel, normes, couts = COUTS_DEFAUT) {
  a <- actuel[, c("code_geo", "cycle", "nb_salles_bon_etat", "nb_ens_t", "nb_tables_bancs", "nb_latrines")]
  a$cycle <- as.character(a$cycle)
  x <- merge(proj, a, by = c("code_geo", "cycle"))
  cyc <- x$cycle
  x$salles_requises <- ceiling(x$prevision / norme(normes, "TAILLE_CLASSE_MAX", cyc))
  x$enseignants_requis <- ceiling(x$prevision / norme(normes, "RATIO_ELEVES_ENSEIGNANT", cyc))
  x$tables_bancs_requises <- ifelse(cyc == "PRESCOLAIRE", NA, ceiling(x$prevision / norme(normes, "PLACES_TABLE_BANC", cyc)))
  x$latrines_requises <- ceiling(x$prevision / norme(normes, "ELEVES_PAR_LATRINE", cyc))
  x$besoin_salles <- pmax(0, x$salles_requises - x$nb_salles_bon_etat)
  x$besoin_enseignants <- pmax(0, x$enseignants_requis - x$nb_ens_t)
  x$besoin_tables_bancs <- pmax(0, x$tables_bancs_requises - x$nb_tables_bancs)
  x$besoin_latrines <- pmax(0, x$latrines_requises - x$nb_latrines)
  cu <- setNames(couts$cout_unitaire, couts$ressource)
  b <- cbind(x$besoin_salles * cu[["salles"]], x$besoin_enseignants * cu[["enseignants"]],
             x$besoin_tables_bancs * cu[["tables_bancs"]], x$besoin_latrines * cu[["latrines"]])
  x$cout_estime <- rowSums(b, na.rm = TRUE)
  x$cout_estime[rowSums(!is.na(b)) == 0] <- NA   # stocks inconnus : coût non évaluable
  x
}
