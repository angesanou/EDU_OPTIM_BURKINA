# ==========================================================================
# 22_econometrie.R — Modèles explicatifs des déficits éducatifs
# ==========================================================================
# RAPPEL MÉTHODOLOGIQUE : ces modèles mesurent des ASSOCIATIONS statistiques
# conditionnelles. Sans stratégie d'identification (expérience naturelle,
# variable instrumentale…), aucun coefficient ne doit être lu comme un effet
# causal. Les effets fixes neutralisent les facteurs constants propres à
# chaque province, pas les facteurs variables dans le temps non observés.

VARIABLES_EXPLICATIVES <- c(
  taux_pauvrete = "Taux de pauvreté", part_urbain = "Part des élèves en milieu urbain",
  taux_urbanisation = "Taux d'urbanisation", part_pdi = "Part de déplacés internes (PDI)",
  indice_insecurite = "Indice d'insécurité", log_densite = "Densité de population (log)",
  croissance_eff = "Croissance annuelle des effectifs", ratio_eleves_ens = "Élèves par enseignant",
  part_ens_non_qualifies = "Part d'enseignants non qualifiés", taux_electricite = "Taux d'électrification",
  ratio_eleves_salle_utilisable = "Élèves par salle utilisable")
VARIABLES_EXPLIQUEES <- c(
  imde = "IMDE (0-100)", ratio_eleves_salle_utilisable = "Élèves par salle utilisable",
  ratio_eleves_ens = "Élèves par enseignant", taux_deficit_salles = "Taux de déficit en salles",
  tbs = "Taux brut de scolarisation", part_sans_eau = "Part d'établissements sans eau")

#' Prépare le panel province × année d'un cycle pour l'estimation.
preparer_panel_eco <- function(p) {
  p <- p[order(p$code_geo, p$annee_debut), ]
  p$log_densite <- log(pmax(p$densite_pop, 1))
  p$croissance_eff <- stats::ave(p$eff_t, p$code_geo, FUN = function(z) c(NA, diff(log(pmax(z, 1)))))
  # Déplacés : 0 seulement AVANT la première année publiée (crise naissante) ;
  # les années non publiées ensuite (2023-2024) restent manquantes, pas nulles.
  if ("part_pdi" %in% names(p) && any(!is.na(p$part_pdi))) {
    premiere <- min(p$annee_debut[!is.na(p$part_pdi)])
    p$part_pdi[is.na(p$part_pdi) & p$annee_debut < premiere] <- 0
  }
  p
}

#' Estime MCO, effets fixes (within), effets aléatoires et double effets fixes.
estimer_modeles <- function(p, y, x) {
  x <- setdiff(x, y)
  verifier_colonnes(p, c(y, x), "panel économétrique")
  dat <- p[stats::complete.cases(p[, c(y, x)]), c("code_geo", "annee_debut", "nom", y, x)]
  if (nrow(dat) < length(x) + 10) stop("Trop peu d'observations complètes pour estimer le modèle.")
  f <- stats::as.formula(paste(y, "~", paste(x, collapse = " + ")))
  pd <- plm::pdata.frame(dat, index = c("code_geo", "annee_debut"))
  m <- list(
    mco = stats::lm(f, data = dat),
    ef = plm::plm(f, data = pd, model = "within", effect = "individual"),
    ea = tryCatch(plm::plm(f, data = pd, model = "random", effect = "individual"), error = function(e) NULL),
    ef2 = tryCatch(plm::plm(f, data = pd, model = "within", effect = "twoways"), error = function(e) NULL))
  list(modeles = m, donnees = dat, formule = f, y = y, x = x, pd = pd)
}

#' Tableau des coefficients avec erreurs-types robustes (cluster province).
coefficients_robustes <- function(est, modele = "ef", niveau = 0.95) {
  m <- est$modeles[[modele]]
  if (is.null(m)) m <- est$modeles$ef
  V <- if (inherits(m, "plm")) plm::vcovHC(m, method = "arellano", cluster = "group", type = "HC1")
       else sandwich::vcovCL(m, cluster = est$donnees$code_geo, type = "HC1")
  ct <- lmtest::coeftest(m, vcov. = V)
  z <- stats::qt(1 - (1 - niveau) / 2, df = max(1, stats::df.residual(m)))
  out <- data.frame(terme = rownames(ct), estimation = ct[, 1], erreur_type = ct[, 2],
                    stat_t = ct[, 3], p_valeur = ct[, 4], row.names = NULL)
  out$ic_bas <- out$estimation - z * out$erreur_type
  out$ic_haut <- out$estimation + z * out$erreur_type
  out$libelle <- ifelse(out$terme %in% names(VARIABLES_EXPLICATIVES),
                        unname(VARIABLES_EXPLICATIVES[out$terme]), out$terme)
  out$significativite <- cut(out$p_valeur, c(-Inf, 0.01, 0.05, 0.1, Inf), labels = c("***", "**", "*", ""))
  out
}

#' Tests de spécification et diagnostics.
diagnostics_modeles <- function(est) {
  m <- est$modeles
  res <- list()
  ajout <- function(test, stat, p, conclusion) res[[length(res) + 1]] <<-
    data.frame(test = test, statistique = round(as.numeric(stat), 3), p_valeur = as.numeric(p), conclusion = conclusion)
  t <- tryCatch(plm::pFtest(m$ef, plm::plm(est$formule, data = est$pd, model = "pooling")), error = function(e) NULL)
  if (!is.null(t)) ajout("F de Fisher : effets individuels", t$statistic, t$p.value,
                         if (t$p.value < 0.05) "Effets provinces significatifs : préférer le panel au MCO poolé." else "MCO poolé acceptable.")
  if (!is.null(m$ea)) {
    t <- tryCatch(plm::phtest(m$ef, m$ea), error = function(e) NULL)
    if (!is.null(t)) ajout("Hausman (EF contre EA)", t$statistic, t$p.value,
                           if (t$p.value < 0.05) "Rejet de l'exogénéité des effets : retenir les effets fixes." else "Effets aléatoires cohérents et plus efficaces.")
  }
  t <- tryCatch(lmtest::bptest(m$mco), error = function(e) NULL)
  if (!is.null(t)) ajout("Breusch-Pagan (hétéroscédasticité)", t$statistic, t$p.value,
                         if (t$p.value < 0.05) "Hétéroscédasticité : erreurs-types robustes utilisées." else "Pas d'hétéroscédasticité détectée.")
  t <- tryCatch(plm::pbgtest(m$ef, order = 1), error = function(e) NULL)
  if (!is.null(t)) ajout("Breusch-Godfrey/Wooldridge (autocorrélation)", t$statistic, t$p.value,
                         if (t$p.value < 0.05) "Autocorrélation des résidus : erreurs-types « cluster » par province." else "Pas d'autocorrélation détectée.")
  t <- tryCatch(plm::pcdtest(m$ef, test = "cd"), error = function(e) NULL)
  if (!is.null(t)) ajout("Pesaran CD (dépendance transversale)", t$statistic, t$p.value,
                         if (t$p.value < 0.05) "Dépendance entre provinces (chocs communs, spatiaux) : prudence." else "Pas de dépendance transversale détectée.")
  vif <- if (length(est$x) >= 2) tryCatch(car::vif(m$mco), error = function(e) NULL) else NULL
  r2 <- function(mm) if (is.null(mm)) NA_real_ else unname(summary(mm)$r.squared[1])
  list(tests = do.call(rbind, res),
       vif = if (!is.null(vif)) data.frame(variable = names(vif), libelle = unname(VARIABLES_EXPLICATIVES[names(vif)]),
                                           vif = round(as.numeric(vif), 2), row.names = NULL),
       ajustement = data.frame(
         modele = c("MCO poolé", "Effets fixes", "Effets aléatoires", "Double effets fixes"),
         n = c(stats::nobs(m$mco), rep(nrow(est$donnees), 3)),
         r2 = c(summary(m$mco)$r.squared, r2(m$ef), r2(m$ea), r2(m$ef2))))
}

#' Bootstrap par grappes (on rééchantillonne des PROVINCES entières).
bootstrap_ef <- function(est, B = 199, graine = 1) {
  set.seed(graine)
  dat <- est$donnees; ids <- unique(dat$code_geo)
  tirages <- replicate(B, {
    s <- sample(ids, length(ids), replace = TRUE)
    bd <- do.call(rbind, lapply(seq_along(s), function(i) { z <- dat[dat$code_geo == s[i], ]; z$code_geo <- paste0(s[i], "_", i); z }))
    cf <- tryCatch(stats::coef(plm::plm(est$formule, data = plm::pdata.frame(bd, index = c("code_geo", "annee_debut")),
                                        model = "within")), error = function(e) NULL)
    if (is.null(cf)) rep(NA_real_, length(est$x)) else unname(cf[est$x])
  })
  tirages <- matrix(tirages, nrow = length(est$x), dimnames = list(est$x, NULL))
  data.frame(terme = est$x, libelle = unname(VARIABLES_EXPLICATIVES[est$x]),
             estimation = unname(stats::coef(est$modeles$ef)[est$x]),
             bootstrap_et = apply(tirages, 1, stats::sd, na.rm = TRUE),
             ic_bas = apply(tirages, 1, stats::quantile, 0.025, na.rm = TRUE),
             ic_haut = apply(tirages, 1, stats::quantile, 0.975, na.rm = TRUE), row.names = NULL)
}

#' Interprétation automatique, prudente, en français.
interpreter <- function(coefs, y_lib) {
  coefs <- coefs[coefs$terme != "(Intercept)", ]
  phr <- vapply(seq_len(nrow(coefs)), function(i) {
    r <- coefs[i, ]
    if (r$p_valeur >= 0.1) return(sprintf("• %s : pas d'association statistiquement significative (p = %s).", r$libelle, fmt_nombre(r$p_valeur, 2)))
    sprintf("• %s : association %s (coef. = %s ; IC 95 %% [%s ; %s] ; p = %s). Toutes choses égales par ailleurs dans le modèle, une hausse d'une unité va de pair avec une variation de %s de « %s ».",
            r$libelle, if (r$estimation > 0) "positive" else "négative", fmt_nombre(r$estimation, 3),
            fmt_nombre(r$ic_bas, 3), fmt_nombre(r$ic_haut, 3), fmt_nombre(r$p_valeur, 4), fmt_nombre(r$estimation, 3), y_lib)
  }, character(1))
  c(phr, "", "⚠ Association n'est pas causalité : ces coefficients décrivent des corrélations conditionnelles ; ils ne permettent pas de conclure qu'agir sur une variable modifiera le déficit.")
}
