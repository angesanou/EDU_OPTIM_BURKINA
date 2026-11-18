# ==========================================================================
# 25_priorisation.R — Priorisation territoriale multicritère (TOPSIS)
# ==========================================================================
# TOPSIS (Hwang & Yoon, 1981) classe les territoires selon leur proximité à une
# situation « la plus prioritaire » idéale et leur éloignement de la moins
# prioritaire. Choisi car : (i) compensatoire et transparent, (ii) gère des
# critères à maximiser ET à minimiser, (iii) produit un score continu
# interprétable (0-1), (iv) se prête à l'analyse de sensibilité des poids.

CRITERES_PRIORISATION <- data.frame(
  code = c("deficit", "population", "vulnerabilite", "accessibilite", "pression", "cout", "equite"),
  libelle = c("Niveau de déficit (IMDE)", "Population scolaire", "Vulnérabilité", "Accessibilité (densité d'établissements)",
              "Pression démographique", "Coût par élève bénéficiaire", "Équité (écart de TBS et parité)"),
  sens = c(1, 1, 1, -1, 1, -1, 1),
  poids = c(0.30, 0.15, 0.15, 0.10, 0.10, 0.10, 0.10),
  explication = c("Plus l'IMDE est élevé, plus la priorité est forte.",
                  "Plus il y a d'enfants concernés, plus l'impact potentiel est grand.",
                  "Pauvreté, déplacés internes et fermetures d'écoles.",
                  "Moins d'établissements au km² = enfants plus éloignés : priorité plus forte (critère à minimiser).",
                  "Croissance annuelle médiane de la population d'âge scolaire sur 5 ans (robuste aux révisions des projections).",
                  "Coût de résorption des déficits rapporté aux élèves : à minimiser (efficience).",
                  "Écart au TBS moyen du périmètre et indice de parité défavorable aux filles."),
  stringsAsFactors = FALSE)

#' TOPSIS générique.
#' @param M matrice alternatives × critères ; @param w poids ; @param sens +1 à maximiser, -1 à minimiser
topsis <- function(M, w, sens) {
  M <- as.matrix(M)
  storage.mode(M) <- "double"
  # Valeurs manquantes : remplacées par la médiane du critère (neutre)
  for (j in seq_len(ncol(M))) {
    med <- stats::median(M[, j], na.rm = TRUE)
    M[is.na(M[, j]) | !is.finite(M[, j]), j] <- if (is.finite(med)) med else 0
  }
  nrm <- sqrt(colSums(M^2)); nrm[nrm == 0] <- 1
  R <- sweep(M, 2, nrm, "/")
  w <- as.numeric(w) / sum(w)
  V <- sweep(R, 2, w, "*")
  ideal <- ifelse(sens > 0, apply(V, 2, max), apply(V, 2, min))
  anti  <- ifelse(sens > 0, apply(V, 2, min), apply(V, 2, max))
  d_plus <- sqrt(rowSums(sweep(V, 2, ideal)^2))
  d_moins <- sqrt(rowSums(sweep(V, 2, anti)^2))
  score <- d_moins / (d_plus + d_moins)
  score[!is.finite(score)] <- 0.5
  # Contribution de chaque critère à l'éloignement de la situation la moins prioritaire
  contrib <- sweep(V, 2, anti)^2
  contrib <- contrib / pmax(rowSums(contrib), 1e-12)
  list(score = score, d_plus = d_plus, d_moins = d_moins, contributions = contrib, V = V)
}

#' Écrêtage aux percentiles 5 % et 95 % de chaque critère : évite qu'une valeur
#' extrême (ex. la densité d'établissements de la capitale) écrase l'échelle
#' d'un critère et domine le classement.
ecreter <- function(M, probs = c(0.05, 0.95)) {
  M <- as.data.frame(M)
  for (j in seq_along(M)) {
    x <- as.numeric(M[[j]])
    if (sum(!is.na(x)) >= 5) { q <- stats::quantile(x, probs, na.rm = TRUE); M[[j]] <- pmin(pmax(x, q[1]), q[2]) }
  }
  M
}

#' Construit la matrice de critères à partir des indicateurs (année et cycle donnés).
matrice_criteres <- function(ind, imde, couts = COUTS_DEFAUT, annee) {
  x <- ind[ind$annee_debut == annee, ]
  x$imde <- imde$imde[match(x$code_geo, imde$code_geo)]
  # Pression démographique : MÉDIANE des croissances annuelles sur 5 ans. Une
  # croissance moyenne 5 ans mesurerait surtout les révisions des projections
  # (passage RGPH 2006 -> 2019 en 2021 : -5 % ; révision 2024 : +7 %).
  fen <- ind[ind$annee_debut >= annee - 5 & ind$annee_debut <= annee, c("code_geo", "annee_debut", "pop_age_scolaire")]
  fen <- fen[order(fen$code_geo, fen$annee_debut), ]
  fen$g <- stats::ave(fen$pop_age_scolaire, fen$code_geo, FUN = function(z) c(NA, z[-1] / z[-length(z)] - 1))
  press <- tapply(fen$g, fen$code_geo, function(z) if (all(is.na(z))) NA_real_ else stats::median(z, na.rm = TRUE))
  cu <- setNames(couts$cout_unitaire, couts$ressource)
  defs <- cbind(x$deficit_salles * cu[["salles"]], x$deficit_enseignants * cu[["enseignants"]],
                x$deficit_tables_bancs * cu[["tables_bancs"]], x$deficit_latrines * cu[["latrines"]],
                x$deficit_eau * cu[["eau"]], x$deficit_electricite * cu[["electricite"]])
  cout_total <- rowSums(defs, na.rm = TRUE)
  # Aucun déficit calculable (stocks non publiés) : coût inconnu, pas nul (critère neutre)
  cout_total[rowSums(!is.na(defs)) == 0] <- NA
  tbs_ref <- 100 * sum(x$eff_t, na.rm = TRUE) / sum(x$pop_age_scolaire, na.rm = TRUE)
  pauv <- if ("taux_pauvrete" %in% names(x)) x$taux_pauvrete else rep(NA_real_, nrow(x))
  pdi <- if ("part_pdi" %in% names(x)) 10 * x$part_pdi else rep(NA_real_, nrow(x))
  vuln <- rowMeans(cbind(pauv, pdi, x$part_eleves_affectes, 1 - x$taux_fonctionnalite), na.rm = TRUE)
  vuln[is.nan(vuln)] <- NA
  ips <- if ("ips_tbs" %in% names(x)) x$ips_tbs else rep(1, nrow(x))
  data.frame(code_geo = x$code_geo, nom = x$nom, code_region = x$code_region,
             deficit = x$imde,
             population = x$pop_age_scolaire,
             vulnerabilite = vuln,
             accessibilite = x$etab_100km2,
             pression = 100 * as.numeric(press[x$code_geo]),
             cout = diviser(cout_total, x$eff_t),
             equite = pmax(0, tbs_ref - x$tbs) + 100 * pmax(0, 1 - ifelse(is.na(ips), 1, ips)),
             cout_total = cout_total, eff_t = x$eff_t, stringsAsFactors = FALSE)
}

#' Priorisation complète : scores, rangs national et régional, justification.
prioriser <- function(mc, poids = setNames(CRITERES_PRIORISATION$poids, CRITERES_PRIORISATION$code)) {
  crit <- CRITERES_PRIORISATION$code
  t <- topsis(ecreter(mc[, crit]), poids[crit], CRITERES_PRIORISATION$sens)
  mc$score <- t$score
  mc$rang_national <- rank(-mc$score, ties.method = "min")
  mc$rang_regional <- stats::ave(-mc$score, mc$code_region, FUN = function(z) rank(z, ties.method = "min"))
  colnames(t$contributions) <- crit
  ctb <- as.data.frame(t$contributions)
  mc$critere_principal <- sub(" \\(.*", "", CRITERES_PRIORISATION$libelle[apply(t$contributions, 1, which.max)])
  seuils <- unique(stats::quantile(mc$score, c(0.5, 0.8)))
  mc$niveau_priorite <- if (length(seuils) == 2 && nrow(mc) >= 3)
    as.character(cut(mc$score, c(-Inf, seuils, Inf), labels = c("Priorité normale", "Priorité élevée", "Priorité maximale")))
    else ifelse(mc$rang_national == 1, "Priorité maximale", "Priorité normale")
  ctb$code_geo <- mc$code_geo
  o <- order(mc$rang_national)
  out <- mc[o, ]
  attr(out, "contributions") <- ctb
  out
}

#' Sensibilité aux poids : perturbation aléatoire ±`amplitude`, `n` tirages.
sensibilite_priorites <- function(mc, poids, n = 300, amplitude = 0.3, graine = 1) {
  set.seed(graine)
  crit <- CRITERES_PRIORISATION$code
  rangs <- replicate(n, {
    w <- poids[crit] * stats::runif(length(crit), 1 - amplitude, 1 + amplitude)
    rank(-topsis(ecreter(mc[, crit]), w, CRITERES_PRIORISATION$sens)$score, ties.method = "min")
  })
  rangs <- matrix(rangs, nrow = nrow(mc))
  data.frame(code_geo = mc$code_geo, nom = mc$nom,
             rang = rank(-topsis(ecreter(mc[, crit]), poids[crit], CRITERES_PRIORISATION$sens)$score, ties.method = "min"),
             rang_min = apply(rangs, 1, min), rang_max = apply(rangs, 1, max),
             rang_median = apply(rangs, 1, stats::median),
             prob_top10 = rowMeans(rangs <= 10), stringsAsFactors = FALSE)
}
