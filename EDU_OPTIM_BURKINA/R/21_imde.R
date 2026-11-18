# ==========================================================================
# 21_imde.R — Indice Multidimensionnel de Déficit Éducatif (IMDE)
# ==========================================================================
# Méthode principale (documentée dans documentation/methodologie_scientifique.md) :
#   1. 5 dimensions, 2 à 4 indicateurs chacune (choix justifiés : normes
#      sectorielles, disponibilité dans les annuaires, faible redondance).
#   2. Orientation : chaque indicateur est tourné pour que 1 = déficit maximal.
#   3. Normalisation min-max à BORNES FIXES calculées sur toutes les années et
#      tous les territoires du cycle (comparabilité dans le temps), après
#      écrêtage aux percentiles 2 et 98 (robustesse aux valeurs extrêmes).
#   4. Score de dimension = moyenne des indicateurs disponibles (repondération
#      automatique si un indicateur manque).
#   5. IMDE = moyenne pondérée des dimensions × 100 (poids égaux par défaut,
#      convention de l'IDH/IPM : aucune dimension n'est jugée prioritaire a priori).
# Variantes de robustesse : normalisation z-score ou par rangs, agrégation
# géométrique (non compensatoire), poids issus de l'ACP ou de l'entropie.

DIMENSIONS_IMDE <- list(
  "Infrastructures"            = c("ratio_eleves_salle_utilisable", "part_salles_mauvais", "taux_deficit_salles"),
  "Ressources humaines"        = c("ratio_eleves_ens", "part_ens_non_qualifies"),
  "Équipements"                = c("ratio_eleves_table_banc", "ratio_eleves_latrine", "part_sans_latrines"),
  "Conditions d'apprentissage" = c("part_sans_eau", "part_sans_electricite", "part_sans_cantine", "ratio_eleves_classe"),
  "Accès et couverture"        = c("tbs", "taux_fonctionnalite", "etab_100km2", "part_eleves_affectes")
)
SENS_IMDE <- setNames(CATALOGUE_INDICATEURS$sens, CATALOGUE_INDICATEURS$code)

#' Bornes de normalisation (par cycle) : percentiles 2 % et 98 % du pool.
bornes_imde <- function(df, probs = c(0.02, 0.98)) {
  ind <- unlist(DIMENSIONS_IMDE)
  do.call(rbind, lapply(split(df, as.character(df$cycle)), function(x) {
    q <- function(v, p) if (v %in% names(x) && any(!is.na(x[[v]]))) unname(stats::quantile(x[[v]], p, na.rm = TRUE)) else NA_real_
    data.frame(cycle = as.character(x$cycle[1]), indicateur = ind,
               min = vapply(ind, q, numeric(1), p = probs[1]),
               max = vapply(ind, q, numeric(1), p = probs[2]),
               moyenne = vapply(ind, function(v) if (v %in% names(x)) mean(x[[v]], na.rm = TRUE) else NA_real_, numeric(1)),
               ecart_type = vapply(ind, function(v) if (v %in% names(x)) stats::sd(x[[v]], na.rm = TRUE) else NA_real_, numeric(1)),
               row.names = NULL, stringsAsFactors = FALSE)
  }))
}

#' Normalise un indicateur en score de déficit dans [0, 1].
normaliser <- function(x, sens, b, methode = c("minmax", "zscore", "rang")) {
  methode <- match.arg(methode)
  s <- switch(methode,
    minmax = { e <- b$max - b$min; if (!is.finite(e) || e <= 0) rep(0.5, length(x)) else (pmin(pmax(x, b$min), b$max) - b$min) / e },
    zscore = { if (!is.finite(b$ecart_type) || b$ecart_type == 0) rep(0.5, length(x)) else stats::pnorm((x - b$moyenne) / b$ecart_type) },
    rang   = { r <- rank(x, na.last = "keep", ties.method = "average"); (r - 1) / max(1, sum(!is.na(x)) - 1) })
  s[is.na(x)] <- NA
  if (sens < 0) s <- 1 - s
  s
}

#' Poids des dimensions par l'ACP (carrés des saturations du 1er axe).
poids_acp <- function(scores) {
  m <- scores[stats::complete.cases(scores), , drop = FALSE]
  if (nrow(m) < 5) return(rep(1 / ncol(scores), ncol(scores)))
  ok <- apply(m, 2, stats::sd) > 0
  if (sum(ok) < 2) return(rep(1 / ncol(scores), ncol(scores)))
  pc <- stats::prcomp(m[, ok, drop = FALSE], scale. = TRUE)
  w <- rep(0, ncol(scores)); w[ok] <- pc$rotation[, 1]^2
  w <- w + 0.01   # aucune dimension totalement ignorée
  w / sum(w)
}
#' Poids par la méthode de l'entropie (dimensions plus dispersées = plus de poids).
poids_entropie <- function(scores) {
  m <- scores[stats::complete.cases(scores), , drop = FALSE] + 1e-6
  if (nrow(m) < 3) return(rep(1 / ncol(scores), ncol(scores)))
  p <- sweep(m, 2, colSums(m), "/")
  e <- -colSums(p * log(p)) / log(nrow(m))
  d <- 1 - e
  d / sum(d)
}

#' Calcule l'IMDE.
#' @param df      panel d'indicateurs (un ou plusieurs cycles ; bornes par cycle).
#' @param bornes  bornes fixes (bornes_imde) ; calculées sur df si NULL.
#' @param normalisation "minmax" (principale), "zscore" ou "rang".
#' @param agregation "arithmetique" (principale) ou "geometrique".
#' @param poids  vecteur nommé de poids des dimensions, ou "egal", "acp", "entropie".
calculer_imde <- function(df, bornes = NULL, normalisation = "minmax", agregation = "arithmetique",
                          poids = "egal") {
  if (is.null(bornes)) bornes <- bornes_imde(df)
  dims <- names(DIMENSIONS_IMDE)
  sc_dim <- matrix(NA_real_, nrow(df), length(dims), dimnames = list(NULL, dims))
  cyc <- as.character(df$cycle)
  for (d in dims) {
    inds <- DIMENSIONS_IMDE[[d]]
    m <- matrix(NA_real_, nrow(df), length(inds), dimnames = list(NULL, inds))
    for (v in inds) {
      if (!v %in% names(df)) next
      for (cy in unique(cyc)) {
        k <- cyc == cy
        b <- bornes[bornes$cycle == cy & bornes$indicateur == v, ]
        if (nrow(b)) m[k, v] <- normaliser(df[[v]][k], SENS_IMDE[[v]], b[1, ], normalisation)
      }
      df[[paste0("s_", v)]] <- m[, v]
    }
    sc_dim[, d] <- ifelse(rowSums(!is.na(m)) == 0, NA, rowMeans(m, na.rm = TRUE))
  }
  w <- if (is.numeric(poids)) poids[dims] else switch(poids,
    egal = rep(1, length(dims)), acp = poids_acp(sc_dim), entropie = poids_entropie(sc_dim))
  w <- as.numeric(w) / sum(w)
  wm <- matrix(w, nrow(df), length(dims), byrow = TRUE)
  wm[is.na(sc_dim)] <- 0
  imde <- if (agregation == "arithmetique") rowSums(sc_dim * wm, na.rm = TRUE) / rowSums(wm)
          else exp(rowSums(log(pmax(sc_dim, 0.01)) * wm, na.rm = TRUE) / rowSums(wm))
  # Indice non calculé si moins de 3 dimensions renseignées
  imde[rowSums(!is.na(sc_dim)) < 3] <- NA
  for (d in dims) df[[paste0("dim_", d)]] <- 100 * sc_dim[, d]
  df$imde <- 100 * imde
  df$nb_dimensions <- rowSums(!is.na(sc_dim))
  df$classe_imde <- classer_imde(df$imde)
  attr(df, "poids") <- setNames(w, dims)
  attr(df, "bornes") <- bornes
  df
}

#' Classes à seuils FIXES (comparables dans le temps).
SEUILS_IMDE <- c(0, 25, 37.5, 50, 62.5, 100)
classer_imde <- function(x) cut(x, SEUILS_IMDE, labels = CLASSES_IMDE, include.lowest = TRUE)

#' Classement (1 = déficit le plus élevé) au sein de groupes (année × cycle).
rang_imde <- function(df, groupe = c("annee_debut", "cycle")) {
  g <- interaction(df[, groupe], drop = TRUE)
  stats::ave(-df$imde, g, FUN = function(z) rank(z, na.last = "keep", ties.method = "min"))
}

#' Analyse de sensibilité aux poids : tirages de Dirichlet(1,…,1).
sensibilite_imde <- function(df, n = 500, graine = 1) {
  set.seed(graine)
  dims <- names(DIMENSIONS_IMDE)
  sc <- as.matrix(df[, paste0("dim_", dims)]) / 100
  W <- matrix(stats::rgamma(n * length(dims), 1), n)
  W <- W / rowSums(W)
  rangs <- apply(W, 1, function(w) {
    wm <- matrix(w, nrow(sc), length(dims), byrow = TRUE); wm[is.na(sc)] <- 0
    v <- rowSums(sc * wm, na.rm = TRUE) / rowSums(wm)
    rank(-v, ties.method = "min")
  })
  rangs <- matrix(rangs, nrow = nrow(sc))
  base <- rank(-df$imde, ties.method = "min")
  k <- max(1, round(nrow(df) / 4))
  data.frame(code_geo = df$code_geo, nom = df$nom, rang_principal = base,
             rang_median = apply(rangs, 1, stats::median),
             rang_p05 = apply(rangs, 1, stats::quantile, 0.05), rang_p95 = apply(rangs, 1, stats::quantile, 0.95),
             prob_quartile_critique = rowMeans(rangs <= k), stringsAsFactors = FALSE)
}

#' Robustesse : compare 6 variantes méthodologiques (corrélations de rangs).
robustesse_imde <- function(df, bornes) {
  variantes <- list(
    "Principale (min-max, arithm., égaux)" = list("minmax", "arithmetique", "egal"),
    "Géométrique"                          = list("minmax", "geometrique", "egal"),
    "Z-score"                              = list("zscore", "arithmetique", "egal"),
    "Rangs"                                = list("rang", "arithmetique", "egal"),
    "Poids ACP"                            = list("minmax", "arithmetique", "acp"),
    "Poids entropie"                       = list("minmax", "arithmetique", "entropie"))
  sc <- vapply(variantes, function(v) calculer_imde(df, bornes, v[[1]], v[[2]], v[[3]])$imde, numeric(nrow(df)))
  sc <- matrix(sc, nrow = nrow(df), dimnames = list(NULL, names(variantes)))
  rg <- apply(-sc, 2, rank, ties.method = "min")
  rg <- matrix(rg, nrow = nrow(df), dimnames = list(NULL, names(variantes)))
  list(scores = data.frame(code_geo = df$code_geo, nom = df$nom, sc, check.names = FALSE),
       rangs = data.frame(code_geo = df$code_geo, nom = df$nom, rg, check.names = FALSE),
       spearman = suppressWarnings(stats::cor(rg, method = "spearman", use = "pairwise.complete.obs")),
       ecart_max = apply(rg, 1, function(r) max(r) - min(r)))
}
