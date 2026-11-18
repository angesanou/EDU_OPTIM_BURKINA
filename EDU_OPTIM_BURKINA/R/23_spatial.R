# ==========================================================================
# 23_spatial.R — Analyse spatiale (autocorrélation, LISA, accessibilité)
# ==========================================================================

#' Joint des indicateurs à une couche géographique (les colonnes déjà
#' présentes dans la couche — nom, superficie… — sont conservées).
joindre_geo <- function(couche, df, cle = "code_geo") {
  if (is.null(couche)) return(NULL)
  df <- as.data.frame(df)
  df <- df[, c(cle, setdiff(names(df), names(couche))), drop = FALSE]
  merge(couche, df, by = cle, all.x = TRUE)
}

#' Matrice de voisinage (contiguïté « reine », poids standardisés en ligne).
voisinage <- function(couche) {
  nb <- spdep::poly2nb(couche, queen = TRUE)
  # Îlots éventuels : voisinage par les 2 plus proches voisins
  if (any(spdep::card(nb) == 0)) {
    cent <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(couche)))
    nb <- spdep::knn2nb(spdep::knearneigh(cent, k = 2))
  }
  spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
}

#' I de Moran global (test analytique + permutations).
moran_global <- function(couche, variable, n_perm = 999) {
  x <- couche[[variable]]
  ok <- !is.na(x)
  if (sum(ok) < 8) return(NULL)
  sous <- couche[ok, ]
  lw <- voisinage(sous)
  t1 <- spdep::moran.test(sous[[variable]], lw, zero.policy = TRUE)
  set.seed(1)
  t2 <- spdep::moran.mc(sous[[variable]], lw, nsim = n_perm, zero.policy = TRUE)
  list(I = unname(t1$estimate[1]), esperance = unname(t1$estimate[2]), variance = unname(t1$estimate[3]),
       z = unname(t1$statistic), p_analytique = t1$p.value, p_permutation = t2$p.value,
       interpretation = if (t2$p.value < 0.05) {
         if (t1$estimate[1] > 0) "Autocorrélation spatiale POSITIVE significative : les territoires voisins ont des niveaux semblables (regroupements géographiques)."
         else "Autocorrélation spatiale NÉGATIVE significative : alternance de territoires contrastés."
       } else "Pas d'autocorrélation spatiale significative : la répartition ne se distingue pas d'une répartition aléatoire.")
}

#' Indicateurs locaux d'association spatiale (LISA de Moran).
lisa <- function(couche, variable, seuil = 0.05) {
  x <- couche[[variable]]
  ok <- !is.na(x)
  couche$lisa_classe <- "Pas de regroupement marqué"
  couche$lisa_I <- NA_real_; couche$lisa_p <- NA_real_
  if (sum(ok) < 8) return(couche)
  sous <- couche[ok, ]
  lw <- voisinage(sous)
  li <- spdep::localmoran(sous[[variable]], lw, zero.policy = TRUE)
  z <- as.numeric(scale(sous[[variable]]))
  lag <- spdep::lag.listw(lw, z, zero.policy = TRUE)
  p <- li[, ncol(li)]
  cl <- ifelse(p >= seuil, "Pas de regroupement marqué",
         ifelse(z > 0 & lag > 0, "Groupe de valeurs élevées",
         ifelse(z < 0 & lag < 0, "Groupe de valeurs faibles",
         ifelse(z > 0 & lag < 0, "Valeur élevée isolée", "Valeur faible isolée"))))
  couche$lisa_classe[ok] <- cl; couche$lisa_I[ok] <- li[, 1]; couche$lisa_p[ok] <- p
  couche
}
COULEURS_LISA <- c("Groupe de valeurs élevées" = "#D1495B", "Groupe de valeurs faibles" = "#2F6FDE",
                   "Valeur élevée isolée" = "#F0A35E", "Valeur faible isolée" = "#8EC5E8",
                   "Pas de regroupement marqué" = "#E9EDF2")

#' Plus proche voisin euclidien (coordonnées métriques), vectorisé par blocs :
#' bien plus rapide que des distances géodésiques pour quelques milliers de points.
plus_proche <- function(a, b, bloc = 100) {
  dist <- numeric(nrow(a)); idx <- integer(nrow(a))
  for (s in seq(1, nrow(a), by = bloc)) {
    k <- s:min(nrow(a), s + bloc - 1)
    d2 <- outer(a[k, 1], b[, 1], "-")^2 + outer(a[k, 2], b[, 2], "-")^2
    j <- max.col(-d2, ties.method = "first")
    idx[k] <- j
    dist[k] <- sqrt(d2[cbind(seq_along(k), j)])
  }
  list(dist_km = dist / 1000, index = idx)
}

#' Accessibilité : distance de « foyers » simulés (5 par commune) à
#' l'établissement fonctionnel le plus proche du cycle.
accessibilite <- function(communes, etab, cycle, annee, seuil_km = 3) {
  if (is.null(communes) || is.null(etab)) return(NULL)
  e <- etab[as.character(etab$cycle) == cycle & etab$annee_debut == annee & etab$ferme == 0 & !is.na(etab$lon), ]
  if (!nrow(e)) return(NULL)
  pts_e <- sf::st_transform(sf::st_as_sf(e, coords = c("lon", "lat"), crs = 4326), 32630)
  set.seed(42)  # foyers reproductibles
  com_m <- sf::st_transform(communes, 32630)
  foyers <- sf::st_sample(com_m, rep(5, nrow(com_m)), type = "random", exact = TRUE)
  id_com <- rep(com_m$code_geo, each = 5)[seq_along(foyers)]
  pp <- plus_proche(sf::st_coordinates(foyers), sf::st_coordinates(pts_e))
  res <- data.frame(code_geo = id_com, dist_km = pp$dist_km)
  out <- stats::aggregate(dist_km ~ code_geo, res, mean)
  out$part_au_dela_seuil <- as.numeric(tapply(res$dist_km > seuil_km, res$code_geo, mean)[out$code_geo])
  out
}

#' Densité d'établissements sur une grille hexagonale (≈ 25 km).
densite_hex <- function(contour, etab, cycle, annee, taille_km = 25) {
  if (is.null(contour) || is.null(etab)) return(NULL)
  e <- etab[as.character(etab$cycle) == cycle & etab$annee_debut == annee & !is.na(etab$lon), ]
  if (!nrow(e)) return(NULL)
  c_m <- sf::st_transform(sf::st_union(contour), 32630)
  grille <- sf::st_sf(geometry = sf::st_make_grid(c_m, cellsize = taille_km * 1000, square = FALSE))
  grille <- grille[lengths(sf::st_intersects(grille, c_m)) > 0, ]
  pts <- sf::st_transform(sf::st_as_sf(e, coords = c("lon", "lat"), crs = 4326), 32630)
  idx <- sf::st_intersects(grille, pts)
  grille$nb_etab <- lengths(idx)
  grille$nb_fonctionnels <- vapply(idx, function(i) sum(pts$ferme[i] == 0), numeric(1))
  grille$eleves <- vapply(idx, function(i) sum(pts$eff_t[i], na.rm = TRUE), numeric(1))
  grille <- grille[grille$nb_etab > 0, ]
  sf::st_transform(grille, 4326)
}
