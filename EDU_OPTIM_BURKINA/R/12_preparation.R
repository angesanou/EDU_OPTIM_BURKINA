# ==========================================================================
# 12_preparation.R — Préparation complète des données pour l'application
# ==========================================================================
# Enchaîne : chargement -> contrôles -> nettoyage -> indicateurs -> IMDE.
# Le résultat est mis en cache (data/prepare/) : l'application démarre en
# quelques secondes tant que les fichiers sources ne changent pas.

VERSION_PREPARATION <- "1.1.1"   # 1.1 : cadrage sur les données réelles

preparer_tout <- function(cfg = lire_config(), forcer = FALSE) {
  d <- charger_donnees(cfg, forcer = forcer)
  cache <- file.path("data/prepare", paste0("prepare_", cfg$source_donnees, ".rds"))
  if (!forcer && file.exists(cache)) {
    x <- tryCatch(readRDS(cache), error = function(e) NULL)
    if (!is.null(x) && identical(x$empreinte, d$meta$empreinte) && identical(x$version, VERSION_PREPARATION))
      return(x$res)
  }
  controles <- list(panel = controler_panel(d$panel, d$geo),
                    etablissements = controler_etablissements(d$etablissements))
  d$etablissements <- nettoyer_etablissements(d$etablissements)
  ind <- preparer_indicateurs(d)
  imde <- list(); bornes <- list(); imde_com <- list()
  for (cy in c(names(CYCLES), "TOUS")) {
    p <- if (cy == "TOUS") ind$province_tous else ind$province[as.character(ind$province$cycle) == cy, ]
    if (!nrow(p)) next
    bornes[[cy]] <- bornes_imde(p)
    imde[[cy]] <- calculer_imde(p, bornes[[cy]])
    imde[[cy]]$rang <- rang_imde(imde[[cy]])
    if (!is.null(ind$commune)) {
      cm <- ind$commune[as.character(ind$commune$cycle) == cy, ]
      if (nrow(cm)) { imde_com[[cy]] <- calculer_imde(cm, bornes_imde(cm)); imde_com[[cy]]$rang <- rang_imde(imde_com[[cy]]) }
    }
  }
  journaliser("Préparation", sprintf("Indicateurs et IMDE calculés (%d cycles, %d provinces, %s communes)",
                                     length(imde), length(unique(ind$province$code_geo)),
                                     if (is.null(ind$commune)) "0" else length(unique(ind$commune$code_geo))))
  res <- list(
    d = d[c("geo", "normes", "dictionnaire", "geometries", "contexte", "meta", "panel", "panel_region")],
    ind = ind, imde = imde, bornes = bornes, imde_communes = if (length(imde_com)) imde_com else NULL,
    controles = controles, journal = JOURNAL$entrees,
    distances = tryCatch(distances_provinces(d$geometries$provinces), error = function(e) NULL))
  res$d$meta$journal <- JOURNAL$entrees
  dir.create(dirname(cache), showWarnings = FALSE, recursive = TRUE)
  saveRDS(list(empreinte = d$meta$empreinte, version = VERSION_PREPARATION, res = res), cache)
  res
}

#' Restreint les données au périmètre d'un utilisateur (régional / provincial).
restreindre <- function(D, provinces) {
  if (is.null(provinces)) return(D)
  f <- function(x, col = "code_geo") if (is.null(x)) NULL else x[x[[col]] %in% provinces, , drop = FALSE]
  regions <- unique(D$ind$province$code_region[D$ind$province$code_geo %in% provinces])
  D$ind$province <- f(D$ind$province)
  D$ind$province_tous <- f(D$ind$province_tous)
  # Agrégats régionaux recalculés sur les seules provinces autorisées
  D$ind$region <- agreger(D$ind$province, "region", D$d$normes, tous_cycles = TRUE)
  D$ind$commune <- f(D$ind$commune, "code_province")
  D$ind$etablissements <- f(D$ind$etablissements, "code_province")
  # Agrégat « national » = agrégat du périmètre autorisé
  D$ind$national <- agreger(D$ind$province, "national", D$d$normes, tous_cycles = TRUE)
  D$ind$national$nom <- "Périmètre autorisé"
  D$imde <- lapply(D$imde, f)
  if (!is.null(D$imde_communes)) D$imde_communes <- lapply(D$imde_communes, f, col = "code_province")
  g <- D$d$geometries
  if (!is.null(g)) {
    g$provinces <- g$provinces[g$provinces$code_geo %in% provinces, ]
    g$regions <- g$regions[g$regions$code_geo %in% regions, ]
    if (!is.null(g$communes)) g$communes <- g$communes[g$communes$code_province %in% provinces, ]
    D$d$geometries <- g
  }
  D
}

#' Panel province d'un cycle (« TOUS » = tous cycles confondus).
panel_cycle <- function(D, cycle) {
  if (identical(cycle, "TOUS")) D$ind$province_tous else D$ind$province[as.character(D$ind$province$cycle) == cycle, ]
}
libelle_cycle <- function(cycle) if (identical(cycle, "TOUS")) "Tous cycles" else unname(CYCLES[cycle])
