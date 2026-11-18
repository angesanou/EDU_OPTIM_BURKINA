# ==========================================================================
# 10_chargement.R — Lecture des données (contrat de données EXX_01)
# ==========================================================================
# PRINCIPE CLÉ : l'application ne lit QUE des fichiers au format du classeur
# « EXX_01 — structure de base de données » (feuilles DIM_GEO, PANEL_PROVINCE,
# PANEL_REGION, NORMES, DICTIONNAIRE). Les données synthétiques sont écrites
# dans ce format ; vos données réelles (Collecte_data_rempli_v3.xlsx) aussi.
# Remplacer les unes par les autres = changer un chemin dans config.yml.

# Variables saisies (non calculées) attendues dans PANEL_PROVINCE.
VARIABLES_SAISIE <- c(
  "eff_g", "eff_f", "eff_t", "eff_public_t", "eff_prive_t", "eff_communautaire_t",
  "eff_urbain_t", "eff_rural_t", "nouv_cp1_g", "nouv_cp1_f", "nouv_cp1_t",
  "redoublants_cm2_t", "eff_cm2_t", "tba_g", "tba_f", "tba_t", "tbs_g", "tbs_f",
  "tbs_t", "tap_g", "tap_f", "tap_t", "cep_g", "cep_f", "cep_t", "ips_tba",
  "ips_tbs", "ips_tap", "nb_etab_public", "nb_etab_prive", "nb_etab_communautaire",
  "nb_etab_t", "nb_salles_t", "nb_salles_bon_etat", "nb_salles_abris",
  "nb_groupes_pedago", "nb_ens_g", "nb_ens_f", "nb_ens_t", "nb_ens_public",
  "nb_ens_qualifies", "nb_etab_eau", "nb_etab_latrines", "nb_etab_electricite",
  "nb_etab_cantine", "nb_latrines", "nb_tables_bancs", "pop_totale",
  "pop_age_scolaire", "pop_age_entree", "pop_age_fin", "superficie_km2",
  "nb_etab_fermes", "nb_etab_fonctionnels", "nb_eleves_affectes", "nb_pdi",
  "nb_pdi_age_scolaire")
# Variables de « comptage » : elles s'additionnent pour agréger les territoires.
VARIABLES_ADDITIVES <- setdiff(VARIABLES_SAISIE, c(
  "tba_g", "tba_f", "tba_t", "tbs_g", "tbs_f", "tbs_t", "tap_g", "tap_f", "tap_t",
  "cep_g", "cep_f", "cep_t", "ips_tba", "ips_tbs", "ips_tap"))

#' Lit une feuille dont la ligne d'en-tête est repérée par une colonne clé
#' (robuste aux lignes de titre ajoutées au-dessus du tableau).
lire_feuille <- function(chemin, feuille, colonne_cle, max_lignes = 15) {
  if (!feuille %in% readxl::excel_sheets(chemin)) return(NULL)
  tete <- suppressMessages(readxl::read_excel(chemin, sheet = feuille, col_names = FALSE,
                                              n_max = max_lignes, .name_repair = "minimal"))
  ligne <- which(apply(tete, 1, function(r) any(trimws(as.character(r)) == colonne_cle)))[1]
  if (is.na(ligne)) stop(sprintf("Feuille %s : en-tête « %s » introuvable.", feuille, colonne_cle), call. = FALSE)
  df <- suppressMessages(suppressWarnings(readxl::read_excel(
    chemin, sheet = feuille, skip = ligne - 1, col_names = TRUE, guess_max = 5000,
    .name_repair = "minimal")))
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  names(df) <- trimws(names(df))
  df[, names(df) != "" & !duplicated(names(df)), drop = FALSE]
}

#' Convertit en numérique (texte « 1 234,5 » toléré).
en_numerique <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  x <- gsub("[   ]", "", as.character(x))
  x <- gsub(",", ".", x)
  x[x %in% c("", "NA", "-", "–", "ND", "nd")] <- NA
  suppressWarnings(as.numeric(x))
}

#' Lit et normalise un panel (PANEL_PROVINCE ou PANEL_REGION).
lire_panel <- function(chemin, feuille) {
  p <- lire_feuille(chemin, feuille, "id_ligne")
  if (is.null(p)) return(NULL)
  verifier_colonnes(p, c("cycle", "annee_debut", "niveau_geo", "code_geo"), feuille)
  p <- p[!is.na(p$code_geo) & !is.na(p$cycle) & p$cycle %in% names(CYCLES), , drop = FALSE]
  p$annee_debut <- as.integer(en_numerique(p$annee_debut))
  for (v in intersect(VARIABLES_SAISIE, names(p))) p[[v]] <- en_numerique(p[[v]])
  # Colonnes absentes (base partielle) créées vides : mode dégradé, sans plantage.
  for (v in setdiff(VARIABLES_SAISIE, names(p))) p[[v]] <- NA_real_
  garder <- c("id_ligne", "cycle", "annee_scolaire", "annee_debut", "niveau_geo", "code_geo",
              "nom_region", "nom_province", VARIABLES_SAISIE,
              intersect(c("qualite", "sources", "remarque"), names(p)))
  p <- p[, intersect(garder, names(p)), drop = FALSE]
  p$cycle <- factor(p$cycle, levels = names(CYCLES))
  p
}

#' Lit la base complète au format EXX_01.
lire_base_exx <- function(chemin) {
  if (!file.exists(chemin))
    stop("Base introuvable : ", chemin, "\nVérifiez config/config.yml (section donnees).", call. = FALSE)
  geo <- lire_feuille(chemin, "DIM_GEO", "code_geo")
  geo <- geo[!is.na(geo$code_geo) & geo$niveau_geo %in% c("NATIONAL", "REGION", "PROVINCE"),
             c("code_geo", "nom_geo", "niveau_geo", "code_parent", "region_rattachement")]
  panel <- lire_panel(chemin, "PANEL_PROVINCE")
  if (is.null(panel) || !nrow(panel)) stop("PANEL_PROVINCE vide ou absent dans ", chemin, call. = FALSE)
  panel <- panel[panel$niveau_geo == "PROVINCE", , drop = FALSE]
  # Rattachement à la région depuis DIM_GEO (référentiel, plus sûr que le texte saisi)
  prov <- geo[geo$niveau_geo == "PROVINCE", ]
  panel$code_region <- prov$code_parent[match(panel$code_geo, prov$code_geo)]
  panel$nom_province <- prov$nom_geo[match(panel$code_geo, prov$code_geo)]
  panel$nom_region <- unname(REGIONS[panel$code_region])
  panel_region <- tryCatch(lire_panel(chemin, "PANEL_REGION"), error = function(e) NULL)
  normes <- tryCatch(lire_feuille(chemin, "NORMES", "code_parametre"), error = function(e) NULL)
  dico <- tryCatch(lire_feuille(chemin, "DICTIONNAIRE", "variable"), error = function(e) NULL)
  journaliser("Chargement", sprintf("%s : %d lignes PANEL_PROVINCE, %d provinces, années %s–%s",
                                    basename(chemin), nrow(panel), length(unique(panel$code_geo)),
                                    min(panel$annee_debut, na.rm = TRUE), max(panel$annee_debut, na.rm = TRUE)))
  list(geo = geo, panel = panel, panel_region = panel_region,
       normes = preparer_normes(normes), dictionnaire = dico)
}

#' Normes : valeurs de la feuille NORMES, complétées par les valeurs de démonstration.
preparer_normes <- function(normes) {
  n <- NORMES_DEFAUT
  n$source <- "Valeur par défaut de l'application (démonstration)"
  if (!is.null(normes) && nrow(normes) && all(c("code_parametre", "cycle", "valeur") %in% names(normes))) {
    normes$valeur <- en_numerique(normes$valeur)
    normes <- normes[!is.na(normes$valeur) & !is.na(normes$code_parametre), , drop = FALSE]
    for (i in seq_len(nrow(normes))) {
      cyc <- if (identical(normes$cycle[i], "TOUS")) unique(NORMES_DEFAUT$cycle) else normes$cycle[i]
      k <- n$code_parametre == normes$code_parametre[i] & n$cycle %in% cyc
      src <- paste("Feuille NORMES :", if ("source" %in% names(normes)) normes$source[i] %||% "" else "")
      if (any(k)) { n$valeur[k] <- normes$valeur[i]; n$source[k] <- src }
      else n <- rbind(n, data.frame(code_parametre = normes$code_parametre[i], cycle = cyc,
                                    valeur = normes$valeur[i], source = src, stringsAsFactors = FALSE))
    }
  }
  n
}

#' Valeur d'une norme pour un vecteur de cycles.
norme <- function(normes, code, cycles) {
  cycles <- as.character(cycles)
  sous <- normes[normes$code_parametre == code, ]
  v <- sous$valeur[match(cycles, sous$cycle)]
  if (anyNA(v)) {
    d <- NORMES_DEFAUT[NORMES_DEFAUT$code_parametre == code, ]
    v[is.na(v)] <- d$valeur[match(cycles[is.na(v)], d$cycle)]
  }
  v
}

#' Établissements (optionnel) : une ligne par établissement × année.
lire_etablissements <- function(chemin) {
  if (is.null(chemin) || !nzchar(chemin) || !file.exists(chemin)) return(NULL)
  e <- as.data.frame(data.table::fread(chemin, encoding = "UTF-8", showProgress = FALSE))
  verifier_colonnes(e, c("id_etab", "code_province", "code_commune", "cycle", "annee_debut"), "établissements")
  e$cycle <- factor(e$cycle, levels = names(CYCLES))
  e
}

#' Variables de contexte (pauvreté, urbanisation…) — optionnel.
lire_contexte <- function(chemin) {
  if (is.null(chemin) || !nzchar(chemin) || !file.exists(chemin)) return(NULL)
  ctx <- as.data.frame(data.table::fread(chemin, encoding = "UTF-8", showProgress = FALSE))
  verifier_colonnes(ctx, c("code_geo", "annee_debut"), "contexte")
  ctx
}

#' Géométries : couches regions / provinces / communes d'un GeoPackage,
#' ou fichier de limites (ex. COD-AB) rapproché par le NOM des provinces.
lire_geometries <- function(chemin, geo) {
  if (is.null(chemin) || !nzchar(chemin) || !file.exists(chemin)) return(NULL)
  couches <- sf::st_layers(chemin)$name
  lire <- function(n) if (n %in% couches) sf::st_transform(sf::st_read(chemin, layer = n, quiet = TRUE), 4326) else NULL
  if ("provinces" %in% couches) {
    g <- list(regions = lire("regions"), provinces = lire("provinces"), communes = lire("communes"))
  } else {
    # Fichier externe : recherche d'une colonne de noms correspondant aux provinces
    brut <- sf::st_transform(sf::st_read(chemin, quiet = TRUE), 4326)
    prov <- geo[geo$niveau_geo == "PROVINCE", ]
    cible <- normaliser_nom(prov$nom_geo)
    col <- names(brut)[vapply(names(brut), function(n) {
      is.character(brut[[n]]) && mean(normaliser_nom(brut[[n]]) %in% cible) > 0.6 }, logical(1))][1]
    if (is.na(col)) { warning("Aucune colonne de noms de provinces reconnue dans ", chemin); return(NULL) }
    brut$code_geo <- prov$code_geo[match(normaliser_nom(brut[[col]]), cible)]
    brut <- brut[!is.na(brut$code_geo), ]
    provinces <- stats::aggregate(brut["code_geo"], by = list(code = brut$code_geo), FUN = function(x) x[1])
    provinces$code_geo <- provinces$code; provinces$code <- NULL
    provinces$superficie_km2 <- as.numeric(sf::st_area(sf::st_transform(provinces, 32630))) / 1e6
    provinces$approximatif <- FALSE
    g <- list(provinces = provinces, regions = NULL, communes = NULL)
  }
  if (is.null(g$provinces)) return(NULL)
  prov <- geo[geo$niveau_geo == "PROVINCE", ]
  g$provinces$nom <- prov$nom_geo[match(g$provinces$code_geo, prov$code_geo)]
  g$provinces$code_region <- prov$code_parent[match(g$provinces$code_geo, prov$code_geo)]
  if (is.null(g$regions)) {
    g$regions <- stats::aggregate(g$provinces["code_region"], by = list(code_geo = g$provinces$code_region),
                                  FUN = function(x) x[1])
    g$regions$code_region <- NULL
  }
  g$regions$nom <- unname(REGIONS[g$regions$code_geo])
  g
}

#' Point d'entrée unique : charge TOUT ce dont l'application a besoin.
#' Résultat mis en cache (data/prepare/) tant que les fichiers ne changent pas.
charger_donnees <- function(cfg = lire_config(), forcer = FALSE) {
  a <- cfg$actif
  fichiers <- unlist(a[c("base_exx", "etablissements", "contexte", "geometries")])
  fichiers <- fichiers[nzchar(fichiers) & file.exists(fichiers)]
  empreinte <- digest::digest(c(cfg$source_donnees, fichiers,
                                as.character(file.info(fichiers)$mtime), "v1"))
  cache <- file.path("data/prepare", paste0("donnees_", cfg$source_donnees, ".rds"))
  if (!forcer && file.exists(cache)) {
    d <- tryCatch(readRDS(cache), error = function(e) NULL)
    if (!is.null(d) && identical(d$meta$empreinte, empreinte)) return(d)
  }
  base <- lire_base_exx(a$base_exx)
  d <- c(base, list(
    etablissements = lire_etablissements(a$etablissements),
    contexte = lire_contexte(a$contexte),
    geometries = tryCatch(lire_geometries(a$geometries, base$geo),
                          error = function(e) { warning(conditionMessage(e)); NULL })
  ))
  d$meta <- list(type = a$type, mention = a$mention, fichier = a$base_exx,
                 date_fichier = file.info(a$base_exx)$mtime, empreinte = empreinte,
                 annees = sort(unique(d$panel$annee_debut)),
                 charge_le = Sys.time(), journal = JOURNAL$entrees)
  dir.create(dirname(cache), showWarnings = FALSE, recursive = TRUE)
  saveRDS(d, cache)
  d
}
