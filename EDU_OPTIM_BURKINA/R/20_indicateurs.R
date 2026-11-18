# ==========================================================================
# 20_indicateurs.R — Indicateurs, déficits et agrégations territoriales
# ==========================================================================
# Toutes les formules sont documentées dans CATALOGUE_INDICATEURS (affiché dans
# le module Documentation). Les déficits sont calculés au niveau le plus fin
# disponible puis ADDITIONNÉS : un excédent dans une province ne compense pas
# un déficit dans une autre (les salles ne se déplacent pas).

CATALOGUE_INDICATEURS <- utils::read.csv(text = "
code;libelle;dimension;formule;sens;unite
ratio_eleves_salle;Élèves par salle de classe;Infrastructures;eff_t / nb_salles_t;1;élèves
ratio_eleves_salle_utilisable;Élèves par salle en bon état;Infrastructures;eff_t / nb_salles_bon_etat;1;élèves
part_salles_mauvais;Part de salles en mauvais état ou précaires;Infrastructures;(nb_salles_t − nb_salles_bon_etat) / nb_salles_t;1;%
taux_deficit_salles;Taux de déficit en salles;Infrastructures;deficit_salles / besoin_salles;1;%
ratio_eleves_ens;Élèves par enseignant;Ressources humaines;eff_t / nb_ens_t;1;élèves
part_ens_non_qualifies;Part d'enseignants non qualifiés;Ressources humaines;1 − nb_ens_qualifies / nb_ens_t;1;%
ratio_eleves_table_banc;Élèves par table-banc;Équipements;eff_t / nb_tables_bancs;1;élèves
ratio_eleves_latrine;Élèves par cabine de latrine;Équipements;eff_t / nb_latrines;1;élèves
part_sans_latrines;Part d'établissements sans latrines;Équipements;1 − nb_etab_latrines / établissements fonctionnels;1;%
part_sans_eau;Part d'établissements sans point d'eau;Conditions d'apprentissage;1 − nb_etab_eau / établissements fonctionnels;1;%
part_sans_electricite;Part d'établissements sans électricité;Conditions d'apprentissage;1 − nb_etab_electricite / établissements fonctionnels;1;%
part_sans_cantine;Part d'établissements sans cantine;Conditions d'apprentissage;1 − nb_etab_cantine / établissements fonctionnels;1;%
ratio_eleves_classe;Élèves par groupe pédagogique;Conditions d'apprentissage;eff_t / nb_groupes_pedago;1;élèves
tbs;Taux brut de scolarisation;Accès et couverture;100 × eff_t / pop_age_scolaire;-1;%
taux_fonctionnalite;Taux de fonctionnalité des établissements;Accès et couverture;nb_etab_fonctionnels / nb_etab_t;-1;%
etab_100km2;Établissements pour 100 km²;Accès et couverture;100 × nb_etab_t / superficie_km2;-1;étab.
part_eleves_affectes;Part des élèves touchés par les fermetures;Accès et couverture;nb_eleves_affectes / (eff_t + nb_eleves_affectes);1;%
", sep = ";", stringsAsFactors = FALSE, encoding = "UTF-8")

DEFICITS <- data.frame(
  code = c("deficit_salles", "deficit_enseignants", "deficit_tables_bancs", "deficit_latrines",
           "deficit_eau", "deficit_electricite", "deficit_cantines"),
  ressource = c("salles", "enseignants", "tables_bancs", "latrines", "eau", "electricite", "cantines"),
  libelle = c("Salles de classe", "Enseignants", "Tables-bancs", "Cabines de latrines",
              "Établissements sans eau", "Établissements sans électricité", "Établissements sans cantine"),
  formule = c("max(0, ⌈eff_t / taille_classe_max⌉ − nb_salles_bon_etat)",
              "max(0, ⌈eff_t / ratio_eleves_enseignant⌉ − nb_ens_t)",
              "max(0, ⌈eff_t / places_table_banc⌉ − nb_tables_bancs)",
              "max(0, ⌈eff_t / eleves_par_latrine⌉ − nb_latrines)",
              "fonctionnels − nb_etab_eau", "fonctionnels − nb_etab_electricite",
              "fonctionnels − nb_etab_cantine"),
  stringsAsFactors = FALSE)

# Stocks auxquels on rapporte des élèves : un ratio agrégé ne compte que les
# élèves des unités où le stock est observé (colonnes eff_avec_<stock>).
STOCKS_RATIO <- c("nb_salles_t", "nb_salles_bon_etat", "nb_ens_t", "nb_tables_bancs",
                  "nb_latrines", "nb_groupes_pedago")
# Équipements rapportés aux établissements fonctionnels : même principe
# (colonnes fonc_avec_<équipement>), sinon les structures du préscolaire sans
# données d'équipement entrent au dénominateur de l'agrégat « Tous cycles ».
EQUIPEMENTS_ETAB <- c("nb_etab_eau", "nb_etab_electricite", "nb_etab_latrines", "nb_etab_cantine")

COLONNES_DEFICIT <- c("besoin_salles", "deficit_salles", "besoin_enseignants", "deficit_enseignants",
                      "besoin_tables_bancs", "deficit_tables_bancs", "besoin_latrines", "deficit_latrines",
                      "deficit_eau", "deficit_electricite", "deficit_cantines")

#' Calcule ratios, taux de couverture et déficits.
#' @param df       panel (province ou établissement) au format EXX_01.
#' @param normes   table des normes (preparer_normes()).
#' @param deficits TRUE = calcule les déficits ; FALSE = les conserve (agrégats).
calculer_indicateurs <- function(df, normes, deficits = TRUE) {
  cyc <- as.character(df$cycle)
  fonc <- ifelse(is.na(df$nb_etab_fonctionnels), df$nb_etab_t, df$nb_etab_fonctionnels)
  # Élèves rapportés à un stock : seuls comptent les élèves des unités où ce stock
  # est observé (eff_avec_* sommées par agreger()). Sans cela, un agrégat mélange
  # des élèves sans salles ni enseignants connus (post-primaire, secondaire).
  eff_pour <- function(stock) {
    h <- paste0("eff_avec_", stock)
    if (h %in% names(df)) df[[h]] else ifelse(is.na(df[[stock]]), NA_real_, df$eff_t)
  }
  df$ratio_eleves_salle <- diviser(eff_pour("nb_salles_t"), df$nb_salles_t)
  df$ratio_eleves_salle_utilisable <- diviser(eff_pour("nb_salles_bon_etat"), df$nb_salles_bon_etat)
  df$nb_salles_mauvais <- df$nb_salles_t - df$nb_salles_bon_etat
  df$part_salles_mauvais <- diviser(df$nb_salles_mauvais, df$nb_salles_t)
  df$ratio_eleves_ens <- diviser(eff_pour("nb_ens_t"), df$nb_ens_t)
  df$part_ens_non_qualifies <- 1 - diviser(df$nb_ens_qualifies, df$nb_ens_t)
  df$ratio_eleves_table_banc <- diviser(eff_pour("nb_tables_bancs"), df$nb_tables_bancs)
  df$ratio_eleves_latrine <- diviser(eff_pour("nb_latrines"), df$nb_latrines)
  df$ratio_eleves_classe <- diviser(eff_pour("nb_groupes_pedago"), df$nb_groupes_pedago)
  fonc_pour <- function(eq) {
    h <- paste0("fonc_avec_", eq)
    if (h %in% names(df)) df[[h]] else ifelse(is.na(df[[eq]]), NA_real_, fonc)
  }
  df$taux_eau <- diviser(df$nb_etab_eau, fonc_pour("nb_etab_eau"))
  df$taux_electricite <- diviser(df$nb_etab_electricite, fonc_pour("nb_etab_electricite"))
  df$taux_latrines <- diviser(df$nb_etab_latrines, fonc_pour("nb_etab_latrines"))
  df$taux_cantine <- diviser(df$nb_etab_cantine, fonc_pour("nb_etab_cantine"))
  df$part_sans_eau <- 1 - df$taux_eau
  df$part_sans_electricite <- 1 - df$taux_electricite
  df$part_sans_cantine <- 1 - df$taux_cantine
  df$part_sans_latrines <- 1 - df$taux_latrines
  # Non renseigné quand les fermetures sont inconnues (sinon 100 % par construction)
  df$taux_fonctionnalite <- ifelse(is.na(df$nb_etab_fonctionnels), NA_real_, diviser(fonc, df$nb_etab_t))
  df$etab_100km2 <- 100 * diviser(df$nb_etab_t, df$superficie_km2)
  df$tbs <- ifelse(is.na(df$pop_age_scolaire), df$tbs_t, 100 * diviser(df$eff_t, df$pop_age_scolaire))
  df$part_eleves_affectes <- diviser(df$nb_eleves_affectes, df$eff_t + df$nb_eleves_affectes)
  df$part_filles <- diviser(df$eff_f, df$eff_t)
  df$part_urbain <- diviser(df$eff_urbain_t, df$eff_t)
  if (deficits) {
    tc  <- norme(normes, "TAILLE_CLASSE_MAX", cyc)
    ree <- norme(normes, "RATIO_ELEVES_ENSEIGNANT", cyc)
    ptb <- norme(normes, "PLACES_TABLE_BANC", cyc)
    epl <- norme(normes, "ELEVES_PAR_LATRINE", cyc)
    # Besoin évalué seulement si le stock correspondant est connu (déficit calculable) :
    # un besoin sans stock gonflerait les agrégats et sous-estimerait les taux de déficit.
    df$besoin_salles <- ifelse(is.na(df$nb_salles_bon_etat), NA_real_, ceiling(df$eff_t / tc))
    df$deficit_salles <- pmax(0, df$besoin_salles - df$nb_salles_bon_etat)
    df$besoin_enseignants <- ifelse(is.na(df$nb_ens_t), NA_real_, ceiling(df$eff_t / ree))
    df$deficit_enseignants <- pmax(0, df$besoin_enseignants - df$nb_ens_t)
    df$besoin_tables_bancs <- ifelse(cyc == "PRESCOLAIRE" | is.na(df$nb_tables_bancs), NA_real_, ceiling(df$eff_t / ptb))
    df$deficit_tables_bancs <- pmax(0, df$besoin_tables_bancs - df$nb_tables_bancs)
    df$besoin_latrines <- ifelse(is.na(df$nb_latrines), NA_real_, ceiling(df$eff_t / epl))
    df$deficit_latrines <- pmax(0, df$besoin_latrines - df$nb_latrines)
    df$deficit_eau <- pmax(0, fonc - df$nb_etab_eau)
    df$deficit_electricite <- pmax(0, fonc - df$nb_etab_electricite)
    df$deficit_cantines <- pmax(0, fonc - df$nb_etab_cantine)
  }
  df$taux_deficit_salles <- diviser(df$deficit_salles, df$besoin_salles)
  df$taux_deficit_enseignants <- diviser(df$deficit_enseignants, df$besoin_enseignants)
  df
}

#' Agrège un panel d'indicateurs à un niveau supérieur.
#' @param df     panel province (déjà passé par calculer_indicateurs).
#' @param niveau "region", "national" ou "province" (pas d'agrégation géographique).
#' @param tous_cycles TRUE = ajoute aussi l'agrégat « TOUS » cycles confondus.
#' @param groupe nom d'une colonne de regroupement personnalisé (ex. code_commune).
agreger <- function(df, niveau = c("province", "region", "national"), normes, tous_cycles = FALSE,
                    groupe = NULL) {
  niveau <- match.arg(niveau)
  df$code_agr <- switch(niveau, province = df$code_geo, region = df$code_region, national = rep("NAT", nrow(df)))
  if (!is.null(groupe)) df$code_agr <- df[[groupe]]
  df$cycle <- as.character(df$cycle)
  for (s in STOCKS_RATIO) if (s %in% names(df))
    df[[paste0("eff_avec_", s)]] <- ifelse(is.na(df[[s]]), NA_real_, df$eff_t)
  fonc0 <- ifelse(is.na(df$nb_etab_fonctionnels), df$nb_etab_t, df$nb_etab_fonctionnels)
  for (e in EQUIPEMENTS_ETAB) if (e %in% names(df))
    df[[paste0("fonc_avec_", e)]] <- ifelse(is.na(df[[e]]), NA_real_, fonc0)
  if (tous_cycles) {
    t <- df; t$cycle <- "TOUS"
    # « Tous cycles » : seulement les années où chaque cycle a des effectifs dans la
    # province (sinon rupture de série : post-primaire et secondaire absents avant 2018).
    cle_ty <- paste(t$code_geo, t$annee_debut)
    n_cyc <- tapply(!is.na(t$eff_t), cle_ty, sum)
    t <- t[n_cyc[cle_ty] == length(unique(df$cycle)), , drop = FALSE]
    df <- rbind(df, t)
  }
  cles <- paste(df$cycle, df$code_agr, df$annee_debut, sep = "|")
  vars <- intersect(c(VARIABLES_ADDITIVES, COLONNES_DEFICIT, paste0("eff_avec_", STOCKS_RATIO),
                    paste0("fonc_avec_", EQUIPEMENTS_ETAB)), names(df))
  m <- rowsum(as.matrix(df[, vars]), cles, na.rm = TRUE)
  n_ok <- rowsum(1 * !is.na(as.matrix(df[, vars])), cles)
  m[n_ok == 0] <- NA
  out <- as.data.frame(m)
  pz <- do.call(rbind, strsplit(rownames(m), "|", fixed = TRUE))
  out$cycle <- pz[, 1]; out$code_geo <- pz[, 2]; out$annee_debut <- as.integer(pz[, 3])
  rownames(out) <- NULL
  if (tous_cycles) {
    # Pour « TOUS » : superficie, population totale et déplacés sont des
    # attributs du territoire (répétés dans chaque cycle) : on ne les additionne
    # pas entre cycles, on les reprend du cycle primaire.
    un <- df[df$cycle == "PRIMAIRE", ]
    k <- paste(un$code_agr, un$annee_debut)
    kt <- paste(out$code_geo, out$annee_debut)
    for (v in intersect(c("superficie_km2", "pop_totale", "nb_pdi", "nb_pdi_age_scolaire"), names(out))) {
      val <- tapply(un[[v]], k, function(z) if (all(is.na(z))) NA else sum(z, na.rm = TRUE))
      out[[v]][out$cycle == "TOUS"] <- val[kt[out$cycle == "TOUS"]]
    }
  }
  # Taux recalculés à partir des sommes
  out$tbs_t <- 100 * diviser(out$eff_t, out$pop_age_scolaire)
  out$tba_t <- 100 * diviser(out$nouv_cp1_t, out$pop_age_entree)
  out$tap_t <- 100 * diviser(pmax(0, out$eff_cm2_t - out$redoublants_cm2_t), out$pop_age_fin)
  out$ips_tbs <- diviser(diviser(out$eff_f, out$pop_age_scolaire * 0.495), diviser(out$eff_g, out$pop_age_scolaire * 0.505))
  out$niveau_geo <- toupper(niveau)
  if (niveau == "region") out$nom <- unname(REGIONS[out$code_geo])
  if (niveau == "national") out$nom <- "Burkina Faso"
  out <- calculer_indicateurs(out, normes, deficits = FALSE)
  out$cycle <- factor(out$cycle, levels = c(names(CYCLES), "TOUS"))
  out
}

#' Ajoute les variables de contexte (pauvreté, insécurité…) et dérivées.
ajouter_contexte <- function(p, contexte) {
  if (!is.null(contexte)) {
    k <- match(paste(p$code_geo, p$annee_debut), paste(contexte$code_geo, contexte$annee_debut))
    for (v in setdiff(names(contexte), c("code_geo", "annee_debut"))) p[[v]] <- contexte[[v]][k]
  }
  p$part_pdi <- diviser(p$nb_pdi, p$pop_totale)
  p$densite_pop <- diviser(p$pop_totale, p$superficie_km2)
  p
}

#' Prépare tous les niveaux une fois pour toutes (appelé au démarrage).
preparer_indicateurs <- function(d) {
  p <- calculer_indicateurs(d$panel, d$normes)
  p$nom <- p$nom_province
  p <- ajouter_contexte(p, d$contexte)
  prov_tous <- agreger(p, "province", d$normes, tous_cycles = TRUE)
  prov_tous <- prov_tous[prov_tous$cycle == "TOUS", ]
  prov_tous$code_region <- p$code_region[match(prov_tous$code_geo, p$code_geo)]
  prov_tous$nom <- p$nom_province[match(prov_tous$code_geo, p$code_geo)]
  prov_tous$niveau_geo <- "PROVINCE"
  prov_tous <- ajouter_contexte(prov_tous, d$contexte)
  communes <- NULL
  if (!is.null(d$etablissements)) {
    e <- d$etablissements
    e$nb_etab_t <- 1; e$nb_etab_fermes <- e$ferme; e$nb_etab_fonctionnels <- 1 - e$ferme
    e$nb_etab_eau <- e$eau; e$nb_etab_electricite <- e$electricite
    e$nb_etab_latrines <- e$latrines; e$nb_etab_cantine <- e$cantine
    e$nb_eleves_affectes <- e$eleves_affectes
    e$eff_urbain_t <- e$eff_t * (e$milieu == "URBAIN"); e$eff_rural_t <- e$eff_t * (e$milieu == "RURAL")
    for (v in setdiff(VARIABLES_SAISIE, names(e))) e[[v]] <- NA_real_
    e <- calculer_indicateurs(e, d$normes)
    e$code_geo <- e$code_province
    communes <- agreger(e, "province", d$normes, groupe = "code_commune")
    communes$code_province <- e$code_province[match(communes$code_geo, e$code_commune)]
    communes$code_region <- e$code_region[match(communes$code_geo, e$code_commune)]
    communes$nom <- e$nom_commune[match(communes$code_geo, e$code_commune)]
    communes$niveau_geo <- "COMMUNE"
    if (!is.null(d$geometries$communes)) {
      sup <- d$geometries$communes$superficie_km2[match(communes$code_geo, d$geometries$communes$code_geo)]
      communes$superficie_km2 <- sup
      communes$etab_100km2 <- 100 * diviser(communes$nb_etab_t, sup)
    }
    d$etablissements <- e
  }
  list(province = p, province_tous = prov_tous,
       region = agreger(p, "region", d$normes, tous_cycles = TRUE),
       national = agreger(p, "national", d$normes, tous_cycles = TRUE),
       commune = communes, etablissements = d$etablissements)
}
