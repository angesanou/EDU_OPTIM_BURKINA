# =============================================================================
#  EDU-OPTIM Burkina - Data Challenge 2026 (Defi 3)
#  02_traitement_donnees.R : traitement complet de la base de collecte
# -----------------------------------------------------------------------------
#  Entree  : Collecte_data_rempli_v6.xlsx (onglets PRESCOLAIRE, PRIMAIRE,
#            POST_PRIMAIRE, SECONDAIRE, PRE/PRI/PPS_REGION, ESU_FERMETURES,
#            ESTIMATIONS)
#  Sorties : <dossier_sortie>/
#              base_traitee.xlsx          panel traite + tracabilite
#              base_traitee.rds           memes objets pour la suite de la chaine R
#              panel_province_traite.csv  panel traite (separateur ;)
#              rapport_traitement.md      synthese chiffree pour la documentation
#
#  Usage   : Rscript R/02_traitement_donnees.R [fichier_entree] [dossier_sortie]
#            ou source("R/02_traitement_donnees.R") apres avoir modifie PARAM.
#
#  Principes (controles C1-C10 de l'architecture de stockage) :
#   1. Rien n'est corrige en silence : chaque modification de cellule est
#      journalisee (onglet IMPUTATIONS), chaque controle echoue est liste
#      (onglet ANOMALIES) avec sa gravite et la decision prise.
#   2. Chaque cellule porte une origine (onglet ORIGINE_CELLULES) :
#        OBS            valeur observee (source officielle)
#        EST            valeur estimee par desagregation (onglet ESTIMATIONS)
#        DED            valeur deduite exactement (identite comptable, definition)
#        IMP_INTERP     interpolation lineaire dans la serie de la province
#        IMP_TENDANCE   annee de bord : valeur voisine x evolution regionale
#        IMP_PONDERE    taux total = moyenne des taux G et F ponderee
#        IMP_RETRO      population totale retropolee (croissance 2020-2024)
#        IMP_CALAGE     composantes imputees recalees sur le total observe
#        IMP_RAPPORT    population incoherente remplacee (rapport au primaire)
#                       et taux qui en dependent recales
#        NEUTRALISE     valeur rejetee (controle bloquant) et non reimputee
#        NON_APPLICABLE variable sans objet pour le cycle
#        NA_LIGNE       aucune donnee scolaire pour la ligne (hors couverture)
#        NA_VARIABLE    variable jamais observee pour le cycle
#        NA_PERIODE     annee entiere manquante pour toutes les provinces
#        NA_PONCTUEL    trou isole (d'autres provinces sont observees cette annee)
#   3. On impute des effectifs puis on recalcule les taux ; on ne repartit
#      jamais un taux ; on ne recopie jamais un chiffre regional sur une province.
#   4. Les ruptures liees a l'insecurite (fermetures massives) sont des
#      donnees : elles sont signalees, jamais lissees.
#   5. Seuls les trous ponctuels et les valeurs rejetees sont imputes ; les
#      annees ou variables entierement absentes restent vides (sauf la
#      population totale, retropolee si PARAM$retropoler_pop_totale).
# =============================================================================

options(warn = 1, stringsAsFactors = FALSE)

pkgs <- c("readxl", "dplyr", "tidyr", "stringr", "stringi", "openxlsx")
manquants <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(manquants)) {
  stop("Packages manquants : ", paste(manquants, collapse = ", "),
       "\nInstaller avec : install.packages(c(",
       paste0('"', manquants, '"', collapse = ", "), "))")
}
suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr); library(stringr); library(openxlsx)
})

# =============================================================================
# 0. PARAMETRES
# =============================================================================
args <- commandArgs(trailingOnly = TRUE)
PARAM <- list(
  fichier_entree = if (length(args) >= 1) args[1] else "data/reelles/Collecte_data_rempli_v6.xlsx",
  dossier_sortie = if (length(args) >= 2) args[2] else "data/traitees",
  onglets_cycles = c("PRESCOLAIRE", "PRIMAIRE", "POST_PRIMAIRE", "SECONDAIRE"),
  annees         = 2015:2024,
  n_provinces    = 45,
  n_regions      = 13,
  ligne_codes    = 3,        # ligne des codes de variables (1 = groupes, 2 = libelles)

  # Tolerances des controles
  tol_abs        = 1,        # identites comptables : ecart absolu tolere
  tol_rel        = 0.005,    # ... ou relatif (0,5 % du total)
  tol_taux       = 1,        # taux publie vs recalcule (points de %)
  tol_ips        = 0.02,     # IPS publie vs taux F / taux G
  taux_min_ips   = 5,        # IPS non controle si un des taux < 5 % (bruit d'arrondi)
  bornes_taux    = list(tba = c(0, 250), tbs = c(0, 200), tns = c(0, 100),
                        tap = c(0, 200), cep = c(0, 100)),
  bornes_ips     = c(0.2, 2.5),
  bornes_ratios  = list(eleves_par_enseignant = c(5, 150), eleves_par_salle = c(5, 200),
                        eleves_par_classe = c(5, 200), part_filles = c(0.25, 0.70)),
  effectif_min_ratio = 100,  # ratios en coupe non controles sous cet effectif

  # Controles temporels
  seuil_variation    = 0.30, # variation annuelle signalee au-dela de 30 %
  seuil_pic          = 0.50, # pic : ecart > 50 % avec l'annee precedente ET suivante
  seuil_comouvement  = 0.15, # autre variable du coeur qui bouge de > 15 % dans le meme sens
  effectif_min_variation = 100,
  seuil_rupture_pop  = 0.10,
  # Les annuaires passent des projections RGPH 2006 aux projections RGPH 2019 en
  # 2021-2022 (mediane des provinces : -5 % au primaire, -6 % au secondaire).
  annee_rupture_pop  = 2021,
  seuil_z            = 4,    # z robuste (mediane / MAD) en coupe
  # Population d'age scolaire des cycles controlee par son rapport a la
  # population 6-11 ans de la meme province (stable d'une annee a l'autre).
  # 25 % isole les 10 provinces du post-primaire / secondaire 2024 (annuaire 2024-25).
  seuil_pop_rapport  = 0.25,
  cycles_controle_pop = c("POST_PRIMAIRE", "SECONDAIRE"),

  # Decisions de traitement
  max_trou_interp          = 2,     # interpolation si le trou fait au plus 2 ans
  neutraliser_pics_isoles  = FALSE, # FALSE = signaler seulement (C6 : "a examiner")
  recalculer_ips_incoherents = TRUE,
  retropoler_pop_totale    = TRUE,
  # Les annuaires ne comptent que les ecoles fonctionnelles : en 2021, 9 provinces
  # ont plus d'ecoles fermees (ESU) que d'ecoles recensees. Donc nb_etab_t
  # n'inclut pas les fermees : fonctionnelles = nb_etab_t ;
  # taux de fonctionnalite = nb_etab_t / (nb_etab_t + nb_etab_fermes).
  etab_t_inclut_fermes     = FALSE,
  tol_agregation           = 0.01,

  # Conversion au format EXX_01 lu par l'application Shiny (etape 14)
  exporter_exx            = TRUE,
  dossier_application     = "EDU_OPTIM_BURKINA",                     # relatif au dossier de travail
  exx_sortie              = "data/reelles/EXX_01_base_reelle.xlsx",  # relatif a l'application
  # L'application calcule taux_fonctionnalite = nb_etab_fonctionnels / nb_etab_t,
  # avec nb_etab_t = ouverts + fermes. Les annuaires ne comptent que les ouverts :
  # TRUE = nb_etab_t exporte = ouverts + fermes (ESU) quand les fermes sont connus.
  exx_total_inclut_fermes = TRUE,
  exx_activer_config      = TRUE   # bascule config.yml sur les donnees reelles
)

# =============================================================================
# 1. OUTILS
# =============================================================================
ETAT <- new.env()
ETAT$anom <- list(); ETAT$imput <- list(); ETAT$journal <- list()

noter_etape <- function(etape, libelle, n) {
  ETAT$journal[[length(ETAT$journal) + 1]] <- tibble(etape = etape, operation = libelle, n = n)
  message(sprintf("  [%s] %s : %s", etape, libelle, format(n, big.mark = " ")))
  invisible(n)
}

# d : tableau avec cycle, id_ligne, nom_region, nom_province, annee_debut
noter_anomalie <- function(d, regle, variables, valeur = NA_real_, attendu = NA_real_,
                           gravite, decision, detail = NA_character_) {
  if (is.null(d) || !nrow(d)) return(invisible(0L))
  n <- nrow(d)
  rep_n <- function(x) if (length(x) == 1) rep(x, n) else x
  valeur <- suppressWarnings(as.numeric(rep_n(valeur)))
  attendu <- suppressWarnings(as.numeric(rep_n(attendu)))
  ETAT$anom[[length(ETAT$anom) + 1]] <- tibble(
    cycle = as.character(d$cycle), id_ligne = as.character(d$id_ligne),
    nom_region = as.character(d$nom_region), nom_province = as.character(d$nom_province),
    annee_debut = as.integer(d$annee_debut), regle = regle,
    variables = rep_n(variables), valeur = valeur, attendu = attendu,
    ecart = valeur - attendu, gravite = gravite, decision = rep_n(decision),
    detail = as.character(rep_n(detail)))
  invisible(n)
}

parse_num <- function(x) {
  x <- str_trim(as.character(x))
  x <- str_replace_all(x, "[\u00a0\u202f ]", "")
  x <- str_replace(x, ",", ".")
  x[x %in% c("", "-", "\u2013", "ND", "nd", "NA", "n.d.", "...", "\u2026", "x", "X")] <- NA
  suppressWarnings(as.numeric(x))
}
non_numerique <- function(x) {
  t <- str_trim(as.character(x))
  !is.na(t) & !(t %in% c("", "-", "\u2013", "ND", "nd", "NA", "n.d.", "...", "\u2026", "x", "X")) &
    is.na(parse_num(t))
}
norm_nom <- function(x) toupper(str_squish(stringi::stri_trans_general(x, "Latin-ASCII")))
div <- function(a, b, k = 1) ifelse(!is.na(a) & !is.na(b) & b != 0, a / b * k, NA_real_)

lire_onglet <- function(f, onglet) {
  ent <- suppressMessages(read_excel(f, sheet = onglet, range = cell_rows(1:PARAM$ligne_codes),
                                     col_names = FALSE, col_types = "text"))
  groupe  <- as.character(unlist(ent[1, ]))
  libelle <- as.character(unlist(ent[2, ]))
  code    <- as.character(unlist(ent[PARAM$ligne_codes, ]))
  if (length(groupe) > 1) for (j in 2:length(groupe)) if (is.na(groupe[j])) groupe[j] <- groupe[j - 1]
  dico <- tibble(onglet = onglet, code = code, libelle = libelle, groupe = groupe,
                 num_groupe = suppressWarnings(as.integer(str_extract(groupe, "^\\d+")))) %>%
    filter(!is.na(code))
  don <- suppressMessages(read_excel(f, sheet = onglet, skip = PARAM$ligne_codes - 1,
                                     col_types = "text"))
  don <- as.data.frame(don)
  don <- don[!is.na(don[[1]]) & str_trim(don[[1]]) != "", , drop = FALSE]
  list(dico = dico, don = don)
}

# =============================================================================
# 2. LECTURE ET CONTROLES STRUCTURELS (C7 unicite, C8 codes, C9 tracabilite)
# =============================================================================
message("== 2. Lecture : ", PARAM$fichier_entree)
f <- PARAM$fichier_entree
if (!file.exists(f)) {
  # Recherche du fichier sous le dossier de travail (nom d'origine ou nom du depot
  # 01 conforme au reglement), puis choix manuel (RStudio)
  nom <- basename(f)
  noms <- unique(c(nom, "Collecte_data_rempli_v6.xlsx",
                   "Datagenius_01-collecte_D3_engineer_base-collecte-provinces-2015-2024_v6.xlsx"))
  motif <- paste0("^(", paste(gsub(".", "\\.", noms, fixed = TRUE), collapse = "|"), ")$")
  trouves <- list.files(getwd(), pattern = motif, recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  trouves <- trouves[!grepl("depots[/\\\\]", trouves)]   # jamais les copies des dossiers de depot
  trouves <- trouves[!grepl("~\\$", trouves)]
  if (length(trouves)) {
    f <- trouves[1]
    message("  Fichier trouve a un autre emplacement : ", f)
  } else if (interactive()) {
    message("  ", nom, " introuvable sous ", getwd(), " : choisir le fichier dans la fenetre.")
    f <- file.choose()
  } else {
    stop("Fichier introuvable : ", f, "\n  Dossier de travail actuel : ", getwd(),
         "\n  Lancer depuis la racine du projet, ou passer le chemin complet :",
         "\n  Rscript R/02_traitement_donnees.R \"C:/.../Collecte_data_rempli_v6.xlsx\" data/traitees")
  }
  PARAM$fichier_entree <- f
}
message("  Dossier de travail : ", getwd())
message("  Sorties dans : ", normalizePath(PARAM$dossier_sortie, mustWork = FALSE))
ONGLETS <- excel_sheets(f)
absents <- setdiff(c(PARAM$onglets_cycles, "ESTIMATIONS"), ONGLETS)
if (length(absents)) stop("Onglets absents : ", paste(absents, collapse = ", "))

LUS  <- setNames(lapply(PARAM$onglets_cycles, function(o) lire_onglet(f, o)), PARAM$onglets_cycles)
DICO <- bind_rows(lapply(LUS, `[[`, "dico"))
DICO_REF <- DICO %>% distinct(code, .keep_all = TRUE)

CLES    <- c("id_ligne", "cycle", "annee_scolaire", "annee_debut", "niveau_geo",
             "code_geo", "nom_region", "nom_province")
GESTION <- c("qualite", "sources", "remarque")
VARS          <- DICO_REF$code[DICO_REF$num_groupe %in% 1:7 & !DICO_REF$code %in% c(CLES, GESTION)]
VARS_DERIVEES <- DICO_REF$code[DICO_REF$num_groupe %in% 8:9]
VARS_SCOLAIRES <- DICO_REF$code[DICO_REF$num_groupe %in% 1:5]
VARS_DEMO      <- DICO_REF$code[DICO_REF$num_groupe == 6]
VARS_CONTEXTE  <- DICO_REF$code[DICO_REF$num_groupe == 7]
if (!length(VARS)) stop("Aucune variable de saisie reperee : verifier PARAM$ligne_codes")

brut <- bind_rows(lapply(LUS, `[[`, "don"))
for (v in setdiff(c(CLES, GESTION, VARS), names(brut))) {
  warning("Colonne absente de la collecte, creee vide : ", v); brut[[v]] <- NA_character_
}
noter_etape("2", "lignes lues (4 cycles)", nrow(brut))

P <- data.frame(
  id_ligne = str_trim(brut$id_ligne), cycle = str_trim(brut$cycle),
  annee_scolaire = str_trim(brut$annee_scolaire),
  annee_debut = as.integer(parse_num(brut$annee_debut)),
  niveau_geo = str_trim(brut$niveau_geo), code_geo = str_trim(brut$code_geo),
  nom_region = str_squish(brut$nom_region), nom_province = str_squish(brut$nom_province),
  qualite_source = str_trim(brut$qualite), sources = brut$sources, remarque = brut$remarque)
for (v in VARS) P[[v]] <- parse_num(brut[[v]])

# Valeurs non numeriques dans les colonnes de saisie
n_nn <- 0
for (v in VARS) {
  i <- which(non_numerique(brut[[v]]))
  if (length(i)) n_nn <- n_nn + noter_anomalie(P[i, ], "VALEUR_NON_NUMERIQUE", v, NA, NA,
    "BLOQUANT", "valeur illisible ignoree (cellule vide)", paste0("texte lu : ", brut[[v]][i]))
}
noter_etape("2", "cellules non numeriques ignorees", n_nn)

# Cycle declare = onglet
P$cycle[is.na(P$cycle)] <- rep(names(LUS), vapply(LUS, function(x) nrow(x$don), 1L))[is.na(P$cycle)]

# Referentiel geographique (C8) : un code = un nom de province et une region
REF_GEO <- P %>% filter(!is.na(code_geo)) %>% count(code_geo, nom_region, nom_province) %>%
  arrange(code_geo, desc(n))
multi <- REF_GEO %>% count(code_geo) %>% filter(n > 1)
if (nrow(multi)) {
  d <- P %>% filter(code_geo %in% multi$code_geo) %>% distinct(code_geo, .keep_all = TRUE)
  noter_anomalie(d, "REFERENTIEL_GEO", "nom_province/nom_region", gravite = "INFO",
                 decision = "nom le plus frequent retenu pour toutes les lignes du code")
}
REF_GEO <- REF_GEO %>% distinct(code_geo, .keep_all = TRUE) %>% select(-n)
if (nrow(REF_GEO) != PARAM$n_provinces)
  warning(sprintf("%d codes province trouves (attendu %d)", nrow(REF_GEO), PARAM$n_provinces))
if (n_distinct(REF_GEO$nom_region) != PARAM$n_regions)
  warning(sprintf("%d regions trouvees (attendu %d)", n_distinct(REF_GEO$nom_region), PARAM$n_regions))
P$nom_region   <- REF_GEO$nom_region[match(P$code_geo, REF_GEO$code_geo)]
P$nom_province <- REF_GEO$nom_province[match(P$code_geo, REF_GEO$code_geo)]

# Unicite (C7) : bloquant
dup_id  <- unique(P$id_ligne[duplicated(P$id_ligne)])
dup_cle <- duplicated(P[c("cycle", "code_geo", "annee_debut")])
if (length(dup_id) || any(dup_cle))
  stop("Doublons sur la cle (C7) : ", paste(head(c(dup_id, P$id_ligne[dup_cle]), 10), collapse = ", "))

# Coherence annee scolaire / annee de debut
att <- paste0(P$annee_debut, "-", P$annee_debut + 1)
i <- which(!is.na(P$annee_debut) & (is.na(P$annee_scolaire) | P$annee_scolaire != att))
if (length(i)) {
  noter_anomalie(P[i, ], "ANNEE_SCOLAIRE", "annee_scolaire", gravite = "INFO",
                 decision = "annee_scolaire recalculee a partir de annee_debut",
                 detail = paste0("lu : ", P$annee_scolaire[i]))
  P$annee_scolaire[i] <- att[i]
}

# Lignes hors periode : retirees
hors <- which(!P$annee_debut %in% PARAM$annees)
if (length(hors)) {
  noter_anomalie(P[hors, ], "ANNEE_HORS_PERIODE", "annee_debut", P$annee_debut[hors], NA,
                 "ALERTE", "ligne retiree du panel")
  P <- P[-hors, ]
}

# Grille complete 45 provinces x 10 annees x 4 cycles (C1)
prefixes <- P %>% mutate(pref = str_extract(id_ligne, "^[A-Z]+")) %>% count(cycle, pref) %>%
  arrange(cycle, desc(n)) %>% distinct(cycle, .keep_all = TRUE)
GRILLE <- expand_grid(cycle = PARAM$onglets_cycles, code_geo = REF_GEO$code_geo,
                      annee_debut = PARAM$annees)
manq <- anti_join(GRILLE, P, by = c("cycle", "code_geo", "annee_debut"))
if (nrow(manq)) {
  ajout <- manq %>% left_join(prefixes %>% select(cycle, pref), by = "cycle") %>%
    left_join(REF_GEO, by = "code_geo") %>%
    transmute(id_ligne = paste0(pref, "_", code_geo, "_", annee_debut), cycle,
              annee_scolaire = paste0(annee_debut, "-", annee_debut + 1), annee_debut,
              niveau_geo = "PROVINCE", code_geo, nom_region, nom_province,
              qualite_source = "MANQUANT", sources = NA_character_,
              remarque = "ligne ajoutee par le traitement (absente de la collecte)")
  P <- bind_rows(P, as.data.frame(ajout))
}
noter_etape("2", "lignes ajoutees pour completer la grille", nrow(manq))
P <- P[order(match(P$cycle, PARAM$onglets_cycles), P$code_geo, P$annee_debut), ]
rownames(P) <- NULL
N <- nrow(P)
CLE_SERIE <- paste(P$cycle, P$code_geo)          # une serie = cycle x province
CLE_LIGNE <- paste(P$cycle, P$id_ligne)

# Origine initiale des cellules
O <- as.data.frame(lapply(P[VARS], function(x) ifelse(is.na(x), NA_character_, "OBS")))

# Tracabilite (C9)
a_obs <- Reduce(`|`, lapply(VARS, function(v) O[[v]] %in% "OBS"))
i <- which(a_obs & (is.na(P$sources) | str_trim(P$sources) == ""))
noter_anomalie(P[i, ], "SOURCE_ABSENTE", "sources", gravite = "ALERTE",
               decision = "ligne conservee ; source a documenter")

# =============================================================================
# 3. INTEGRATION DES ESTIMATIONS (onglet ESTIMATIONS)
# =============================================================================
message("== 3. Estimations")
est_brut <- suppressMessages(read_excel(f, sheet = "ESTIMATIONS", col_types = "text"))
req <- c("onglet", "id_ligne", "variable", "methode", "valeur_estimee")
if (!all(req %in% names(est_brut))) stop("Colonnes attendues dans ESTIMATIONS : ", paste(req, collapse = ", "))
col_ou_na <- function(d, n) if (n %in% names(d)) d[[n]] else rep(NA_character_, nrow(d))
EST <- tibble(
  cycle = str_trim(est_brut$onglet), id_ligne = str_trim(est_brut$id_ligne),
  variable = str_trim(est_brut$variable), annee = as.integer(parse_num(col_ou_na(est_brut, "annee"))),
  methode = col_ou_na(est_brut, "methode"), annee_reference = col_ou_na(est_brut, "annee_reference"),
  valeur_estimee = parse_num(est_brut$valeur_estimee),
  variante = parse_num(col_ou_na(est_brut, "variante")),
  ecart_variante_pct = parse_num(col_ou_na(est_brut, "ecart_variante_%")),
  total_regional_observe = parse_num(col_ou_na(est_brut, "total_regional_observe")))

dup <- duplicated(EST[c("cycle", "id_ligne", "variable")])
if (any(dup)) warning(sum(dup), " estimations en double : premiere occurrence conservee")
EST <- EST[!dup, ]
inconnues <- EST %>% filter(!variable %in% VARS | !paste(cycle, id_ligne) %in% CLE_LIGNE)
if (nrow(inconnues)) warning(nrow(inconnues), " estimations sans ligne ou variable correspondante : ignorees")
EST <- EST %>% filter(variable %in% VARS, paste(cycle, id_ligne) %in% CLE_LIGNE, !is.na(valeur_estimee))

n_est <- 0; n_conf <- 0
for (v in unique(EST$variable)) {
  e <- EST[EST$variable == v, ]
  i <- match(paste(e$cycle, e$id_ligne), CLE_LIGNE)
  cell <- P[[v]][i]
  conf <- !is.na(cell) & abs(cell - e$valeur_estimee) > pmax(0.5, 0.001 * abs(e$valeur_estimee))
  if (any(conf)) n_conf <- n_conf + noter_anomalie(P[i[conf], ], "CONFLIT_CELLULE_ESTIMATION", v,
    cell[conf], e$valeur_estimee[conf], "INFO", "valeur de l'onglet ESTIMATIONS retenue")
  P[[v]][i] <- e$valeur_estimee
  O[[v]][i] <- "EST"
  n_est <- n_est + length(i)
}
noter_etape("3", "cellules estimees integrees", n_est)
noter_etape("3", "cellules dont la valeur differait de ESTIMATIONS", n_conf)

# =============================================================================
# 4. VARIABLES SANS OBJET ET TYPOLOGIE DES MANQUANTS
# =============================================================================
message("== 4. Typologie des valeurs manquantes")
NON_APPLICABLE <- list(
  PRESCOLAIRE   = c("nouv_cp1_g", "nouv_cp1_f", "nouv_cp1_t", "nouv_entree_g", "nouv_entree_f",
                    "nouv_entree_t", "redoublants_cm2_t", "eff_cm2_t", "cep_g", "cep_f", "cep_t"),
  PRIMAIRE      = c("nouv_entree_g", "nouv_entree_f", "nouv_entree_t"),
  # BEPC / BAC ecartes (publies seulement au niveau regional) : decision du 15/11
  POST_PRIMAIRE = c("nouv_cp1_g", "nouv_cp1_f", "nouv_cp1_t", "redoublants_cm2_t", "eff_cm2_t",
                    "cep_g", "cep_f", "cep_t"),
  SECONDAIRE    = c("nouv_cp1_g", "nouv_cp1_f", "nouv_cp1_t", "redoublants_cm2_t", "eff_cm2_t",
                    "cep_g", "cep_f", "cep_t"))
n_na <- 0
for (cy in names(NON_APPLICABLE)) for (v in intersect(NON_APPLICABLE[[cy]], VARS)) {
  idx <- P$cycle == cy
  pres <- which(idx & !is.na(P[[v]]))
  if (length(pres)) noter_anomalie(P[pres, ], "VALEUR_NON_APPLICABLE", v, P[[v]][pres], NA,
    "ALERTE", "valeur retiree (variable sans objet pour ce cycle)")
  P[[v]][idx] <- NA_real_; O[[v]][idx] <- "NON_APPLICABLE"; n_na <- n_na + sum(idx)
}
noter_etape("4", "cellules sans objet (non applicables)", n_na)

A_DONNEE_SCOL <- Reduce(`|`, lapply(intersect(VARS_SCOLAIRES, VARS), function(v) O[[v]] %in% c("OBS", "EST")))
for (v in VARS) {
  na <- is.na(P[[v]]) & is.na(O[[v]])
  if (!any(na)) next
  obs <- as.numeric(!is.na(P[[v]]))
  any_cycle <- ave(obs, P$cycle, FUN = max) > 0
  any_annee <- ave(obs, P$cycle, P$annee_debut, FUN = max) > 0
  code <- ifelse(v %in% VARS_SCOLAIRES & !A_DONNEE_SCOL, "NA_LIGNE",
          ifelse(!any_cycle, "NA_VARIABLE",
          ifelse(!any_annee, "NA_PERIODE", "NA_PONCTUEL")))
  O[[v]][na] <- code[na]
}
tab_na <- table(unlist(O))
for (k in c("NA_LIGNE", "NA_VARIABLE", "NA_PERIODE", "NA_PONCTUEL"))
  noter_etape("4", paste("cellules", k), if (k %in% names(tab_na)) as.integer(tab_na[[k]]) else 0L)

# =============================================================================
# 5. FONCTIONS DE MODIFICATION (toutes journalisees)
# =============================================================================
CIBLES_DEDUCTION  <- c("NA_PONCTUEL", "NA_PERIODE", "NA_VARIABLE", "NEUTRALISE")
CIBLES_IMPUTATION <- c("NA_PONCTUEL", "NEUTRALISE")
ANCRES            <- c("OBS", "EST", "DED")

gv <- function(v) if (v %in% names(P)) P[[v]] else rep(NA_real_, N)
modifiable <- function(v, cibles = CIBLES_DEDUCTION) if (v %in% names(O)) O[[v]] %in% cibles else rep(FALSE, N)

modifier <- function(i, v, val, origine, methode) {
  keep <- !is.na(val); i <- i[keep]; val <- val[keep]
  if (!length(i)) return(0L)
  ETAT$imput[[length(ETAT$imput) + 1]] <- tibble(
    cycle = P$cycle[i], id_ligne = P$id_ligne[i], nom_province = P$nom_province[i],
    annee_debut = P$annee_debut[i], variable = v, valeur_avant = P[[v]][i],
    origine_avant = O[[v]][i], valeur_apres = val, origine_apres = origine, methode = methode)
  P[[v]][i] <<- val
  O[[v]][i] <<- origine
  length(i)
}
neutraliser <- function(i, v, regle) {
  i <- i[!is.na(P[[v]][i])]
  if (!length(i)) return(0L)
  ETAT$imput[[length(ETAT$imput) + 1]] <- tibble(
    cycle = P$cycle[i], id_ligne = P$id_ligne[i], nom_province = P$nom_province[i],
    annee_debut = P$annee_debut[i], variable = v, valeur_avant = P[[v]][i],
    origine_avant = O[[v]][i], valeur_apres = NA_real_, origine_apres = "NEUTRALISE",
    methode = paste("valeur rejetee :", regle))
  P[[v]][i] <<- NA_real_
  O[[v]][i] <<- "NEUTRALISE"
  length(i)
}

# =============================================================================
# 6. REGLES
# =============================================================================
IDENTITES <- list(
  list(regle = "SEXE_EFFECTIFS",        total = "eff_t",         parts = c("eff_g", "eff_f")),
  list(regle = "SEXE_NOUV_CP1",         total = "nouv_cp1_t",    parts = c("nouv_cp1_g", "nouv_cp1_f")),
  list(regle = "SEXE_NOUV_ENTREE",      total = "nouv_entree_t", parts = c("nouv_entree_g", "nouv_entree_f")),
  list(regle = "SEXE_ENSEIGNANTS",      total = "nb_ens_t",      parts = c("nb_ens_g", "nb_ens_f")),
  list(regle = "STATUT_EFFECTIFS",      total = "eff_t",         parts = c("eff_public_t", "eff_prive_t", "eff_communautaire_t")),
  list(regle = "MILIEU_EFFECTIFS",      total = "eff_t",         parts = c("eff_urbain_t", "eff_rural_t")),
  list(regle = "STATUT_ETABLISSEMENTS", total = "nb_etab_t",     parts = c("nb_etab_public", "nb_etab_prive", "nb_etab_communautaire")))

INEGALITES <- tribble(
  ~regle,              ~partie,               ~tout,               ~gravite,
  "SALLES_BON_ETAT",   "nb_salles_bon_etat",  "nb_salles_t",       "BLOQUANT",
  "SALLES_ABRIS",      "nb_salles_abris",     "nb_salles_t",       "ALERTE",
  "ETAB_EAU",          "nb_etab_eau",         "nb_etab_t",         "BLOQUANT",
  "ETAB_LATRINES",     "nb_etab_latrines",    "nb_etab_t",         "BLOQUANT",
  "ETAB_ELECTRICITE",  "nb_etab_electricite", "nb_etab_t",         "BLOQUANT",
  "ETAB_CANTINE",      "nb_etab_cantine",     "nb_etab_t",         "BLOQUANT",
  "ENS_PUBLIC",        "nb_ens_public",       "nb_ens_t",          "BLOQUANT",
  "ENS_QUALIFIES",     "nb_ens_qualifies",    "nb_ens_t",          "BLOQUANT",
  "REDOUBLANTS_CM2",   "redoublants_cm2_t",   "eff_cm2_t",         "BLOQUANT",
  "EFF_CM2",           "eff_cm2_t",           "eff_t",             "BLOQUANT",
  "NOUV_CP1",          "nouv_cp1_t",          "eff_t",             "BLOQUANT",
  "NOUV_ENTREE",       "nouv_entree_t",       "eff_t",             "BLOQUANT",
  "ETAB_FONCTIONNELS", "nb_etab_fonctionnels","nb_etab_t",         "BLOQUANT",
  "POP_ENTREE",        "pop_age_entree",      "pop_age_scolaire",  "ALERTE",
  "POP_FIN",           "pop_age_fin",         "pop_age_scolaire",  "ALERTE",
  "POP_SCOLAIRE",      "pop_age_scolaire",    "pop_totale",        "ALERTE",
  "PDI_AGE_SCOLAIRE",  "nb_pdi_age_scolaire", "nb_pdi",            "ALERTE",
  "TNS_TBS_G",         "tns_g",               "tbs_g",             "BLOQUANT",
  "TNS_TBS_F",         "tns_f",               "tbs_f",             "BLOQUANT",
  "TNS_TBS_T",         "tns_t",               "tbs_t",             "BLOQUANT")

FAMILLES_TAUX <- c("tba", "tbs", "tns", "tap", "cep")
IPS <- list(ips_tba = "tba", ips_tbs = "tbs", ips_tap = "tap")
type_var <- function(v) ifelse(str_detect(v, "^(tba|tbs|tns|tap|cep)_"), "taux",
                        ifelse(str_detect(v, "^ips_"), "ips", "compte"))
VARS_TAUX   <- VARS[type_var(VARS) %in% c("taux", "ips")]
VARS_COMPTE <- VARS[type_var(VARS) == "compte"]
# Comptes imputables par interpolation : ecole + population d'age scolaire (jamais le contexte)
VARS_INTERP <- intersect(c(VARS_COMPTE[VARS_COMPTE %in% VARS_SCOLAIRES],
                           "pop_age_scolaire", "pop_age_entree", "pop_age_fin"), VARS)
NULLES_SI_EFF0 <- intersect(c("eff_g", "eff_f", "eff_public_t", "eff_prive_t", "eff_communautaire_t",
  "eff_urbain_t", "eff_rural_t", "nouv_cp1_g", "nouv_cp1_f", "nouv_cp1_t", "nouv_entree_g",
  "nouv_entree_f", "nouv_entree_t", "redoublants_cm2_t", "eff_cm2_t", "nb_etab_public",
  "nb_etab_prive", "nb_etab_communautaire", "nb_etab_t", "nb_salles_t", "nb_salles_bon_etat",
  "nb_salles_abris", "nb_groupes_pedago", "nb_ens_g", "nb_ens_f", "nb_ens_t", "nb_ens_public",
  "nb_ens_qualifies", "nb_etab_eau", "nb_etab_latrines", "nb_etab_electricite", "nb_etab_cantine",
  "nb_latrines", "nb_tables_bancs"), VARS)
num_tba <- function() ifelse(P$cycle %in% c("POST_PRIMAIRE", "SECONDAIRE"), gv("nouv_entree_t"), gv("nouv_cp1_t"))

deduire_identites <- function(etape) {
  n_tot <- 0
  for (passe in 1:3) {
    n_passe <- 0
    for (id in IDENTITES) {
      if (!all(c(id$total, id$parts) %in% VARS)) next
      Tt <- P[[id$total]]
      M  <- sapply(id$parts, function(p) P[[p]])
      n_na <- rowSums(is.na(M))
      i <- which(is.na(Tt) & n_na == 0 & modifiable(id$total))
      n_passe <- n_passe + modifier(i, id$total, rowSums(M)[i], "DED",
                                    paste0(id$regle, " : total = somme des composantes"))
      for (k in seq_along(id$parts)) {
        p <- id$parts[k]
        i <- which(!is.na(Tt) & n_na == 1 & is.na(M[, k]) & modifiable(p))
        if (!length(i)) next
        val <- Tt[i] - rowSums(M[i, -k, drop = FALSE])
        neg <- val < -PARAM$tol_abs
        if (any(neg)) noter_anomalie(P[i[neg], ], paste0(id$regle, "_DEDUCTION_NEGATIVE"), p,
          val[neg], 0, "ALERTE", "deduction impossible (composante negative) : cellule laissee vide")
        val[neg] <- NA
        val <- pmax(val, 0)
        n_passe <- n_passe + modifier(i, p, val, "DED",
                                      paste0(id$regle, " : composante = total - autres composantes"))
      }
    }
    n_tot <- n_tot + n_passe
    if (n_passe == 0) break
  }
  noter_etape(etape, "cellules deduites par identite comptable", n_tot)
}

deduire_taux <- function(etape) {
  n <- 0
  den <- gv("pop_age_scolaire"); num <- gv("eff_t")
  i <- which(is.na(gv("tbs_t")) & modifiable("tbs_t") & !is.na(num) & !is.na(den) & den > 0)
  n <- n + modifier(i, "tbs_t", (num / den * 100)[i], "DED", "TBS = effectif / population d'age scolaire x 100")
  den <- gv("pop_age_entree"); num <- num_tba()
  i <- which(is.na(gv("tba_t")) & modifiable("tba_t") & !is.na(num) & !is.na(den) & den > 0)
  n <- n + modifier(i, "tba_t", (num / den * 100)[i], "DED", "TBA = nouveaux inscrits / population d'age d'entree x 100")
  for (ips in intersect(names(IPS), VARS)) {
    tg <- gv(paste0(IPS[[ips]], "_g")); tf <- gv(paste0(IPS[[ips]], "_f"))
    i <- which(is.na(gv(ips)) & modifiable(ips) & !is.na(tg) & !is.na(tf) & tg > 0)
    n <- n + modifier(i, ips, (tf / tg)[i], "DED", "IPS = taux filles / taux garcons")
  }
  noter_etape(etape, "taux et IPS deduits de leurs composantes", n)
}

# =============================================================================
# 7. CONTEXTE : FERMETURES (ESU) ET DEDUCTIONS EXACTES
# =============================================================================
message("== 7. Contexte et deductions exactes")
ESU <- NULL
if ("ESU_FERMETURES" %in% ONGLETS) {
  ESU <- lire_onglet(f, "ESU_FERMETURES")$don
  for (v in intersect(c("annee_debut", "nb_structures_fermees", "eleves_f", "eleves_g", "eleves_t",
                        "ens_f", "ens_h", "ens_t"), names(ESU))) ESU[[v]] <- parse_num(ESU[[v]])
  if ("date_releve" %in% names(ESU)) {
    serie <- str_detect(ESU$date_releve, "^\\d{5}(\\.0+)?$") %in% TRUE
    ESU$date_releve[serie] <- as.character(as.Date(as.numeric(ESU$date_releve[serie]), origin = "1899-12-30"))
  }
  d_esu <- transmute(ESU, cycle = ordre_enseignement, id_ligne, nom_region, nom_province, annee_debut)
  e1 <- which(abs(ESU$eleves_f + ESU$eleves_g - ESU$eleves_t) > PARAM$tol_abs)
  noter_anomalie(d_esu[e1, ], "ESU_SEXE_ELEVES", "eleves_f+eleves_g=eleves_t",
                 ESU$eleves_f[e1] + ESU$eleves_g[e1], ESU$eleves_t[e1], "ALERTE", "releve conserve")
  e2 <- which(abs(ESU$ens_f + ESU$ens_h - ESU$ens_t) > PARAM$tol_abs)
  noter_anomalie(d_esu[e2, ], "ESU_SEXE_ENSEIGNANTS", "ens_f+ens_h=ens_t",
                 ESU$ens_f[e2] + ESU$ens_h[e2], ESU$ens_t[e2], "ALERTE", "releve conserve")
  noter_etape("7", "releves ESU lus", nrow(ESU))
}
zones <- unique(c(if (!is.null(ESU)) ESU$code_geo[which(ESU$nb_structures_fermees > 0)] else character(),
                  P$code_geo[which(gv("nb_etab_fermes") > 0)]))
P$zone_fermetures <- P$code_geo %in% zones
P$max_fermetures_esu <- NA_real_
if (!is.null(ESU)) {
  ordre <- ifelse(P$cycle %in% c("POST_PRIMAIRE", "SECONDAIRE"), "POST_PRIMAIRE_SECONDAIRE", P$cycle)
  mx <- ESU %>% group_by(ordre_enseignement, code_geo) %>%
    summarise(m = max(nb_structures_fermees, na.rm = TRUE), .groups = "drop")
  P$max_fermetures_esu <- mx$m[match(paste(ordre, P$code_geo), paste(mx$ordre_enseignement, mx$code_geo))]
  P$max_fermetures_esu[is.na(P$max_fermetures_esu) & P$code_geo %in% unique(ESU$code_geo)] <- 0
}
noter_etape("7", "provinces touchees par des fermetures (ESU)", length(zones))
P$periode_population <- ifelse(P$annee_debut < PARAM$annee_rupture_pop, "RGPH2006_PROJ", "RGPH2019_PROJ")

deduire_identites("7.1")

# Effectif nul (fermeture totale) : les comptes manquants de la ligne valent 0
eff0 <- which(gv("eff_t") == 0)
n0 <- 0
for (v in NULLES_SI_EFF0) {
  # seulement les trous ponctuels : une variable jamais publiee pour le cycle
  # (ex. nb_etab_t au post-primaire) ne doit pas apparaitre a 0 sur quelques lignes
  i <- eff0[is.na(P[[v]][eff0]) & modifiable(v, CIBLES_IMPUTATION)[eff0]]
  n0 <- n0 + modifier(i, v, rep(0, length(i)), "DED",
                      "effectif nul : aucun etablissement fonctionnel (annuaire = ecoles ouvertes)")
}
noter_etape("7.2", "cellules mises a 0 (lignes a effectif nul)", n0)
if (length(eff0)) noter_anomalie(P[eff0, ], "EFFECTIF_NUL", "eff_t", 0, NA, "INFO",
  "conserve : fermeture totale probable (insecurite)",
  ifelse(P$zone_fermetures[eff0], "province touchee par les fermetures ESU", "province hors releves ESU : a verifier"))

# =============================================================================
# 8. CONTROLES DE COHERENCE
# =============================================================================
message("== 8. Controles de coherence")

# 8.0 Population d'age scolaire : coherence avec le primaire (meme province, meme annee)
n_pop <- 0
if ("pop_age_scolaire" %in% VARS) {
  ipri <- match(paste("PRIMAIRE", P$code_geo, P$annee_debut), paste(P$cycle, P$code_geo, P$annee_debut))
  ref_pri <- P$pop_age_scolaire[ipri]
  TAUX_DE_POP <- list(pop_age_scolaire = c("tbs_g", "tbs_f", "tbs_t", "tns_g", "tns_f", "tns_t"),
                      pop_age_entree   = c("tba_g", "tba_f", "tba_t"),
                      pop_age_fin      = c("tap_g", "tap_f", "tap_t"))
  for (v in intersect(names(TAUX_DE_POP), VARS)) {
    rap <- P[[v]] / ref_pri
    med <- ave(rap, CLE_SERIE, FUN = function(z) median(z, na.rm = TRUE))
    i <- which(P$cycle %in% PARAM$cycles_controle_pop & O[[v]] %in% "OBS" &
               abs(rap / med - 1) > PARAM$seuil_pop_rapport)
    if (!length(i)) next
    anc <- P[[v]][i]; nouv <- med[i] * ref_pri[i]
    noter_anomalie(P[i, ], "POP_INCOHERENTE_CYCLES", v, anc, nouv, "BLOQUANT",
      "population remplacee (population 6-11 ans x rapport median de la province) ; taux recales",
      sprintf("rapport au primaire %.3f ; mediane de la province %.3f", rap[i], med[i]))
    for (tx in intersect(TAUX_DE_POP[[v]], VARS)) {
      j <- !is.na(P[[tx]][i])
      modifier(i[j], tx, (P[[tx]][i] * anc / nouv)[j], "IMP_RAPPORT",
               paste("taux recale sur", v, "corrigee (x ancienne / nouvelle population)"))
    }
    n_pop <- n_pop + modifier(i, v, nouv, "IMP_RAPPORT",
      "population = population 6-11 ans de la province x rapport median des autres annees")
  }
}
noter_etape("8.0", "populations incoherentes avec le primaire corrigees", n_pop)

# 8.1 Domaines de valeurs (C5)
n_neutr <- 0
for (v in VARS) {
  x <- P[[v]]; if (all(is.na(x))) next
  ty <- type_var(v)
  if (ty == "compte") {
    i <- which(x < 0)
    noter_anomalie(P[i, ], "DOMAINE_NEGATIF", v, x[i], 0, "BLOQUANT", "valeur rejetee puis reimputee si possible")
    n_neutr <- n_neutr + neutraliser(i, v, "valeur negative")
  } else if (ty == "taux") {
    b <- PARAM$bornes_taux[[str_extract(v, "^[a-z]+")]]
    i <- which(x < b[1] | x > b[2])
    noter_anomalie(P[i, ], "DOMAINE_TAUX", v, x[i], NA, "BLOQUANT",
                   "valeur rejetee puis reimputee si possible", sprintf("plage admise [%s ; %s]", b[1], b[2]))
    n_neutr <- n_neutr + neutraliser(i, v, "taux hors plage")
  } else {
    fam <- IPS[[v]]; petit <- pmin(gv(paste0(fam, "_g")), gv(paste0(fam, "_f"))) < PARAM$taux_min_ips
    hors <- x < PARAM$bornes_ips[1] | x > PARAM$bornes_ips[2]
    i <- which(hors & !(petit %in% TRUE))
    noter_anomalie(P[i, ], "DOMAINE_IPS", v, x[i], NA, "BLOQUANT", "valeur rejetee puis recalculee si possible")
    n_neutr <- n_neutr + neutraliser(i, v, "IPS hors plage")
    j <- which(hors & petit %in% TRUE)
    noter_anomalie(P[j, ], "IPS_TAUX_FAIBLES", v, x[j], NA, "INFO",
                   "conserve : IPS peu significatif (taux < seuil)")
  }
}
noter_etape("8.1", "valeurs rejetees (hors domaine)", n_neutr)

# 8.2 Partie <= tout
n_neutr <- 0
for (k in seq_len(nrow(INEGALITES))) {
  r <- INEGALITES[k, ]
  if (!all(c(r$partie, r$tout) %in% VARS)) next
  a <- P[[r$partie]]; b <- P[[r$tout]]
  i <- which(a > b * (1 + PARAM$tol_rel) + PARAM$tol_abs)
  if (!length(i)) next
  if (r$gravite == "BLOQUANT") {
    noter_anomalie(P[i, ], paste0("INEGALITE_", r$regle), paste(r$partie, "<=", r$tout), a[i], b[i],
                   "BLOQUANT", paste("total conserve ;", r$partie, "rejete puis reimpute si possible"))
    n_neutr <- n_neutr + neutraliser(i, r$partie, paste("superieur a", r$tout))
  } else {
    noter_anomalie(P[i, ], paste0("INEGALITE_", r$regle), paste(r$partie, "<=", r$tout), a[i], b[i],
                   "ALERTE", "valeurs conservees (sources differentes) ; ligne SUSPECT")
  }
}
if (all(c("nb_etab_fermes", "nb_etab_t") %in% VARS)) {
  i <- which(gv("nb_etab_fermes") > gv("nb_etab_t"))
  noter_anomalie(P[i, ], "ETAB_FERMES_SUP_TOTAL", "nb_etab_fermes <= nb_etab_t",
                 gv("nb_etab_fermes")[i], gv("nb_etab_t")[i], "INFO",
                 if (PARAM$etab_t_inclut_fermes) "a examiner" else
                   "attendu : l'annuaire ne compte que les ecoles fonctionnelles (PARAM$etab_t_inclut_fermes = FALSE)")
}
noter_etape("8.2", "valeurs rejetees (partie > tout)", n_neutr)

# 8.3 Identites comptables (C2, C4), apres deduction
for (id in IDENTITES) {
  if (!all(c(id$total, id$parts) %in% VARS)) next
  Tt <- P[[id$total]]; M <- sapply(id$parts, function(p) P[[p]])
  s <- rowSums(M); e <- s - Tt
  i <- which(abs(e) > pmax(PARAM$tol_abs, PARAM$tol_rel * abs(Tt)))
  if (!length(i)) next
  avec_est <- Reduce(`|`, lapply(c(id$total, id$parts), function(v) O[[v]][i] %in% "EST"))
  noter_anomalie(P[i, ], paste0("IDENTITE_", id$regle),
                 paste(id$total, "=", paste(id$parts, collapse = " + ")), s[i], Tt[i],
                 ifelse(avec_est, "INFO", "ALERTE"), "total publie conserve ; composantes a verifier")
}

# 8.4 Taux publies vs recalcules (C10) et encadrement G / F / T
tx <- list(list(regle = "TBS_RECALCULE", taux = "tbs_t", num = gv("eff_t"), den = gv("pop_age_scolaire")),
           list(regle = "TBA_RECALCULE", taux = "tba_t", num = num_tba(), den = gv("pop_age_entree")))
for (r in tx) {
  if (!r$taux %in% VARS) next
  rec <- div(r$num, r$den, 100); x <- P[[r$taux]]
  i <- which(abs(x - rec) > PARAM$tol_taux)
  noter_anomalie(P[i, ], r$regle, r$taux, x[i], rec[i],
                 ifelse(O[[r$taux]][i] %in% "EST", "INFO", "ALERTE"), "taux publie conserve")
}
n_neutr <- 0
for (fam in FAMILLES_TAUX) {
  vg <- paste0(fam, "_g"); vf <- paste0(fam, "_f"); vt <- paste0(fam, "_t")
  if (!all(c(vg, vf, vt) %in% VARS)) next
  tg <- P[[vg]]; tf <- P[[vf]]; tt <- P[[vt]]
  i <- which(tt < pmin(tg, tf) - PARAM$tol_taux | tt > pmax(tg, tf) + PARAM$tol_taux)
  noter_anomalie(P[i, ], paste0("ENCADREMENT_", toupper(fam)), paste(vt, "entre", vg, "et", vf),
                 tt[i], (tg[i] + tf[i]) / 2, "BLOQUANT", "taux total rejete puis reconstruit (pondere)",
                 sprintf("G = %s ; F = %s", tg[i], tf[i]))
  n_neutr <- n_neutr + neutraliser(i, vt, "taux total hors de l'intervalle [G ; F]")
}
noter_etape("8.4", "taux totaux rejetes (hors intervalle G-F)", n_neutr)

# 8.5 IPS publie vs taux F / taux G
n_ips <- 0
for (ips in intersect(names(IPS), VARS)) {
  tg <- gv(paste0(IPS[[ips]], "_g")); tf <- gv(paste0(IPS[[ips]], "_f"))
  rec <- div(tf, tg); x <- P[[ips]]
  i <- which(abs(x - rec) > PARAM$tol_ips & pmin(tg, tf) >= PARAM$taux_min_ips)
  if (!length(i)) next
  noter_anomalie(P[i, ], "IPS_INCOHERENT", ips, x[i], rec[i],
                 if (PARAM$recalculer_ips_incoherents) "INFO" else "ALERTE",
                 if (PARAM$recalculer_ips_incoherents) "IPS remplace par taux F / taux G (publie dans valeur)" else "IPS publie conserve")
  if (PARAM$recalculer_ips_incoherents)
    n_ips <- n_ips + modifier(i, ips, rec[i], "DED", "IPS recalcule = taux F / taux G (publie incoherent)")
}
noter_etape("8.5", "IPS recalcules (publie incoherent)", n_ips)

# 8.6 Coherence temporelle (C6) : variations, pics, rupture de serie
lagv  <- function(x) ave(x, CLE_SERIE, FUN = function(z) c(NA, head(z, -1)))
leadv <- function(x) ave(x, CLE_SERIE, FUN = function(z) c(tail(z, -1), NA))
COEUR <- intersect(c("eff_t", "nb_etab_t", "nb_ens_t"), VARS)
lr_coeur <- lapply(setNames(COEUR, COEUR), function(v) log((P[[v]] + 1) / (lagv(P[[v]]) + 1)))
VARS_TEMPO <- intersect(c("eff_t", "nouv_cp1_t", "nouv_entree_t", "nb_etab_t", "nb_salles_t",
                          "nb_ens_t", "pop_age_scolaire"), VARS)
n_pic <- 0
for (v in VARS_TEMPO) {
  x <- P[[v]]; xp <- lagv(x); xn <- leadv(x)
  ok <- O[[v]] %in% ANCRES
  okp <- lagv(as.numeric(ok)) %in% 1; okn <- leadv(as.numeric(ok)) %in% 1
  lr <- log((x + 1) / (xp + 1)); ln <- log((x + 1) / (xn + 1))
  taille <- pmax(x, xp, na.rm = TRUE) >= PARAM$effectif_min_variation
  comove <- Reduce(`|`, c(list(rep(FALSE, N)), lapply(setdiff(COEUR, v), function(cv)
    (sign(lr_coeur[[cv]]) == sign(lr) & abs(lr_coeur[[cv]]) > log(1 + PARAM$seuil_comouvement)) %in% TRUE)))
  # mouvement national : la mediane des provinces bouge dans le meme sens
  # (ex. nouveaux inscrits en 2nde, qui suivent les resultats du BEPC)
  nat <- ave(lr, paste(P$cycle, P$annee_debut), FUN = function(z) median(z[is.finite(z)], na.rm = TRUE))
  national <- (sign(nat) == sign(lr) & abs(nat) > log(1 + PARAM$seuil_comouvement)) %in% TRUE
  contexte <- P$zone_fermetures | comove | national
  motif <- ifelse(P$zone_fermetures, "province touchee par les fermetures",
           ifelse(national, sprintf("mouvement national (mediane %+.0f %%)", 100 * (exp(nat) - 1)),
                  "variables du coeur en co-mouvement"))
  if (v == "pop_age_scolaire") {
    rupt <- ok & okp & P$annee_debut == PARAM$annee_rupture_pop & abs(lr) > log(1 + PARAM$seuil_rupture_pop)
    i <- which(rupt)
    noter_anomalie(P[i, ], "RUPTURE_SERIE_POPULATION", v, x[i], xp[i], "INFO",
      "conservee : changement de projections (RGPH 2006 puis RGPH 2019) ; utiliser periode_population dans les modeles")
    vp <- ok & okp & P$annee_debut != PARAM$annee_rupture_pop & abs(lr) > log(1 + PARAM$seuil_variation)
    i <- which(vp & !(P$zone_fermetures | national))
    noter_anomalie(P[i, ], "VARIATION_POPULATION", v, x[i], xp[i], "ALERTE", "conservee ; a verifier",
                   sprintf("variation %+.0f %%", 100 * (exp(lr[i]) - 1)))
    i <- which(vp & (P$zone_fermetures | national))
    noter_anomalie(P[i, ], "VARIATION_POPULATION_CONTEXTE", v, x[i], xp[i], "INFO",
                   "conservee : revision des projections dans une zone de deplacements",
                   paste(motif[i], sprintf("; variation %+.0f %%", 100 * (exp(lr[i]) - 1))))
    next
  }
  pic <- ok & okp & okn & taille &
    ((lr > log(1 + PARAM$seuil_pic) & ln > log(1 + PARAM$seuil_pic)) |
     (lr < -log(1 + PARAM$seuil_pic) & ln < -log(1 + PARAM$seuil_pic)))
  pic[is.na(pic)] <- FALSE
  vari <- ok & okp & taille & abs(lr) > log(1 + PARAM$seuil_variation) & !pic
  vari[is.na(vari)] <- FALSE
  i <- which(pic & !contexte)
  noter_anomalie(P[i, ], "PIC_ISOLE", v, x[i], (xp[i] + xn[i]) / 2,
                 if (PARAM$neutraliser_pics_isoles) "BLOQUANT" else "ALERTE",
                 if (PARAM$neutraliser_pics_isoles) "valeur rejetee puis interpolee" else "conservee ; a verifier dans la source",
                 sprintf("annee precedente %s ; suivante %s", xp[i], xn[i]))
  if (PARAM$neutraliser_pics_isoles) n_pic <- n_pic + neutraliser(i, v, "pic isole")
  i <- which(pic & contexte)
  noter_anomalie(P[i, ], "RUPTURE_CONTEXTE", v, x[i], (xp[i] + xn[i]) / 2, "INFO",
                 "conservee : choc coherent avec le contexte", motif[i])
  i <- which(vari & !contexte)
  noter_anomalie(P[i, ], "VARIATION_ATYPIQUE", v, x[i], xp[i], "ALERTE", "conservee ; a verifier",
                 sprintf("variation %+.0f %%", 100 * (exp(lr[i]) - 1)))
  i <- which(vari & contexte)
  noter_anomalie(P[i, ], "VARIATION_CONTEXTE", v, x[i], xp[i], "INFO",
                 "conservee : variation coherente avec le contexte",
                 paste(motif[i], sprintf("; variation %+.0f %%", 100 * (exp(lr[i]) - 1))))
}
noter_etape("8.6", "pics isoles rejetes", n_pic)

# =============================================================================
# 9. IMPUTATION DES MANQUANTS PONCTUELS ET DES VALEURS REJETEES
# =============================================================================
message("== 9. Imputation")
interpoler_serie <- function(y, ancre, cible, max_trou) {
  res <- rep(NA_real_, length(y))
  ia <- which(ancre & !is.na(y))
  if (length(ia) < 2) return(res)
  for (k in which(cible & is.na(y))) {
    g <- ia[ia < k]; d <- ia[ia > k]
    if (!length(g) || !length(d)) next
    a <- max(g); b <- min(d)
    if ((b - a - 1) > max_trou) next
    res[k] <- y[a] + (y[b] - y[a]) * (k - a) / (b - a)
  }
  res
}
imputer_interpolation <- function(vars, etape) {
  n <- 0
  for (v in vars) {
    cible <- O[[v]] %in% CIBLES_IMPUTATION & is.na(P[[v]])
    if (!any(cible)) next
    ancre <- O[[v]] %in% ANCRES
    for (s in unique(CLE_SERIE[cible])) {
      i <- which(CLE_SERIE == s)
      res <- interpoler_serie(P[[v]][i], ancre[i], cible[i], PARAM$max_trou_interp)
      k <- !is.na(res)
      n <- n + modifier(i[k], v, res[k], "IMP_INTERP",
                        sprintf("interpolation lineaire entre annees observees (trou <= %d ans)", PARAM$max_trou_interp))
    }
  }
  noter_etape(etape, "cellules interpolees", n)
}
imputer_tendance <- function(vars, etape) {
  n <- 0
  idx <- setNames(seq_len(N), paste(P$cycle, P$code_geo, P$annee_debut))
  for (v in vars) {
    ancre <- O[[v]] %in% ANCRES & !is.na(P[[v]])
    for (i in which(O[[v]] %in% CIBLES_IMPUTATION & is.na(P[[v]]))) {
      for (pas in c(-1, 1)) {
        j <- idx[paste(P$cycle[i], P$code_geo[i], P$annee_debut[i] + pas)]
        if (is.na(j) || !ancre[j]) next
        # pas d'ancre de l'autre cote : sinon c'est un trou interieur (interpolation)
        autre <- idx[paste(P$cycle[i], P$code_geo[i], P$annee_debut[i] - pas)]
        if (!is.na(autre) && ancre[autre]) next
        reg <- which(P$cycle == P$cycle[i] & P$nom_region == P$nom_region[i] & P$code_geo != P$code_geo[i])
        a <- reg[P$annee_debut[reg] == P$annee_debut[i] & ancre[reg]]
        b <- reg[P$annee_debut[reg] == P$annee_debut[j] & ancre[reg]]
        com <- intersect(P$code_geo[a], P$code_geo[b])
        if (length(com) < 2) next
        sa <- sum(P[[v]][a][match(com, P$code_geo[a])]); sb <- sum(P[[v]][b][match(com, P$code_geo[b])])
        if (sb <= 0) next
        n <- n + modifier(i, v, P[[v]][j] * sa / sb, "IMP_TENDANCE",
          sprintf("annee de bord : valeur %d x evolution des autres provinces de la region (%d provinces)",
                  P$annee_debut[j], length(com)))
        break
      }
    }
  }
  noter_etape(etape, "cellules imputees par tendance regionale", n)
}
imputer_taux_ponderes <- function(etape) {
  n <- 0
  wg <- gv("eff_g"); wf <- gv("eff_f")
  for (fam in FAMILLES_TAUX) {
    vg <- paste0(fam, "_g"); vf <- paste0(fam, "_f"); vt <- paste0(fam, "_t")
    if (!all(c(vg, vf, vt) %in% VARS)) next
    i <- which(is.na(P[[vt]]) & O[[vt]] %in% CIBLES_IMPUTATION & !is.na(P[[vg]]) & !is.na(P[[vf]]))
    if (!length(i)) next
    pond <- ifelse(!is.na(wg[i]) & !is.na(wf[i]) & (wg[i] + wf[i]) > 0,
                   (P[[vg]][i] * wg[i] + P[[vf]][i] * wf[i]) / (wg[i] + wf[i]),
                   (P[[vg]][i] + P[[vf]][i]) / 2)
    n <- n + modifier(i, vt, pond, "IMP_PONDERE", "taux total = moyenne des taux G et F ponderee par les effectifs G et F")
  }
  noter_etape(etape, "taux totaux reconstruits (ponderation G/F)", n)
}

# Composantes imputees independamment de leur total (ex. urbain / rural interpoles
# depuis des annees d'avant crise) : recalage proportionnel sur le total observe.
caler_composantes <- function(etape) {
  n <- 0
  for (id in IDENTITES) {
    if (!all(c(id$total, id$parts) %in% VARS)) next
    Tt <- P[[id$total]]
    M  <- sapply(id$parts, function(p) P[[p]])
    OI <- sapply(id$parts, function(p) str_starts(O[[p]], "IMP_") %in% TRUE)
    lignes <- which(!is.na(Tt) & O[[id$total]] %in% ANCRES & rowSums(is.na(M)) == 0 & rowSums(OI) > 0)
    for (i in lignes) {
      imp <- OI[i, ]
      reste <- Tt[i] - sum(M[i, !imp])
      if (abs(sum(M[i, imp]) - reste) <= max(PARAM$tol_abs, PARAM$tol_rel * Tt[i])) next
      if (reste < 0) {
        noter_anomalie(P[i, ], paste0("CALAGE_", id$regle), paste(id$parts, collapse = " + "),
                       sum(M[i, ]), Tt[i], "ALERTE", "composantes observees superieures au total : non recalees")
        next
      }
      s <- sum(M[i, imp])
      nv <- if (s > 0) M[i, imp] * reste / s else rep(reste / sum(imp), sum(imp))
      ks <- which(imp)
      for (j in seq_along(ks))
        n <- n + modifier(i, id$parts[ks[j]], nv[j], "IMP_CALAGE",
                          paste0(id$regle, " : composante imputee recalee sur le total observe"))
    }
  }
  noter_etape(etape, "composantes imputees recalees sur le total", n)
}

imputer_interpolation(VARS_INTERP, "9.1")
imputer_tendance(VARS_INTERP, "9.2")
deduire_identites("9.3")
deduire_taux("9.4")
imputer_taux_ponderes("9.5")
imputer_interpolation(VARS_TAUX, "9.6")   # taux restants sans composantes
deduire_taux("9.7")                       # IPS a partir des taux reconstruits
caler_composantes("9.7b")

# Etablissements fonctionnels (definition)
if (all(c("nb_etab_fonctionnels", "nb_etab_fermes", "nb_etab_t") %in% VARS)) {
  val <- if (PARAM$etab_t_inclut_fermes) gv("nb_etab_t") - gv("nb_etab_fermes") else gv("nb_etab_t")
  i <- which(is.na(gv("nb_etab_fonctionnels")) & !is.na(gv("nb_etab_fermes")) & !is.na(val) &
             modifiable("nb_etab_fonctionnels"))
  n <- modifier(i, "nb_etab_fonctionnels", pmax(val[i], 0), "DED",
                if (PARAM$etab_t_inclut_fermes) "fonctionnels = total - fermes" else "fonctionnels = total recense (annuaire)")
  noter_etape("9.8", "etablissements fonctionnels deduits", n)
}

# Superficie constante par province
if ("superficie_km2" %in% VARS) {
  sup <- tapply(P$superficie_km2[O$superficie_km2 %in% ANCRES], P$code_geo[O$superficie_km2 %in% ANCRES], median)
  i <- which(is.na(P$superficie_km2) & P$code_geo %in% names(sup) & O$superficie_km2 != "NON_APPLICABLE")
  noter_etape("9.9", "superficies completees (constante)",
              modifier(i, "superficie_km2", as.numeric(sup[P$code_geo[i]]), "DED", "superficie constante de la province"))
}

# Population totale : retropolation 2015-2019 (croissance annuelle 2020-2024 de la province)
if (PARAM$retropoler_pop_totale && "pop_totale" %in% VARS) {
  ok <- O$pop_totale %in% c("OBS", "EST") & !is.na(P$pop_totale)
  ref <- data.frame(code_geo = P$code_geo[ok], annee = P$annee_debut[ok], pop = P$pop_totale[ok]) %>%
    group_by(code_geo, annee) %>% summarise(n_val = n_distinct(round(pop)), pop = median(pop), .groups = "drop")
  if (any(ref$n_val > 1)) warning("pop_totale differente selon le cycle pour une meme province-annee : mediane retenue")
  cr <- ref %>% group_by(code_geo) %>% filter(n() >= 2) %>%
    summarise(a0 = min(annee), p0 = pop[which.min(annee)], a1 = max(annee), p1 = pop[which.max(annee)], .groups = "drop") %>%
    mutate(r = (p1 / p0)^(1 / (a1 - a0)) - 1)
  k <- match(P$code_geo, cr$code_geo)
  i <- which(is.na(P$pop_totale) & !is.na(k) & P$annee_debut < cr$a0[k])
  val <- cr$p0[k[i]] / (1 + cr$r[k[i]])^(cr$a0[k[i]] - P$annee_debut[i])
  noter_etape("9.10", "populations totales retropolees",
    modifier(i, "pop_totale", val, "IMP_RETRO", "retropolation : population de la premiere annee / (1 + croissance annuelle moyenne de la province)^ecart"))
}

# Controles apres imputation (aucune neutralisation, alerte seulement)
for (k in seq_len(nrow(INEGALITES))) {
  r <- INEGALITES[k, ]
  if (!all(c(r$partie, r$tout) %in% VARS)) next
  a <- P[[r$partie]]; b <- P[[r$tout]]
  i <- which(a > b * (1 + PARAM$tol_rel) + PARAM$tol_abs &
             (O[[r$partie]] %in% c("DED", "IMP_INTERP", "IMP_TENDANCE") | O[[r$tout]] %in% c("DED", "IMP_INTERP", "IMP_TENDANCE")))
  noter_anomalie(P[i, ], paste0("POST_IMPUTATION_", r$regle), paste(r$partie, "<=", r$tout), a[i], b[i],
                 "ALERTE", "valeur imputee incoherente : a verifier")
}

# =============================================================================
# 10. INDICATEURS DERIVES (recalcules dans R, les formules Excel ne sont pas lues)
# =============================================================================
message("== 10. Indicateurs derives")
fonct_ref <- if (PARAM$etab_t_inclut_fermes) gv("nb_etab_t") else gv("nb_etab_t") + gv("nb_etab_fermes")
DER <- data.frame(
  part_filles           = div(gv("eff_f"), gv("eff_t")),
  eleves_par_etab       = div(gv("eff_t"), gv("nb_etab_t")),
  eleves_par_salle      = div(gv("eff_t"), gv("nb_salles_t")),
  eleves_par_classe     = div(gv("eff_t"), gv("nb_groupes_pedago")),
  eleves_par_enseignant = div(gv("eff_t"), gv("nb_ens_t")),
  part_ens_qualifies    = div(gv("nb_ens_qualifies"), gv("nb_ens_t")),
  part_etab_eau         = div(gv("nb_etab_eau"), gv("nb_etab_t")),
  part_etab_latrines    = div(gv("nb_etab_latrines"), gv("nb_etab_t")),
  part_etab_electricite = div(gv("nb_etab_electricite"), gv("nb_etab_t")),
  part_etab_cantine     = div(gv("nb_etab_cantine"), gv("nb_etab_t")),
  eleves_par_latrine    = div(gv("eff_t"), gv("nb_latrines")),
  eleves_par_table_banc = div(gv("eff_t"), gv("nb_tables_bancs")),
  taux_fonctionnalite   = div(gv("nb_etab_fonctionnels"), fonct_ref),
  densite_etab_10000hab = div(gv("nb_etab_t"), gv("pop_totale"), 10000),
  etab_par_100km2       = div(gv("nb_etab_t"), gv("superficie_km2"), 100),
  pression_demo         = div(gv("pop_age_scolaire"), gv("nb_salles_t")),
  tbs_recalcule         = div(gv("eff_t"), gv("pop_age_scolaire"), 100),
  tba_recalcule         = div(num_tba(), gv("pop_age_entree"), 100),
  ips_tbs_recalcule     = div(gv("tbs_f"), gv("tbs_g")))
DER$ctrl_sexe_eff    <- gv("eff_g") + gv("eff_f") - gv("eff_t")
DER$ctrl_statut_etab <- gv("nb_etab_public") + gv("nb_etab_prive") + gv("nb_etab_communautaire") - gv("nb_etab_t")
DER$ctrl_ecart_tbs   <- gv("tbs_t") - DER$tbs_recalcule
autres <- setdiff(VARS_DERIVEES, names(DER))
if (length(autres)) warning("Indicateurs derives du classeur non recalcules : ", paste(autres, collapse = ", "))

# Coherence en coupe : ratios plausibles et valeurs atypiques (z robuste)
grp <- paste(P$cycle, P$annee_debut)
for (r in names(PARAM$bornes_ratios)) {
  x <- DER[[r]]; b <- PARAM$bornes_ratios[[r]]
  assez <- gv("eff_t") >= PARAM$effectif_min_ratio
  med <- ave(x, grp, FUN = function(z) median(z, na.rm = TRUE))
  md  <- ave(x, grp, FUN = function(z) mad(z, na.rm = TRUE))
  z <- (x - med) / md
  i <- which(assez & (x < b[1] | x > b[2]))
  noter_anomalie(P[i, ], "RATIO_IMPLAUSIBLE", r, x[i], NA, "ALERTE",
                 "conserve ; verifier les composantes", sprintf("plage admise [%s ; %s]", b[1], b[2]))
  i <- which(assez & x >= b[1] & x <= b[2] & is.finite(z) & abs(z) > PARAM$seuil_z)
  noter_anomalie(P[i, ], "RATIO_ATYPIQUE", r, x[i], med[i], "INFO",
                 "conserve : valeur extreme mais plausible", sprintf("z robuste = %.1f", z[i]))
}

# =============================================================================
# 11. COHERENCE PROVINCES -> REGION -> NATIONAL (C3)
# =============================================================================
message("== 11. Agregation")
lire_region <- function(o) {
  if (!o %in% ONGLETS) return(NULL)
  r <- lire_onglet(f, o)$don
  for (v in setdiff(names(r), c("id_ligne", "cycle", "annee_scolaire", "niveau_geo", "code_geo",
                                "nom_region", "qualite", "sources", "remarque"))) r[[v]] <- parse_num(r[[v]])
  r$annee_debut <- as.integer(r$annee_debut)
  r
}
REG <- Filter(Negate(is.null), list(PRE_REGION = lire_region("PRE_REGION"),
                                   PRI_REGION = lire_region("PRI_REGION"),
                                   PPS_REGION = lire_region("PPS_REGION")))
AGREG_REGLES <- tribble(
  ~cycles,                    ~var_prov,     ~onglet_reg,  ~var_reg,        ~gravite, ~note,
  "PRIMAIRE",                 "eff_t",       "PRI_REGION", "eff_t",         "ALERTE", "",
  "PRIMAIRE",                 "eff_f",       "PRI_REGION", "eff_f",         "ALERTE", "",
  "PRIMAIRE",                 "nouv_cp1_t",  "PRI_REGION", "nouv_cp1_t",    "ALERTE", "",
  "PRIMAIRE",                 "nb_etab_t",   "PRI_REGION", "nb_etab_t",     "ALERTE", "",
  "PRIMAIRE",                 "nb_salles_t", "PRI_REGION", "nb_salles_t",   "ALERTE", "",
  "PRIMAIRE",                 "nb_ens_t",    "PRI_REGION", "nb_ens_t",      "ALERTE", "",
  "PRESCOLAIRE",              "eff_t",       "PRE_REGION", "eff_t",         "ALERTE", "",
  "PRESCOLAIRE",              "nb_etab_t",   "PRE_REGION", "structures_t",  "ALERTE", "",
  "PRESCOLAIRE",              "nb_salles_t", "PRE_REGION", "salles_t",      "ALERTE", "",
  "POST_PRIMAIRE|SECONDAIRE", "eff_t",       "PPS_REGION", "eleves_tous_t", "INFO",
  "champ different : tableaux de bord regionaux (par type d'etablissement) vs annuaires provinciaux")
AGREG <- list()
for (k in seq_len(nrow(AGREG_REGLES))) {
  a <- AGREG_REGLES[k, ]
  r <- REG[[a$onglet_reg]]
  if (is.null(r) || !a$var_reg %in% names(r) || !a$var_prov %in% VARS) next
  cy <- str_split(a$cycles, "\\|")[[1]]
  sp <- data.frame(nom_region = P$nom_region, annee_debut = P$annee_debut, x = P[[a$var_prov]],
                   o = O[[a$var_prov]])[P$cycle %in% cy, ] %>%
    group_by(nom_region, annee_debut) %>%
    summarise(somme_provinces = sum(x, na.rm = TRUE), n_manquantes = sum(is.na(x)),
              part_estimee = mean(o %in% c("EST", "IMP_INTERP", "IMP_TENDANCE", "IMP_RAPPORT", "IMP_CALAGE")), .groups = "drop")
  rr <- r[r$niveau_geo %in% "REGION", c("id_ligne", "nom_region", "annee_debut", a$var_reg)]
  names(rr)[4] <- "valeur_region"
  rr$cle <- norm_nom(rr$nom_region)
  res <- sp %>% mutate(cle = norm_nom(nom_region)) %>%
    inner_join(rr %>% select(-nom_region), by = c("cle", "annee_debut")) %>%
    filter(!is.na(valeur_region)) %>%
    mutate(controle = paste(a$cycles, a$var_prov, "vs", a$onglet_reg, a$var_reg),
           ecart_pct = 100 * (somme_provinces / valeur_region - 1),
           statut = case_when(n_manquantes > 0 ~ "INCOMPLET",
                              abs(ecart_pct) > 100 * PARAM$tol_agregation ~ "ECART",
                              TRUE ~ "OK"),
           note = a$note)
  AGREG[[length(AGREG) + 1]] <- res %>% select(controle, nom_region, annee_debut, somme_provinces,
                                              valeur_region, ecart_pct, n_manquantes, part_estimee, statut, note)
  e <- res %>% filter(statut == "ECART")
  if (nrow(e)) noter_anomalie(
    transmute(e, cycle = a$cycles, id_ligne, nom_region, nom_province = NA_character_, annee_debut),
    "AGREGATION_REGION", paste(a$var_prov, "vs", a$var_reg), e$somme_provinces, e$valeur_region,
    a$gravite, "valeurs provinciales conservees", sprintf("ecart %.2f %%", e$ecart_pct))
}
# Regions -> national (variables additives)
for (o in names(REG)) {
  r <- REG[[o]]
  nat <- r[r$niveau_geo %in% "NATIONAL", ]; reg <- r[r$niveau_geo %in% "REGION", ]
  if (!nrow(nat)) next
  add <- names(r)[str_detect(names(r), "^(eff|nb|structures|salles|struct|etab|eleves|profs|nouv)_")]
  for (v in add) {
    s <- tapply(reg[[v]], reg$annee_debut, function(z) if (all(is.na(z))) NA_real_ else sum(z, na.rm = TRUE))
    n <- nat[[v]][match(names(s), nat$annee_debut)]
    e <- 100 * (s / n - 1)
    j <- which(abs(e) > 100 * PARAM$tol_agregation)
    if (length(j)) AGREG[[length(AGREG) + 1]] <- tibble(
      controle = paste(o, v, ": somme des regions vs national"), nom_region = "NATIONAL",
      annee_debut = as.integer(names(s)[j]), somme_provinces = as.numeric(s[j]), valeur_region = n[j],
      ecart_pct = as.numeric(e[j]), n_manquantes = NA_integer_, part_estimee = NA_real_,
      statut = "ECART", note = "somme des regions (colonne somme_provinces) vs ligne nationale")
  }
}
AGREG <- bind_rows(AGREG)
noter_etape("11", "controles d'agregation en ecart",
            if (nrow(AGREG)) sum(AGREG$statut == "ECART") else 0L)

# =============================================================================
# 12. QUALITE DE LIGNE ET SYNTHESES
# =============================================================================
message("== 12. Qualite et syntheses")
ANOMALIES <- bind_rows(ETAT$anom)
if (!nrow(ANOMALIES)) ANOMALIES <- tibble(cycle = character(), id_ligne = character(), nom_region = character(),
  nom_province = character(), annee_debut = integer(), regle = character(), variables = character(),
  valeur = numeric(), attendu = numeric(), ecart = numeric(), gravite = character(), decision = character(),
  detail = character())
IMPUTATIONS <- bind_rows(ETAT$imput)
if (!nrow(IMPUTATIONS)) IMPUTATIONS <- tibble(cycle = character(), id_ligne = character(), nom_province = character(),
  annee_debut = integer(), variable = character(), valeur_avant = numeric(), origine_avant = character(),
  valeur_apres = numeric(), origine_apres = character(), methode = character())

OM <- as.matrix(O[VARS])
compte <- function(codes) rowSums(matrix(OM %in% codes, nrow = N))
CODES_IMP <- c("DED", "IMP_INTERP", "IMP_TENDANCE", "IMP_PONDERE", "IMP_RETRO", "IMP_RAPPORT", "IMP_CALAGE")
P$n_observees  <- compte("OBS")
P$n_estimees   <- compte("EST")
P$n_imputees   <- compte(CODES_IMP)
P$n_rejetees   <- compte("NEUTRALISE")
P$n_applicables <- rowSums(matrix(!OM %in% "NON_APPLICABLE", nrow = N))
P$part_observee <- round(P$n_observees / P$n_applicables, 3)
suspects <- ANOMALIES %>% filter(gravite == "ALERTE", !regle %in% "AGREGATION_REGION") %>%
  distinct(cycle, id_ligne) %>% mutate(k = paste(cycle, id_ligne)) %>% pull(k)
P$qualite <- case_when(
  !A_DONNEE_SCOL                                   ~ "MANQUANT",
  CLE_LIGNE %in% suspects | P$n_rejetees > 0       ~ "SUSPECT",
  P$n_estimees > 0                                 ~ "ESTIME",
  P$n_imputees > 0                                 ~ "IMPUTE",
  TRUE                                             ~ "OK")
obs_scol <- Reduce(`|`, lapply(intersect(VARS_SCOLAIRES, VARS), function(v) O[[v]] %in% c("OBS", "EST")))
couv <- data.frame(cycle = P$cycle, a = P$annee_debut, ok = obs_scol) %>% filter(ok) %>%
  group_by(cycle) %>% summarise(a0 = min(a), a1 = max(a), .groups = "drop")
k <- match(P$cycle, couv$cycle)
P$dans_periode_couverte <- !is.na(k) & P$annee_debut >= couv$a0[k] & P$annee_debut <= couv$a1[k]

# Panel final
PANEL <- bind_cols(
  P[c(CLES, "qualite", "qualite_source", "dans_periode_couverte", "part_observee",
      "n_observees", "n_estimees", "n_imputees", "n_rejetees")],
  P[VARS], DER[intersect(names(DER), VARS_DERIVEES)],
  P[c("zone_fermetures", "max_fermetures_esu", "periode_population", "sources", "remarque")])
ORIGINE <- bind_cols(P[c("id_ligne", "cycle", "annee_debut", "code_geo", "nom_province")], O[VARS])

# Completude
classe_origine <- function(o) case_when(o == "OBS" ~ "observe", o == "EST" ~ "estime",
  o %in% CODES_IMP ~ "impute", o == "NON_APPLICABLE" ~ "sans_objet", TRUE ~ "manquant")
COMPLETUDE <- bind_cols(P[c("cycle", "annee_debut")], O[VARS]) %>%
  pivot_longer(-c(cycle, annee_debut), names_to = "variable", values_to = "origine") %>%
  mutate(classe = classe_origine(origine)) %>%
  count(cycle, variable, annee_debut, classe) %>%
  pivot_wider(names_from = classe, values_from = n, values_fill = 0)
for (cl in c("observe", "estime", "impute", "manquant", "sans_objet"))
  if (!cl %in% names(COMPLETUDE)) COMPLETUDE[[cl]] <- 0L
COMPLETUDE <- COMPLETUDE %>%
  mutate(applicables = observe + estime + impute + manquant,
         pct_rempli_final = ifelse(applicables > 0, round(100 * (observe + estime + impute) / applicables, 1), NA),
         pct_observe = ifelse(applicables > 0, round(100 * observe / applicables, 1), NA)) %>%
  arrange(match(cycle, PARAM$onglets_cycles), match(variable, VARS), annee_debut)
COMPLETUDE_TABLEAU <- COMPLETUDE %>% filter(applicables > 0) %>%
  select(cycle, variable, annee_debut, pct_rempli_final) %>%
  pivot_wider(names_from = annee_debut, values_from = pct_rempli_final) %>%
  left_join(DICO %>% transmute(cycle = onglet, variable = code, libelle), by = c("cycle", "variable")) %>%
  relocate(libelle, .after = variable)

# Dictionnaire de la base traitee
DICT <- DICO %>% transmute(onglet, variable = code, libelle, groupe,
  type = case_when(code %in% CLES ~ "cle", code %in% VARS ~ "saisie (origine dans ORIGINE_CELLULES)",
                   code %in% VARS_DERIVEES ~ "derive (recalcule par le script)", TRUE ~ "gestion")) %>%
  bind_rows(tibble(onglet = "TOUS", groupe = "Ajouts du traitement", type = "ajout",
    variable = c("qualite", "qualite_source", "dans_periode_couverte", "part_observee", "n_observees",
                 "n_estimees", "n_imputees", "n_rejetees", "zone_fermetures", "max_fermetures_esu",
                 "periode_population"),
    libelle = c("Qualite apres traitement : OK, ESTIME, IMPUTE, SUSPECT, MANQUANT",
                "Qualite declaree dans la collecte", "Annee comprise dans la couverture observee du cycle",
                "Part des cellules applicables observees", "Cellules observees", "Cellules estimees (ESTIMATIONS)",
                "Cellules deduites ou imputees par le traitement", "Cellules rejetees non reimputees",
                "Province touchee par des fermetures (releves ESU ou nb_etab_fermes > 0)",
                "Maximum d'etablissements fermes sur les releves ESU (ordre d'enseignement du cycle)",
                "Base des projections de population (rupture de serie en 2021)")))

REGLES <- tribble(
  ~code, ~controle, ~gravite_si_echec, ~decision,
  "C1 Grille", "45 provinces x 10 annees x 4 cycles", "-", "lignes manquantes ajoutees (MANQUANT)",
  "C2/C4 IDENTITE_*", "G + F = T ; somme statuts / milieux = total", "ALERTE (INFO si estimation)", "composante unique manquante deduite ; sinon total conserve",
  "C5 DOMAINE_*", "comptes >= 0 ; taux et IPS dans leur plage", "BLOQUANT", "valeur rejetee puis reimputee",
  "C5 INEGALITE_*", "partie <= tout (salles, equipements, enseignants, populations)", "BLOQUANT / ALERTE", "partie rejetee puis reimputee (BLOQUANT) ; signalee (ALERTE)",
  "C5 ENCADREMENT_*", "taux total entre taux G et taux F", "BLOQUANT", "taux total reconstruit (ponderation G/F)",
  "C6 PIC_ISOLE", "ecart > 50 % avec annee precedente ET suivante, sans contexte", "ALERTE", "signale (rejet si PARAM$neutraliser_pics_isoles)",
  "C6 VARIATION_ATYPIQUE", "variation annuelle > 30 % sans contexte", "ALERTE", "signalee",
  "C6 RUPTURE_CONTEXTE", "choc dans une province touchee par les fermetures ou en co-mouvement", "INFO", "conserve (donnee reelle)",
  "POP_INCOHERENTE_CYCLES", "population du cycle / population 6-11 ans stable (ecart > 25 %)", "BLOQUANT", "population remplacee, taux recales",
  "C6 VARIATION_CONTEXTE", "variation en zone de fermetures, en co-mouvement ou nationale", "INFO", "conservee",
  "C6 RUPTURE_SERIE_POPULATION", "saut 2020 -> 2021 (projections RGPH 2006 -> 2019)", "INFO", "conserve ; indicatrice periode_population",
  "CALAGE_*", "composantes imputees = total observe - composantes observees", "-", "recalage proportionnel (IMP_CALAGE)",
  "C3 AGREGATION_REGION", "somme des provinces = region ; somme des regions = national", "ALERTE / INFO", "provinces conservees",
  "C10 TBS/TBA_RECALCULE", "taux publie = effectif / population x 100", "ALERTE", "taux publie conserve",
  "IPS_INCOHERENT", "IPS = taux F / taux G", "INFO", "IPS recalcule",
  "RATIO_*", "ratios d'encadrement plausibles ; z robuste en coupe", "ALERTE / INFO", "conserve")

# =============================================================================
# 13. ECRITURE DES SORTIES
# =============================================================================
message("== 13. Ecriture")
dir.create(PARAM$dossier_sortie, showWarnings = FALSE, recursive = TRUE)
f_xlsx <- file.path(PARAM$dossier_sortie, "base_traitee.xlsx")
f_rds  <- file.path(PARAM$dossier_sortie, "base_traitee.rds")
f_csv  <- file.path(PARAM$dossier_sortie, "panel_province_traite.csv")
f_md   <- file.path(PARAM$dossier_sortie, "rapport_traitement.md")

JOURNAL <- bind_rows(ETAT$journal)
synth_origine <- as.data.frame(table(origine = unlist(O[VARS])), stringsAsFactors = FALSE) %>%
  arrange(desc(Freq)) %>% rename(cellules = Freq)
synth_anom <- ANOMALIES %>% count(regle, gravite, decision, name = "n") %>% arrange(match(gravite, c("BLOQUANT", "ALERTE", "INFO")), desc(n))
synth_imp  <- IMPUTATIONS %>% count(origine_apres, methode, name = "n") %>% arrange(desc(n))

wb <- createWorkbook()
st_ent <- createStyle(textDecoration = "bold", fontColour = "#FFFFFF", fgFill = "#1F4E78",
                      wrapText = TRUE, valign = "center", halign = "center")
ecrire <- function(nom, d, largeur = 14) {
  addWorksheet(wb, nom)
  writeData(wb, nom, d, headerStyle = st_ent)
  freezePane(wb, nom, firstRow = TRUE)
  if (ncol(d)) {
    setColWidths(wb, nom, cols = seq_len(ncol(d)), widths = largeur)
    addFilter(wb, nom, rows = 1, cols = seq_len(ncol(d)))
  }
}
lisez_moi <- data.frame(LISEZ_MOI = c(
  "EDU-OPTIM Burkina - base traitee par R/02_traitement_donnees.R",
  paste("Source :", basename(PARAM$fichier_entree), "| traitement du", format(Sys.time(), "%Y-%m-%d %H:%M")),
  "",
  "PANEL_PROVINCE : 4 cycles empiles (45 provinces x 10 annees), valeurs finales et indicateurs derives recalcules.",
  "  Couleurs : bleu = estimee (EST) ; vert = deduite (DED) ; orange = imputee (IMP_*) ; rouge = rejetee non reimputee.",
  "ORIGINE_CELLULES : origine de chaque cellule (OBS, EST, DED, IMP_*, NEUTRALISE, NON_APPLICABLE, NA_*).",
  "ANOMALIES : controles echoues, gravite (BLOQUANT, ALERTE, INFO) et decision prise.",
  "IMPUTATIONS : chaque cellule modifiee (valeur avant / apres, methode).",
  "COMPLETUDE / COMPLETUDE_TABLEAU : taux de remplissage par cycle, variable et annee.",
  "AGREGATION : somme des provinces vs regions, somme des regions vs national.",
  "VARIANTES_ESTIMATION : seconde estimation (autre regle de repartition) pour les tests de robustesse.",
  "REGLES, PARAMETRES, JOURNAL, DICTIONNAIRE : documentation du traitement.",
  "",
  "Qualite de ligne : MANQUANT (aucune donnee scolaire) > SUSPECT (alerte non resolue ou valeur rejetee)",
  "  > ESTIME (au moins une cellule estimee) > IMPUTE (au moins une cellule deduite ou imputee) > OK.",
  "Regles : on impute des effectifs et on recalcule les taux ; un taux n'est jamais reparti ; aucun",
  "  chiffre regional n'est recopie sur une province ; les chocs lies a l'insecurite sont conserves."))
addWorksheet(wb, "LISEZ_MOI"); writeData(wb, "LISEZ_MOI", lisez_moi, headerStyle = st_ent)
setColWidths(wb, "LISEZ_MOI", cols = 1, widths = 130)

ecrire("PANEL_PROVINCE", PANEL, 12)
couleurs <- list(EST = "#DDEBF7", DED = "#E2EFDA", IMP_INTERP = "#FCE4D6", IMP_TENDANCE = "#FCE4D6",
                 IMP_PONDERE = "#FCE4D6", IMP_RETRO = "#FCE4D6", IMP_RAPPORT = "#FCE4D6", IMP_CALAGE = "#FCE4D6", NEUTRALISE = "#F4B183")
col_pos <- match(VARS, names(PANEL))
for (cd in names(couleurs)) {
  pos <- which(OM == cd, arr.ind = TRUE)
  if (!nrow(pos)) next
  addStyle(wb, "PANEL_PROVINCE", createStyle(fgFill = couleurs[[cd]]),
           rows = pos[, 1] + 1, cols = col_pos[pos[, 2]], gridExpand = FALSE, stack = TRUE)
}
ecrire("ORIGINE_CELLULES", ORIGINE, 12)
ecrire("ANOMALIES", ANOMALIES, 16)
ecrire("IMPUTATIONS", IMPUTATIONS, 16)
ecrire("COMPLETUDE_TABLEAU", COMPLETUDE_TABLEAU, 10)
ecrire("COMPLETUDE", COMPLETUDE, 12)
ecrire("AGREGATION", if (nrow(AGREG)) AGREG else tibble(controle = character()), 16)
ecrire("VARIANTES_ESTIMATION", EST, 14)
ecrire("REGLES", REGLES, 40)
ecrire("PARAMETRES", tibble(parametre = names(PARAM),
  valeur = vapply(PARAM, function(x) paste(deparse(x), collapse = ""), character(1))), 40)
ecrire("JOURNAL", JOURNAL, 40)
ecrire("DICTIONNAIRE", DICT, 25)
for (o in names(REG)) ecrire(o, REG[[o]], 12)
if (!is.null(ESU)) ecrire("ESU_FERMETURES", ESU, 12)
saveWorkbook(wb, f_xlsx, overwrite = TRUE)

saveRDS(list(panel = PANEL, origine = ORIGINE, anomalies = ANOMALIES, imputations = IMPUTATIONS,
             completude = COMPLETUDE, agregation = AGREG, estimations = EST, regions = REG,
             esu = ESU, dictionnaire = DICT, regles = REGLES, parametres = PARAM,
             journal = JOURNAL, date_traitement = Sys.time()), f_rds)
utils::write.csv2(PANEL, f_csv, row.names = FALSE, na = "", fileEncoding = "UTF-8")

# Rapport markdown
md_table <- function(d) {
  if (!nrow(d)) return("_(aucune ligne)_")
  d <- as.data.frame(d); d[] <- lapply(d, function(x) ifelse(is.na(x), "", as.character(x)))
  c(paste0("| ", paste(names(d), collapse = " | "), " |"),
    paste0("|", paste(rep("---", ncol(d)), collapse = "|"), "|"),
    apply(d, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |")))
}
qual <- PANEL %>% count(cycle, qualite) %>% pivot_wider(names_from = qualite, values_from = n, values_fill = 0)
couv_txt <- couv %>% mutate(periode = paste(a0, "-", a1)) %>% select(cycle, periode)
alertes <- ANOMALIES %>% filter(gravite %in% c("BLOQUANT", "ALERTE")) %>%
  arrange(match(gravite, c("BLOQUANT", "ALERTE")), regle) %>%
  select(gravite, regle, cycle, nom_province, annee_debut, variables, valeur, attendu, decision) %>%
  mutate(across(c(valeur, attendu), ~ round(.x, 2))) %>% head(40)
writeLines(c(
  "# Rapport de traitement des donnees - EDU-OPTIM Burkina", "",
  paste0("Fichier source : `", basename(PARAM$fichier_entree), "` - traitement du ", format(Sys.time(), "%d/%m/%Y %H:%M"), "."), "",
  "## Couverture observee par cycle", "", md_table(couv_txt), "",
  "## Qualite des lignes apres traitement", "", md_table(qual), "",
  "## Origine des cellules", "", md_table(synth_origine), "",
  "## Controles echoues", "", md_table(synth_anom), "",
  "## Imputations et deductions", "", md_table(synth_imp), "",
  "## Agregation provinces / regions", "",
  md_table(if (nrow(AGREG)) AGREG %>% count(controle, statut) %>% pivot_wider(names_from = statut, values_from = n, values_fill = 0) else tibble()), "",
  "## Alertes a examiner (40 premieres)", "", md_table(alertes), "",
  "## Etapes", "", md_table(JOURNAL)), f_md, useBytes = TRUE)

# =============================================================================
# 14. CONVERSION AU FORMAT EXX_01 (contrat de donnees de l'application)
# -----------------------------------------------------------------------------
#  L'application lit : DIM_GEO, PANEL_PROVINCE, PANEL_REGION, NORMES, DICTIONNAIRE
#  (en-tete repere par la colonne cle ; variables inconnues ignorees).
# =============================================================================
f_exx <- NA_character_
if (isTRUE(PARAM$exporter_exx)) {
  message("== 14. Conversion EXX_01 pour l'application")
  app <- PARAM$dossier_application
  modele <- file.path(app, "data", "modele", "EXX_01_modele_structure.xlsx")
  if (!dir.exists(app) || !file.exists(modele)) {
    warning("Conversion EXX_01 ignoree : modele introuvable (", modele, "). ",
            "Verifier PARAM$dossier_application.")
  } else {
    lire_modele <- function(feuille, cle) {
      tete <- suppressMessages(read_excel(modele, sheet = feuille, col_names = FALSE,
                                          n_max = 15, col_types = "text"))
      l <- which(apply(tete, 1, function(r) any(str_trim(r) %in% cle)))[1]
      if (is.na(l)) stop("En-tete '", cle, "' introuvable dans ", feuille, " du modele")
      d <- as.data.frame(suppressMessages(read_excel(modele, sheet = feuille, skip = l - 1,
                                                     col_types = "text")))
      names(d) <- str_trim(names(d))
      d[, !is.na(names(d)) & names(d) != "" & !str_starts(names(d), "\\.\\.\\.") &
          !duplicated(names(d)), drop = FALSE]
    }

    # DIM_GEO : referentiel du modele (codes NAT / REGxx / PRVxxxx)
    GEO <- lire_modele("DIM_GEO", "code_geo") %>% filter(!is.na(code_geo))
    hors_ref <- setdiff(unique(PANEL$code_geo), GEO$code_geo[GEO$niveau_geo %in% "PROVINCE"])
    if (length(hors_ref)) warning("Provinces absentes de DIM_GEO (ignorees par l'application) : ",
                                  paste(hors_ref, collapse = ", "))

    # PANEL_PROVINCE
    X <- PANEL
    X$remarque <- as.character(X$remarque)
    ajouter_remarque <- function(k, txt)
      ifelse(is.na(X$remarque[k]) | X$remarque[k] == "", txt, paste0(X$remarque[k], " | ", txt))
    # (a) L'application lit nouv_cp1_* comme les nouveaux inscrits de l'annee d'entree
    #     (TBA des agregats) : au post-primaire / secondaire, 6e / 2nde = nouv_entree_*
    pps <- X$cycle %in% c("POST_PRIMAIRE", "SECONDAIRE")
    for (s in c("g", "f", "t")) {
      a <- paste0("nouv_cp1_", s); b <- paste0("nouv_entree_", s)
      if (all(c(a, b) %in% names(X))) X[[a]][pps] <- X[[b]][pps]
    }
    # (b) Ecoles fermees : convention de l'application (total = ouverts + fermes)
    n_ferm <- 0
    if (isTRUE(PARAM$exx_total_inclut_fermes) &&
        all(c("nb_etab_t", "nb_etab_fermes", "nb_etab_fonctionnels") %in% names(X))) {
      k <- which(!is.na(X$nb_etab_fermes) & !is.na(X$nb_etab_t))
      X$nb_etab_fonctionnels[k] <- X$nb_etab_t[k]
      X$nb_etab_t[k] <- X$nb_etab_t[k] + X$nb_etab_fermes[k]
      X$remarque[k] <- ajouter_remarque(k, "EXX : nb_etab_t = ouverts (annuaire) + fermes (ESU)")
      n_ferm <- length(k)
    }
    noter_etape("14", "lignes dont nb_etab_t inclut les ecoles fermees", n_ferm)

    # PANEL_REGION : primaire et prescolaire (le post-primaire / secondaire regional
    # melange les deux cycles et n'a pas le meme champ : non exporte)
    renommer <- c(structures_t = "nb_etab_t", salles_t = "nb_salles_t")
    PR <- bind_rows(lapply(intersect(c("PRE_REGION", "PRI_REGION"), names(REG)), function(o) {
      r <- REG[[o]]
      for (a in names(renommer))
        if (a %in% names(r) && !renommer[[a]] %in% names(r)) names(r)[names(r) == a] <- renommer[[a]]
      garder <- c("id_ligne", "cycle", "annee_scolaire", "annee_debut", "niveau_geo", "code_geo",
                  "nom_region", intersect(VARS, names(r)), "sources")
      r <- r[, intersect(garder, names(r)), drop = FALSE]
      if ("sources" %in% names(r)) r$sources <- as.character(r$sources)
      r
    }))

    # DICTIONNAIRE : celui du traitement, avec la convention d'export
    DX <- DICT %>% filter(onglet %in% c("PRIMAIRE", "TOUS")) %>% select(-onglet) %>%
      mutate(libelle = case_when(
        str_starts(variable, "nouv_cp1_") ~ paste(libelle, "(post-primaire / secondaire : nouveaux inscrits en 6e / 2nde)"),
        variable == "nb_etab_t" & isTRUE(PARAM$exx_total_inclut_fermes) ~ paste(libelle, "(ouverts + fermes quand nb_etab_fermes est renseigne)"),
        TRUE ~ libelle))

    # Normes : fichier source de l'application s'il existe, sinon feuille (vide) du modele
    f_normes <- file.path(app, "config", "normes_officielles.csv")
    NORMES_MOD <- if (file.exists(f_normes)) {
      x <- utils::read.csv2(f_normes, fileEncoding = "UTF-8", stringsAsFactors = FALSE, check.names = FALSE)
      noter_etape("14", "normes reprises de config/normes_officielles.csv", nrow(x))
      x
    } else tryCatch(lire_modele("NORMES", "code_parametre"), error = function(e) NULL)

    wbx <- createWorkbook()
    feuille <- function(nom, d) {
      addWorksheet(wbx, nom); writeData(wbx, nom, d, headerStyle = st_ent)
      freezePane(wbx, nom, firstRow = TRUE)
      if (ncol(d)) setColWidths(wbx, nom, cols = seq_len(ncol(d)), widths = 13)
    }
    feuille("LISEZ_MOI", data.frame(LISEZ_MOI = c(
      "Base EDU-OPTIM au format EXX_01, produite par 02_traitement_donnees.R (etape 14).",
      paste("Source :", basename(PARAM$fichier_entree), "| traitement du", format(Sys.time(), "%Y-%m-%d %H:%M")),
      "PANEL_PROVINCE = panel traite (valeurs observees, estimees, deduites ou imputees ; origine detaillee dans base_traitee.xlsx).",
      "Post-primaire / secondaire : nouv_cp1_* contient les nouveaux inscrits en 6e / 2nde (nouv_entree_*).",
      if (isTRUE(PARAM$exx_total_inclut_fermes)) "nb_etab_t = ouverts + fermes quand nb_etab_fermes est renseigne (2021, releve ESU du 31/12/2021)." else "nb_etab_t = etablissements recenses (ouverts).",
      "NORMES : config/normes_officielles.csv de l'application (sources et statut par norme) ; a defaut, normes de demonstration.",
      "Ne pas diffuser sans validation : donnees de collecte a valider.")))
    feuille("DIM_GEO", GEO)
    feuille("PANEL_PROVINCE", X)
    feuille("PANEL_REGION", PR)
    if (!is.null(NORMES_MOD)) feuille("NORMES", NORMES_MOD)
    feuille("DICTIONNAIRE", DX)
    f_exx <- file.path(app, PARAM$exx_sortie)
    dir.create(dirname(f_exx), showWarnings = FALSE, recursive = TRUE)
    saveWorkbook(wbx, f_exx, overwrite = TRUE)
    noter_etape("14", "lignes exportees dans PANEL_PROVINCE (EXX_01)", nrow(X))

    # Bascule de l'application sur les donnees reelles (copie de sauvegarde gardee)
    if (isTRUE(PARAM$exx_activer_config)) {
      cf <- file.path(app, "config", "config.yml")
      if (file.exists(cf)) {
        L <- readLines(cf, encoding = "UTF-8", warn = FALSE)
        if (!file.exists(paste0(cf, ".avant_reel"))) file.copy(cf, paste0(cf, ".avant_reel"))
        L <- sub("^source_donnees:.*$", 'source_donnees: "reel"', L)
        i_reel <- grep("^\\s+reel:\\s*$", L)[1]
        j <- grep("^\\s+base_exx:", L); j <- j[j > i_reel][1]
        if (!is.na(i_reel) && !is.na(j))
          L[j] <- sub("base_exx:.*$", sprintf('base_exx: "%s"', PARAM$exx_sortie), L[j])
        writeLines(L, cf, useBytes = TRUE)
        message("  config.yml : source_donnees = reel ; base_exx = ", PARAM$exx_sortie,
                " (sauvegarde : config.yml.avant_reel)")
      }
    }
  }
}

message("\n== Termine ==")
message("  Panel : ", nrow(PANEL), " lignes x ", ncol(PANEL), " colonnes")
message("  Qualite : ", paste(names(table(PANEL$qualite)), table(PANEL$qualite), sep = " = ", collapse = " | "))
message("  Anomalies : ", paste(names(table(ANOMALIES$gravite)), table(ANOMALIES$gravite), sep = " = ", collapse = " | "))
message("  Cellules modifiees : ", nrow(IMPUTATIONS))
message("  Sorties : ", f_xlsx, " | ", f_rds, " | ", f_csv, " | ", f_md)
if (!is.na(f_exx)) message("  Base de l'application : ", f_exx,
  "\n  Lancer l'application : setwd(\"", PARAM$dossier_application, "\") ; shiny::runApp()")
