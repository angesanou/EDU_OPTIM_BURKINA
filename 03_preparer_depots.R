# ==========================================================================
# 03_preparer_depots.R - Dossiers de depot du challenge (Defi 3, EDU-OPTIM)
# ==========================================================================
# Construit depots/01_collecte ... depots/05_interpretation a partir des
# fichiers produits par la chaine, avec le nommage du reglement :
#   EQUIPE_ETAPE_DEFI_ROLE_description_vN.ext   (sans espace ni accent)
# et un LISEZ-MOI par dossier. Les fichiers sources ne sont jamais modifies :
# ils sont COPIES. Relancer le script ecrase les copies precedentes.
#
# Usage : depuis le dossier Hackaton_MTDPCE
#   source("03_preparer_depots.R", encoding = "UTF-8")
# Avant le depot final : renseigner EQUIPE ci-dessous (numero attribue).
# Le Data Pack n'est pas depose (fichiers publics du challenge, non modifies).
# ==========================================================================

EQUIPE  <- "Datagenius" # nom de l'equipe (pas de numero attribue)
DEFI    <- "D3"
VERSION <- "v1"
NOM_EQUIPE <- "Datagenius"

racine <- getwd()
if (!file.exists("02_traitement_donnees.R")) stop("Lancer ce script depuis le dossier Hackaton_MTDPCE.")
app <- "EDU_OPTIM_BURKINA"
sortie <- file.path(racine, "depots")

# --- Contenu de chaque depot -----------------------------------------------
# role : engineer / architect / analyst / scientist (cf. memo de cadrage)
# src  : chemin relatif a Hackaton_MTDPCE ; un dossier est zippe.
DEPOTS <- list(
  "01_collecte" = list(
    titre = "Collecte des donnees",
    objet = "Base collectee par province et par annee (prescolaire, primaire, post-primaire, secondaire), variables de contexte et limites administratives.",
    fichiers = list(
      list(src = "Collecte_data_rempli_v6.xlsx", role = "engineer", desc = "base-collecte-provinces-2015-2024", v = "v6",
           texte = "Base de collecte remplie (onglets PANEL_PROVINCE, PANEL_REGION, DIM_GEO, DICTIONNAIRE...). Version 6."),
      list(src = "EXX_01-collecte_D3_architect_structure-base-donnees_v1.xlsx", role = "architect", desc = "structure-base-donnees",
           texte = "Gabarit vide de la base : structure des onglets, dictionnaire des variables et dimensions."),
      list(src = file.path(app, "data/reelles/contexte_province.csv"), role = "engineer", desc = "contexte-pauvrete-urbanisation-insecurite",
           texte = "Variables de contexte par province et annee : pauvrete (INSD EHCVM 2018 et 2021, niveau regional interpole), urbanisation (RGPH 2019, tableau 59), insecurite (part des ecoles primaires fermees au 31/12/2021)."),
      list(src = file.path(app, "data/reelles/LISEZ_MOI_contexte.txt"), role = "engineer", desc = "sources-contexte",
           texte = "Sources et regles de construction du fichier de contexte."),
      list(src = file.path(app, "data/reelles/limites_provinces_geoBoundaries_BFA_ADM2.geojson"), role = "engineer", desc = "limites-45-provinces",
           texte = "Limites des 45 provinces (geoBoundaries, d'apres la Banque mondiale, licence CC BY 4.0). Komonjdjari renomme Komondjari."))),
  "02_nettoyage" = list(
    titre = "Nettoyage et controle des donnees",
    objet = "Traitement des valeurs manquantes et incoherentes, imputations tracees cellule par cellule.",
    fichiers = list(
      list(src = "02_traitement_donnees.R", role = "engineer", desc = "script-traitement-donnees",
           texte = "Script R complet : controles structurels, types de valeurs manquantes, regles de coherence, controles temporels, imputations (interpolation, tendance, calage), indicateurs derives, export."),
      list(src = "data/traitees/rapport_traitement.md", role = "engineer", desc = "rapport-traitement",
           texte = "Rapport genere par le script : anomalies detectees, corrections, imputations et origine de chaque valeur."),
      list(src = "data/traitees/base_traitee.xlsx", role = "engineer", desc = "base-traitee-tracee",
           texte = "Base apres traitement, avec la colonne d'origine de chaque cellule (OBS, EST, DED, IMP_*, NEUTRALISE, NA_*)."))),
  "03_transformation" = list(
    titre = "Transformation pour l'analyse",
    objet = "Base au format d'entree de l'application (EXX_01) et panel province x annee.",
    fichiers = list(
      list(src = file.path(app, "data/reelles/EXX_01_base_reelle.xlsx"), role = "engineer", desc = "base-application-exx01",
           texte = "Base au format EXX_01 (DIM_GEO, PANEL_PROVINCE, PANEL_REGION, NORMES, DICTIONNAIRE) lue par l'application."),
      list(src = "data/traitees/panel_province_traite.csv", role = "engineer", desc = "panel-province-annee",
           texte = "Panel province x annee x cycle traite, format plat (CSV, UTF-8)."),
      list(src = file.path(app, "config/normes_officielles.csv"), role = "architect", desc = "normes-officielles",
           texte = "Normes utilisees pour les deficits (taille de classe, eleves par enseignant, places par table-banc, eleves par latrine), avec leurs sources (MENA 2022, PSEF 2017-2030...)."))),
  "04_analyse" = list(
    titre = "Analyse et application",
    objet = "Application Shiny EDU-OPTIM : indicateurs, IMDE, cartographie et autocorrelation spatiale, econometrie de panel, previsions 2030, priorisation TOPSIS, optimisation des ressources.",
    fichiers = list(
      list(src = app, role = "architect", desc = "application-edu-optim", zip = TRUE,
           texte = "Code complet de l'application (R/, modules/, config/, tests/, documentation/, data/reelles/). Lancement : LANCER_EDU_OPTIM.bat ou run_all.R --app."),
      list(src = "run_all.R", role = "engineer", desc = "reproduction-chaine-complete",
           texte = "Relance toute la chaine : environnement, traitement, preparation de l'application, tests unitaires."),
      list(src = file.path(app, "documentation/methodologie_scientifique.md"), role = "scientist", desc = "methodologie-scientifique",
           texte = "Methodologie : indicateurs, IMDE (5 dimensions, 16 indicateurs), econometrie, previsions, TOPSIS, optimisation lineaire, adaptations aux donnees disponibles."))),
  "05_interpretation" = list(
    titre = "Interpretation et restitution",
    objet = "Rapports produits par l'application et documentation du projet.",
    fichiers = list(
      list(src = file.path(app, "resultats/rapports"), role = "analyst", desc = "rapports-application", zip = TRUE, dernier = TRUE,
           texte = "Derniers rapports generes par l'application sur les donnees reelles (rapport executif PDF, fiche territoriale, classeur Excel)."),
      list(src = "documentation/EDU-OPTIM_documentation_15p.pdf", role = "analyst", desc = "documentation-15-pages",
           texte = "Documentation du projet (15 pages, plan officiel du challenge)."),
      list(src = "documentation/journal_usage_IA.md", role = "analyst", desc = "journal-usage-ia",
           texte = "Journal d'usage de l'IA : ce qui a ete demande, ce qui a ete retenu, verifications faites par l'equipe.")))
)

# --- Outils ----------------------------------------------------------------
sans_accent <- function(x) {
  x <- iconv(x, "UTF-8", "ASCII//TRANSLIT", sub = "")
  x <- gsub("[^A-Za-z0-9._-]+", "-", x)
  gsub("-+", "-", x)
}
nom_depot <- function(etape, f) {
  num <- sub("_.*", "", etape); lib <- sub("^[0-9]+_", "", etape)
  ext <- if (isTRUE(f$zip)) "zip" else tools::file_ext(f$src)
  sans_accent(sprintf("%s_%s-%s_%s_%s_%s_%s.%s", EQUIPE, num, lib, DEFI, f$role, f$desc, f$v %||% VERSION, ext))
}
`%||%` <- function(a, b) if (is.null(a)) b else a
zipper <- function(dossier, cible, dernier = FALSE) {
  exclus <- c("archives", "renv/library", "renv/staging", "outils", ".Rproj.user", "resultats/cache", "resultats/rapports",
              "data/synthetique", "data/prepare")   # regenerables par run_all.R
  fich <- list.files(dossier, recursive = TRUE, all.files = FALSE)
  fich <- fich[!Reduce(`|`, lapply(exclus, function(e) startsWith(fich, e)), FALSE)]
  fich <- fich[!grepl("\\.log$|\\.avant_", fich)]
  if (dernier) {   # rapports : seule la version la plus recente de chaque produit
    produit <- sub("_[0-9]{8}_[0-9]{4}\\.[a-z]+$", "", fich)
    mt <- file.mtime(file.path(dossier, fich))
    fich <- fich[ave(as.numeric(mt), produit, FUN = function(z) z == max(z)) == 1]
  }
  if (requireNamespace("zip", quietly = TRUE)) {
    zip::zip(cible, files = fich, root = dossier)          # paquet zip (installe avec openxlsx)
  } else {
    old <- setwd(dossier); on.exit(setwd(old))
    utils::zip(cible, fich, flags = "-r9Xq")               # necessite un programme zip (Rtools)
  }
  length(fich)
}

# --- Construction ----------------------------------------------------------
dir.create(sortie, showWarnings = FALSE)
bilan <- list()
for (etape in names(DEPOTS)) {
  d <- DEPOTS[[etape]]; dos <- file.path(sortie, etape)
  dir.create(dos, showWarnings = FALSE)
  lignes <- c()
  for (f in d$fichiers) {
    nom <- nom_depot(etape, f); cible <- file.path(dos, nom); src <- file.path(racine, f$src)
    statut <- if (!file.exists(src)) "A AJOUTER (fichier absent)" else {
      if (isTRUE(f$zip)) { n <- zipper(src, cible, dernier = isTRUE(f$dernier)); sprintf("archive de %d fichiers", n) }
      else { file.copy(src, cible, overwrite = TRUE); "copie" }
    }
    bilan[[length(bilan) + 1]] <- data.frame(depot = etape, fichier = nom, origine = f$src, statut = statut)
    lignes <- c(lignes, sprintf("- %s", nom), sprintf("    Origine : %s", f$src),
                sprintf("    Contenu : %s", f$texte), if (!file.exists(src)) "    ETAT : a ajouter avant le depot final.", "")
  }
  lisez <- c(
    sprintf("LISEZ-MOI - %s - %s", etape, d$titre), strrep("=", 60), "",
    sprintf("Equipe : %s (%s) - Defi 3 : EDU-OPTIM Burkina - Data Challenge Burkina 2026", NOM_EQUIPE, EQUIPE),
    sprintf("Genere le : %s par 03_preparer_depots.R", format(Sys.time(), "%d/%m/%Y %H:%M")), "",
    "Objet du depot", "--------------", d$objet, "",
    "Fichiers", "--------", lignes,
    "Nommage", "-------",
    "EQUIPE_ETAPE_DEFI_ROLE_description_vN.ext (reglement du challenge), sans espace ni accent.", "",
    "Reproduction", "------------",
    "Depuis le dossier du projet : Rscript run_all.R (chaine complete) ; Rscript run_all.R --app (avec l'application).",
    "Versions des paquets : EDU_OPTIM_BURKINA/versions_paquets.csv (et renv.lock si present : renv::restore()).", "",
    "Sources et regles", "-----------------",
    "Sources officielles uniquement (MENAPLN/DGESS, CNS, INSD, CONASUR, OCHA/HDX, geoBoundaries).",
    "Chaque valeur de la base traitee porte son origine (observee, estimee, deduite, imputee, neutralisee).",
    "Les chiffres regionaux ne sont jamais reportes sur les provinces.")
  writeLines(enc2utf8(lisez), file.path(dos, sprintf("%s_%s_LISEZ-MOI.txt", EQUIPE, sub("_.*", "", etape))), useBytes = TRUE)
}
bilan <- do.call(rbind, bilan)
print(bilan, right = FALSE, row.names = FALSE)
utils::write.csv(bilan, file.path(sortie, "inventaire_depots.csv"), row.names = FALSE, fileEncoding = "UTF-8")

a_ajouter <- sum(grepl("^A AJOUTER", bilan$statut))
if (a_ajouter) message(a_ajouter, " fichier(s) a ajouter avant le depot final (voir inventaire_depots.csv).")
