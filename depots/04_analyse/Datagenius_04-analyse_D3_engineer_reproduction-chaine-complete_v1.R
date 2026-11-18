# ==========================================================================
# run_all.R - Reproduction complete de la chaine EDU-OPTIM (Defi 3)
# ==========================================================================
# Usage (depuis le dossier Hackaton_MTDPCE, dans RStudio ou en ligne de commande) :
#   source("run_all.R")              # chaine complete
#   Rscript run_all.R                # idem, hors RStudio
#   Rscript run_all.R --sans-tests   # sans les tests unitaires
#   Rscript run_all.R --app          # lance l'application a la fin
#   Rscript run_all.R --renv         # restaure d'abord les versions figees (renv.lock)
#
# Etapes :
#   1. Verification de l'environnement (version de R, paquets, renv si present)
#   2. Traitement des donnees collectees  -> 02_traitement_donnees.R
#        (valeurs manquantes, incoherences, imputations, indicateurs,
#         export au format EXX_01 dans EDU_OPTIM_BURKINA/data/reelles/)
#   3. Controle et preparation des donnees de l'application (scripts 02 et 03
#      de EDU_OPTIM_BURKINA : lecture EXX_01, indicateurs, IMDE, cache)
#   4. Tests unitaires (tests/testthat)
#   5. Journal de reproduction : resultats/journal_run_all.txt
#   6. (optionnel) lancement de l'application
# Le script s'arrete a la premiere erreur et indique l'etape en cause.
# ==========================================================================

t_debut <- Sys.time()
args <- commandArgs(trailingOnly = TRUE)
avec_tests <- !("--sans-tests" %in% args)
lancer_app <- "--app" %in% args

# --- Dossier racine : celui qui contient ce script -------------------------
racine <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) NULL)
if (is.null(racine)) {
  a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  racine <- if (length(a)) dirname(normalizePath(sub("^--file=", "", a[1]))) else getwd()
}
setwd(racine)
dossier_app <- file.path(racine, "EDU_OPTIM_BURKINA")
dir.create(file.path(dossier_app, "resultats"), showWarnings = FALSE, recursive = TRUE)
journal <- file.path(dossier_app, "resultats", "journal_run_all.txt")
cat("", file = journal)
ecrire <- function(...) {
  ligne <- paste0(format(Sys.time(), "%H:%M:%S"), "  ", paste0(..., collapse = ""))
  message(ligne); cat(ligne, "\n", file = journal, append = TRUE, sep = "")
}
etape <- function(nom, expr) {
  ecrire("=== ", nom, " ===")
  t0 <- Sys.time()
  res <- tryCatch(force(expr), error = function(e) {
    ecrire("ECHEC a l'etape '", nom, "' : ", conditionMessage(e))
    stop("Chaine interrompue a l'etape : ", nom, call. = FALSE)
  })
  ecrire("    termine en ", round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1), " s")
  invisible(res)
}

# --- 1. Environnement ------------------------------------------------------
etape("1. Environnement", {
  ecrire("R ", R.version$major, ".", R.version$minor, " | ", R.version$platform, " | dossier : ", racine)
  # Versions figees : restauration seulement sur demande (option --renv), car renv
  # bascule alors sur une bibliotheque propre au projet.
  if ("--renv" %in% args && file.exists(file.path(dossier_app, "renv.lock")) && requireNamespace("renv", quietly = TRUE)) {
    ecrire("Option --renv : restauration des versions figees (renv::restore)")
    libs <- .libPaths()
    renv::restore(project = dossier_app, library = libs[1], prompt = FALSE)
    .libPaths(libs)
  }
  requis <- c(# traitement des donnees (02_traitement_donnees.R)
              "readxl", "dplyr", "tidyr", "stringr", "stringi", "openxlsx",
              # application (EDU_OPTIM_BURKINA/DESCRIPTION)
              "shiny", "bslib", "plotly", "leaflet", "DT", "htmltools", "data.table", "yaml",
              "digest", "commonmark", "sf", "spdep", "plm", "lmtest", "sandwich", "car",
              "forecast", "lpSolve", "ggplot2", "testthat")
  manquants <- requis[!vapply(requis, requireNamespace, logical(1), quietly = TRUE)]
  if (length(manquants)) stop("Paquets manquants : ", paste(manquants, collapse = ", "),
                              ". Installez-les avec install.packages() ou renv::restore().")
  ecrire("Paquets requis presents (", length(requis), ")")
  # Trace des versions exactes (complement de renv.lock, ou a defaut de renv)
  v <- data.frame(paquet = requis, version = vapply(requis, function(p) as.character(utils::packageVersion(p)), ""),
                  R = paste(R.version$major, R.version$minor, sep = "."))
  utils::write.csv(v, file.path(dossier_app, "versions_paquets.csv"), row.names = FALSE)
  ecrire("    versions figees dans EDU_OPTIM_BURKINA/versions_paquets.csv")
})

# --- 2. Traitement des donnees collectees ----------------------------------
etape("2. Traitement des donnees (02_traitement_donnees.R)", {
  if (!file.exists("02_traitement_donnees.R")) stop("02_traitement_donnees.R introuvable dans ", racine)
  env <- new.env()
  sys.source("02_traitement_donnees.R", envir = env, keep.source = FALSE)
  sorties <- c("data/traitees/base_traitee.xlsx", "data/traitees/rapport_traitement.md",
               file.path("EDU_OPTIM_BURKINA", "data", "reelles", "EXX_01_base_reelle.xlsx"))
  absentes <- sorties[!file.exists(sorties)]
  if (length(absentes)) stop("Sorties attendues absentes : ", paste(absentes, collapse = ", "))
  for (s in sorties) ecrire("    ", s, " (", round(file.size(s) / 1024), " Ko)")
})

# --- 3. Preparation des donnees de l'application ---------------------------
# Scripts de l'application : controle qualite de la base EXX_01, puis calcul
# des indicateurs, de l'IMDE et du cache lu au demarrage.
etape("3. Controle et preparation des donnees de l'application", {
  setwd(dossier_app); on.exit(setwd(racine), add = TRUE)
  cfg <- yaml::read_yaml("config/config.yml")
  ecrire("    source des donnees : ", cfg$source_donnees)
  if (!identical(cfg$source_donnees, "reel")) warning("config.yml n'est pas sur la source 'reel'")
  for (s in c("scripts/02_controler_donnees.R", "scripts/03_preparer_donnees.R")) {
    ecrire("    ", s)
    sys.source(s, envir = new.env(), keep.source = FALSE)
  }
})

# --- 4. Tests unitaires ----------------------------------------------------
if (avec_tests) etape("4. Tests unitaires", {
  setwd(dossier_app); on.exit(setwd(racine), add = TRUE)
  r <- as.data.frame(testthat::test_dir("tests/testthat", reporter = "summary", stop_on_failure = FALSE))
  ecrire("    ", nrow(r), " tests ; echecs = ", sum(r$failed), " ; erreurs = ", sum(r$error))
  if (sum(r$failed) + sum(r$error) > 0) stop("des tests echouent")
})

ecrire("Chaine complete reproduite en ",
       round(as.numeric(difftime(Sys.time(), t_debut, units = "mins")), 1), " min. Journal : ", journal)

# --- 5. Lancement de l'application (optionnel) -----------------------------
if (lancer_app) shiny::runApp(dossier_app, launch.browser = TRUE)
