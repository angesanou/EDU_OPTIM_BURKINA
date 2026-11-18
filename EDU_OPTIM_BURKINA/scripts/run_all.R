# ==========================================================================
# run_all.R — Chaîne complète reproductible
# ==========================================================================
#   source("scripts/run_all.R", encoding = "UTF-8")
# 1. génère les données synthétiques (seulement si source = synthetique)
# 2. contrôle la qualité   3. prépare indicateurs et IMDE   4. lance les tests

t0 <- Sys.time()
cfg <- yaml::read_yaml("config/config.yml")
if (identical(cfg$source_donnees, "synthetique")) source("scripts/01_generer_donnees_synthetiques.R", encoding = "UTF-8")
source("scripts/02_controler_donnees.R", encoding = "UTF-8")
source("scripts/03_preparer_donnees.R", encoding = "UTF-8")
testthat::test_dir("tests/testthat", reporter = "summary", stop_on_failure = FALSE)
message(sprintf("Chaîne complète terminée en %.1f min. Lancez l'application : shiny::runApp()",
                as.numeric(Sys.time() - t0, units = "mins")))
