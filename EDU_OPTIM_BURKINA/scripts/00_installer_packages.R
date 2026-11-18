# ==========================================================================
# 00_installer_packages.R — Installe les packages nécessaires (une seule fois)
# ==========================================================================
# À lancer depuis RStudio, projet EDU_OPTIM_BURKINA ouvert :
#   source("scripts/00_installer_packages.R")
# Connexion Internet requise UNIQUEMENT pour cette étape.

PACKAGES <- c(
  # Application
  "shiny", "bslib", "plotly", "leaflet", "DT", "htmltools",
  # Données
  "readxl", "openxlsx", "data.table", "yaml", "digest", "commonmark",
  # Géographie
  "sf", "spdep",
  # Statistiques, économétrie, prévision, optimisation
  "plm", "lmtest", "sandwich", "car", "forecast", "lpSolve",
  # Graphiques statiques et tests
  "ggplot2", "testthat", "renv")

manquants <- setdiff(PACKAGES, rownames(installed.packages()))
if (length(manquants)) {
  message("Installation de : ", paste(manquants, collapse = ", "))
  install.packages(manquants, repos = "https://cloud.r-project.org",
                   type = if (.Platform$OS.type == "windows") "binary" else getOption("pkgType"))
} else message("Tous les packages sont déjà installés.")
ok <- vapply(PACKAGES, requireNamespace, logical(1), quietly = TRUE)
if (all(ok)) message("✔ Environnement prêt.") else warning("Packages non chargés : ", paste(PACKAGES[!ok], collapse = ", "))
