# ==========================================================================
# 04_figer_dependances_renv.R — Crée renv.lock (versions exactes des packages)
# ==========================================================================
# À lancer UNE fois, quand l'application fonctionne sur votre poste :
#   source("scripts/04_figer_dependances_renv.R")
# Le fichier renv.lock produit permet ensuite de réinstaller exactement les
# mêmes versions sur un autre poste avec :  renv::restore()
# Les dépendances sont déclarées dans le fichier DESCRIPTION.

if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
renv::settings$snapshot.type("explicit", persist = FALSE)
renv::snapshot(project = ".", library = .libPaths(), type = "explicit", prompt = FALSE)
message("renv.lock créé. Sur un autre poste : install.packages('renv') puis renv::restore()")
