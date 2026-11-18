# ==========================================================================
# global.R — Chargé UNE fois au démarrage de l'application
# ==========================================================================
# 1. Packages  2. Fonctions (R/)  3. Modules (modules/)  4. Données préparées
# Lancement : ouvrir EDU_OPTIM_BURKINA.Rproj dans RStudio puis
#   shiny::runApp()      (ou bouton « Run App » sur ui.R / server.R)

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(plotly)
  library(leaflet)
  library(DT)
  library(sf)
  library(ggplot2)
})
options(shiny.maxRequestSize = 200 * 1024^2, scipen = 999)
sf::sf_use_s2(FALSE)

# Fonctions générales (ordre maîtrisé par le préfixe numérique)
for (f in sort(list.files("R", pattern = "^[0-9].*\\.R$", full.names = TRUE))) source(f, encoding = "UTF-8")
# Modules Shiny (un dossier par module)
for (f in list.files("modules", pattern = "^mod_.*\\.R$", full.names = TRUE, recursive = TRUE)) source(f, encoding = "UTF-8")

CONFIG <- lire_config()
initialiser_comptes_demo()
message("EDU-OPTIM : préparation des données (", CONFIG$source_donnees, ")…")
DONNEES <- preparer_tout(CONFIG)
ANNEES <- sort(unique(DONNEES$ind$province$annee_debut))
ANNEE_MAX <- max(ANNEES)
EST_SYNTHETIQUE <- identical(DONNEES$d$meta$type, "synthetique")
message("EDU-OPTIM : prêt (", length(ANNEES), " années, ",
        length(unique(DONNEES$ind$province$code_geo)), " provinces).")

# Navigation : identifiant de page, libellé, icône, section
PAGES <- data.frame(
  id = c("accueil", "tableau_bord", "donnees", "diagnostic", "imde", "cartographie", "econometrie",
         "previsions", "priorisation", "optimisation", "simulations", "rapports", "documentation", "utilisateurs"),
  titre = c("Accueil", "Tableau de bord", "Données et sources", "Diagnostic éducatif", "Indice IMDE",
            "Cartographie et analyse spatiale", "Analyse économétrique", "Prévisions et projections",
            "Priorisation territoriale", "Optimisation des ressources", "Simulations et scénarios",
            "Rapports et exportations", "Documentation et méthodologie", "Utilisateurs et administration"),
  court = c("Accueil", "Tableau de bord", "Données et sources", "Diagnostic", "Indice IMDE", "Cartographie",
            "Économétrie", "Prévisions", "Priorisation", "Optimisation", "Simulations", "Rapports",
            "Documentation", "Utilisateurs"),
  icone = c("house", "gauge-high", "database", "stethoscope", "layer-group", "map-location-dot", "chart-line",
            "arrow-trend-up", "ranking-star", "sliders", "flask", "file-arrow-down", "book-open", "user-shield"),
  section = c("Pilotage", "Pilotage", "Données", "Analyse", "Analyse", "Analyse", "Analyse", "Analyse",
              "Décision", "Décision", "Décision", "Restitution", "Restitution", "Administration"),
  stringsAsFactors = FALSE)
