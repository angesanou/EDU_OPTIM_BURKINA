# ==========================================================================
# 02_controler_donnees.R — Contrôle de cohérence et de qualité des données
# ==========================================================================
# Applique les contrôles à la source déclarée dans config/config.yml
# (synthétique ou réelle) et écrit un rapport Excel dans resultats/exports/.
#   source("scripts/02_controler_donnees.R", encoding = "UTF-8")

for (f in sort(list.files("R", pattern = "^[0-9].*\\.R$", full.names = TRUE))) source(f, encoding = "UTF-8")
cfg <- lire_config()
d <- charger_donnees(cfg, forcer = TRUE)
q_panel <- controler_panel(d$panel, d$geo)
q_etab <- controler_etablissements(d$etablissements)
manq <- manquants_par_variable(d$panel)
print(q_panel[, c("controle", "anomalies", "statut")])
if (!is.null(q_etab)) print(q_etab[, c("controle", "anomalies", "statut")])
dir.create("resultats/exports", showWarnings = FALSE, recursive = TRUE)
fichier <- file.path("resultats/exports", sprintf("controle_qualite_%s_%s.xlsx", cfg$source_donnees, format(Sys.Date(), "%Y%m%d")))
exporter_excel(list(Controles_panel = q_panel, Controles_etablissements = q_etab,
                    Manquants_par_variable = manq[order(-manq$taux), ], Normes = d$normes,
                    Journal = JOURNAL$entrees), fichier, cfg$actif$mention)
message("Rapport de contrôle : ", fichier)
