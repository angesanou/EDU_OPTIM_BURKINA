# ==========================================================================
# 00_config.R — Constantes, palette, configuration
# ==========================================================================
# Ce fichier ne contient AUCUN calcul : seulement des constantes partagées
# par tous les autres fichiers (couleurs, codes des cycles, libellés).

# --- Cycles couverts (ordre logique) ---------------------------------------
CYCLES <- c(
  PRESCOLAIRE   = "Préscolaire",
  PRIMAIRE      = "Primaire",
  POST_PRIMAIRE = "Post-primaire",
  SECONDAIRE    = "Secondaire"
)
PREFIXE_CYCLE <- c(PRESCOLAIRE = "PRE", PRIMAIRE = "PRI",
                   POST_PRIMAIRE = "PPR", SECONDAIRE = "SEC")

# --- Les 13 anciennes régions (découpage de référence du projet) ----------
REGIONS <- c(
  REG01 = "Boucle du Mouhoun", REG02 = "Cascades", REG03 = "Centre",
  REG04 = "Centre-Est", REG05 = "Centre-Nord", REG06 = "Centre-Ouest",
  REG07 = "Centre-Sud", REG08 = "Est", REG09 = "Hauts-Bassins",
  REG10 = "Nord", REG11 = "Plateau Central", REG12 = "Sahel",
  REG13 = "Sud-Ouest"
)

# --- Identité visuelle ------------------------------------------------------
COULEURS <- list(
  nuit       = "#0B1A2E",
  nuit_2     = "#13263F",
  petrole    = "#0E6E7E",
  turquoise  = "#1FB5A8",
  surface    = "#F6F7F9",
  carte      = "#FFFFFF",
  bordure    = "#E6EAF0",
  texte      = "#142235",
  texte_2    = "#5A6B80",
  vert       = "#2E9E6B",
  orange     = "#E08A2E",
  rouge      = "#D1495B",
  violet     = "#6C5CE7",
  bleu       = "#2F6FDE"
)
# Palette qualitative (séries) et séquentielle (déficits : favorable -> critique)
PALETTE_SERIES <- c("#0E6E7E", "#1FB5A8", "#E08A2E", "#6C5CE7",
                    "#2F6FDE", "#D1495B", "#2E9E6B", "#8A94A6")
PALETTE_CYCLES <- setNames(c("#6C5CE7", "#0E6E7E", "#1FB5A8", "#E08A2E"), names(CYCLES))
PALETTE_DEFICIT <- c("#2E9E6B", "#A8D5BA", "#F3E3A6", "#F0A35E", "#D1495B")
CLASSES_IMDE <- c("Favorable", "Modéré", "Préoccupant", "Sévère", "Critique")

# --- Configuration (config/config.yml) ---------------------------------------
#' Lit la configuration et résout les chemins de la source active.
lire_config <- function(chemin = "config/config.yml") {
  if (!file.exists(chemin)) stop("Fichier de configuration introuvable : ", chemin,
                                 "\nLe répertoire de travail doit être la racine du projet.", call. = FALSE)
  cfg <- yaml::read_yaml(chemin)
  src <- cfg$source_donnees
  if (!src %in% names(cfg$donnees))
    stop("source_donnees doit valoir l'une de : ", paste(names(cfg$donnees), collapse = ", "), call. = FALSE)
  cfg$actif <- cfg$donnees[[src]]
  cfg$actif$type <- src
  cfg
}

# Valeurs par défaut des normes si la feuille NORMES est vide.
# Ce sont des PARAMÈTRES DE DÉMONSTRATION, à remplacer par les normes
# officielles (feuille NORMES de la base).
NORMES_DEFAUT <- data.frame(
  code_parametre = c(rep("TAILLE_CLASSE_MAX", 4), rep("RATIO_ELEVES_ENSEIGNANT", 4),
                     rep("PLACES_TABLE_BANC", 4), rep("ELEVES_PAR_LATRINE", 4)),
  cycle = rep(c("PRESCOLAIRE", "PRIMAIRE", "POST_PRIMAIRE", "SECONDAIRE"), 4),
  valeur = c(35, 60, 70, 60,   30, 55, 60, 50,   2, 2, 2, 2,   40, 50, 50, 50),
  stringsAsFactors = FALSE
)
# Coûts unitaires (FCFA), modifiables dans le module Optimisation.
# Deux coûts sont appuyés sur des sources (ordre de grandeur) ; les autres restent
# des valeurs de démonstration À CONFIRMER (aucune source officielle trouvée).
COUTS_DEFAUT <- data.frame(
  ressource = c("salles", "enseignants", "tables_bancs", "latrines", "eau", "electricite", "cantines"),
  libelle   = c("Salles de classe", "Enseignants (coût annuel)", "Tables-bancs",
                "Cabines de latrines", "Points d'eau (forages)", "Électrification (kit solaire)",
                "Cantines scolaires"),
  unite     = c("salle", "enseignant", "table-banc", "cabine", "forage", "kit", "cantine"),
  cout_unitaire = c(12e6, 2.4e6, 45e3, 1.2e6, 8e6, 2.5e6, 1.5e6),
  statut = c("Estimation sourcée", "À confirmer", "À confirmer", "À confirmer", "Estimation sourcée",
             "À confirmer", "À confirmer"),
  source = c("Entre 4,8 M (salle semi-finie équipée, Cluster Éducation Burkina) et environ 20 M (salle en dur avec annexes : EPP EZACA, 3 salles et annexes pour 64,9 M FCFA, 2025)",
             "Valeur indicative de l'équipe, à confirmer avec la DAF", "Valeur indicative de l'équipe, à confirmer avec la DAF", "Valeur indicative de l'équipe, à confirmer avec la DAF",
             "Forage équipé d'une pompe : 5,3 à 10 M FCFA selon les devis publiés (2019-2026)",
             "Valeur indicative de l'équipe, à confirmer avec la DAF", "Valeur indicative de l'équipe, à confirmer avec la DAF"),
  stringsAsFactors = FALSE
)
