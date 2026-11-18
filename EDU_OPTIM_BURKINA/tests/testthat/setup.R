# Chargement des fonctions de l'application pour les tests
racine <- normalizePath(file.path(testthat::test_path(), "..", ".."))
ancien <- setwd(racine)
for (f in sort(list.files("R", pattern = "^[0-9].*\\.R$", full.names = TRUE))) source(f, encoding = "UTF-8")
setwd(ancien)

# Petit panel fictif de test (3 provinces × 2 années, cycle primaire)
panel_test <- function() {
  d <- expand.grid(code_geo = c("P1", "P2", "P3"), annee_debut = 2023:2024, stringsAsFactors = FALSE)
  d$cycle <- factor("PRIMAIRE", levels = names(CYCLES))
  d$code_region <- c("REG01", "REG01", "REG02")[match(d$code_geo, c("P1", "P2", "P3"))]
  d$nom <- d$code_geo
  for (v in VARIABLES_SAISIE) d[[v]] <- NA_real_
  d$eff_t <- c(600, 1200, 300, 660, 1260, 330); d$eff_g <- d$eff_t / 2; d$eff_f <- d$eff_t / 2
  d$nb_salles_t <- c(10, 12, 8, 10, 12, 8); d$nb_salles_bon_etat <- c(10, 10, 8, 10, 10, 8)
  d$nb_ens_t <- c(12, 15, 8, 12, 15, 8); d$nb_ens_qualifies <- d$nb_ens_t
  d$nb_tables_bancs <- c(300, 400, 150, 300, 400, 150); d$nb_latrines <- c(12, 10, 6, 12, 10, 6)
  d$nb_etab_t <- c(3, 5, 2, 3, 5, 2); d$nb_etab_fonctionnels <- d$nb_etab_t; d$nb_etab_fermes <- 0
  d$nb_etab_eau <- c(3, 2, 2, 3, 2, 2); d$nb_etab_electricite <- c(2, 1, 2, 2, 1, 2); d$nb_etab_latrines <- d$nb_etab_t
  d$nb_etab_cantine <- c(1, 4, 1, 1, 4, 1); d$nb_groupes_pedago <- d$nb_salles_t
  d$pop_age_scolaire <- c(700, 1500, 300, 720, 1520, 310); d$superficie_km2 <- c(1000, 2000, 500, 1000, 2000, 500)
  d$nb_eleves_affectes <- 0; d$eff_urbain_t <- d$eff_t * 0.3; d$eff_rural_t <- d$eff_t * 0.7
  d
}
