test_that("ratios et déficits respectent les formules", {
  p <- calculer_indicateurs(panel_test(), NORMES_DEFAUT)
  r <- p[p$code_geo == "P2" & p$annee_debut == 2023, ]
  expect_equal(r$ratio_eleves_salle_utilisable, 1200 / 10)
  expect_equal(r$ratio_eleves_ens, 1200 / 15)
  # Norme primaire : 60 élèves par classe -> besoin 20 salles, 10 en bon état
  expect_equal(r$besoin_salles, 20)
  expect_equal(r$deficit_salles, 10)
  # Norme 55 élèves par enseignant -> ceiling(1200/55) = 22 ; 15 en poste
  expect_equal(r$deficit_enseignants, 22 - 15)
  expect_equal(r$deficit_eau, 5 - 2)
  expect_equal(r$taux_eau, 2 / 5)
  # Aucun déficit négatif
  expect_true(all(p$deficit_salles >= 0, na.rm = TRUE))
})

test_that("agrégation : déficits additionnés sans compensation", {
  p <- calculer_indicateurs(panel_test(), NORMES_DEFAUT)
  n <- agreger(p, "national", NORMES_DEFAUT)
  n23 <- n[n$annee_debut == 2023, ]
  expect_equal(n23$eff_t, 600 + 1200 + 300)
  expect_equal(n23$deficit_salles, sum(p$deficit_salles[p$annee_debut == 2023]))
  # Le ratio agrégé est recalculé sur les sommes (pas une moyenne de ratios)
  expect_equal(n23$ratio_eleves_salle_utilisable, 2100 / 28)
  r <- agreger(p, "region", NORMES_DEFAUT)
  expect_equal(sort(unique(r$code_geo)), c("REG01", "REG02"))
})

test_that("agrégat tous cycles : les élèves sans stock connu ne gonflent pas les ratios", {
  a <- panel_test(); b <- a
  b$cycle <- factor("POST_PRIMAIRE", levels = names(CYCLES))
  for (v in c(STOCKS_RATIO, "nb_etab_t", "nb_etab_fonctionnels", "nb_etab_eau", "nb_etab_electricite",
              "nb_etab_latrines", "nb_etab_cantine")) b[[v]] <- NA_real_
  p <- calculer_indicateurs(rbind(a, b), NORMES_DEFAUT)
  expect_true(all(is.na(p$deficit_salles[p$cycle == "POST_PRIMAIRE"])))
  expect_true(all(is.na(p$besoin_salles[p$cycle == "POST_PRIMAIRE"])))
  n <- agreger(p, "national", NORMES_DEFAUT, tous_cycles = TRUE)
  t23 <- n[n$cycle == "TOUS" & n$annee_debut == 2023, ]
  p23 <- n[n$cycle == "PRIMAIRE" & n$annee_debut == 2023, ]
  expect_equal(t23$eff_t, 2 * 2100)
  expect_equal(t23$ratio_eleves_salle_utilisable, 2100 / 28)
  expect_equal(t23$taux_deficit_salles, p23$taux_deficit_salles)
})

test_that("agrégat tous cycles : équipements rapportés aux seuls établissements renseignés", {
  a <- panel_test(); b <- a
  b$cycle <- factor("PRESCOLAIRE", levels = names(CYCLES))
  for (v in EQUIPEMENTS_ETAB) b[[v]] <- NA_real_
  p <- calculer_indicateurs(rbind(a, b), NORMES_DEFAUT)
  n <- agreger(p, "national", NORMES_DEFAUT, tous_cycles = TRUE)
  t23 <- n[n$cycle == "TOUS" & n$annee_debut == 2023, ]
  p23 <- n[n$cycle == "PRIMAIRE" & n$annee_debut == 2023, ]
  expect_equal(t23$taux_eau, p23$taux_eau)
  expect_equal(t23$nb_etab_t, 2 * p23$nb_etab_t)
})

test_that("taux de fonctionnalité non renseigné sans données de fermeture", {
  d <- panel_test(); d$nb_etab_fonctionnels <- NA_real_
  expect_true(all(is.na(calculer_indicateurs(d, NORMES_DEFAUT)$taux_fonctionnalite)))
})

test_that("contrôles de qualité détectent les incohérences", {
  p <- panel_test(); p$id_ligne <- paste("PRI", p$code_geo, p$annee_debut, sep = "_")
  geo <- data.frame(code_geo = c("P1", "P2", "P3"))
  q <- controler_panel(p, geo)
  expect_true(all(q$statut[q$controle == "Doublons de clé (cycle × province × année)"] == "Conforme"))
  p2 <- rbind(p, p[1, ]); p2$eff_g[2] <- 999
  q2 <- controler_panel(p2, geo)
  expect_equal(q2$anomalies[q2$controle == "Doublons de clé (cycle × province × année)"], 1)
  expect_gte(q2$anomalies[q2$controle == "Effectifs : garçons + filles = total"], 1)
})
