unites_test <- function() data.frame(
  code_geo = c("P1", "P2", "P3"), cycle = "PRIMAIRE", poids = c(1.5, 1, 0.6),
  classe_imde = c("Critique", "Modéré", "Favorable"),
  deficit_salles = c(10, 5, 2), deficit_enseignants = c(4, 0, 1), stringsAsFactors = FALSE)
couts_test <- data.frame(ressource = c("salles", "enseignants"), libelle = c("Salles", "Enseignants"),
                         cout_unitaire = c(10, 3))

test_that("le budget est respecté et l'allocation ne dépasse pas les déficits", {
  o <- optimiser_allocation(unites_test(), couts_test, budget = 80, ressources = c("salles", "enseignants"), lambda = 0)
  expect_lte(o$cout_total, 80)
  expect_true(all(o$allocation$alloue <= o$allocation$deficit))
  expect_true(all(o$allocation$alloue == round(o$allocation$alloue)))
})

test_that("budget suffisant : tous les déficits sont couverts", {
  o <- optimiser_allocation(unites_test(), couts_test, budget = 1e6, ressources = c("salles", "enseignants"))
  expect_equal(sum(o$allocation$alloue), sum(o$allocation$deficit))
})

test_that("PLNE exacte et relaxation donnent des solutions réalisables", {
  o1 <- optimiser_allocation(unites_test(), couts_test, 57, c("salles", "enseignants"), lambda = 0, mu = 0, mode = "exacte")
  o2 <- optimiser_allocation(unites_test(), couts_test, 57, c("salles", "enseignants"), lambda = 0, mu = 0, mode = "relaxation")
  expect_lte(o1$cout_total, 57); expect_lte(o2$cout_total, 57)
  expect_gte(o1$valeur_objectif + 1e-9, o2$valeur_objectif - 1e-6)
})

test_that("la disponibilité limite l'allocation", {
  o <- optimiser_allocation(unites_test(), couts_test, 1e6, c("salles", "enseignants"), dispo = c(salles = 4))
  expect_lte(sum(o$allocation$alloue[o$allocation$ressource == "salles"]), 4)
})

test_that("l'équité relève la couverture minimale", {
  o0 <- optimiser_allocation(unites_test(), couts_test, 60, c("salles", "enseignants"), lambda = 0)
  o5 <- optimiser_allocation(unites_test(), couts_test, 60, c("salles", "enseignants"), lambda = 5)
  expect_gte(o5$couverture_min + 1e-9, o0$couverture_min - 0.05)
})

test_that("l'équilibre entre ressources évite de délaisser une ressource coûteuse", {
  u <- unites_test(); u$deficit_enseignants <- c(400, 300, 200)
  c2 <- data.frame(ressource = c("salles", "enseignants"), libelle = c("S", "E"), cout_unitaire = c(100, 1))
  o0 <- optimiser_allocation(u, c2, 500, c("salles", "enseignants"), lambda = 0, mu = 0)
  o1 <- optimiser_allocation(u, c2, 500, c("salles", "enseignants"), lambda = 0, mu = 5)
  s0 <- sum(o0$allocation$alloue[o0$allocation$ressource == "salles"])
  s1 <- sum(o1$allocation$alloue[o1$allocation$ressource == "salles"])
  expect_gte(s1, s0)
  expect_gt(s1, 0)
})
