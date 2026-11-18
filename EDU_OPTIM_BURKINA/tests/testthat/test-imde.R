test_that("l'IMDE est borné entre 0 et 100 et ordonne correctement", {
  p <- calculer_indicateurs(panel_test(), NORMES_DEFAUT)
  im <- calculer_imde(p)
  expect_true(all(im$imde >= 0 & im$imde <= 100, na.rm = TRUE))
  # P2 (plus d'élèves par salle et par enseignant, moins d'eau) doit être plus déficitaire que P1
  a <- im[im$annee_debut == 2023, ]
  expect_gt(a$imde[a$code_geo == "P2"], a$imde[a$code_geo == "P1"])
  expect_equal(sum(attr(im, "poids")), 1)
})

test_that("normalisation : sens des indicateurs", {
  b <- data.frame(min = 0, max = 100, moyenne = 50, ecart_type = 10)
  expect_equal(normaliser(c(0, 50, 100), 1, b), c(0, 0.5, 1))
  expect_equal(normaliser(c(0, 50, 100), -1, b), c(1, 0.5, 0))
  expect_equal(normaliser(c(-20, 140), 1, b), c(0, 1))   # écrêtage aux bornes
})

test_that("classes à seuils fixes", {
  expect_equal(as.character(classer_imde(c(10, 30, 45, 55, 80))), CLASSES_IMDE)
})

test_that("variantes de robustesse et sensibilité", {
  p <- calculer_indicateurs(panel_test(), NORMES_DEFAUT)
  b <- bornes_imde(p)
  im <- calculer_imde(p, b)
  r <- robustesse_imde(im, b)
  expect_equal(dim(r$spearman), c(6, 6))
  s <- sensibilite_imde(im[im$annee_debut == 2023, ], n = 50)
  expect_true(all(s$rang_p05 <= s$rang_p95))
})
