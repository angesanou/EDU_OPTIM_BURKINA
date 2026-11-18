test_that("la base synthétique respecte le contrat EXX_01 (si générée)", {
  racine <- normalizePath(file.path(testthat::test_path(), "..", ".."))
  f <- file.path(racine, "data/synthetique/EXX_01_base_synthetique.xlsx")
  skip_if_not(file.exists(f), "Base synthétique non générée")
  b <- lire_base_exx(f)
  expect_equal(length(unique(b$panel$code_geo)), 45)
  expect_setequal(levels(droplevels(b$panel$cycle)), names(CYCLES))
  expect_true(all(VARIABLES_SAISIE %in% names(b$panel)))
  q <- controler_panel(b$panel, b$geo)
  expect_equal(q$anomalies[q$controle == "Doublons de clé (cycle × province × année)"], 0)
  expect_equal(q$anomalies[q$controle == "Effectifs : garçons + filles = total"], 0)
})

test_that("les normes se complètent par défaut", {
  n <- preparer_normes(NULL)
  expect_equal(norme(n, "TAILLE_CLASSE_MAX", "PRIMAIRE"), 60)
  n2 <- preparer_normes(data.frame(code_parametre = "TAILLE_CLASSE_MAX", cycle = "PRIMAIRE", valeur = "50", source = "test"))
  expect_equal(norme(n2, "TAILLE_CLASSE_MAX", "PRIMAIRE"), 50)
})
