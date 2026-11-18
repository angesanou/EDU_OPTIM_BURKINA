test_that("division protégée et sommes", {
  expect_equal(diviser(10, 2), 5)
  expect_true(is.na(diviser(1, 0)))
  expect_true(is.na(diviser(NA, 3)))
  expect_true(is.na(somme(c(NA, NA))))
  expect_equal(somme(c(1, NA, 2)), 3)
  expect_equal(moy_ponderee(c(10, 20), c(1, 3)), 17.5)
})

test_that("indice de Gini", {
  expect_equal(gini(c(5, 5, 5, 5)), 0)
  expect_gt(gini(c(0, 0, 0, 100)), 0.7)
  expect_true(is.na(gini(c(1))))
})

test_that("formats français", {
  expect_equal(fmt_nombre(1234.5, 1), "1 234,5")
  expect_equal(fmt_pct(0.256), "25,6 %")
  expect_equal(fmt_compact(2.8e6), "2,8 M")
  expect_equal(annee_scolaire(2024), "2024-2025")
})
