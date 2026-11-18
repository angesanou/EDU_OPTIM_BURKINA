test_that("TOPSIS : exemple de référence", {
  # 3 alternatives, 2 critères à maximiser, poids égaux : A domine B qui domine C
  M <- matrix(c(9, 9, 5, 5, 1, 1), ncol = 2, byrow = TRUE)
  t <- topsis(M, c(0.5, 0.5), c(1, 1))
  expect_equal(order(-t$score), c(1, 2, 3))
  expect_equal(t$score[1], 1)
  expect_equal(t$score[3], 0)
  # Critère à minimiser : l'ordre s'inverse
  t2 <- topsis(M, c(0.5, 0.5), c(-1, -1))
  expect_equal(order(-t2$score), c(3, 2, 1))
})

test_that("TOPSIS gère les valeurs manquantes", {
  M <- matrix(c(9, NA, 5, 5, 1, 1), ncol = 2, byrow = TRUE)
  t <- topsis(M, c(0.5, 0.5), c(1, 1))
  expect_false(anyNA(t$score))
})
