test_that("prévisions : dimensions, positivité et tendance", {
  y <- c(100, 110, 121, 133, 146, 161, 177, 195, 214, 236)
  an <- 2015:2024
  for (m in c("lineaire", "log_lineaire", "holt")) {
    f <- prevoir_serie(y, an, 3, m)
    expect_equal(nrow(f), 3)
    expect_true(all(f$prevision > y[10] * 0.95), info = m)
    expect_true(all(f$bas95 <= f$prevision & f$prevision <= f$haut95), info = m)
  }
  f <- prevoir_serie(y, an, 3, "demographique", pop = y * 2)
  expect_equal(round(f$prevision[1] / y[10], 1), 1.1)
})

test_that("backtesting : une ligne par méthode, MAPE positif", {
  y <- c(100, 104, 109, 113, 118, 122, 127, 133, 138, 144)
  b <- backtester(y, 2015:2024, pop = y * 2)
  expect_equal(nrow(b), length(METHODES_PREVISION))
  expect_true(all(b$mape >= 0, na.rm = TRUE))
})
