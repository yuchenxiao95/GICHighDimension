test_that("information criteria return value and inverse; short forms agree", {
  skip_if_no_julia()
  set.seed(3)
  Y <- rnorm(40)
  X <- matrix(rnorm(200), nrow = 40)
  Inv <- solve(crossprod(X))

  criteria <- c("AIC", "AICc", "AttIC", "SIC", "BIC", "CAIC", "CAICF",
                "GIC2", "GIC3", "GIC4", "GIC5", "GIC6", "EBIC")
  for (ic in criteria) {
    full <- get(paste0("Calculate_", ic), mode = "function")(Y, X)
    short <- get(paste0("Calculate_", ic, "_Short"), mode = "function")(Y, X, Inv)
    expect_named(full, c("CriterionValue", "InverseMatrix"))
    expect_true(is.numeric(full$CriterionValue), info = ic)
    expect_equal(full$InverseMatrix, Inv, tolerance = 1e-8, info = ic)
    expect_equal(as.numeric(short), as.numeric(full$CriterionValue), tolerance = 1e-8, info = ic)
  }
})

test_that("BIC matches its closed form", {
  skip_if_no_julia()
  set.seed(4)
  n <- 50; X <- matrix(rnorm(n * 3), n, 3); Y <- rnorm(n)
  fit <- lm.fit(X, Y)
  yhy <- sum(fit$fitted.values * Y)
  s2 <- sum(fit$residuals^2) / (n - 3)
  expect_equal(Calculate_BIC(Y, X)$CriterionValue, yhy / n - 3 * s2 / n * log(n), tolerance = 1e-8)
})
