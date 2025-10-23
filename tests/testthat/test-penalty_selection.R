library(testthat)
library(JuliaCall)

test_that("Information criteria functions return valid output", {
  skip_on_cran()

  # Initialize Julia once
  expect_silent(julia_setup(installJulia = FALSE))

  # Simulate inputs
  Y <- rnorm(20)
  X <- matrix(rnorm(100), nrow = 20)

  # Exported R function names
  criteria <- c(
    "Calculate_AIC", "Calculate_AICc", "Calculate_AttIC", "Calculate_SIC",
    "Calculate_BIC", "Calculate_CAIC", "Calculate_CAICF",
    "Calculate_GIC2", "Calculate_GIC3", "Calculate_GIC4",
    "Calculate_GIC5", "Calculate_GIC6" "Calculate_EBIC"
  )

  for (fn in criteria) {
    criterion_fun <- get(fn, mode = "function")
    result <- criterion_fun(Y, X)

    expect_type(result, "list")
    expect_named(result, c("CriterionValue", "InverseMatrix"))
    expect_true(is.numeric(result$CriterionValue), info = paste(fn, "did not return a numeric CriterionValue"))
    expect_true(is.matrix(result$InverseMatrix), info = paste(fn, "did not return a valid InverseMatrix"))
  }
})
