# test-gicselection.R — Minimal Example for GICSelection

library(testthat)
library(JuliaCall)
library(MASS)

test_that("GICSelection works with example inputs", {
  skip_on_cran()  # Avoid CRAN timeouts and Julia dependency issues

  # Ensure Julia is available and initialized
  expect_silent(julia_setup(installJulia = FALSE))

  # Load required Julia backend
  julia_script <- system.file("julia", "penalty_selection.jl", package = "GICHighDimension")
  expect_true(file.exists(julia_script))
  JuliaCall::julia_source(julia_script)

  # Reproducibility
  set.seed(123)

  # Step 1: Simulate design matrix
  n <- 100
  p <- 10
  X <- mvrnorm(n, mu = rep(0, p), Sigma = diag(p))

  # Step 2: Set sparse coefficients
  beta <- numeric(p)
  idx <- sample(1:p, 4, replace = FALSE)
  beta[idx] <- c(runif(2, 5, 10), runif(1, -10, -5), runif(1, -1, 1))

  # Step 3: Generate response
  Y <- LP_to_Y(X, beta, family = "Normal", std = 1.0)

  # Run model selection
  result <- GICSelection(
    X = X,
    Y = Y,
    Initial_Column = 1:p,
    Calculate_GIC = "Calculate_AIC",
    Calculate_GIC_short = "Calculate_AIC_short",
    Nsim = 1
  )

  # Basic checks
  expect_type(result, "list")
  expect_true("GIC_values" %in% names(result))
  expect_true("selected_coeffs" %in% names(result))
})
