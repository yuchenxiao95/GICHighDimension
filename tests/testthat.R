# testthat.R - Test suite for GICHighDimension package

# Load necessary packages
library(testthat)
library(GICHighDimension)

# Setup Julia environment for tests
setup_julia_for_tests <- function() {
  # Initialize Julia connection
  tryCatch({
    # Check if Julia is available by attempting setup
    julia_ok <- JuliaCall::julia_setup(installJulia = FALSE, silent = TRUE)
    if (!julia_ok) {
      testthat::skip("Julia not available - skipping tests")
    }

    # Verify Julia is working
    JuliaCall::julia_eval("1+1")

    # Load required Julia packages
    JuliaCall::julia_library("Distributions")
    JuliaCall::julia_library("LinearAlgebra")

    return(TRUE)
  }, error = function(e) {
    testthat::skip(paste("Julia initialization failed:", e$message))
  })
}

# Test Suite ---------------------------------------------------------------

test_that("Univariate Normal Model Selection works correctly", {
  # Skip if Julia not available
  if (!setup_julia_for_tests()) return()

  # Parameters
  N <- 100L  # Reduced from 3000 for faster testing
  P <- 20L   # Reduced from 100 for faster testing
  k <- 3L    # Reduced from 5 for faster testing
  rho <- 0.0

  # Generate test data directly in R (faster than Julia for small cases)
  true_columns <- sort(sample(1:P, k, replace = FALSE))
  cov_matrix <- diag(P)
  X <- matrix(rnorm(N*P), N, P) %*% chol(cov_matrix)
  true_beta <- rep(0, P)
  true_beta[true_columns] <- 2
  Y <- X %*% true_beta + rnorm(N)

  # Run model selection
  result <- GICSelection(
    X = X,
    Y = Y,
    Initial_Column = 1:P,
    Calculate_GIC = "Calculate_SIC",
    Calculate_GIC_short = "Calculate_SIC_short",
    Nsim = 2L  # Reduced from 5 for faster testing
  )

  # Tests
  selected_cols <- result$GIC_coeff[[length(result$GIC_coeff)]]
  false_positives <- setdiff(selected_cols, true_columns)
  false_negatives <- setdiff(true_columns, selected_cols)

  expect_lte(length(false_positives), 5,
             info = "Too many false positives in selection")
  expect_lte(length(false_negatives), 2,  # More lenient for smaller test
             info = "Too many false negatives in selection")
})

test_that("Univariate Poisson Model Selection works correctly", {
  # Skip if Julia not available
  if (!setup_julia_for_tests()) return()

  # Reduced parameters for faster testing
  N <- 100L
  P <- 20L
  k <- 3L

  # Generate test data in R
  true_columns <- sort(sample(1:P, k, replace = FALSE))
  X <- matrix(rnorm(N*P), N, P)
  true_beta <- rep(0, P)
  true_beta[true_columns] <- 0.3  # Smaller coefficients for Poisson
  Y <- rpois(N, exp(X %*% true_beta))

  # Run model selection
  result <- GICSelection(
    X = X,
    Y = Y_to_LP(Y, "Poisson"),
    Initial_Column = 1:P,
    Calculate_GIC = "Calculate_SIC",
    Calculate_GIC_short = "Calculate_SIC_short",
    Nsim = 2L
  )

  # Tests
  selected_cols <- result$GIC_coeff[[length(result$GIC_coeff)]]
  expect_lte(length(setdiff(selected_cols, true_columns)), 5,
             info = "Too many false positives in Poisson model")
  expect_lte(length(setdiff(true_columns, selected_cols)), 2,
             info = "Too many false negatives in Poisson model")
})

test_that("Multivariate Normal Model Selection works correctly", {
  # Skip if Julia not available
  if (!setup_julia_for_tests()) return()

  # Reduced parameters
  N <- 100L
  P <- 20L
  k <- 3L
  m <- 3L

  # Generate test data in R
  X <- matrix(rnorm(N*P), N, P)
  multi_beta <- matrix(0, P, m)
  true_columns <- integer(0)

  for (i in 1:m) {
    cols <- sort(sample(1:P, k, replace = FALSE))
    true_columns <- union(true_columns, cols)
    multi_beta[cols, i] <- seq(1, 0.1, length.out = k)
  }

  # Generate response
  Y <- X %*% multi_beta + matrix(rnorm(N*m), N, m)

  # Run model selection
  result <- GICSelection(
    X = X,
    Y = Y,
    Initial_Column = 1:P,
    Calculate_GIC = "Calculate_SIC",
    Calculate_GIC_short = "Calculate_SIC_short",
    Nsim = 2L
  )

  # Tests
  selected_cols <- result$GIC_coeff[[length(result$GIC_coeff)]]
  expect_lte(length(setdiff(selected_cols, true_columns)), 5,
             info = "Too many false positives in multivariate model")
  expect_lte(length(setdiff(true_columns, selected_cols)), 3,
             info = "Too many false negatives in multivariate model")
})
