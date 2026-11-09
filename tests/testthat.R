# testthat.R - Test suite for IChop package

library(testthat)
library(MASS)
library(IChop)

# Safe check for Julia availability
setup_julia_for_tests <- function() {
  tryCatch({
    if (!JuliaCall::julia_exists("Base.sqrt")) {
      testthat::skip("Julia not initialized. Skipping test.")
    }
    TRUE
  }, error = function(e) {
    testthat::skip(paste("Julia not available or failed:", e$message))
  })
}

generate_X <- function(p, rho){
  g <- rnorm()
}

# Test: Univariate Normal Model Selection
test_that("Univariate Normal Model Selection works correctly", {
  skip_on_cran()
  if (!setup_julia_for_tests()) return()

  set.seed(123)
  N <- 600L
  P <- 8000L
  k <- 30L
  true_columns <- sort(sample(1:P, k, replace = FALSE))
  rho = 0.4
  # Set mean vector and covariance matrix
  mu <- rep(0, P)
  cov <- matrix(rho, P, P)
  diag(cov) <- 1  # Identity matrix with off-diagonal correlation

  # Generate the design matrix X
  X <- mvrnorm(N, mu, cov)

  #X <- matrix(rnorm(N * P), N, P)
  true_beta <- numeric(P)
  true_beta[true_columns] <- 3.5
  Y <- LP_to_Y(X, true_beta, family = "Normal", std = 1.0)

  Init_col <- sample(1:P, 50, replace = FALSE)

  result <- GICSelection(
    X = X,
    Y = Y,
    Initial_Column = Init_col,
    Calculate_GIC = "Calculate_EBIC",
    Calculate_GIC_short = "Calculate_EBIC_short",
    gamma = 1.0,
    Nsim = 20L
  )

  selected_cols <- result$selected_coeffs[[length(result$selected_coeffs)]]
  false_positives <- setdiff(selected_cols, true_columns)
  false_negatives <- setdiff(true_columns, selected_cols)

  expect_true(length(false_positives) <= 5, info = "Too many false positives")
  expect_true(length(false_negatives) <= 5, info = "Too many false negatives")
})

# Test: Univariate Poisson Model Selection
test_that("Univariate Poisson Model Selection works correctly", {
  skip_on_cran()
  if (!setup_julia_for_tests()) return()

  set.seed(123)
  N <- 1000L
  P <- 50L
  k <- 3L
  true_columns <- sort(sample(1:P, k, replace = FALSE))

  X <- matrix(rnorm(N * P), N, P)
  true_beta <- numeric(P)
  true_beta[true_columns] <- 0.3
  Y <- LP_to_Y(X, true_beta, family = "Poisson")

  result <- GICSelection(
    X = X,
    Y = Y_to_LP(Y, "Poisson"),
    Initial_Column = 1:P,
    Calculate_GIC = "Calculate_SIC",
    Calculate_GIC_short = "Calculate_SIC_short",
    Nsim = 20L
  )

  selected_cols <- result$selected_coeffs[[length(result$selected_coeffs)]]
  false_positives <- setdiff(selected_cols, true_columns)
  false_negatives <- setdiff(true_columns, selected_cols)

  expect_true(length(false_positives) <= 5, info = "Too many false positives")
  expect_true(length(false_negatives) <= 5, info = "Too many false negatives")
})

# Test: Multivariate Normal Model Selection
test_that("Multivariate Normal Model Selection works correctly", {
  skip_on_cran()
  if (!setup_julia_for_tests()) return()

  set.seed(123)
  N <- 600L
  P <- 8000L
  k <- 4L
  m <- 3L

  X <- matrix(rnorm(N * P), N, P)
  multi_beta <- matrix(0, P, m)
  true_columns <- integer(0)

  for (i in 1:m) {
    cols <- sort(sample(1:P, k, replace = FALSE))
    true_columns <- union(true_columns, cols)
    multi_beta[cols, i] <- seq(1, 0.1, length.out = k)
  }

  rho <- 0.2
  cov_p <- matrix(rho, m, m)
  diag(cov_p) <- 1.0

  Y <- LP_to_Y(X, multi_beta, family = "MultivariateNormal", cov_matrix = cov_p)

  result <- GICSelection(
    X = X,
    Y = Y,
    Initial_Column = 1:P,
    Calculate_GIC = "Calculate_SIC",
    Calculate_GIC_short = "Calculate_SIC_short",
    Nsim = 2L
  )

  selected_cols <- result$selected_coeffs[[length(result$selected_coeffs)]]
  false_positives <- setdiff(selected_cols, true_columns)
  false_negatives <- setdiff(true_columns, selected_cols)

  expect_true(length(false_positives) <= 5, info = "Too many false positives")
  expect_true(length(false_negatives) <= 5, info = "Too many false negatives")
})
