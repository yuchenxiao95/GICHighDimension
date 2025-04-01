# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

# testthat.R
library(testthat)
library(GICHighDimension)

# Initialize Julia connection
JuliaCall::julia_setup()
JuliaCall::julia_library("Distributions")

test_that("Univariate Normal Model Selection works", {
  # Set parameters
  N <- 3000L
  P <- 500L
  k <- 5L
  rho <- 0.0

  # Generate true columns
  true_columns <- sort(sample(1:P, k, replace = FALSE))

  # Create design matrix
  mu <- rep(0, P)
  cov_matrix <- matrix(rho, P, P)
  diag(cov_matrix) <- 1.0

  # Generate random matrix in Julia
  JuliaCall::julia_assign("mu", mu)
  JuliaCall::julia_assign("cov_matrix", cov_matrix)
  JuliaCall::julia_assign("N", N)
  X <- JuliaCall::julia_eval("rand(MvNormal(mu, cov_matrix), N)'")

  # Create true beta
  true_beta <- rep(0, P)
  true_beta[true_columns] <- 2

  # Generate response
  Y <- LP_to_Y(X, true_beta, family = "Normal")

  # Run model selection
  init_cols <- 1:P
  result <- GICSelection(
    X = X,
    Y = Y,
    init_cols,
    "Calculate_SIC",
    "Calculate_SIC_short",
    Nsim = 5L
  )

  # Tests for the result structure
  setdiff(result$GIC_coeff[[length(result$GIC_coeff)]], true_columns)
  setdiff(true_columns, result$GIC_coeff[[length(result$GIC_coeff)]])
})

test_that("Univariate Poisson Model Selection works", {
  N <- 3000L
  P <- 500L
  k <- 5L
  rho <- 0.0
  true_columns <- sort(sample(1:P, k, replace = FALSE))

  mu <- rep(0, P)
  cov_matrix <- matrix(rho, P, P)
  diag(cov_matrix) <- 1.0

  JuliaCall::julia_assign("mu", mu)
  JuliaCall::julia_assign("cov_matrix", cov_matrix)
  JuliaCall::julia_assign("N", N)
  X <- JuliaCall::julia_eval("rand(MvNormal(mu, cov_matrix), N)'")

  true_beta <- rep(0, P)
  true_beta[true_columns] <- 2

  Y <- LP_to_Y(X, true_beta, family = "Poisson")

  init_cols <- 1:P
  result <- GICSelection(
    X = X,
    Y = Y_to_lp(Y, "Poisson"),
    init_cols,
    "Calculate_SIC",
    "Calculate_SIC_short",
    Nsim = 5L
  )

  setdiff(result$GIC_coeff[[length(result$GIC_coeff)]], true_columns)
  setdiff(true_columns, result$GIC_coeff[[length(result$GIC_coeff)]])
})

test_that("Multivariate Normal Model Selection works", {
  N <- 3000L
  P <- 500L
  k <- 5L
  rho <- 0.0

  # Create mean vector and covariance matrix
  mu <- rep(0, P)
  cov_matrix <- matrix(rho, P, P)
  diag(cov_matrix) <- 1.0

  # Assign to Julia
  JuliaCall::julia_assign("mu", mu)
  JuliaCall::julia_assign("cov_matrix", cov_matrix)
  JuliaCall::julia_assign("N", N)
  X <- JuliaCall::julia_eval("rand(MvNormal(mu, cov_matrix), N)'")

  # Create true beta coefficients
  m <- 5L
  multi_beta <- matrix(0, P, m)
  multi_beta_true_columns <- vector("list", m)

  for (i in 1:m) {
    cols <- sort(sample(1:P, k, replace = FALSE))
    multi_beta_true_columns[[i]] <- cols
    multi_beta[cols, i] <- seq(10, 0.1, length.out = k)
  }

  true_signal_columns <- unique(unlist(multi_beta_true_columns))

  # Create response covariance matrix (different from design matrix)
  response_cov <- matrix(0.5, m, m)  # Example: 0.5 correlation between responses
  diag(response_cov) <- 1.0          # Unit variance

  # Generate response - NOW WITH COV_MATRIX SPECIFIED
  Y <- LP_to_Y(
    X = X,
    true_beta = multi_beta,
    family = "MultivariateNormal",
    cov_matrix = response_cov  # This was missing!
  )

  init_cols <- 1:P
  result <- GICSelection(
    X = X,
    Y = Y,
    init_cols,
    "Calculate_SIC",
    "Calculate_SIC_short",
    Nsim = 5L
  )

  setdiff(result$GIC_coeff[[length(result$GIC_coeff)]],   true_signal_columns )
  setdiff(true_signal_columns , result$GIC_coeff[[length(result$GIC_coeff)]])
})
