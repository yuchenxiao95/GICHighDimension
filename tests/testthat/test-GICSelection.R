library(testthat)
library(JuliaCall)
library(MASS)

test_that("GICSelection works with example inputs", {
  # Skip on CRAN to avoid Julia dependency issues
  skip_on_cran()

  # Ensure Julia is set up and source the Julia script
  julia_setup()
  JuliaCall::julia_source(
    system.file("julia", "penalty_selection.jl", package = "GICHighDimension")
  )

  # Set seed for reproducibility
  set.seed(123)

  # Step 1: Generate X from a multivariate normal distribution
  n <- 100   # Reduced number of observations
  p <- 10    # Reduced number of predictors

  # Create a covariance matrix for the multivariate normal distribution
  # Using an identity matrix for independent variables
  cov_matrix <- diag(p)

  # Generate X
  X <- mvrnorm(n = n, mu = rep(0, p), Sigma = cov_matrix)

  # Step 2: Generate 4 random indices and coefficients
  num_indices <- 4  # Reduced number of non-zero coefficients
  indices <- sample(1:p, num_indices, replace = FALSE)  # Random indices
  true_beta = numeric(p)  # Initialize coefficients

  # Generate coefficients from the specified ranges
  true_beta[indices] <- c(
    runif(2, min = 5, max = 10),        # 2 from 5 to 10
    runif(1, min = -10, max = -5),      # 1 from -10 to -5
    runif(1, min = -1, max = 1)         # 1 from -1 to 1
  )

  # Step 3: Generate Y using the selected indices and coefficients

  Y = LP_to_Y(X, true_beta, family = "Normal", std = 1.0)

  # Call the R wrapper
  result <- GICSelection(
    X, Y, 1:p,
    "Calculate_AIC",
    "Calculate_AIC_short",
    Nsim = 1
  )

  # Check the output
  expect_true(is.list(result))                 # Result should be a list
})
