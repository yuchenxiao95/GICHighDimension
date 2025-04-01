library(testthat)
library(JuliaCall)
library(MASS)


test_that("GICSelection works with example inputs", {
  # Skip on CRAN to avoid Julia dependency issues
  skip_on_cran()

  # Ensure Julia is set up and source the Julia script
  julia_setup()
  julia_source(system.file("julia", "penalty_selection.jl", package = "GICModelSelection"))

  # Set seed for reproducibility
  set.seed(123)

  # Step 1: Generate X from a multivariate normal distribution
  n <- 100   # Reduced number of observations
  p <- 10   # Reduced number of predictors

  # Create a covariance matrix for the multivariate normal distribution
  # Using an identity matrix for independent variables
  cov_matrix <- diag(p)

  # Generate X
  X <- mvrnorm(n = n, mu = rep(0, p), Sigma = cov_matrix)

  # Step 2: Generate 4 random indices and coefficients
  num_indices <- 4  # Reduced number of non-zero coefficients
  indices <- sample(1:p, num_indices, replace = FALSE)  # Random indices
  coefficients <- numeric(p)  # Initialize coefficients

  # Generate coefficients from the specified ranges
  coefficients[indices] <- c(
    runif(2, min = 5, max = 10),        # 2 from 5 to 10
    runif(1, min = -10, max = -5),      # 1 from -10 to -5
    runif(1, min = -1, max = 1)         # 1 from -1 to 1
  )

  # Step 3: Generate Y using the selected indices and coefficients
  Y <- X %*% coefficients + rnorm(n)  # Add random noise


  # Call the R wrapper
  result <- GICSelection(
    X, Y, 1:p,
    Calculate_GIC = "Calculate_AIC",
    Calculate_GIC_short = "Calculate_AIC_short",
    debug = FALSE, Nsim = 1
  )

  # Check the output
  expect_true(is.list(result))                 # Result should be a list
  expect_true("GIC_list" %in% names(result))   # List should contain GIC_list
  expect_true("GIC_coeff" %in% names(result))  # List should contain GIC_coeff
  expect_true(length(result$GIC_list) > 0)     # GIC_list should not be empty
})
