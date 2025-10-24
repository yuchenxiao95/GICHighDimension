# Load necessary libraries
library(MASS)
library(GICHighDimension)
library(devtools)



# Remove the installed package and restart R
remove.packages("GICHighDimension")
.rs.restartR()

# Document the changes
devtools::document()
devtools::check_man()

# Load all functions for testing
devtools::load_all()

# Run tests
devtools::test()

# Export the functions on the namespace
roxygen2::roxygenise()

# Build vignettes
devtools::build_vignettes()


# Check package integrity
devtools::check()  # Fix any ERRORs/WARNINGs

# Install the package locally from source
devtools::install(build_vignettes = FALSE, force = TRUE)

# Verify the Julia script paths
JuliaCall::julia_setup("/Applications/Julia-1.9.app/Contents/Resources/julia/bin")
# JuliaCall::julia_setup()  # Alternative setup call if path is set correctly

# Build the package
devtools::build()


# -------------------------------------------------------------
# Run GICSelection on synthetic data (Univariate Normal)
# -------------------------------------------------------------

# Set seed for reproducibility
set.seed(101)

# Step 1: Generate X from a multivariate normal distribution
N <- 3000  # Number of observations
P <- 100   # Number of predictors
k <- 10  # Number of true signals

SNR <- c(0.09, 0.14, 0.25, 0.42, 0.71, 1.22, 2.07, 3.52, 6.00)
init_cols <- sort(sample(1:P, P / 3, replace = FALSE))
rho <- 0.0

# Set mean vector and covariance matrix
mu <- rep(0, P)
cov <- matrix(rho, P, P)
diag(cov) <- 1  # Identity matrix with off-diagonal correlation

# Generate the design matrix X
X <- mvrnorm(N, mu, cov)

# # Initialize coefficient matrix
# multi_beta <- matrix(0, nrow = P, ncol = m)
# multi_beta_true_columns <- vector("list", m)
#
# # Generate sparse beta matrix
# for (i in seq_len(m)) {
#   cols <- sort(sample(seq_len(P), k, replace = FALSE))
#   multi_beta_true_columns[[i]] <- cols
#   multi_beta[cols, i] <- seq(10, 0.1, length.out = k)
# }
#
# # Construct multivariate covariance matrix (cov_p)
# cov_p <- matrix(rho, nrow = m, ncol = m)
# diag(cov_p) <- 1.0
#
# # Simulate multivariate response
# Y <- LP_to_Y(X, multi_beta, family = "MultivariateNormal", cov_matrix = cov_p)

##
# # Generate sparse true coefficients
result <- Generate_Beta(P, k, type = 2)
true_beta <- result$beta
indices <- result$indices

# Generate outcome Y
#Y <- LP_to_Y(X, true_beta, family = "Normal", std = 1.0)

Y <- LP_to_Y(X, true_beta, family = "Poisson")

# Initialize all columns
init_cols <- seq(1, P)
#init_cols <- sort(sample(1:P, floor(P/10), replace = FALSE))

# Run selection and time it
time <- system.time(
  result <- GICSelection(
    X, Y_to_LP(Y, family = "Poisson"), init_cols,
    "Calculate_SIC", "Calculate_SIC_short",
    Nsim = 4
  )
)["elapsed"]
