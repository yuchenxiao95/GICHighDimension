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
k <- 5     # Number of true signals
SNR <- c(0.09, 0.14, 0.25, 0.42, 0.71, 1.22, 2.07, 3.52, 6.00)
init_cols <- sort(sample(1:P, P / 3, replace = FALSE))
rho <- 0

# Set mean vector and covariance matrix
mu <- rep(0, P)
cov <- matrix(rho, P, P)
diag(cov) <- 1  # Identity matrix with off-diagonal correlation

# Generate the design matrix X
X <- mvrnorm(N, mu, cov)

# Generate sparse true coefficients
result <- Generate_Beta(P, k, type = 3)
true_beta <- result$beta
indices <- result$indices

# Generate outcome Y
Y <- LP_to_Y(X, true_beta, family = "Normal", std = 1.0)

# Initialize all columns
init_cols <- seq(1, P)

# Run selection and time it
time <- system.time(
  result1 <- GICSelection(
    X, Y, init_cols,
    "Calculate_SIC", "Calculate_SIC_short",
    Nsim = 1
  )
)["elapsed"]

time
