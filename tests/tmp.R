# Load necessary library
library(MASS)  # For mvrnorm
library(GICHighDimension)
library(tidyverse)
library(devtools)
# library(glmnet)
# library(ncpen)
# library(foreach)
library(patchwork)


# remove the installed package and reinstall
remove.packages("GICHighDimension")
# restart the R
.rs.restartR()
# Document the changes
devtools::document()

# Load all functions for testing
devtools::load_all()

# Run tests
devtools::test()

# Check package integrity
devtools::check()  # Fix any ERRORs/WARNINGs

# installing a custom-built R package
devtools::install(build_vignettes = FALSE, force = TRUE)

# verify the julia script paths
JuliaCall::julia_setup("/Applications/Julia-1.9.app/Contents/Resources/julia/bin")
#JuliaCall::julia_setup()

# Export the functions on the namespace
roxygen2::roxygenise()

# build the package and check for errors locally
devtools::build()
devtools::check()



# Set seed for reproducibility
set.seed(101)
# Step 1: Generate X from a multivariate normal distribution
N <- 3000  # Number of observations
P <- 100   # Number of predictors
k <- 10
SNR = c(0.09, 0.14, 0.25, 0.42, 0.71, 1.22, 2.07, 3.52, 6.00)
init_cols = sort(sample(1:P, P/3, replace = FALSE))
rho = 0
# Set mean vector and covariance matrix
mu <- rep(0, P)  # Mean vector
cov <- matrix(rho, P, P)
diag(cov) <- 1  # Covariance matrix with specified correlation

# Generate the design matrix X
X <- mvrnorm(N, mu, cov)

result <- Beta_Generation(P, k, type = 3)
true_beta <- result$beta
indices <- result$indices

Y = LP_to_Y(X, true_beta, family = "Normal", std = 1.0)

init_cols = init_cols <- seq(1,P)

time <- system.time(result1 <- GICSelection(X, Y, init_cols, "Calculate_SIC", "Calculate_SIC_short", Nsim = 1))["elapsed"]
time

calculate_aic()







