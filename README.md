# GICHighDimension

[![R-CMD-check](https://github.com/yuchenxiao95/GICHighDimension/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/yuchenxiao95/GICHighDimension/actions)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

**GICHighDimension**  is an R package designed for efficient variable selection in high-dimensional settings using the Generalized Information Criterion (GIC). By leveraging a Hopfield network optimization framework and integrating Julia's computational speed, this package enables scalable model selection for generalized linear models (GLMs) such as Normal, Poisson, and Multivariate Normal.

---

## 📦 Installation

### ✅ Requirements
- [R](https://cran.r-project.org/) (≥ 4.0.0)
- [Julia](https://julialang.org/download/) (≥ 1.6)
- R package: `JuliaCall`

### 🔧 Install from GitHub

```r
# Install from GitHub
if (!require("remotes")) install.packages("remotes")
remotes::install_github("yuchenxiao95/GICHighDimension")
```

## Quick Example
```r
library(GICHighDimension)

# Generate synthetic data
X <- matrix(rnorm(1000 * 500), 1000, 500)  # 1000 samples, 500 features
true_beta <- c(rep(2, 5), rep(0, 495))     # 5 true signals
Y <- LP_to_Y(X, true_beta, family = "Normal")
init_cols <- 1:100

# Run model selection
result <- GICSelection(
  X = X,
  Y = Y,
  Initial_Column = init_cols,
  Calculate_GIC = "Calculate_BIC",
  Calculate_GIC_short = "Calculate_BIC_short",
  Nsim = 10L
)

# Extract results
selected_vars <- which(rowSums(abs(result$GIC_coeff)) > 1e-6)
```

# Univariate Normal Model Selection
```r
library(GICHighDimension)
setup_julia()

N <- 100L; P <- 20L; k <- 3L
true_columns <- sort(sample(1:P, k))
X <- matrix(rnorm(N * P), N, P)
true_beta <- rep(0, P); true_beta[true_columns] <- 2
Y <- X %*% true_beta + rnorm(N)

result <- GICSelection(
  X = X,
  Y = Y,
  Initial_Column = 1:P,
  Calculate_GIC = "Calculate_SIC",
  Calculate_GIC_short = "Calculate_SIC_short",
  Nsim = 2L
)

print(result$GIC_coeff)

```

# Univariate Poisson Model Selection
```r
N <- 100L; P <- 20L; k <- 3L
true_columns <- sort(sample(1:P, k))
X <- matrix(rnorm(N * P), N, P)
true_beta <- rep(0, P); true_beta[true_columns] <- 0.3
Y <- rpois(N, exp(X %*% true_beta))

result <- GICSelection(
  X = X,
  Y = Y_to_LP(Y, "Poisson"),
  Initial_Column = 1:P,
  Calculate_GIC = "Calculate_SIC",
  Calculate_GIC_short = "Calculate_SIC_short",
  Nsim = 2L
)

print(result$GIC_coeff)
```
# Multivariate Normal Model Selection
```r
N <- 100L; P <- 20L; k <- 3L; m <- 3L
X <- matrix(rnorm(N * P), N, P)
multi_beta <- matrix(0, P, m)
true_columns <- integer(0)

for (i in 1:m) {
  cols <- sort(sample(1:P, k))
  true_columns <- union(true_columns, cols)
  multi_beta[cols, i] <- seq(1, 0.1, length.out = k)
}
Y <- X %*% multi_beta + matrix(rnorm(N * m), N, m)

result <- GICSelection(
  X = X,
  Y = Y,
  Initial_Column = 1:P,
  Calculate_GIC = "Calculate_SIC",
  Calculate_GIC_short = "Calculate_SIC_short",
  Nsim = 2L
)

print(result$GIC_coeff)
```


## Features
- 🚀 **High Performance**: Julia-accelerated critical routines
- 📊 **GLM**: Normal, Poisson, Multivariate Normal

## Documentation
- Function reference: `?GICSelection`
- Tutorial: `vignette("GICModelSelection
