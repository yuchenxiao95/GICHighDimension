# GICModelSelection

[![R-CMD-check](https://github.com/yourusername/GICModelSelection/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/yourusername/GICModelSelection/actions)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

R package for high-dimensional model selection using Generalized Information Criterion (GIC) with Julia backend acceleration.

## Installation

### Prerequisites
- [Julia](https://julialang.org/download/) (≥ 1.6)
- [R](https://cran.r-project.org/) (≥ 4.0.0)

### Install Package
```r
# Install from GitHub
if (!require("remotes")) install.packages("remotes")
remotes::install_github("yourusername/GICModelSelection")

# First-time setup (installs Julia dependencies)
GICModelSelection::install_julia_dependencies()
```

## Quick Example
```r
library(GICModelSelection)

# Generate synthetic data
X <- matrix(rnorm(1000*500), 1000, 500)  # 1000 samples, 500 features
true_beta <- c(rep(2, 5), rep(0, 495))   # 5 true signals
Y <- LP_to_Y(X, true_beta, family = "Normal")
init_cols <- 1:P
# Run model selection
result <- GICSelection(
  X = X,
  Y = Y,
  init_cols,
  "Calculate_BIC",
  "Calculate_BIC_short",
  Nsim = 10L
)

# Extract results
selected_vars <- which(rowSums(abs(result$GIC_coeff)) > 1e-6)
```

## Features
- 🚀 **High Performance**: Julia-accelerated critical routines
- 📊 **Multiple Families**: Normal, Poisson, Multivariate Normal
- 🔍 **Model Diagnostics**: Built-in visualization tools

## Documentation
- Function reference: `?GICSelection`
- Tutorial: `vignette("GICModelSelection
