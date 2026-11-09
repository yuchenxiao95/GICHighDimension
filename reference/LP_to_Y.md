# Transform Linear Predictor to Response

Converts a linear predictor to observed responses using canonical link
functions from generalized linear models (GLMs). Supports univariate and
multivariate distributions.

## Usage

``` r
LP_to_Y(
  X,
  true_beta,
  family = "Normal",
  n_trials = NULL,
  std = NULL,
  shape = NULL,
  theta = NULL,
  cov_matrix = NULL,
  n_categories = NULL,
  baseline_rate = NULL,
  censor_rate = NULL
)
```

## Arguments

- X:

  A numeric design matrix.

- true_beta:

  A numeric vector or matrix of true coefficients.

- family:

  A character string specifying the distribution family.

- n_trials:

  Optional integer, number of trials for the Binomial family.

- std:

  Optional numeric, standard deviation for the Normal family.

- shape:

  Optional numeric shape parameter (Gamma, InverseGaussian).

- theta:

  Optional numeric size parameter for the NegativeBinomial family.

- cov_matrix:

  Optional covariance matrix for multivariate families.

- n_categories:

  Optional integer, number of categories for Multinomial family.

- baseline_rate:

  Optional baseline hazard rate \\\lambda\\ for "Cox" (default 1);
  `shape` is its Weibull shape \\\nu\\ (default 1).

- censor_rate:

  Optional rate of the exponential censoring times for "Cox" (default
  0.3; 0 means no censoring).

## Value

A numeric vector of responses (integer categories for "Multinomial"), an
\\n \times q\\ matrix for "MultivariateNormal", or an \\n \times 2\\
matrix with columns `time` and `status` (1 = event) for "Cox".

## Details

Simulates responses from the linear predictor \\\eta = X\beta\\ using
the 'Julia' backend. The supported families and links (the same as in
[`Y_to_LP()`](https://yuchenxiao95.github.io/IChop/reference/Y_to_LP.md)
and
[`Beta_estimate()`](https://yuchenxiao95.github.io/IChop/reference/Beta_estimate.md))
are:

|  |  |  |
|----|----|----|
| **Family** | **Link** | **Extra arguments** |
| "Normal" | identity | `std` (default 1) |
| "Bernoulli" | logit |  |
| "Binomial" | logit | `n_trials` (required) |
| "Poisson" | log |  |
| "Gamma" | log (mean \\e^\eta\\) | `shape` (default 1) |
| "Exponential" | log |  |
| "InverseGaussian" | log | `shape` (\\\lambda\\, default 1) |
| "NegativeBinomial" | log | `theta` (size, default 1) |
| "Geometric" | log |  |
| "MultivariateNormal" | identity | `cov_matrix` (required) |
| "Multinomial" | baseline-category logit | `n_categories` (required) |
| "Cox" | log hazard | `baseline_rate`, `shape`, `censor_rate` |

For "MultivariateNormal", `true_beta` is a \\p \times q\\ matrix; for
"Multinomial" it is \\p \times (K-1)\\ and the last category is the
baseline. GLM stands for *generalized linear model*.

For "Cox", survival times follow a Cox proportional hazards model with
Weibull baseline hazard \\h_0(t) = \lambda \nu t^{\nu - 1}\\, simulated
by inverting the survival function (Bender, Augustin and Blettner,
2005): \\T = (-\log U / (\lambda e^{\eta}))^{1/\nu}\\ with \\U \sim U(0,
1)\\. Times are censored by independent exponential times with rate
`censor_rate`. `shape` = 1 gives an exponential baseline.

## References

Bender, R., Augustin, T. and Blettner, M. (2005). Generating survival
times to simulate Cox proportional hazards models. *Statistics in
Medicine*, 24(11), 1713–1723.
[doi:10.1002/sim.2059](https://doi.org/10.1002/sim.2059)

## Examples

``` r
if (FALSE) { # \dontrun{
if (interactive() && requireNamespace("JuliaCall", quietly = TRUE)) {
  julia_ready <- FALSE
  tryCatch({
    JuliaCall::julia_setup(installJulia = FALSE)
    julia_ready <- TRUE
  }, error = function(e) message("Julia setup failed"))

  if (julia_ready) {
    set.seed(123)
    n <- 100
    p <- 5
    X <- matrix(rnorm(n * p), n, p)
    beta <- c(1.5, -2, 0, 0, 3)
    Y <- LP_to_Y(X, beta, family = "Normal", std = 1.0)
    head(Y)
  }
}
} # }
```
