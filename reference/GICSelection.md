# GIC-Based Variable Selection

Performs variable selection using the Generalized Information Criterion
(GIC) with Hopfield Network optimization. The computationally intensive
components are implemented in 'Julia' for performance, while an 'R'
interface provides user-friendly access.

## Usage

``` r
GICSelection(X, Y, Initial_Column, Calculate_GIC, Calculate_GIC_short, k,
             Nsim = 1L, gamma = 0.6)
```

## Arguments

- X:

  Numeric design matrix (n x p), where n is the number of observations
  and p is the number of predictors.

- Y:

  Numeric response (either a vector of length n or a matrix with n
  rows).

- Initial_Column:

  Integer vector of initial feature indices to consider.

- Calculate_GIC:

  Character name of the 'Julia' function for full GIC calculation.

- Calculate_GIC_short:

  Character name of the matching `_short` 'Julia' function. Kept for
  backward compatibility; the built-in criteria use fast rank-one
  updates instead.

- k:

  Integer model-size cap: a move that would give a model with `k` or
  more variables is skipped. Models also never exceed \\n - 1\\
  variables (\\n\\ = sample size), so selected models have at most
  `min(k, n) - 1` variables.

- Nsim:

  Integer number of passes over the variables, each in a new random
  order (default: 1).

- gamma:

  EBIC tuning constant \\\gamma\\ (default 0.6); ignored by other
  criteria.

## Value

A list containing:

- `GIC_values`: Calculated GIC values for all candidate models

- `selected_coeffs`: Indices of selected variables

- `elapsed`: Elapsed time in seconds since the search started, after
  each step (the Julia search only, excluding data transfer)

## Details

This function implements a variable selection algorithm that:

- Uses customizable information criteria (such as AIC, BIC, and ICOMP)

- Leverages a 'Julia' backend for computational efficiency

- Supports various model families, including generalized linear models
  (GLMs) such as the normal and Poisson families

- Accepts both univariate and multivariate responses

## References

Xiao, Y. and Walker, S. G. (2026). Fast variable selection under
\\\ell_0\\ regularization in high-dimensions. *Statistics and
Computing*, 36(3), 100.
[doi:10.1007/s11222-026-10853-5](https://doi.org/10.1007/s11222-026-10853-5)

## Examples

``` r
if (FALSE) { # \dontrun{
if (requireNamespace("JuliaCall", quietly = TRUE)) {
  julia_available <- FALSE
  tryCatch({
    JuliaCall::julia_setup(installJulia = FALSE)
    julia_available <- TRUE
  }, error = function(e) {
    message("Julia not available: ", e$message)
  })

  if (julia_available) {
    set.seed(123)
    n <- 100; p <- 10; k <- 3
    X <- matrix(rnorm(n * p), n, p)
    beta <- c(rep(1.5, k), rep(0, p - k))
    Y_uni <- LP_to_Y(X, beta, family = "Normal", std = 1.0)

    result_uni <- GICSelection(
      X = X,
      Y = Y_uni,
      Initial_Column = 1:p,
      Calculate_GIC = "Calculate_SIC",
      Calculate_GIC_short = "Calculate_SIC_short",
      k = 3,
      Nsim = 3
    )
    print(result_uni$selected_coeffs)

    m <- 3
    multi_beta <- matrix(0, p, m)
    multi_beta[1:3, ] <- 1
    rho <- 0.2
    cov_p <- matrix(rho, nrow = m, ncol = m)
    diag(cov_p) <- 1.0
    Y_multi <- LP_to_Y(X, multi_beta, family = "MultivariateNormal", cov_matrix = cov_p)

    result_multi <- GICSelection(
      X = X,
      Y = Y_multi,
      Initial_Column = 1:p,
      Calculate_GIC = "Calculate_SIC",
      Calculate_GIC_short = "Calculate_SIC_short",
      k = 3,
      Nsim = 3
    )
    print(result_multi$selected_coeffs)
  }
}
} # }
```
