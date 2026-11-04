# Coefficient Estimation via Julia

Calls the Julia function **`Beta_estimate`** to obtain coefficient
estimates for OLS, GLM, multivariate-normal, or multinomial models. When
`family = "auto"` Julia chooses the estimator automatically.

## Usage

``` r
Beta_estimate(
  X,
  Y,
  family = "auto",
  link = "auto",
  n_trials = NULL,
  theta = NULL,
  add_intercept = TRUE
)
```

## Arguments

- X:

  Numeric design matrix (\\n \times p\\).

- Y:

  Numeric vector (\\n\\) or matrix (\\n \times q\\) of responses. A
  one-column matrix is automatically converted to a vector.

- family:

  One of `"auto"` (default, guessed from `Y`), `"Linear"` (or
  `"Normal"`), `"MultivariateNormal"`, `"Bernoulli"`, `"Binomial"`,
  `"Poisson"`, `"Gamma"`, `"Exponential"`, `"InverseGaussian"`,
  `"NegativeBinomial"`, `"Geometric"`, or `"Multinomial"` (one-vs-rest
  logistic). The default links match
  [`LP_to_Y()`](https://yuchenxiao95.github.io/GICHighDimension/reference/LP_to_Y.md):
  identity, logit, or log.

- link:

  Character/symbol/Julia `Link`; default `"auto"`.

- n_trials:

  Integer trials for a binomial response (default `NULL`).

- theta:

  Optional NegativeBinomial size; estimated from the data if `NULL`.

- add_intercept:

  Logical. If `TRUE` (default) the wrapper asks Julia to prepend a
  column of 1s and **keeps the intercept coefficient** in the returned
  vector/matrix. Set to `FALSE` if `X` already contains an intercept
  column.

## Value

Numeric vector or matrix of coefficients (intercept first if
`add_intercept = TRUE`).
