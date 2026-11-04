# Information Criterion Calculations via Julia

Compute information criteria using efficient 'Julia' backends via the
'JuliaCall' package. These functions are useful for variable selection
and model comparison in high-dimensional settings. Each function returns
either the computed criterion value, or a list with additional data.

## Usage

``` r
Calculate_AIC(Y, X)

Calculate_AICc(Y, X)

Calculate_AttIC(Y, X)

Calculate_SIC(Y, X)

Calculate_BIC(Y, X)

Calculate_CAIC(Y, X)

Calculate_CAICF(Y, X)

Calculate_EBIC(Y, X, P = ncol(X))

Calculate_GIC2(Y, X, P = ncol(X))

Calculate_GIC3(Y, X, P = ncol(X))

Calculate_GIC4(Y, X, P = ncol(X))

Calculate_GIC5(Y, X, P = ncol(X))

Calculate_GIC6(Y, X, P = ncol(X))

Calculate_AIC_Short(Y, X, Inverse)

Calculate_AICc_Short(Y, X, Inverse)

Calculate_AttIC_Short(Y, X, Inverse)

Calculate_SIC_Short(Y, X, Inverse)

Calculate_BIC_Short(Y, X, Inverse)

Calculate_CAIC_Short(Y, X, Inverse)

Calculate_CAICF_Short(Y, X, Inverse)

Calculate_EBIC_Short(Y, X, Inverse, P = ncol(X))

Calculate_GIC2_Short(Y, X, Inverse, P = ncol(X))

Calculate_GIC3_Short(Y, X, Inverse, P = ncol(X))

Calculate_GIC4_Short(Y, X, Inverse, P = ncol(X))

Calculate_GIC5_Short(Y, X, Inverse, P = ncol(X))

Calculate_GIC6_Short(Y, X, Inverse, P = ncol(X))
```

## Arguments

- Y:

  A numeric response: a vector of length n, or an n x q matrix for a
  multivariate response (CAICF supports vector responses only).

- X:

  A numeric design matrix (n × p).

- P:

  (only for EBIC and GIC2–GIC6) integer total number of candidate
  predictors. Defaults to `ncol(X)`.

- Inverse:

  (for `_Short` variants only) a numeric matrix — the inverse of X'X.

## Value

For full versions: a named list with components:

- CriterionValue:

  Numeric scalar of the selected information criterion.

- InverseMatrix:

  Matrix inverse used in computation (if applicable).

For `_Short` versions: a numeric scalar of the criterion value.

## Details

This interface leverages 'Julia' to compute information-theoretic
criteria efficiently, especially when repeatedly evaluating models.
Supported criteria include:

- AIC: Akaike Information Criterion

- AICc: Corrected AIC for small samples

- BIC / SIC: Bayesian or Schwarz Information Criterion

- CAIC: Consistent AIC

- CAICF: Consistent AIC with Fisher Information

- EBIC: Extended BIC (depends on the candidate pool size \\P\\)

- GIC2–GIC6: Generalized Information Criterion family (depend on \\P\\)

- AttIC: Attention-weighted Information Criterion

Short versions (e.g., `Calculate_AIC_Short`) allow reusing a precomputed
inverse matrix to save computational time.

## Examples

``` r
if (FALSE) { # \dontrun{
if (interactive() && requireNamespace("JuliaCall", quietly = TRUE)) {
  setup_julia(install_julia = FALSE)
  X <- matrix(rnorm(100), ncol = 5)
  Y <- rnorm(20)
  out1 <- Calculate_AIC(Y, X)
  out2 <- Calculate_EBIC(Y, X)        # uses P = ncol(X) by default
  val_short <- Calculate_EBIC_Short(Y, X, solve(crossprod(X)))
}
} # }
```
