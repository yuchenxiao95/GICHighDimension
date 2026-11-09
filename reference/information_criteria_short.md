# Information criteria with a precomputed inverse

Variants of the
[information_criteria](https://yuchenxiao95.github.io/IChop/reference/information_criteria.md)
functions that reuse a precomputed \\(X^\top X)^{-1}\\ and return only
the criterion value. They are kept for backward compatibility; the
search in
[`GICSelection()`](https://yuchenxiao95.github.io/IChop/reference/GICSelection.md)
and
[`GICSelectionBoltzmann()`](https://yuchenxiao95.github.io/IChop/reference/GICSelectionBoltzmann.md)
does not call them.

## Usage

``` r
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

- Inverse:

  A numeric matrix, the inverse of \\X^\top X\\.

- P:

  (only for EBIC and GIC2–GIC6) integer total number of candidate
  predictors. Defaults to `ncol(X)`.

## Value

A numeric scalar, the criterion value.
