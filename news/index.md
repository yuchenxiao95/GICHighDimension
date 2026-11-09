# Changelog

## GICHighDimension (development version)

### Speed

- The selection search no longer forms the n x n hat matrix. Every
  criterion is computed from X’Y and (X’X)^{-1}, and each add/remove
  move is scored with a rank-one update, at O(nK + K^2) cost. On
  simulated data this is 38-371 times faster than 0.1.2 and selects the
  same variables at every step.
- EBIC computes log(choose(P, K)) in floating point instead of with big
  integers.
- The Julia code is loaded once per session instead of on every call.

### New features

- New article,
  [`vignette("examples")`](https://yuchenxiao95.github.io/GICHighDimension/articles/examples.md):
  worked simulations for a linear model with 500 predictors, a
  multivariate-normal response, and Poisson, binomial and multinomial
  GLMs, each comparing AIC, BIC and GIC6 with the lasso and adaptive
  lasso at n = 1000 and 2000.
- [`LP_to_Y()`](https://yuchenxiao95.github.io/GICHighDimension/reference/LP_to_Y.md),
  [`Y_to_LP()`](https://yuchenxiao95.github.io/GICHighDimension/reference/Y_to_LP.md)
  and
  [`Beta_estimate()`](https://yuchenxiao95.github.io/GICHighDimension/reference/Beta_estimate.md)
  support Normal, Bernoulli, Binomial, Poisson, Gamma, Exponential,
  InverseGaussian, NegativeBinomial, Geometric, MultivariateNormal and
  Multinomial responses.
- [`GICSelection()`](https://yuchenxiao95.github.io/GICHighDimension/reference/GICSelection.md)
  also returns `elapsed`, the time in seconds since the search started,
  after each step.
- [`GICSelection()`](https://yuchenxiao95.github.io/GICHighDimension/reference/GICSelection.md)
  and
  [`GICSelectionBoltzmann()`](https://yuchenxiao95.github.io/GICHighDimension/reference/GICSelectionBoltzmann.md)
  gain a `gamma` argument for EBIC (default 0.6);
  [`GICSelectionBoltzmann()`](https://yuchenxiao95.github.io/GICHighDimension/reference/GICSelectionBoltzmann.md)
  gains an optional `k`.
- `citation("GICHighDimension")` cites Xiao and Walker (2026),
  *Statistics and Computing* <doi:10.1007/s11222-026-10853-5>.
- A pkgdown website.

### Bug fixes and behaviour changes

- [`Calculate_AIC()`](https://yuchenxiao95.github.io/GICHighDimension/reference/information_criteria.md)
  and the AIC search now use the standard AIC penalty, 2 K sigma^2 / n
  (Akaike, 1974). Earlier versions used half of it, K sigma^2 / n, and
  so kept more variables.
- Models never exceed n - 1 variables (n = sample size), so the search
  no longer produces a singular fit when p \> n.
- [`GICSelectionBoltzmann()`](https://yuchenxiao95.github.io/GICHighDimension/reference/GICSelectionBoltzmann.md)
  works with criteria that use P (GIC2-GIC6, EBIC) and no longer calls
  `Pkg.add()` when loaded.
- [`Calculate_EBIC()`](https://yuchenxiao95.github.io/GICHighDimension/reference/information_criteria.md)
  and `Calculate_EBIC_short()` use the same gamma (they used 0.6 and 1).
- [`GICSelection()`](https://yuchenxiao95.github.io/GICHighDimension/reference/GICSelection.md)
  stops with an error instead of returning `NA` with a warning, and
  `selected_coeffs` is a plain R list.
- [`LP_to_Y()`](https://yuchenxiao95.github.io/GICHighDimension/reference/LP_to_Y.md)
  with `family = "Gamma"` has mean exp(eta) for every `shape` (before,
  the mean was shape \* exp(eta)); univariate families return a vector.
- [`Beta_estimate()`](https://yuchenxiao95.github.io/GICHighDimension/reference/Beta_estimate.md)
  uses the log link by default for Gamma (as
  [`LP_to_Y()`](https://yuchenxiao95.github.io/GICHighDimension/reference/LP_to_Y.md)
  does), treats integer counts as Poisson when `family = "auto"`, and
  fits Binomial counts as proportions with `n_trials` weights.

## GICHighDimension 0.1.0

- Initial CRAN release.
- Added GIC-based variable selection using Boltzmann and Hopfield
  optimization strategies.
- Includes interfaces to fast Julia implementations of AIC, AICc, BIC
  (SIC), CAIC, CAICF, and GIC2–GIC6.
- Provides utility functions for data generation and coefficient vector
  construction.
- Vignette demonstrates usage and model selection workflows.
