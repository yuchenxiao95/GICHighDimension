# GICHighDimension

[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![DOI](https://img.shields.io/badge/DOI-10.1007%2Fs11222--026--10853--5-blue)](https://doi.org/10.1007/s11222-026-10853-5)

**GICHighDimension** selects variables in high-dimensional regression by
maximizing a generalized information criterion (GIC) with a
Hopfield-network style coordinate search. It covers the usual
information criteria (AIC, BIC, EBIC, GIC2–GIC6, …), univariate and
multivariate responses, and the common generalized linear model
families. The search runs in ‘Julia’ and is called from R through
‘JuliaCall’.

The method is described in [Xiao and Walker (2026), *Statistics and
Computing*](https://doi.org/10.1007/s11222-026-10853-5).

## Installation

You need R (≥ 3.5), [Julia](https://julialang.org/downloads/) (≥ 1.9)
and the R package ‘JuliaCall’.

``` r

# install.packages("remotes")
remotes::install_github("yuchenxiao95/GICHighDimension")
```

Then connect R to Julia once per session (the first call also installs
the Julia packages the package needs):

``` r

library(GICHighDimension)
setup_julia()               # or setup_julia("/path/to/julia/bin")
install_julia_packages()    # first time only
```

## Example

Simulate a sparse linear model with 30 candidate predictors, of which 3
are active, and select variables with EBIC:

``` r

set.seed(123)
n <- 300; p <- 30
X <- matrix(rnorm(n * p), n, p)
beta <- c(2, -1.5, 1, rep(0, p - 3))
Y <- LP_to_Y(X, beta, family = "Normal")

fit <- GICSelection(
  X, Y,
  Initial_Column      = 1:5,
  Calculate_GIC       = "Calculate_EBIC",
  Calculate_GIC_short = "Calculate_EBIC_short",
  k    = 15,            # skip models with 15 or more variables
  Nsim = 3              # passes over the variables
)

sort(fit$selected_coeffs[[length(fit$selected_coeffs)]])   # final selected set
#> [1] 1 2 3
```

`fit$GIC_values` holds the criterion after every step of the search, and
`fit$selected_coeffs` the selected set.

## What is included

| Task | Functions |
|:---|:---|
| Variable selection | [`GICSelection()`](https://yuchenxiao95.github.io/GICHighDimension/reference/GICSelection.md) (greedy), [`GICSelectionBoltzmann()`](https://yuchenxiao95.github.io/GICHighDimension/reference/GICSelectionBoltzmann.md) (stochastic) |
| Information criteria | [`Calculate_AIC()`](https://yuchenxiao95.github.io/GICHighDimension/reference/information_criteria.md), [`Calculate_AICc()`](https://yuchenxiao95.github.io/GICHighDimension/reference/information_criteria.md), [`Calculate_BIC()`](https://yuchenxiao95.github.io/GICHighDimension/reference/information_criteria.md), [`Calculate_SIC()`](https://yuchenxiao95.github.io/GICHighDimension/reference/information_criteria.md), [`Calculate_CAIC()`](https://yuchenxiao95.github.io/GICHighDimension/reference/information_criteria.md), [`Calculate_CAICF()`](https://yuchenxiao95.github.io/GICHighDimension/reference/information_criteria.md), [`Calculate_EBIC()`](https://yuchenxiao95.github.io/GICHighDimension/reference/information_criteria.md), [`Calculate_GIC2()`](https://yuchenxiao95.github.io/GICHighDimension/reference/information_criteria.md) … [`Calculate_GIC6()`](https://yuchenxiao95.github.io/GICHighDimension/reference/information_criteria.md), [`Calculate_AttIC()`](https://yuchenxiao95.github.io/GICHighDimension/reference/information_criteria.md) |
| Simulation | [`LP_to_Y()`](https://yuchenxiao95.github.io/GICHighDimension/reference/LP_to_Y.md), [`Generate_Beta()`](https://yuchenxiao95.github.io/GICHighDimension/reference/Generate_Beta.md) |
| Link transforms and estimation | [`Y_to_LP()`](https://yuchenxiao95.github.io/GICHighDimension/reference/Y_to_LP.md), [`Beta_estimate()`](https://yuchenxiao95.github.io/GICHighDimension/reference/Beta_estimate.md) |
| Julia setup | [`setup_julia()`](https://yuchenxiao95.github.io/GICHighDimension/reference/setup_julia.md), [`install_julia_packages()`](https://yuchenxiao95.github.io/GICHighDimension/reference/install_julia_packages.md), [`install_julia_dependencies()`](https://yuchenxiao95.github.io/GICHighDimension/reference/install_julia_dependencies.md) |

Supported response families in
[`LP_to_Y()`](https://yuchenxiao95.github.io/GICHighDimension/reference/LP_to_Y.md),
[`Y_to_LP()`](https://yuchenxiao95.github.io/GICHighDimension/reference/Y_to_LP.md)
and
[`Beta_estimate()`](https://yuchenxiao95.github.io/GICHighDimension/reference/Beta_estimate.md):
Normal, Bernoulli, Binomial, Poisson, Gamma, Exponential, inverse
Gaussian, negative binomial, geometric, multivariate normal and
multinomial.

## Speed

The search never forms the $`n \times n`$ hat matrix. Each candidate
move is scored with a rank-one update of $`(X_S^\top X_S)^{-1}`$, which
costs $`O(nK + K^2)`$ for a model with $`K`$ variables.

## Citation

If you use this package, please cite:

> Xiao, Y. and Walker, S. (2026). Fast variable selection under
> $`\ell_0`$ regularization in high-dimensions. *Statistics and
> Computing*, 36(3), 100. <https://doi.org/10.1007/s11222-026-10853-5>

``` bibtex
@article{xiao2026fast,
  title   = {Fast variable selection under $\ell_0$ regularization in high-dimensions},
  author  = {Xiao, Yuchen and Walker, Stephen},
  journal = {Statistics and Computing},
  year    = {2026},
  volume  = {36},
  number  = {3},
  pages   = {100},
  doi     = {10.1007/s11222-026-10853-5}
}
```

`citation("GICHighDimension")` prints the same reference.

## Authors

- Yuchen Xiao ([ORCID](https://orcid.org/0000-0003-2322-9660)), author
  and maintainer
- Stephen G. Walker, author

## License

MIT © Yuchen Xiao and Stephen Walker
