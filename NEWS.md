# IChop 0.1.2.9000

This is the first entry in the changelog. It describes the package as it
stands; later entries will list changes relative to this version.

* The package is renamed from GICHighDimension to **IChop** (information
  criterion selection by Hopfield networks). The Julia version is renamed
  from GICVariableSelection to IChop.jl.

## Variable selection

* `GICSelection()` selects variables by a Hopfield-network coordinate search:
  it visits the candidate variables in random order, proposes adding or
  removing each one, and keeps the move when the information criterion
  improves (Xiao and Walker, 2026, *Statistics and Computing*,
  <doi:10.1007/s11222-026-10853-5>).
* `GICSelectionBoltzmann()` runs the same search but also accepts a move that
  lowers the criterion, with probability $1/(1 + e^{\Delta/T})$, so it can
  leave a local optimum.
* Both return the criterion value and the selected variables after each step;
  `GICSelection()` also returns `elapsed`, the time since the search started.
* `k` caps the model size, and models never exceed $n - 1$ variables, so the
  search also works when $p > n$.
* Vector responses and matrix responses (several outcomes sharing the same
  predictors) are supported.

## Information criteria

* AIC, AICc, BIC, CAIC, CAICF, SIC, GIC2–GIC6, EBIC and AttIC, each as a
  `Calculate_*()` function that scores a single model. See
  `?information_criteria` for their penalties and references.
* EBIC's $\gamma$ is set with the `gamma` argument (default 0.6).

## Speed

* The search never forms the $n \times n$ hat matrix. Every criterion is
  computed from $X^\top Y$ and $(X^\top X)^{-1}$, and each move is scored with a
  rank-one update at $O(nK + K^2)$ cost for a model with $K$ variables. On
  simulated data this is 38–371 times faster than version 0.1.2.
* The search runs in Julia, and the Julia code is loaded once per session.
  `setup_julia()` connects R to Julia and `install_julia_packages()` installs
  the Julia packages the package needs.

## Generalized linear models

* `LP_to_Y()` simulates responses from a linear predictor, `Y_to_LP()` maps
  responses to the linear-predictor scale before selection, and
  `Beta_estimate()` fits the selected model. They support Normal, Bernoulli,
  Binomial, Poisson, Gamma, Exponential, InverseGaussian, NegativeBinomial,
  Geometric, MultivariateNormal and Multinomial responses.
* `Generate_Beta()` generates sparse coefficient vectors.

## Documentation

* A pkgdown website, <https://yuchenxiao95.github.io/IChop/>, with
  a function reference and one article, "An Introduction to IChop",
  covering every response family and comparing the search with the lasso and
  adaptive lasso from glmnet.
* `citation("IChop")` cites Xiao and Walker (2026).
