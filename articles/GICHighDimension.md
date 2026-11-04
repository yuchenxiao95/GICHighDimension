# Get started with GICHighDimension

`GICHighDimension` selects variables in high-dimensional regression by
maximizing a generalized information criterion (GIC). The search is a
Hopfield-network style coordinate search: it visits the candidate
variables in random order, proposes adding a variable that is out of the
model or removing one that is in it, and keeps the move when the
criterion improves. The method is described in Xiao and Walker (2026),
*Statistics and Computing*,
<https://doi.org/10.1007/s11222-026-10853-5>.

## Setting up Julia

The search runs in ‘Julia’ (≥ 1.9). Install Julia from
<https://julialang.org/downloads/>, then connect R to it:

``` r

library(GICHighDimension)
setup_julia()               # or setup_julia("/path/to/julia/bin")
install_julia_packages()    # first time only
```

## Selecting variables

Simulate a sparse linear model with 30 candidate predictors, 3 of them
active:

``` r

set.seed(123)
n <- 300; p <- 30
X <- matrix(rnorm(n * p), n, p)
beta <- c(2, -1.5, 1, rep(0, p - 3))
Y <- LP_to_Y(X, beta, family = "Normal")
```

Run the search with EBIC, starting from the first five variables:

``` r

fit <- GICSelection(
  X, Y,
  Initial_Column      = 1:5,
  Calculate_GIC       = "Calculate_EBIC",
  Calculate_GIC_short = "Calculate_EBIC_short",
  k    = 15,     # skip models with 15 or more variables
  Nsim = 3       # passes over the variables
)

sort(fit$selected_coeffs[[length(fit$selected_coeffs)]])
#> [1] 1 2 3
```

`fit$GIC_values` records the criterion after each of the `Nsim * p`
steps, so `plot(fit$GIC_values, type = "l")` shows the search
converging. Models never have more than `min(k, n) - 1` variables.

## Choosing a criterion

Every criterion has the form
``` math
\mathrm{GIC} = \frac{Y^\top H Y}{n} - \frac{K\,\hat\sigma^2}{n}\,c_n ,
```
where $`H`$ is the hat matrix of the selected columns, $`K`$ the number
of selected variables, $`\hat\sigma^2`$ the residual variance, and
$`c_n`$ the penalty:

| Function | Penalty $`c_n`$ |
|:---|:---|
| `Calculate_AIC` | $`1`$ |
| `Calculate_BIC` | $`\log n`$ |
| `Calculate_SIC` | $`\sqrt n`$ |
| `Calculate_CAIC` | $`1 + \log n`$ |
| `Calculate_GIC2` | $`P^{1/3}`$ |
| `Calculate_GIC3` | $`2 \log P`$ |
| `Calculate_GIC4` | $`2(\log P + \log\log P)`$ |
| `Calculate_GIC5` | $`\log\log n \, \log P`$ |
| `Calculate_GIC6` | $`\log n \, \log P`$ |
| `Calculate_EBIC` | $`\log n + \gamma \log \binom{P}{K}`$, with $`\gamma = 0.6`$ by default |

Here $`P`$ is the number of candidate variables. `Calculate_AICc`,
`Calculate_CAICF` and `Calculate_AttIC` use slightly different forms;
see
[`?information_criteria`](https://yuchenxiao95.github.io/GICHighDimension/reference/information_criteria.md).
Pass the name of a criterion and of its `_short` version to
[`GICSelection()`](https://yuchenxiao95.github.io/GICHighDimension/reference/GICSelection.md).
For EBIC, change $`\gamma`$ with the `gamma` argument.

## Other response types

For a matrix response (several outcomes sharing the same predictors),
pass an $`n \times q`$ matrix as `Y`; the criterion is then the trace of
a $`q \times q`$ matrix.

For a non-Gaussian response, transform it to the linear-predictor scale
with
[`Y_to_LP()`](https://yuchenxiao95.github.io/GICHighDimension/reference/Y_to_LP.md)
before selecting, and estimate the coefficients of the selected model
with
[`Beta_estimate()`](https://yuchenxiao95.github.io/GICHighDimension/reference/Beta_estimate.md):

``` r

y_count <- LP_to_Y(X, beta / 2, family = "Poisson")
fit_pois <- GICSelection(X, Y_to_LP(y_count, family = "Poisson"), 1:5,
                         "Calculate_EBIC", "Calculate_EBIC_short", k = 15, Nsim = 3)
selected <- sort(fit_pois$selected_coeffs[[length(fit_pois$selected_coeffs)]])
Beta_estimate(X[, selected, drop = FALSE], y_count, family = "Poisson")
```

The supported families are Normal, Bernoulli, Binomial, Poisson, Gamma,
Exponential, InverseGaussian, NegativeBinomial, Geometric,
MultivariateNormal and Multinomial.
