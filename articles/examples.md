# Examples

This article works through complete simulations: a high-dimensional
linear model, a multivariate normal model, and Poisson, binomial and
multinomial GLMs. Each example simulates $`n = 2000`$ observations on
$`p = 500`$ candidate predictors and compares three criteria, on the
first 1000 observations and on all 2000. In increasing order of penalty
per variable, the criteria are AIC ($`c_n = 2`$), BIC ($`\log n`$) and
GIC6 ($`\log n \, \log P`$), where $`P`$ is the number of candidate
predictors.

Each table reports the size, false positives and false negatives of the
selected model. See
[`vignette("get-started")`](https://yuchenxiao95.github.io/GICHighDimension/articles/get-started.md)
for the setup steps and the other criteria. The output shown was
produced on an Apple M-series laptop with Julia 1.11.

``` r

library(GICHighDimension)
setup_julia()
```

## Helpers

First define a few helpers. Data simulation and the search order use
Julia’s random number generator, which
[`set.seed()`](https://rdrr.io/r/base/Random.html) does not control, so
`set_seeds()` seeds both. Run the examples in the order shown to
reproduce the output.

``` r

set_seeds <- function(seed) {
  set.seed(seed)
  JuliaCall::julia_command(sprintf("import Random; Random.seed!(%d);", seed))
}

criteria <- c(AIC = "Calculate_AIC", BIC = "Calculate_BIC", GIC6 = "Calculate_GIC6")
sample_sizes <- c(n1000 = 1000, n2000 = 2000)

# Run the search once per criterion, on the first n observations for each n
compare_criteria <- function(X, Y, init, k, Nsim) {
  lapply(sample_sizes, function(n) {
    Xn <- X[1:n, ]
    Yn <- if (is.matrix(Y)) Y[1:n, , drop = FALSE] else Y[1:n]
    lapply(criteria, function(crit)
      GICSelection(Xn, Yn, init, crit, paste0(crit, "_short"), k = k, Nsim = Nsim))
  })
}

last_model <- function(fit) sort(fit$selected_coeffs[[length(fit$selected_coeffs)]])

# Size, false positives and false negatives of each final model
selection_table <- function(fits, truth) {
  rows <- lapply(fits, function(by_crit) t(sapply(by_crit, function(fit) {
    s <- last_model(fit)
    c(size = length(s), false_pos = length(setdiff(s, truth)),
      false_neg = length(setdiff(truth, s)))
  })))
  out <- do.call(rbind, rows)
  rownames(out) <- paste(rep(names(fits), each = length(fits[[1]])), rownames(out))
  out
}
```

Each figure shows, for every criterion, its value and the number of
selected variables after each search step: solid lines for $`n = 1000`$
and dotted lines for $`n = 2000`$. Light grey vertical lines mark a new
pass over the predictors, and the dashed line marks the true model size.
Criteria with larger penalties take lower values, so compare the shapes
of the curves, not their heights. The script `data-raw/readme_figures.R`
in the package source runs the code in this section and redraws the
figures.

All examples use the same setting. The design matrix $`X`$ has
$`n = 2000`$ observations of $`p = 500`$ independent standard normal
predictors, so the predictor covariance is $`\Sigma = I`$, and only a
few predictors affect the response. Every search runs twice, on the
first 1000 observations and on all 2000.

## Linear model

Here the response is $`Y = X\beta + \varepsilon`$ with
$`\varepsilon \sim N(0, \sigma^2 I)`$. Five randomly chosen coefficients
equal 2 and the rest are 0, and $`\sigma^2`$ gives a signal-to-noise
ratio of $`\beta^\top\Sigma\beta/\sigma^2 = 0.71`$. Every search starts
from the full model:

``` r

set_seeds(1)
n <- 2000; p <- 500
X <- matrix(rnorm(n * p), n, p)
true_cols <- sort(sample(p, 5))
beta <- numeric(p); beta[true_cols] <- 2
Y <- LP_to_Y(X, beta, family = "Normal", std = sqrt(sum(beta^2) / 0.71))

fits <- compare_criteria(X, Y, 1:p, k = p, Nsim = 5)
selection_table(fits, true_cols)
#>            size false_pos false_neg
#> n1000 AIC   152       147         0
#> n1000 BIC     8         3         0
#> n1000 GIC6    5         0         0
#> n2000 AIC   107       102         0
#> n2000 BIC     9         4         0
#> n2000 GIC6    5         0         0
```

![Criteria and model size along the search, linear model with p =
500](../reference/figures/README-linear-log.png)

Criteria and model size along the search, linear model with p = 500

## Multivariate normal

Five responses, each with its own 5 predictors and coefficients from 10
down to 0.1. Pass the $`n \times 5`$ response matrix as `Y`:

``` r

m <- 5
B <- matrix(0, p, m)
true_by_response <- vector("list", m)
for (i in 1:m) {
  cols <- sort(sample(p, 5))
  true_by_response[[i]] <- cols
  B[cols, i] <- seq(10, 0.1, length.out = 5)
}
Y_multi <- LP_to_Y(X, B, family = "MultivariateNormal", cov_matrix = diag(m))
true_union <- sort(unique(unlist(true_by_response)))

fits <- compare_criteria(X, Y_multi, 1:p, k = p, Nsim = 5)
selection_table(fits, true_union)
#>            size false_pos false_neg
#> n1000 AIC    90        67         1
#> n1000 BIC    20         0         4
#> n1000 GIC6   19         0         5
#> n2000 AIC    76        53         1
#> n2000 BIC    20         0         4
#> n2000 GIC6   19         0         5
```

![Criteria and model size along the search, multivariate normal
response](../reference/figures/README-multivariate-log.png)

Criteria and model size along the search, multivariate normal response

## Generalized linear models

For a non-Gaussian response, map it to the linear-predictor scale with
[`Y_to_LP()`](https://yuchenxiao95.github.io/GICHighDimension/reference/Y_to_LP.md),
select with
[`GICSelection()`](https://yuchenxiao95.github.io/GICHighDimension/reference/GICSelection.md),
and then fit the selected model with
[`Beta_estimate()`](https://yuchenxiao95.github.io/GICHighDimension/reference/Beta_estimate.md).
The GLM examples reuse the design matrix `X` from above ($`n = 2000`$,
$`p = 500`$), with 5 true predictors, and start each search from the
first 10 columns.

``` r

set_seeds(2)
true_cols <- c(3, 17, 42, 88, 150)
beta <- numeric(p); beta[true_cols] <- c(1, -1, 0.8, -0.8, 0.6)

compare_glm <- function(Y, ...) compare_criteria(X, Y_to_LP(Y, ...), 1:10, k = 50, Nsim = 3)
```

### Poisson regression

``` r

y_pois <- LP_to_Y(X, beta, family = "Poisson")
fits <- compare_glm(y_pois, family = "Poisson")
selection_table(fits, true_cols)
#>            size false_pos false_neg
#> n1000 AIC    48        43         0
#> n1000 BIC     9         4         0
#> n1000 GIC6    5         0         0
#> n2000 AIC    49        44         0
#> n2000 BIC    10         5         0
#> n2000 GIC6    5         0         0

# Coefficients of the GIC6 model for n = 2000, intercept first
round(Beta_estimate(X[, last_model(fits$n2000$GIC6)], y_pois, family = "Poisson"), 2)
#> [1] -0.03  1.00 -1.01  0.79 -0.80  0.62
```

![Criteria and model size along the search, Poisson
response](../reference/figures/README-poisson.png)

Criteria and model size along the search, Poisson response

### Binomial counts (10 trials)

``` r

y_binom <- LP_to_Y(X, beta, family = "Binomial", n_trials = 10)
fits <- compare_glm(y_binom, family = "Binomial", n_trials = 10)
selection_table(fits, true_cols)
#>            size false_pos false_neg
#> n1000 AIC    49        44         0
#> n1000 BIC     9         4         0
#> n1000 GIC6    5         0         0
#> n2000 AIC    49        45         1
#> n2000 BIC     8         3         0
#> n2000 GIC6    5         0         0
```

![Criteria and model size along the search, binomial
response](../reference/figures/README-binomial.png)

Criteria and model size along the search, binomial response

### Multinomial (three categories)

The coefficients form a $`p \times 2`$ matrix, and category 3 is the
baseline:

``` r

B_mn <- matrix(0, p, 2)
B_mn[true_cols, 1] <- 1.5
B_mn[true_cols, 2] <- c(-1.5, 1.5, -1.5, 1.5, -1.5)
y_mn <- LP_to_Y(X, B_mn, family = "Multinomial", n_categories = 3)
fits <- compare_glm(y_mn, family = "Multinomial", n_categories = 3)
selection_table(fits, true_cols)
#>            size false_pos false_neg
#> n1000 AIC    49        47         3
#> n1000 BIC     9         4         0
#> n1000 GIC6    5         0         0
#> n2000 AIC    49        44         0
#> n2000 BIC     8         3         0
#> n2000 GIC6    5         0         0
```

![Criteria and model size along the search, multinomial
response](../reference/figures/README-multinomial.png)

Criteria and model size along the search, multinomial response
