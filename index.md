# GICHighDimension

GICHighDimension is an R package for variable selection in
high-dimensional regression. It searches over subsets of predictors for
the one that maximizes a generalized information criterion (GIC), such
as AIC, BIC, EBIC or GIC2–GIC6, using a fast Hopfield-network style
coordinate search. Univariate and multivariate responses are supported,
along with the common generalized linear model families.

The search engine is written in [Julia](https://julialang.org/) and
called from R through `JuliaCall`. It never forms the $`n \times n`$ hat
matrix: every proposed move is scored with a rank-one update, so a pass
over thousands of candidate predictors takes milliseconds. The method is
described in [Xiao and Walker
(2026)](https://doi.org/10.1007/s11222-026-10853-5).

## Installation

Install the development version of `GICHighDimension` from GitHub:

``` r

devtools::install_github("yuchenxiao95/GICHighDimension")
```

You also need Julia (≥ 1.9), which you can download from
<https://julialang.org/downloads/>. Then connect R to Julia; the first
time, also install the Julia packages the package uses:

``` r

library(GICHighDimension)
setup_julia()
install_julia_packages()   # first time only
```

## Example

Simulate data with 200 candidate predictors, 5 of them active, and
select variables with EBIC:

``` r

set.seed(1)
n <- 300; p <- 200
X <- matrix(rnorm(n * p), n, p)
beta <- c(2, -1.5, 1, 1, -1, rep(0, p - 5))
Y <- LP_to_Y(X, beta, family = "Normal")

fit <- GICSelection(X, Y,
  Initial_Column      = 1:20,
  Calculate_GIC       = "Calculate_EBIC",
  Calculate_GIC_short = "Calculate_EBIC_short",
  k = 50, Nsim = 3
)

sort(fit$selected_coeffs[[length(fit$selected_coeffs)]])
#> [1] 1 2 3 4 5
```

The search starts from 20 variables, removes the 15 noise variables and
keeps the 5 true ones within its first pass over the predictors (each
dotted line marks a new pass):

![EBIC and model size along the
search](reference/figures/README-search.png)

EBIC and model size along the search

For the available criteria and response families, see the Get started
article,
[`vignette("get-started")`](https://yuchenxiao95.github.io/GICHighDimension/articles/get-started.md).

## More examples

The examples below compare four criteria on the same data. In increasing
order of penalty per variable, they are AIC ($`c_n = 1`$), BIC
($`\log n`$), GIC5 ($`\log\log n \, \log P`$) and GIC6
($`\log n \, \log P`$). The [Examples
article](https://yuchenxiao95.github.io/GICHighDimension/articles/examples.html)
([`vignette("examples")`](https://yuchenxiao95.github.io/GICHighDimension/articles/examples.md))
explains each step.

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

criteria <- c(AIC = "Calculate_AIC", BIC = "Calculate_BIC",
              GIC5 = "Calculate_GIC5", GIC6 = "Calculate_GIC6")

# Run the search once per criterion
compare_criteria <- function(X, Y, init, k, Nsim) {
  lapply(criteria, function(crit)
    GICSelection(X, Y, init, crit, paste0(crit, "_short"), k = k, Nsim = Nsim))
}

last_model <- function(fit) sort(fit$selected_coeffs[[length(fit$selected_coeffs)]])

# Size, false positives and false negatives of each final model
selection_table <- function(fits, truth) {
  t(sapply(fits, function(fit) {
    s <- last_model(fit)
    c(size = length(s), false_pos = length(setdiff(s, truth)),
      false_neg = length(setdiff(truth, s)))
  }))
}
```

Each figure shows, for every criterion, its value and the number of
selected variables after each search step. Dotted lines mark a new pass
over the predictors, and the dashed line marks the true model size.
Criteria with larger penalties take lower values, so compare the shapes
of the curves, not their heights. The script
[`data-raw/readme_figures.R`](https://yuchenxiao95.github.io/GICHighDimension/data-raw/readme_figures.R)
runs the code in this section and redraws the figures.

### Linear model with 500 candidate predictors

Take $`n = 1000`$ and $`p = 500`$, with 5 true coefficients equal to 2
and the noise set for a signal-to-noise ratio of
$`\beta^\top\Sigma\beta/\sigma^2 = 0.71`$. Start every search from the
full model:

``` r

set_seeds(1)
n <- 1000; p <- 500
X <- matrix(rnorm(n * p), n, p)
true_cols <- sort(sample(p, 5))
beta <- numeric(p); beta[true_cols] <- 2
Y <- LP_to_Y(X, beta, family = "Normal", std = sqrt(sum(beta^2) / 0.71))

fits <- compare_criteria(X, Y, 1:p, k = p, Nsim = 5)
selection_table(fits, true_cols)
#>      size false_pos false_neg
#> AIC   211       206         0
#> BIC     6         1         0
#> GIC5    5         0         0
#> GIC6    5         0         0
```

![Criteria and model size along the search, linear model with p =
500](reference/figures/README-linear.png)

Criteria and model size along the search, linear model with p = 500

AIC keeps about 200 variables, because its penalty does not grow with
$`n`$ or $`P`$. BIC keeps all 5 true predictors and one noise variable.
GIC5 and GIC6, whose penalties grow with $`\log P`$, select exactly the
true model, and GIC6, the largest penalty, gets there fastest.

### Several responses (multivariate normal)

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
#>      size false_pos false_neg
#> AIC   276       251         0
#> BIC    20         0         5
#> GIC5   20         0         5
#> GIC6   20         0         5
```

![Criteria and model size along the search, multivariate normal
response](reference/figures/README-multivariate.png)

Criteria and model size along the search, multivariate normal response

BIC, GIC5 and GIC6 select the same 20 predictors with no false
positives. The 5 they miss are the predictors with coefficient 0.1, one
per response, which are too small to detect with $`n = 1000`$. AIC finds
those 5 too, but only inside a model of 276 variables.

### Generalized linear models

For a non-Gaussian response, map it to the linear-predictor scale with
[`Y_to_LP()`](https://yuchenxiao95.github.io/GICHighDimension/reference/Y_to_LP.md),
select with
[`GICSelection()`](https://yuchenxiao95.github.io/GICHighDimension/reference/GICSelection.md),
and then fit the selected model with
[`Beta_estimate()`](https://yuchenxiao95.github.io/GICHighDimension/reference/Beta_estimate.md).
All GLM examples share this setup: $`n = 1000`$, $`p = 200`$, 5 true
predictors, and searches that start from the first 10 columns.

``` r

set_seeds(2)
n <- 1000; p <- 200
X <- matrix(rnorm(n * p), n, p)
true_cols <- c(3, 17, 42, 88, 150)
beta <- numeric(p); beta[true_cols] <- c(1, -1, 0.8, -0.8, 0.6)

compare_glm <- function(Y, ...) compare_criteria(X, Y_to_LP(Y, ...), 1:10, k = 50, Nsim = 3)
```

#### Logistic regression (Bernoulli)

``` r

y_bern <- LP_to_Y(X, beta, family = "Bernoulli")
fits <- compare_glm(y_bern, family = "Bernoulli")
selection_table(fits, true_cols)
#>      size false_pos false_neg
#> AIC    49        45         1
#> BIC     7         2         0
#> GIC5    5         0         0
#> GIC6    5         0         0

# Coefficients of the GIC6 model, intercept first
round(Beta_estimate(X[, last_model(fits$GIC6)], y_bern, family = "Bernoulli"), 2)
#> [1] -0.07  0.81 -1.05  0.78 -0.85  0.59
```

![Criteria and model size along the search, Bernoulli
response](reference/figures/README-bernoulli.png)

Criteria and model size along the search, Bernoulli response

#### Binomial counts (10 trials)

``` r

y_binom <- LP_to_Y(X, beta, family = "Binomial", n_trials = 10)
fits <- compare_glm(y_binom, family = "Binomial", n_trials = 10)
selection_table(fits, true_cols)
#>      size false_pos false_neg
#> AIC    49        47         3
#> BIC     5         0         0
#> GIC5    5         0         0
#> GIC6    5         0         0
```

![Criteria and model size along the search, binomial
response](reference/figures/README-binomial.png)

Criteria and model size along the search, binomial response

#### Poisson regression

``` r

y_pois <- LP_to_Y(X, beta, family = "Poisson")
fits <- compare_glm(y_pois, family = "Poisson")
selection_table(fits, true_cols)
#>      size false_pos false_neg
#> AIC    49        45         1
#> BIC     8         3         0
#> GIC5    5         0         0
#> GIC6    5         0         0

round(Beta_estimate(X[, last_model(fits$GIC6)], y_pois, family = "Poisson"), 2)
#> [1] -0.02  1.01 -1.01  0.81 -0.80  0.59
```

![Criteria and model size along the search, Poisson
response](reference/figures/README-poisson.png)

Criteria and model size along the search, Poisson response

#### Overdispersed counts (negative binomial, size 2)

``` r

y_nb <- LP_to_Y(X, beta, family = "NegativeBinomial", theta = 2)
fits <- compare_glm(y_nb, family = "NegativeBinomial")
selection_table(fits, true_cols)
#>      size false_pos false_neg
#> AIC    49        44         0
#> BIC     5         0         0
#> GIC5    5         0         0
#> GIC6    5         0         0
```

![Criteria and model size along the search, negative binomial
response](reference/figures/README-negbin.png)

Criteria and model size along the search, negative binomial response

#### Gamma (log link)

``` r

y_gamma <- LP_to_Y(X, beta, family = "Gamma", shape = 2)
fits <- compare_glm(y_gamma, family = "Gamma")
selection_table(fits, true_cols)
#>      size false_pos false_neg
#> AIC    48        43         0
#> BIC     6         1         0
#> GIC5    6         1         0
#> GIC6    5         0         0
```

![Criteria and model size along the search, gamma
response](reference/figures/README-gamma.png)

Criteria and model size along the search, gamma response

#### Inverse Gaussian (log link)

``` r

y_ig <- LP_to_Y(X, beta, family = "InverseGaussian", shape = 2)
fits <- compare_glm(y_ig, family = "InverseGaussian")
selection_table(fits, true_cols)
#>      size false_pos false_neg
#> AIC    49        44         0
#> BIC     5         0         0
#> GIC5    5         0         0
#> GIC6    5         0         0
```

![Criteria and model size along the search, inverse Gaussian
response](reference/figures/README-invgauss.png)

Criteria and model size along the search, inverse Gaussian response

#### Multinomial (three categories)

The coefficients form a $`p \times 2`$ matrix, and category 3 is the
baseline:

``` r

B_mn <- matrix(0, p, 2)
B_mn[true_cols, 1] <- 1.5
B_mn[true_cols, 2] <- c(-1.5, 1.5, -1.5, 1.5, -1.5)
y_mn <- LP_to_Y(X, B_mn, family = "Multinomial", n_categories = 3)
fits <- compare_glm(y_mn, family = "Multinomial", n_categories = 3)
selection_table(fits, true_cols)
#>      size false_pos false_neg
#> AIC    49        45         1
#> BIC     6         1         0
#> GIC5    5         0         0
#> GIC6    5         0         0
```

![Criteria and model size along the search, multinomial
response](reference/figures/README-multinomial.png)

Criteria and model size along the search, multinomial response

Across the seven families, GIC6 selects exactly the 5 true predictors
every time, and GIC5 does so in all but the gamma example. BIC adds up
to 3 noise variables. AIC grows the model until it hits the size cap
(`k = 50`, so at most 49 variables), and once that cap blocks further
additions it can miss a true predictor.
