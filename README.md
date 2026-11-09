# GICHighDimension <img src="man/figures/logo.png" align="right" height="139" alt="GICHighDimension logo" />

GICHighDimension addresses the computational barrier of solving
$\ell_0$-penalized regression in linear models where the number of
predictors $p$ is large, in the hundreds or even the thousands, yet smaller
than the number of observations $n$. Finding the optimal solution of
$\ell_0$-penalized regression has been treated as a combinatorial problem
that requires discrete optimization and is NP-hard. This package builds on a
connection with Hopfield networks, which are used for associative memory
retrieval ([Xiao and Walker, 2026](https://doi.org/10.1007/s11222-026-10853-5)).
It makes generalized information criteria (GIC;
[Kim, Kwon and Choi, 2012](https://jmlr.org/papers/v13/kim12a.html)) for variable selection,
which are equivalent to $\ell_0$-penalized regression, computationally
feasible. The rationale is that there remains strong interest in GIC, whose
statistical properties are superior to those of $\ell_1$-based
regularization.

A model with $d$ selected predictors scores
$\mathrm{GIC}_d = y^\top H_d y / n - p(n, d)$, where $H_d$ is its hat matrix and
$p(n, d)$ is a penalty. Table 1 of Xiao and Walker (2026) lists some
criteria in the literature with different penalties $p(n, d)$:

| Name | $p(n, d)$ | Reference |
|:--|:--|:--|
| GIC2 | $d \frac{\sigma^2}{n} d^{1/3}$ | [Kim et al. (2012)](https://jmlr.org/papers/v13/kim12a.html) |
| GIC3 | $2d \frac{\sigma^2}{n} \log d$ | [Kim et al. (2012)](https://jmlr.org/papers/v13/kim12a.html) |
| GIC4 | $2d \frac{\sigma^2}{n} (\log d + \log\log d)$ | [Kim et al. (2012)](https://jmlr.org/papers/v13/kim12a.html) |
| GIC5 | $d \frac{\sigma^2}{n} \log\log n \log d$ | [Kim et al. (2012)](https://jmlr.org/papers/v13/kim12a.html) |
| GIC6 | $d \frac{\sigma^2}{n} \log n \log d$ | [Kim et al. (2012)](https://jmlr.org/papers/v13/kim12a.html) |
| CAIC | $d \frac{\sigma^2}{n} (\log n + 1)$ | [Bozdogan (1987)](https://doi.org/10.1007/BF02294361) |
| SIC | $d \frac{\sigma^2}{n} \sqrt{n}$ | Shao (1997) |

The examples below highlight some well-known statistical properties of these
criteria:

1. In linear regression, AIC is minimax-rate optimal and BIC is consistent.
   In the linear-model figure, AIC (blue line) attains the largest criterion
   value at the expense of many false positives.
2. Criteria in the GIC family achieve consistency at different rates. GIC6
   (green line) reaches the true model within two passes of the search,
   while BIC (orange line) still includes noise variables at the end.
3. Consistency is an asymptotic result. Doubling the sample size (dotted
   lines) is not yet enough for BIC: with 500 candidate predictors, it still
   adds a few noise variables at $n = 2000$ and needs a much larger sample to
   select the true model.

## Installation

Install the development version of `GICHighDimension` from GitHub:

```r
devtools::install_github("yuchenxiao95/GICHighDimension")
```

You also need Julia (≥ 1.9), which you can download from
<https://julialang.org/downloads/>. Then connect R to Julia; the first time,
also install the Julia packages the package uses:

```r
library(GICHighDimension)
setup_julia()
install_julia_packages()   # first time only
```

## Examples

The examples below compare three criteria on the same data. In increasing
order of penalty per variable, they are AIC ($c_n = 1$), BIC ($\log n$) and
GIC6 ($\log n \log P$), where $P$ is the number of candidate
predictors. The
[Examples article](https://yuchenxiao95.github.io/GICHighDimension/articles/examples.html)
(`vignette("examples")`) explains each step, and the Get started article
(`vignette("get-started")`) lists all criteria and response families.

<details>
<summary>Helper functions used in the examples</summary>

```r
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
  rownames(out) <- paste(rep(names(fits), each = length(criteria)), rownames(out))
  out
}
```

</details>

Each figure shows, for every criterion, its value and the number of selected
variables after each search step: solid lines for $n = 1000$ and dotted lines
for $n = 2000$. Light grey vertical lines mark a new pass over the
predictors, and the dashed line marks the true model size. Criteria with
larger penalties take lower values, so compare the shapes of the curves, not
their heights. The script
[`data-raw/readme_figures.R`](data-raw/readme_figures.R) runs the code in this
section and redraws the figures.

All examples use the same setting. The design matrix $X$ has $n = 2000$
observations of $p = 500$ independent standard normal predictors, so the
predictor covariance is $\Sigma = I$, and only a few predictors affect the
response. Every search runs twice, on the first 1000 observations and on all
2000.

### Linear model

Here the response is $Y = X\beta + \varepsilon$ with
$\varepsilon \sim N(0, \sigma^2 I)$. Five randomly chosen coefficients equal
2 and the rest are 0, and $\sigma^2$ gives a signal-to-noise ratio of
$\beta^\top\Sigma\beta/\sigma^2 = 0.71$. Every search starts from the full
model:

```r
set_seeds(1)
n <- 2000; p <- 500
X <- matrix(rnorm(n * p), n, p)
true_cols <- sort(sample(p, 5))
beta <- numeric(p); beta[true_cols] <- 2
Y <- LP_to_Y(X, beta, family = "Normal", std = sqrt(sum(beta^2) / 0.71))

fits <- compare_criteria(X, Y, 1:p, k = p, Nsim = 5)
selection_table(fits, true_cols)
#>            size false_pos false_neg
#> n1000 AIC   230       225         0
#> n1000 BIC     8         3         0
#> n1000 GIC6    5         0         0
#> n2000 AIC   197       192         0
#> n2000 BIC     9         4         0
#> n2000 GIC6    5         0         0
```

![Criteria and model size along the search, linear model with p = 500](man/figures/README-linear.png)

GIC6 selects exactly the 5 true predictors at both sample sizes. BIC keeps
all 5 but adds 3 noise variables at $n = 1000$ and 4 at $n = 2000$: doubling
$n$ does not help, because its penalty ignores the 500 candidates. AIC keeps
about 200 variables at either size.


### Multivariate normal

Five responses, each with its own 5 predictors and coefficients from 10
down to 0.1. Pass the $n \times 5$ response matrix as `Y`:

```r
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
#> n1000 AIC   268       245         1
#> n1000 BIC    20         0         4
#> n1000 GIC6   19         0         5
#> n2000 AIC   232       208         0
#> n2000 BIC    20         0         4
#> n2000 GIC6   19         0         5
```

![Criteria and model size along the search, multivariate normal response](man/figures/README-multivariate.png)

Two responses share a predictor, so 24 predictors are truly active. BIC and
GIC6 select no noise variables at either sample size. Every predictor they
miss (4 for BIC, 5 for GIC6) has a coefficient of 0.1, which is too small to
detect with 1000 or 2000 observations. AIC finds all 24 at $n = 2000$, but
inside a model of 232 variables.


### Generalized linear models

For a non-Gaussian response, map it to the linear-predictor scale with
`Y_to_LP()`, select with `GICSelection()`, and then fit the selected model
with `Beta_estimate()`. The GLM examples reuse the design matrix `X` from
above ($n = 2000$, $p = 500$), with 5 true predictors, and start each search
from the first 10 columns.

```r
set_seeds(2)
true_cols <- c(3, 17, 42, 88, 150)
beta <- numeric(p); beta[true_cols] <- c(1, -1, 0.8, -0.8, 0.6)

compare_glm <- function(Y, ...) compare_criteria(X, Y_to_LP(Y, ...), 1:10, k = 50, Nsim = 3)
```

#### Poisson regression

```r
y_pois <- LP_to_Y(X, beta, family = "Poisson")
fits <- compare_glm(y_pois, family = "Poisson")
selection_table(fits, true_cols)
#>            size false_pos false_neg
#> n1000 AIC    49        45         1
#> n1000 BIC     9         4         0
#> n1000 GIC6    5         0         0
#> n2000 AIC    49        45         1
#> n2000 BIC    10         5         0
#> n2000 GIC6    5         0         0

# Coefficients of the GIC6 model for n = 2000, intercept first
round(Beta_estimate(X[, last_model(fits$n2000$GIC6)], y_pois, family = "Poisson"), 2)
#> [1] -0.03  1.00 -1.01  0.79 -0.80  0.62
```

![Criteria and model size along the search, Poisson response](man/figures/README-poisson.png)

#### Binomial counts (10 trials)

```r
y_binom <- LP_to_Y(X, beta, family = "Binomial", n_trials = 10)
fits <- compare_glm(y_binom, family = "Binomial", n_trials = 10)
selection_table(fits, true_cols)
#>            size false_pos false_neg
#> n1000 AIC    49        47         3
#> n1000 BIC     9         4         0
#> n1000 GIC6    5         0         0
#> n2000 AIC    49        46         2
#> n2000 BIC     8         3         0
#> n2000 GIC6    5         0         0
```

![Criteria and model size along the search, binomial response](man/figures/README-binomial.png)

#### Multinomial (three categories)

The coefficients form a $p \times 2$ matrix, and category 3 is the baseline:

```r
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
#> n2000 AIC    49        48         4
#> n2000 BIC     8         3         0
#> n2000 GIC6    5         0         0
```

![Criteria and model size along the search, multinomial response](man/figures/README-multinomial.png)

In all three GLMs, GIC6 selects exactly the 5 true predictors at both sample
sizes. BIC keeps the true predictors but adds 3 to 5 noise variables, with no
clear gain from doubling $n$. AIC grows the model until it hits the size cap
(`k = 50`, so at most 49 variables) and misses 1 to 4 true predictors.

