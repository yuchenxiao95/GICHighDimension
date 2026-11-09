# Transform Outcome to Linear Predictor

Uses the Julia function `'Y_to_LP'` to convert observed responses to
linear predictors using canonical link functions from generalized linear
models (GLMs).

## Usage

``` r
Y_to_LP(
  Y,
  family,
  shape = NULL,
  n_trials = NULL,
  std = NULL,
  theta = NULL,
  n_categories = NULL
)
```

## Arguments

- Y:

  A numeric vector or matrix of observed responses.

- family:

  A character string indicating the GLM family to use.

- shape:

  Optional shape parameter (accepted for consistency; unused).

- n_trials:

  Optional number of trials (used for the Binomial family).

- std:

  Optional standard deviation (accepted for consistency; unused).

- theta:

  Optional NegativeBinomial size (accepted for consistency; unused).

- n_categories:

  Optional number of categories for "Multinomial" (default: the largest
  observed category).

## Value

A numeric vector or matrix representing the linear predictor.

## Details

Applies the family's link function to the observed responses, with a
small adjustment so the link is finite at the boundary: logit with 0 and
1 replaced by 0.01 and 0.99 (Bernoulli, Binomial), and \\\log(Y + 0.1)\\
for the log-link families. The families match
[`LP_to_Y()`](https://yuchenxiao95.github.io/IChop/reference/LP_to_Y.md):

|  |  |  |
|----|----|----|
| **Family** | **Link** | **Extra arguments** |
| "Normal" | identity | `std` (default 1) |
| "Bernoulli" | logit |  |
| "Binomial" | logit | `n_trials` (required) |
| "Poisson" | log |  |
| "Gamma" | log (mean \\e^\eta\\) | `shape` (default 1) |
| "Exponential" | log |  |
| "InverseGaussian" | log | `shape` (\\\lambda\\, default 1) |
| "NegativeBinomial" | log | `theta` (size, default 1) |
| "Geometric" | log |  |
| "MultivariateNormal" | identity | `cov_matrix` (required) |
| "Multinomial" | baseline-category logit | `n_categories` (required) |
| "Cox" | null-model deviance residuals |  |

For "Cox", `Y` is an \\n \times 2\\ matrix with columns time and status
(1 = event, 0 = censored), as returned by `LP_to_Y(family = "Cox")`, or
a [`survival::Surv()`](https://rdrr.io/pkg/survival/man/Surv.html)
object. The result is the deviance residual of the null Cox model
(Therneau, Grambsch and Fleming, 1990): with the Nelson-Aalen cumulative
hazard \\\hat\Lambda_0\\, the martingale residual \\M_i = \delta_i -
\hat\Lambda_0(t_i)\\ is made more symmetric as \\D_i =
\mathrm{sign}(M_i)\sqrt{-2\[M_i + \delta_i \log(\delta_i - M_i)\]}\\. It
increases with the linear predictor and is meant for variable selection;
fit the selected model with
[`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html).

For "Multinomial", `Y` holds categories \\1, \dots, K\\ and the result
is an \\n \times (K-1)\\ matrix of baseline-category logits of the
smoothed one-hot vector \\(\mathrm{onehot}(Y_i) + 0.01)/(1 + 0.01K)\\.
For "Binomial", the logit is multiplied by `n_trials`. GLM stands for
*generalized linear model*, a common framework for regression models.

The underlying Julia logic is sourced from `'Y_to_LP.jl'`, and relies on
the 'JuliaCall' interface.

## References

Therneau, T. M., Grambsch, P. M. and Fleming, T. R. (1990).
Martingale-based residuals for survival models. *Biometrika*, 77(1),
147–160.
[doi:10.1093/biomet/77.1.147](https://doi.org/10.1093/biomet/77.1.147)

## Examples

``` r
if (FALSE) { # \dontrun{
if (interactive() && requireNamespace("JuliaCall", quietly = TRUE)) {
  julia_ready <- FALSE
  tryCatch({
    JuliaCall::julia_setup(installJulia = FALSE)
    julia_ready <- TRUE
  }, error = function(e) message("Julia setup failed"))

  if (julia_ready) {
    Y1 <- rbinom(100, 1, 0.5)
    eta1 <- Y_to_LP(Y1, family = "Bernoulli")

    Y2 <- rpois(100, lambda = 5)
    eta2 <- Y_to_LP(Y2, family = "Poisson")

    print(head(eta1))
    print(head(eta2))
  }
}
} # }
```
