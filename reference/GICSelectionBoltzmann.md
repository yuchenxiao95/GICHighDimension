# GIC-Based Variable Selection Using Boltzmann Machine

Performs variable selection using the Generalized Information Criterion
('GIC') combined with a Boltzmann-like simulated annealing approach. The
optimization logic is implemented in 'Julia' and accessed from 'R'
through the 'JuliaCall' interface.

## Usage

``` r
GICSelectionBoltzmann(
  X,
  Y,
  Init_Columns,
  Calculate_GIC,
  Calculate_GIC_short,
  T = 0.1,
  Nsim = 5,
  k = NULL,
  gamma = 0.6
)
```

## Arguments

- X:

  A numeric design matrix with `n` rows (observations) and `p` columns
  (predictors).

- Y:

  A numeric response vector (length `n`) or an `n x q` matrix.

- Init_Columns:

  Integer vector of indices specifying the initial set of predictors.

- Calculate_GIC:

  Character string giving the name of the 'Julia' function to compute
  full GIC values.

- Calculate_GIC_short:

  Character string giving the name of the 'Julia' function to compute
  approximate GIC values.

- T:

  Numeric scalar. The temperature parameter controlling stochasticity in
  the simulated annealing process (default: 0.1).

- Nsim:

  Integer. Number of passes over the (once-shuffled) variables (default:
  5).

- k:

  Optional integer model-size cap, as in
  [`GICSelection()`](https://yuchenxiao95.github.io/IChop/reference/GICSelection.md);
  `NULL` (default) means no cap.

- gamma:

  EBIC tuning constant (default 0.6); ignored by other criteria.

## Value

A list with the following components:

- `GIC_list`:

  List of GIC values returned for each run.

- `GIC_coeff`:

  Indices of the variables selected in the final run.

## Details

This method uses temperature-based control to introduce stochasticity in
model exploration. It supports custom GIC formulations by passing the
names of 'Julia' functions and works with both univariate and
multivariate responses.

## Examples

``` r
if (FALSE) { # \dontrun{
if (requireNamespace("JuliaCall", quietly = TRUE)) {
  julia_available <- FALSE
  tryCatch({
    JuliaCall::julia_setup(installJulia = FALSE)
    julia_available <- TRUE
  }, error = function(e) {
    message("Julia not available: ", e$message)
  })

  if (julia_available) {
    set.seed(123)
    n <- 100; p <- 10; k <- 3
    X <- matrix(rnorm(n * p), n, p)
    beta <- c(rep(1.5, k), rep(0, p - k))
    Y <- X %*% beta + rnorm(n)

    result <- GICSelectionBoltzmann(
      X = X,
      Y = Y,
      Init_Columns = 1:p,
      Calculate_GIC = "Calculate_SIC",
      Calculate_GIC_short = "Calculate_SIC_short",
      Nsim = 3
    )
    print(result$GIC_coeff)
  }
}
} # }
```
