# =====================================================================
#  Beta_estimate.R  –  R wrapper around inst/julia/Beta_estimate.jl
#  Part of the GICHighDimension package
# =====================================================================

#' @title Coefficient Estimation via Julia
#'
#' @description
#' Calls the Julia function **`Beta_estimate`** to obtain coefficient
#' estimates for OLS, GLM, multivariate-normal, or multinomial models.
#' When \code{family = "auto"} Julia chooses the estimator
#' automatically.
#'
#' @param X Numeric design matrix (\eqn{n \times p}).
#' @param Y Numeric vector (\eqn{n}) or matrix (\eqn{n \times q}) of
#'   responses.  A one-column matrix is automatically converted to a
#'   vector.
#' @param family Character/symbol; default \code{"auto"}.
#' @param link   Character/symbol/Julia \code{Link}; default \code{"auto"}.
#' @param n_trials Integer trials for a binomial response (default
#'   \code{NULL}).
#' @param add_intercept Logical.  If \code{TRUE} (default) the wrapper
#'   asks Julia to prepend a column of 1s and **keeps the intercept
#'   coefficient** in the returned vector/matrix.  Set to \code{FALSE}
#'   if \code{X} already contains an intercept column.
#'
#' @return Numeric vector or matrix of coefficients
#'   (intercept first if \code{add_intercept = TRUE}).
#' @export
#'
#' @importFrom JuliaCall julia_setup julia_source julia_call julia_assign
#'                       julia_eval julia_exists julia_install_package_if_needed
Beta_estimate <- function(X, Y,
                          family        = "auto",
                          link          = "auto",
                          n_trials      = NULL,
                          add_intercept = TRUE) {

  # ── 1. Validate inputs ──────────────────────────────────────────────
  if (!is.matrix(X))
    stop("`X` must be a numeric matrix.", call. = FALSE)

  # turn n×1 matrix into vector to appease DataFrames.jl
  if (is.matrix(Y) && ncol(Y) == 1L)
    Y <- drop(Y)

  if (!(is.numeric(Y) && (is.vector(Y) || is.matrix(Y))))
    stop("`Y` must be a numeric vector or matrix.", call. = FALSE)

  if (nrow(X) != nrow(as.matrix(Y)))
    stop("`X` and `Y` must have the same number of rows.", call. = FALSE)

  if (!requireNamespace("JuliaCall", quietly = TRUE))
    stop('Package "JuliaCall" is required. Install with install.packages("JuliaCall").',
         call. = FALSE)

  # ── 2. Start or attach to Julia ─────────────────────────────────────
  JuliaCall::julia_setup(installJulia = FALSE)

  # Ensure required Julia packages are present
  for (pkg in c("DataFrames", "StatsModels", "GLM"))
    JuliaCall::julia_install_package_if_needed(pkg)

  # ── 3. Source Beta_estimate.jl (idempotent) ─────────────────────────
  script_dir <- system.file("julia", package = "GICHighDimension")
  if (length(script_dir) == 0L || script_dir == "")
    stop("inst/julia/ directory not found in GICHighDimension.", call. = FALSE)

  JuliaCall::julia_source(file.path(script_dir[1L], "Beta_estimate.jl"))

  if (!JuliaCall::julia_exists("Beta_estimate"))
    stop("Julia function `Beta_estimate` not found after sourcing.", call. = FALSE)

  # ── 4. Push data into Julia ─────────────────────────────────────────
  JuliaCall::julia_assign("X_r", X)
  JuliaCall::julia_assign("Y_r", Y)

  X_jl <- JuliaCall::julia_eval("convert(Matrix{Float64}, X_r)")
  Y_jl <- if (is.matrix(Y)) {
    JuliaCall::julia_eval("convert(Matrix{Float64}, Y_r)")
  } else {
    JuliaCall::julia_eval("convert(Vector{Float64}, Y_r)")
  }

  n_trials_jl <- if (is.null(n_trials)) JuliaCall::julia_eval("nothing") else as.integer(n_trials)

  # ── 5. Convert family / link to Julia Symbols ───────────────────────
  to_symbol <- function(x)
    JuliaCall::julia_eval(sprintf("Symbol(\"%s\")", x))

  family_jl <- if (identical(family, "auto")) {
    JuliaCall::julia_eval(":auto")
  } else if (is.character(family)) {
    to_symbol(family)
  } else {
    family
  }

  link_jl <- if (identical(link, "auto")) {
    JuliaCall::julia_eval(":auto")
  } else if (is.character(link)) {
    to_symbol(link)
  } else {
    link
  }

  # ── 6. Call Julia & return result ───────────────────────────────────
  JuliaCall::julia_call(
    "Beta_estimate",
    Y_jl, X_jl,
    family        = family_jl,
    link          = link_jl,
    n_trials      = n_trials_jl,
    add_intercept = isTRUE(add_intercept)
  )
}
