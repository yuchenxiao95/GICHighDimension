#' @title Information Criterion Calculations via Julia
#'
#' @description
#' Compute information criteria using efficient 'Julia' backends via the 'JuliaCall' package.
#' These functions are useful for variable selection and model comparison in high-dimensional
#' settings. Each function returns either the computed criterion value, or a list with additional data.
#'
#' @details
#' This interface leverages 'Julia' to compute information-theoretic criteria efficiently,
#' especially when repeatedly evaluating models. Supported criteria include:
#' \itemize{
#'   \item AIC: Akaike Information Criterion
#'   \item AICc: Corrected AIC for small samples
#'   \item BIC / SIC: Bayesian (BIC) and a sqrt(n)-scaled variant (SIC) as implemented in Julia
#'   \item CAIC: Consistent AIC
#'   \item CAICF: CAIC with Fisher Information penalty
#'   \item GIC2–GIC6: Generalized Information Criterion family
#'   \item AttIC: Attention-weighted Information Criterion
#'   \item EBIC: Extended BIC (with \code{gamma})
#' }
#' Short versions (e.g., `Calculate_AIC_Short`) allow reusing a precomputed inverse matrix
#' to save computational time.
#'
#' Internally, the wrappers pass \code{P = ncol(X)} automatically. For all criteria except EBIC,
#' a dummy \code{gamma = 0.0} is sent to match the Julia signatures (it is ignored there).
#'
#' @param Y A numeric vector or matrix of responses (n × 1 or n × q).
#' @param X A numeric design matrix (n × p).
#' @param Inverse (Short variants only) a numeric matrix — the inverse of \eqn{X'X}.
#' @param huber Logical; if \code{TRUE}, use Huber loss for the variance estimate (default \code{FALSE}).
#' @param gamma EBIC tuning parameter (only used by EBIC wrappers). Default \code{0.5}.
#'
#' @return
#' For full versions: a named list with components:
#' \describe{
#'   \item{CriterionValue}{Numeric scalar of the selected information criterion.}
#'   \item{InverseMatrix}{Matrix inverse used in computation (if applicable).}
#' }
#' For `_Short` versions: a numeric scalar of the criterion value.
#'
#' @examples
#' \dontrun{
#' if (interactive() && requireNamespace("JuliaCall", quietly = TRUE)) {
#'   JuliaCall::julia_setup(installJulia = FALSE)
#'   X <- matrix(rnorm(20*5), ncol = 5)
#'   Y <- rnorm(20)
#'   out <- Calculate_AIC(Y, X)
#'   print(out$CriterionValue)
#' }
#' }
#'
#' @name information_criteria
NULL

.pkg_env <- new.env(parent = emptyenv())

julia_setup_once <- function() {
  if (!exists("julia_setup_done", envir = .pkg_env)) {
    if (!requireNamespace("JuliaCall", quietly = TRUE)) {
      stop("Package 'JuliaCall' is required. Install with install.packages('JuliaCall')")
    }
    JuliaCall::julia_setup()
    # Make sure the Julia file that defines Calculate_* is sourced:
    # Adjust the file name below to whatever actually contains those definitions.
    # (kept as in your script; change if your functions live elsewhere)
    JuliaCall::julia_source(system.file("julia", "penalty_selection.jl",
                                        package = "GICHighDimension"))
    assign("julia_setup_done", TRUE, envir = .pkg_env)
  }
}

# -- helpers -------------------------------------------------------------------

.j_to_arg_Y <- function(Y) {
  if (is.matrix(Y)) as.matrix(Y) else as.vector(Y)
}

.safe_call_tuple <- function(fun, ...) {
  # many Julia funcs return (criterion, inverse)
  res <- JuliaCall::julia_call(fun, ...)
  # JuliaCall maps tuples to lists; keep that behavior:
  list(CriterionValue = res[[1]], InverseMatrix = res[[2]])
}

# Full versions expect: (Y, X, P::Integer, gamma::Float64, Huber::Bool)
.Calculate_Criterion <- function(func_name, Y, X, gamma = 0.0, huber = FALSE) {
  julia_setup_once()
  Xmat <- as.matrix(X)
  P    <- as.integer(ncol(Xmat))
  Yarg <- .j_to_arg_Y(Y)

  .safe_call_tuple(
    func_name,
    Yarg,
    Xmat,
    P,
    as.numeric(gamma),
    as.logical(huber)
  )
}

# Short versions expect: (Y, X, Inverse, P::Integer, gamma::Float64, Huber::Bool)
.Calculate_Criterion_Short <- function(func_name, Y, X, Inverse, gamma = 0.0, huber = FALSE) {
  julia_setup_once()
  Xmat <- as.matrix(X)
  Inv  <- as.matrix(Inverse)
  P    <- as.integer(ncol(Xmat))
  Yarg <- .j_to_arg_Y(Y)

  JuliaCall::julia_call(
    paste0(func_name, "_short"),
    Yarg,
    Xmat,
    Inv,
    P,
    as.numeric(gamma),
    as.logical(huber)
  )
}

# ---- Full versions (huber exposed; gamma only meaningful for EBIC) -----------

#' @rdname information_criteria
#' @export
Calculate_AIC   <- function(Y, X, huber = FALSE)
  .Calculate_Criterion("Calculate_AIC",   Y, X, gamma = 0.0, huber = huber)

#' @rdname information_criteria
#' @export
Calculate_AICc  <- function(Y, X, huber = FALSE)
  .Calculate_Criterion("Calculate_AIC_c", Y, X, gamma = 0.0, huber = huber)

#' @rdname information_criteria
#' @export
Calculate_AttIC <- function(Y, X, huber = FALSE)
  .Calculate_Criterion("Calculate_AttIC", Y, X, gamma = 0.0, huber = huber)

#' @rdname information_criteria
#' @export
Calculate_SIC   <- function(Y, X, huber = FALSE)
  .Calculate_Criterion("Calculate_SIC",   Y, X, gamma = 0.0, huber = huber)

#' @rdname information_criteria
#' @export
Calculate_BIC   <- function(Y, X, huber = FALSE)
  .Calculate_Criterion("Calculate_BIC",   Y, X, gamma = 0.0, huber = huber)

#' @rdname information_criteria
#' @export
Calculate_CAIC  <- function(Y, X, huber = FALSE)
  .Calculate_Criterion("Calculate_CAIC",  Y, X, gamma = 0.0, huber = huber)

#' @rdname information_criteria
#' @export
Calculate_CAICF <- function(Y, X, huber = FALSE)
  .Calculate_Criterion("Calculate_CAICF", Y, X, gamma = 0.0, huber = huber)

#' @rdname information_criteria
#' @export
Calculate_GIC2  <- function(Y, X, huber = FALSE)
  .Calculate_Criterion("Calculate_GIC2",  Y, X, gamma = 0.0, huber = huber)

#' @rdname information_criteria
#' @export
Calculate_GIC3  <- function(Y, X, huber = FALSE)
  .Calculate_Criterion("Calculate_GIC3",  Y, X, gamma = 0.0, huber = huber)

#' @rdname information_criteria
#' @export
Calculate_GIC4  <- function(Y, X, huber = FALSE)
  .Calculate_Criterion("Calculate_GIC4",  Y, X, gamma = 0.0, huber = huber)

#' @rdname information_criteria
#' @export
Calculate_GIC5  <- function(Y, X, huber = FALSE)
  .Calculate_Criterion("Calculate_GIC5",  Y, X, gamma = 0.0, huber = huber)

#' @rdname information_criteria
#' @export
Calculate_GIC6  <- function(Y, X, huber = FALSE)
  .Calculate_Criterion("Calculate_GIC6",  Y, X, gamma = 0.0, huber = huber)

# ---- EBIC (full) with gamma --------------------------------------------------

#' @rdname information_criteria
#' @export
Calculate_EBIC  <- function(Y, X, gamma = 0.5, huber = FALSE)
  .Calculate_Criterion("Calculate_EBIC",  Y, X, gamma = gamma, huber = huber)

# ---- Short versions ----------------------------------------------------------

#' @rdname information_criteria
#' @export
Calculate_AIC_Short   <- function(Y, X, Inverse, huber = FALSE)
  .Calculate_Criterion_Short("Calculate_AIC",   Y, X, Inverse, gamma = 0.0, huber = huber)

#' @rdname information_criteria
#' @export
Calculate_AICc_Short  <- function(Y, X, Inverse, huber = FALSE)
  .Calculate_Criterion_Short("Calculate_AIC_c", Y, X, Inverse, gamma = 0.0, huber = huber)

#' @rdname information_criteria
#' @export
Calculate_AttIC_Short <- function(Y, X, Inverse, huber = FALSE)
  .Calculate_Criterion_Short("Calculate_AttIC", Y, X, Inverse, gamma = 0.0, huber = huber)

#' @rdname information_criteria
#' @export
Calculate_SIC_Short   <- function(Y, X, Inverse, huber = FALSE)
  .Calculate_Criterion_Short("Calculate_SIC",   Y, X, Inverse, gamma = 0.0, huber = huber)

#' @rdname information_criteria
#' @export
Calculate_BIC_Short   <- function(Y, X, Inverse, huber = FALSE)
  .Calculate_Criterion_Short("Calculate_BIC",   Y, X, Inverse, gamma = 0.0, huber = huber)

#' @rdname information_criteria
#' @export
Calculate_CAIC_Short  <- function(Y, X, Inverse, huber = FALSE)
  .Calculate_Criterion_Short("Calculate_CAIC",  Y, X, Inverse, gamma = 0.0, huber = huber)

#' @rdname information_criteria
#' @export
Calculate_CAICF_Short <- function(Y, X, Inverse, huber = FALSE)
  .Calculate_Criterion_Short("Calculate_CAICF", Y, X, Inverse, gamma = 0.0, huber = huber)

#' @rdname information_criteria
#' @export
Calculate_GIC2_Short  <- function(Y, X, Inverse, huber = FALSE)
  .Calculate_Criterion_Short("Calculate_GIC2",  Y, X, Inverse, gamma = 0.0, huber = huber)

#' @rdname information_criteria
#' @export
Calculate_GIC3_Short  <- function(Y, X, Inverse, huber = FALSE)
  .Calculate_Criterion_Short("Calculate_GIC3",  Y, X, Inverse, gamma = 0.0, huber = huber)

#' @rdname information_criteria
#' @export
Calculate_GIC4_Short  <- function(Y, X, Inverse, huber = FALSE)
  .Calculate_Criterion_Short("Calculate_GIC4",  Y, X, Inverse, gamma = 0.0, huber = huber)

#' @rdname information_criteria
#' @export
Calculate_GIC5_Short  <- function(Y, X, Inverse, huber = FALSE)
  .Calculate_Criterion_Short("Calculate_GIC5",  Y, X, Inverse, gamma = 0.0, huber = huber)

#' @rdname information_criteria
#' @export
Calculate_GIC6_Short  <- function(Y, X, Inverse, huber = FALSE)
  .Calculate_Criterion_Short("Calculate_GIC6",  Y, X, Inverse, gamma = 0.0, huber = huber)

# ---- EBIC (short) with gamma -------------------------------------------------

#' @rdname information_criteria
#' @export
Calculate_EBIC_Short <- function(Y, X, Inverse, gamma = 0.5, huber = FALSE)
  .Calculate_Criterion_Short("Calculate_EBIC",  Y, X, Inverse, gamma = gamma, huber = huber)
