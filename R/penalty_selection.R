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
#'   \item BIC / SIC: Bayesian or Schwarz Information Criterion
#'   \item CAIC: Consistent AIC
#'   \item CAICF: Consistent Akaike's Information Criterion with Fisher Information
#'   \item EBIC: Extended BIC (depends on the candidate pool size \(P\))
#'   \item GIC2–GIC6: Generalized Information Criterion family (depend on \(P\))
#'   \item AttIC: Attention-weighted Information Criterion
#' }
#' Short versions (e.g., `Calculate_AIC_Short`) allow reusing a precomputed inverse matrix
#' to save computational time.
#'
#' @param Y A numeric vector or matrix of responses.
#' @param X A numeric design matrix.
#' @param Inverse (for `_Short` variants only) a numeric matrix — the inverse of X'X.
#' @param P (only for EBIC and GIC2–GIC6) integer total number of candidate predictors.
#'   Defaults to `ncol(X)`.
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
#'   X <- matrix(rnorm(100), ncol = 5)
#'   Y <- rnorm(20)
#'   out1 <- Calculate_AIC(Y, X)
#'   out2 <- Calculate_EBIC(Y, X)        # uses P = ncol(X) by default
#'   val_short <- Calculate_EBIC_Short(Y, X, solve(crossprod(X)))
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
    # Source all Julia files we rely on
    jl_dir <- system.file("julia", package = "GICHighDimension")
    if (jl_dir == "" || !dir.exists(jl_dir)) {
      stop("Julia scripts directory not found in the package (inst/julia).")
    }
    # load core files (order can matter)
    for (f in c("penalty_selection.jl",
                "GIC_Calculation.jl",
                "GIC_Model_Selection.jl")) {
      fp <- file.path(jl_dir, f)
      if (file.exists(fp)) JuliaCall::julia_source(fp)
    }
    assign("julia_setup_done", TRUE, envir = .pkg_env)
  }
}

# ---------------- Helpers ----------------

# Full versions: criteria that DO NOT depend on P
.Calculate_Criterion_NO_P <- function(func_name, Y, X) {
  julia_setup_once()
  res <- JuliaCall::julia_call(func_name, as.vector(Y), as.matrix(X))
  list(CriterionValue = res[[1]], InverseMatrix = res[[2]])
}

# Full versions: criteria that DO depend on P
.Calculate_Criterion_WITH_P <- function(func_name, Y, X, P = ncol(X)) {
  julia_setup_once()
  res <- JuliaCall::julia_call(func_name, as.vector(Y), as.matrix(X), as.integer(P))
  list(CriterionValue = res[[1]], InverseMatrix = res[[2]])
}

# Short versions: criteria that DO NOT depend on P
.Calculate_Criterion_Short_NO_P <- function(func_name, Y, X, Inverse) {
  julia_setup_once()
  JuliaCall::julia_call(paste0(func_name, "_short"),
                        as.vector(Y),
                        as.matrix(X),
                        as.matrix(Inverse))
}

# Short versions: criteria that DO depend on P
.Calculate_Criterion_Short_WITH_P <- function(func_name, Y, X, Inverse, P = ncol(X)) {
  julia_setup_once()
  JuliaCall::julia_call(paste0(func_name, "_short"),
                        as.vector(Y),
                        as.matrix(X),
                        as.matrix(Inverse),
                        as.integer(P))
}

# ---------------- Full: NO-P criteria ----------------
#' @rdname information_criteria
#' @export
Calculate_AIC   <- function(Y, X) .Calculate_Criterion_NO_P("Calculate_AIC",   Y, X)
#' @rdname information_criteria
#' @export
Calculate_AICc  <- function(Y, X) .Calculate_Criterion_NO_P("Calculate_AIC_c", Y, X)
#' @rdname information_criteria
#' @export
Calculate_AttIC <- function(Y, X) .Calculate_Criterion_NO_P("Calculate_AttIC", Y, X)
#' @rdname information_criteria
#' @export
Calculate_SIC   <- function(Y, X) .Calculate_Criterion_NO_P("Calculate_SIC",   Y, X)
#' @rdname information_criteria
#' @export
Calculate_BIC   <- function(Y, X) .Calculate_Criterion_NO_P("Calculate_BIC",   Y, X)
#' @rdname information_criteria
#' @export
Calculate_CAIC  <- function(Y, X) .Calculate_Criterion_NO_P("Calculate_CAIC",  Y, X)
#' @rdname information_criteria
#' @export
Calculate_CAICF <- function(Y, X) .Calculate_Criterion_NO_P("Calculate_CAICF", Y, X)

# ---------------- Full: WITH-P criteria ----------------
#' @rdname information_criteria
#' @export
Calculate_EBIC  <- function(Y, X, P = ncol(X)) .Calculate_Criterion_WITH_P("Calculate_EBIC", Y, X, P)
#' @rdname information_criteria
#' @export
Calculate_GIC2  <- function(Y, X, P = ncol(X)) .Calculate_Criterion_WITH_P("Calculate_GIC2", Y, X, P)
#' @rdname information_criteria
#' @export
Calculate_GIC3  <- function(Y, X, P = ncol(X)) .Calculate_Criterion_WITH_P("Calculate_GIC3", Y, X, P)
#' @rdname information_criteria
#' @export
Calculate_GIC4  <- function(Y, X, P = ncol(X)) .Calculate_Criterion_WITH_P("Calculate_GIC4", Y, X, P)
#' @rdname information_criteria
#' @export
Calculate_GIC5  <- function(Y, X, P = ncol(X)) .Calculate_Criterion_WITH_P("Calculate_GIC5", Y, X, P)
#' @rdname information_criteria
#' @export
Calculate_GIC6  <- function(Y, X, P = ncol(X)) .Calculate_Criterion_WITH_P("Calculate_GIC6", Y, X, P)

# ---------------- Short: NO-P criteria ----------------
#' @rdname information_criteria
#' @export
Calculate_AIC_Short   <- function(Y, X, Inverse) .Calculate_Criterion_Short_NO_P("Calculate_AIC",   Y, X, Inverse)
#' @rdname information_criteria
#' @export
Calculate_AICc_Short  <- function(Y, X, Inverse) .Calculate_Criterion_Short_NO_P("Calculate_AIC_c", Y, X, Inverse)
#' @rdname information_criteria
#' @export
Calculate_AttIC_Short <- function(Y, X, Inverse) .Calculate_Criterion_Short_NO_P("Calculate_AttIC", Y, X, Inverse)
#' @rdname information_criteria
#' @export
Calculate_SIC_Short   <- function(Y, X, Inverse) .Calculate_Criterion_Short_NO_P("Calculate_SIC",   Y, X, Inverse)
#' @rdname information_criteria
#' @export
Calculate_BIC_Short   <- function(Y, X, Inverse) .Calculate_Criterion_Short_NO_P("Calculate_BIC",   Y, X, Inverse)
#' @rdname information_criteria
#' @export
Calculate_CAIC_Short  <- function(Y, X, Inverse) .Calculate_Criterion_Short_NO_P("Calculate_CAIC",  Y, X, Inverse)
#' @rdname information_criteria
#' @export
Calculate_CAICF_Short <- function(Y, X, Inverse) .Calculate_Criterion_Short_NO_P("Calculate_CAICF", Y, X, Inverse)

# ---------------- Short: WITH-P criteria ----------------
#' @rdname information_criteria
#' @export
Calculate_EBIC_Short <- function(Y, X, Inverse, P = ncol(X)) .Calculate_Criterion_Short_WITH_P("Calculate_EBIC", Y, X, Inverse, P)
#' @rdname information_criteria
#' @export
Calculate_GIC2_Short <- function(Y, X, Inverse, P = ncol(X)) .Calculate_Criterion_Short_WITH_P("Calculate_GIC2", Y, X, Inverse, P)
#' @rdname information_criteria
#' @export
Calculate_GIC3_Short <- function(Y, X, Inverse, P = ncol(X)) .Calculate_Criterion_Short_WITH_P("Calculate_GIC3", Y, X, Inverse, P)
#' @rdname information_criteria
#' @export
Calculate_GIC4_Short <- function(Y, X, Inverse, P = ncol(X)) .Calculate_Criterion_Short_WITH_P("Calculate_GIC4", Y, X, Inverse, P)
#' @rdname information_criteria
#' @export
Calculate_GIC5_Short <- function(Y, X, Inverse, P = ncol(X)) .Calculate_Criterion_Short_WITH_P("Calculate_GIC5", Y, X, Inverse, P)
#' @rdname information_criteria
#' @export
Calculate_GIC6_Short <- function(Y, X, Inverse, P = ncol(X)) .Calculate_Criterion_Short_WITH_P("Calculate_GIC6", Y, X, Inverse, P)
