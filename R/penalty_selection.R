#' @title Information Criteria Calculations via Julia
#' @description
#' These functions calculate various information criteria using optimized Julia implementations.
#' Each function returns a list containing the criterion value and inverse matrix.
#' @param Y Outcome vector or matrix (numeric)
#' @param X Design matrix (numeric)
#' @param Inverse Pre-computed inverse matrix for short versions (numeric matrix)
#' @return A list with components:
#' \item{CriterionValue}{Numeric value of the calculated criterion.}
#' \item{InverseMatrix}{Matrix inverse (not returned by _Short versions).}
#' @details
#' The functions provide R interfaces to optimized Julia implementations of:
#' \itemize{
#'   \item Akaike Information Criterion (AIC)
#'   \item Corrected AIC (AICc)
#'   \item Schwarz Information Criterion (SIC/BIC)
#'   \item Various Generalized IC versions (GIC2-GIC6)
#' }
#'
#' The "_Short" versions use pre-computed inverse matrices.
#'
#' @examples
#' \donttest{
#' if (requireNamespace("JuliaCall", quietly = TRUE)) {
#'   X <- matrix(rnorm(100), ncol = 5)
#'   Y <- rnorm(20)
#'   aic_result <- Calculate_AIC(Y, X)
#' }
#' }
#'
#' @name information_criteria
NULL

# Package environment for Julia setup flag
.pkg_env <- new.env(parent = emptyenv())

# Helper function for Julia setup
julia_setup_once <- function() {
  if (!exists("julia_setup_done", envir = .pkg_env)) {
    if (!requireNamespace("JuliaCall", quietly = TRUE)) {
      stop("JuliaCall package required. Install with install.packages('JuliaCall')")
    }
    JuliaCall::julia_setup()
    JuliaCall::julia_source(system.file("julia", "penalty_selection.jl", package = "GICHighDimension"))
    assign("julia_setup_done", TRUE, envir = .pkg_env)
  }
}

# Generic calculation function
.Calculate_Criterion <- function(func_name, Y, X) {
  julia_setup_once()
  result <- JuliaCall::julia_call(func_name, as.vector(Y), as.matrix(X))
  names(result) <- c(func_name, "Inverse")
  result
}

# Exported full versions
#' @rdname information_criteria
#' @export
Calculate_AIC <- function(Y, X) .Calculate_Criterion("Calculate_AIC", Y, X)

#' @rdname information_criteria
#' @export
Calculate_AICc <- function(Y, X) .Calculate_Criterion("Calculate_AIC_c", Y, X)

#' @rdname information_criteria
#' @export
Calculate_AttIC <- function(Y, X) .Calculate_Criterion("Calculate_AttIC", Y, X)

#' @rdname information_criteria
#' @export
Calculate_SIC <- function(Y, X) .Calculate_Criterion("Calculate_SIC", Y, X)

#' @rdname information_criteria
#' @export
Calculate_BIC <- function(Y, X) .Calculate_Criterion("Calculate_BIC", Y, X)

#' @rdname information_criteria
#' @export
Calculate_CAIC <- function(Y, X) .Calculate_Criterion("Calculate_CAIC", Y, X)

#' @rdname information_criteria
#' @export
Calculate_CAICF <- function(Y, X) .Calculate_Criterion("Calculate_CAICF", Y, X)

#' @rdname information_criteria
#' @export
Calculate_GIC2 <- function(Y, X) .Calculate_Criterion("Calculate_GIC2", Y, X)

#' @rdname information_criteria
#' @export
Calculate_GIC3 <- function(Y, X) .Calculate_Criterion("Calculate_GIC3", Y, X)

#' @rdname information_criteria
#' @export
Calculate_GIC4 <- function(Y, X) .Calculate_Criterion("Calculate_GIC4", Y, X)

#' @rdname information_criteria
#' @export
Calculate_GIC5 <- function(Y, X) .Calculate_Criterion("Calculate_GIC5", Y, X)

#' @rdname information_criteria
#' @export
Calculate_GIC6 <- function(Y, X) .Calculate_Criterion("Calculate_GIC6", Y, X)

# Short versions (with inverse matrix)
.Calculate_Criterion_Short <- function(func_name, Y, X, Inverse) {
  julia_setup_once()
  JuliaCall::julia_call(
    paste0(func_name, "_short"),
    as.vector(Y),
    as.matrix(X),
    as.matrix(Inverse)
  )
}

#' @rdname information_criteria
#' @export
Calculate_AIC_Short <- function(Y, X, Inverse) {
  .Calculate_Criterion_Short("Calculate_AIC", Y, X, Inverse)
}

#' @rdname information_criteria
#' @export
Calculate_AICc_Short <- function(Y, X, Inverse) {
  .Calculate_Criterion_Short("Calculate_AIC_c", Y, X, Inverse)
}

#' @rdname information_criteria
#' @export
Calculate_AttIC_Short <- function(Y, X, Inverse) {
  .Calculate_Criterion_Short("Calculate_AttIC", Y, X, Inverse)
}

#' @rdname information_criteria
#' @export
Calculate_SIC_Short <- function(Y, X, Inverse) {
  .Calculate_Criterion_Short("Calculate_SIC", Y, X, Inverse)
}

#' @rdname information_criteria
#' @export
Calculate_BIC_Short <- function(Y, X, Inverse) {
  .Calculate_Criterion_Short("Calculate_BIC", Y, X, Inverse)
}

#' @rdname information_criteria
#' @export
Calculate_CAIC_Short <- function(Y, X, Inverse) {
  .Calculate_Criterion_Short("Calculate_CAIC", Y, X, Inverse)
}

#' @rdname information_criteria
#' @export
Calculate_CAICF_Short <- function(Y, X, Inverse) {
  .Calculate_Criterion_Short("Calculate_CAICF", Y, X, Inverse)
}

#' @rdname information_criteria
#' @export
Calculate_GIC2_Short <- function(Y, X, Inverse) {
  .Calculate_Criterion_Short("Calculate_GIC2", Y, X, Inverse)
}

#' @rdname information_criteria
#' @export
Calculate_GIC3_Short <- function(Y, X, Inverse) {
  .Calculate_Criterion_Short("Calculate_GIC3", Y, X, Inverse)
}

#' @rdname information_criteria
#' @export
Calculate_GIC4_Short <- function(Y, X, Inverse) {
  .Calculate_Criterion_Short("Calculate_GIC4", Y, X, Inverse)
}

#' @rdname information_criteria
#' @export
Calculate_GIC5_Short <- function(Y, X, Inverse) {
  .Calculate_Criterion_Short("Calculate_GIC5", Y, X, Inverse)
}

#' @rdname information_criteria
#' @export
Calculate_GIC6_Short <- function(Y, X, Inverse) {
  .Calculate_Criterion_Short("Calculate_GIC6", Y, X, Inverse)
}
