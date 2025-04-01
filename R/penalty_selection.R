#' Information Criteria Calculations via Julia
#'
#' @description
#' These functions calculate various information criteria using optimized Julia implementations.
#' Each function returns a list containing the criterion value and inverse matrix.
#'
#' @param Y Outcome vector or matrix (numeric).
#' @param X Design matrix (numeric).
#' @return A list containing:
#' \itemize{
#'   \item The calculated criterion value
#'   \item The inverse matrix (X'X)^{-1}
#' }
#'
#' @name information_criteria
NULL

# Helper function for Julia setup
julia_setup_once <- function() {
  if (!exists(".julia_setup_done", envir = .GlobalEnv)) {
    if (!requireNamespace("JuliaCall", quietly = TRUE)) {
      stop("JuliaCall package required. Install with install.packages('JuliaCall')")
    }
    JuliaCall::julia_setup()
    JuliaCall::julia_source(system.file("julia", "penalty_selection.jl",
                                        package = "GICHighDimension"))
    assign(".julia_setup_done", TRUE, envir = .GlobalEnv)
  }
}

# Generic calculation function
.calculate_criterion <- function(func_name, Y, X) {
  julia_setup_once()
  result <- JuliaCall::julia_call(func_name, as.vector(Y), as.matrix(X))
  names(result) <- c(toupper(func_name), "Inverse")
  result
}

#' @rdname information_criteria
#' @export
calculate_aic <- function(Y, X) .calculate_criterion("Calculate_AIC", Y, X)

#' @rdname information_criteria
#' @export
calculate_aicc <- function(Y, X) .calculate_criterion("Calculate_AIC_c", Y, X)

#' @rdname information_criteria
#' @export
calculate_att <- function(Y, X) .calculate_criterion("Calculate_AttIC", Y, X)

#' @rdname information_criteria
#' @export
calculate_sic <- function(Y, X) .calculate_criterion("Calculate_SIC", Y, X)

#' @rdname information_criteria
#' @export
calculate_bic <- function(Y, X) .calculate_criterion("Calculate_BIC", Y, X)

#' @rdname information_criteria
#' @export
calculate_caic <- function(Y, X) .calculate_criterion("Calculate_CAIC", Y, X)

#' @rdname information_criteria
#' @export
calculate_caicf <- function(Y, X) .calculate_criterion("Calculate_CAICF", Y, X)

#' @rdname information_criteria
#' @export
calculate_gic2 <- function(Y, X) .calculate_criterion("Calculate_GIC2", Y, X)

#' @rdname information_criteria
#' @export
calculate_gic3 <- function(Y, X) .calculate_criterion("Calculate_GIC3", Y, X)

#' @rdname information_criteria
#' @export
calculate_gic4 <- function(Y, X) .calculate_criterion("Calculate_GIC4", Y, X)

#' @rdname information_criteria
#' @export
calculate_gic5 <- function(Y, X) .calculate_criterion("Calculate_GIC5", Y, X)

#' @rdname information_criteria
#' @export
calculate_gic6 <- function(Y, X) .calculate_criterion("Calculate_GIC6", Y, X)

# Short versions (when inverse matrix is already available)
.calculate_criterion_short <- function(func_name, Y, X, Inverse) {
  julia_setup_once()
  JuliaCall::julia_call(paste0(func_name, "_short"),
                        as.vector(Y), as.matrix(X), as.matrix(Inverse))
}

#' @rdname information_criteria
#' @export
calculate_aic_short <- function(Y, X, Inverse) .calculate_criterion_short("Calculate_AIC", Y, X, Inverse)

#' @rdname information_criteria
#' @export
calculate_aicc_short <- function(Y, X, Inverse) .calculate_criterion_short("Calculate_AIC_c", Y, X, Inverse)

#' @rdname information_criteria
#' @export
calculate_att_short <- function(Y, X, Inverse) .calculate_criterion_short("Calculate_AttIC", Y, X, Inverse)

#' @rdname information_criteria
#' @export
calculate_sic_short <- function(Y, X, Inverse) .calculate_criterion_short("Calculate_SIC", Y, X, Inverse)

#' @rdname information_criteria
#' @export
calculate_bic_short <- function(Y, X, Inverse) .calculate_criterion_short("Calculate_BIC", Y, X, Inverse)

#' @rdname information_criteria
#' @export
calculate_caic_short <- function(Y, X, Inverse) .calculate_criterion_short("Calculate_CAIC", Y, X, Inverse)

#' @rdname information_criteria
#' @export
calculate_caicf_short <- function(Y, X, Inverse) .calculate_criterion_short("Calculate_CAICF", Y, X, Inverse)

#' @rdname information_criteria
#' @export
calculate_gic2_short <- function(Y, X, Inverse) .calculate_criterion_short("Calculate_GIC2", Y, X, Inverse)

#' @rdname information_criteria
#' @export
calculate_gic3_short <- function(Y, X, Inverse) .calculate_criterion_short("Calculate_GIC3", Y, X, Inverse)

#' @rdname information_criteria
#' @export
calculate_gic4_short <- function(Y, X, Inverse) .calculate_criterion_short("Calculate_GIC4", Y, X, Inverse)

#' @rdname information_criteria
#' @export
calculate_gic5_short <- function(Y, X, Inverse) .calculate_criterion_short("Calculate_GIC5", Y, X, Inverse)

#' @rdname information_criteria
#' @export
calculate_gic6_short <- function(Y, X, Inverse) .calculate_criterion_short("Calculate_GIC6", Y, X, Inverse)
