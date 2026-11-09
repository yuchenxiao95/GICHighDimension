#' @title Information Criterion Calculations via Julia
#'
#' @description
#' Compute information criteria using efficient 'Julia' backends via the 'JuliaCall' package.
#' These functions are useful for variable selection and model comparison in high-dimensional
#' settings. Each function returns either the computed criterion value, or a list with additional data.
#'
#' @details
#' Every criterion has the form \eqn{Y^\top H Y / n - p(n, K)}, where \eqn{H} is the hat
#' matrix of the \eqn{K} selected columns and \eqn{p(n, K)} is the penalty (Xiao and
#' Walker, 2026). With \eqn{\hat\sigma^2 = \mathrm{RSS}/(n - K)} and \eqn{P} the number
#' of candidate predictors, the supported criteria are:
#' \itemize{
#'   \item AIC, Akaike information criterion (Akaike, 1974):
#'     \eqn{2K\hat\sigma^2/n}.
#'   \item AICc, AIC corrected for small samples (Hurvich and Tsai, 1989):
#'     \eqn{(K+1)\hat\sigma^2/(n-K-2)}.
#'   \item BIC, Bayesian information criterion (Schwarz, 1978):
#'     \eqn{K\hat\sigma^2\log(n)/n}.
#'   \item CAIC, consistent AIC (Bozdogan, 1987):
#'     \eqn{K\hat\sigma^2(\log n + 1)/n}.
#'   \item CAICF, consistent AIC with Fisher information (Bozdogan, 1987): the CAIC
#'     penalty plus \eqn{\hat\sigma^2 \log\det(X^\top X/\hat\sigma^2)/n}.
#'   \item SIC, square-root information criterion (Shao, 1997; Xiao and Walker, 2026):
#'     \eqn{K\hat\sigma^2/\sqrt{n}}.
#'   \item GIC2--GIC6, generalized information criteria for high dimensions
#'     (Kim, Kwon and Choi, 2012), with penalties \eqn{K\hat\sigma^2 P^{1/3}/n},
#'     \eqn{2K\hat\sigma^2\log(P)/n}, \eqn{2K\hat\sigma^2(\log P + \log\log P)/n},
#'     \eqn{K\hat\sigma^2\log\log(n)\log(P)/n} and \eqn{K\hat\sigma^2\log(n)\log(P)/n}.
#'   \item EBIC, extended BIC (Chen and Chen, 2008):
#'     \eqn{K\hat\sigma^2\{\log n + \gamma\log\binom{P}{K}\}/n}, with \eqn{\gamma = 0.6}
#'     by default.
#'   \item AttIC, an attention-weighted criterion specific to this package: the fit
#'     term is replaced by \eqn{Y^\top \mathrm{softmax}(HY)} and the penalty is that of SIC.
#' }
#' Short versions (e.g., `Calculate_AIC_Short`) allow reusing a precomputed inverse matrix
#' to save computational time.
#'
#' @references
#' Akaike, H. (1974). A new look at the statistical model identification.
#'   \emph{IEEE Transactions on Automatic Control}, 19(6), 716--723.
#'   \doi{10.1109/TAC.1974.1100705}
#'
#' Bozdogan, H. (1987). Model selection and Akaike's information criterion (AIC):
#'   The general theory and its analytical extensions. \emph{Psychometrika}, 52(3),
#'   345--370. \doi{10.1007/BF02294361}
#'
#' Chen, J. and Chen, Z. (2008). Extended Bayesian information criteria for model
#'   selection with large model spaces. \emph{Biometrika}, 95(3), 759--771.
#'   \doi{10.1093/biomet/asn034}
#'
#' Hurvich, C. M. and Tsai, C.-L. (1989). Regression and time series model selection
#'   in small samples. \emph{Biometrika}, 76(2), 297--307. \doi{10.1093/biomet/76.2.297}
#'
#' Kim, Y., Kwon, S. and Choi, H. (2012). Consistent model selection criteria on high
#'   dimensions. \emph{Journal of Machine Learning Research}, 13, 1037--1057.
#'   \url{https://jmlr.org/papers/v13/kim12a.html}
#'
#' Schwarz, G. (1978). Estimating the dimension of a model. \emph{The Annals of
#'   Statistics}, 6(2), 461--464. \doi{10.1214/aos/1176344136}
#'
#' Shao, J. (1997). An asymptotic theory for linear model selection.
#'   \emph{Statistica Sinica}, 7(2), 221--264. \url{https://www3.stat.sinica.edu.tw/statistica/j7n2/j7n21/j7n21.htm}
#'
#' Xiao, Y. and Walker, S. (2026). Fast variable selection under \eqn{\ell_0}
#'   regularization in high-dimensions. \emph{Statistics and Computing}, 36(3), 100.
#'   \doi{10.1007/s11222-026-10853-5}
#'
#' @param Y A numeric response: a vector of length n, or an n x q matrix for a
#'   multivariate response (CAICF supports vector responses only).
#' @param X A numeric design matrix (n × p).
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
#'   setup_julia(install_julia = FALSE)
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

#' @importFrom cli cli_alert_info cli_alert_success cli_alert_danger cli_alert_warning
#' @importFrom cli col_red col_blue col_green col_yellow
#' @importFrom JuliaCall julia_setup julia_source julia_call julia_eval julia_exists julia_install_package_if_needed julia_library
NULL

# =========================
# Internal calculation helpers
# =========================

# Vector responses are passed as numeric vectors, matrix responses as matrices.
.Y_arg <- function(Y) {
  if (is.matrix(Y)) { storage.mode(Y) <- "double"; Y } else as.numeric(Y)
}

# Full versions: criteria that DO NOT depend on P
.Calculate_Criterion_NO_P <- function(func_name, Y, X) {
  .source_julia_backend_once(quiet = TRUE)
  res <- JuliaCall::julia_call(func_name, .Y_arg(Y), as.matrix(X))
  list(CriterionValue = res[[1]], InverseMatrix = res[[2]])
}

# Full versions: criteria that DO depend on P
.Calculate_Criterion_WITH_P <- function(func_name, Y, X, P = ncol(X)) {
  .source_julia_backend_once(quiet = TRUE)
  res <- JuliaCall::julia_call(func_name, .Y_arg(Y), as.matrix(X), as.integer(P))
  list(CriterionValue = res[[1]], InverseMatrix = res[[2]])
}

# Short versions: DO NOT depend on P
.Calculate_Criterion_Short_NO_P <- function(func_name, Y, X, Inverse) {
  .source_julia_backend_once(quiet = TRUE)
  JuliaCall::julia_call(paste0(func_name, "_short"),
                        .Y_arg(Y),
                        as.matrix(X),
                        as.matrix(Inverse))
}

# Short versions: DO depend on P
.Calculate_Criterion_Short_WITH_P <- function(func_name, Y, X, Inverse, P = ncol(X)) {
  .source_julia_backend_once(quiet = TRUE)
  JuliaCall::julia_call(paste0(func_name, "_short"),
                        .Y_arg(Y),
                        as.matrix(X),
                        as.matrix(Inverse),
                        as.integer(P))
}

# =========================
# Public API (exports)
# =========================

# -------- Full: NO-P criteria --------
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

# -------- Full: WITH-P criteria --------
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

# -------- Short: NO-P criteria --------
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

# -------- Short: WITH-P criteria --------
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
