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
#' @param family One of \code{"auto"} (default, guessed from \code{Y}), \code{"Linear"}
#'   (or \code{"Normal"}), \code{"MultivariateNormal"}, \code{"Bernoulli"},
#'   \code{"Binomial"}, \code{"Poisson"}, \code{"Gamma"}, \code{"Exponential"},
#'   \code{"InverseGaussian"}, \code{"NegativeBinomial"}, \code{"Geometric"}, or
#'   \code{"Multinomial"} (one-vs-rest logistic). The default links match
#'   \code{LP_to_Y()}: identity, logit, or log.
#' @param link   Character/symbol/Julia \code{Link}; default \code{"auto"}.
#' @param n_trials Integer trials for a binomial response (default
#'   \code{NULL}).
#' @param theta Optional NegativeBinomial size; estimated from the data if \code{NULL}.
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
                          theta         = NULL,
                          add_intercept = TRUE) {

  if (!is.matrix(X))
    stop("`X` must be a numeric matrix.", call. = FALSE)
  storage.mode(X) <- "double"

  if (is.matrix(Y) && ncol(Y) == 1L) Y <- drop(Y)
  if (!(is.numeric(Y) && (is.vector(Y) || is.matrix(Y))))
    stop("`Y` must be a numeric vector or matrix.", call. = FALSE)
  if (nrow(X) != nrow(as.matrix(Y)))
    stop("`X` and `Y` must have the same number of rows.", call. = FALSE)
  if (is.matrix(Y)) storage.mode(Y) <- "double" else Y <- as.numeric(Y)

  .source_julia_backend_once(glm = TRUE)

  args <- list("Beta_estimate", Y, X,
               family = as.character(family), link = link,
               add_intercept = isTRUE(add_intercept))
  if (!is.null(n_trials)) args$n_trials <- as.integer(n_trials)
  if (!is.null(theta))    args$theta <- as.numeric(theta)

  do.call(JuliaCall::julia_call, args)
}
