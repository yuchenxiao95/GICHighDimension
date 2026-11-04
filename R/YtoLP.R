#' @title Transform Outcome to Linear Predictor
#'
#' @description
#' Uses the Julia function `'Y_to_LP'` to convert observed responses to linear predictors
#' using canonical link functions from generalized linear models (GLMs).
#'
#' @details
#' Applies the family's link function to the observed responses, with a small
#' adjustment so the link is finite at the boundary: logit with 0 and 1 replaced
#' by 0.01 and 0.99 (Bernoulli, Binomial), and \eqn{\log(Y + 0.1)} for the
#' log-link families. The families match \code{LP_to_Y()}:
#'
#' \tabular{lll}{
#'   \strong{Family} \tab \strong{Link} \tab \strong{Extra arguments} \cr
#'   "Normal" \tab identity \tab \code{std} (default 1) \cr
#'   "Bernoulli" \tab logit \tab \cr
#'   "Binomial" \tab logit \tab \code{n_trials} (required) \cr
#'   "Poisson" \tab log \tab \cr
#'   "Gamma" \tab log (mean \eqn{e^\eta}) \tab \code{shape} (default 1) \cr
#'   "Exponential" \tab log \tab \cr
#'   "InverseGaussian" \tab log \tab \code{shape} (\eqn{\lambda}, default 1) \cr
#'   "NegativeBinomial" \tab log \tab \code{theta} (size, default 1) \cr
#'   "Geometric" \tab log \tab \cr
#'   "MultivariateNormal" \tab identity \tab \code{cov_matrix} (required) \cr
#'   "Multinomial" \tab baseline-category logit \tab \code{n_categories} (required)
#' }
#'
#' For "Multinomial", \code{Y} holds categories \eqn{1, \dots, K} and the result is an
#' \eqn{n \times (K-1)} matrix of baseline-category logits of the smoothed one-hot vector
#' \eqn{(\mathrm{onehot}(Y_i) + 0.01)/(1 + 0.01K)}. For "Binomial", the logit is multiplied
#' by \code{n_trials}.
#' GLM stands for *generalized linear model*, a common framework for regression models.
#'
#' The underlying Julia logic is sourced from `'Y_to_LP.jl'`, and relies on the 'JuliaCall' interface.
#'
#' @param Y A numeric vector or matrix of observed responses.
#' @param family A character string indicating the GLM family to use.
#' @param shape Optional shape parameter (accepted for consistency; unused).
#' @param n_trials Optional number of trials (used for the Binomial family).
#' @param std Optional standard deviation (accepted for consistency; unused).
#' @param theta Optional NegativeBinomial size (accepted for consistency; unused).
#' @param n_categories Optional number of categories for "Multinomial"
#'   (default: the largest observed category).
#'
#' @return A numeric vector or matrix representing the linear predictor.
#'
#' @examples
#' \dontrun{
#' if (interactive() && requireNamespace("JuliaCall", quietly = TRUE)) {
#'   julia_ready <- FALSE
#'   tryCatch({
#'     JuliaCall::julia_setup(installJulia = FALSE)
#'     julia_ready <- TRUE
#'   }, error = function(e) message("Julia setup failed"))
#'
#'   if (julia_ready) {
#'     Y1 <- rbinom(100, 1, 0.5)
#'     eta1 <- Y_to_LP(Y1, family = "Bernoulli")
#'
#'     Y2 <- rpois(100, lambda = 5)
#'     eta2 <- Y_to_LP(Y2, family = "Poisson")
#'
#'     print(head(eta1))
#'     print(head(eta2))
#'   }
#' }
#' }
#'
#' @export
Y_to_LP <- function(Y, family, shape = NULL, n_trials = NULL, std = NULL,
                    theta = NULL, n_categories = NULL) {
  .source_julia_backend_once()

  if (is.matrix(Y)) storage.mode(Y) <- "double" else Y <- as.numeric(Y)

  args <- list("Y_to_LP", Y, family)
  if (!is.null(shape))        args$shape <- as.numeric(shape)
  if (!is.null(n_trials))     args$n_trials <- as.integer(n_trials)
  if (!is.null(std))          args$std <- as.numeric(std)
  if (!is.null(theta))        args$theta <- as.numeric(theta)
  if (!is.null(n_categories)) args$n_categories <- as.integer(n_categories)

  tryCatch(do.call(JuliaCall::julia_call, args), error = function(e) {
    stop("Error in calling Julia function 'Y_to_LP': ", conditionMessage(e), call. = FALSE)
  })
}
