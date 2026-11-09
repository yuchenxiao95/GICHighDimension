#' @title Transform Linear Predictor to Response
#'
#' @description
#' Converts a linear predictor to observed responses using canonical link functions from
#' generalized linear models (GLMs). Supports univariate and multivariate distributions.
#'
#' @details
#' Simulates responses from the linear predictor \eqn{\eta = X\beta} using the
#' 'Julia' backend. The supported families and links (the same as in
#' \code{Y_to_LP()} and \code{Beta_estimate()}) are:
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
#'   "Multinomial" \tab baseline-category logit \tab \code{n_categories} (required) \cr
#'   "Cox" \tab log hazard \tab \code{baseline_rate}, \code{shape}, \code{censor_rate}
#' }
#'
#' For "MultivariateNormal", \code{true_beta} is a \eqn{p \times q} matrix; for
#' "Multinomial" it is \eqn{p \times (K-1)} and the last category is the baseline.
#' GLM stands for *generalized linear model*.
#'
#' For "Cox", survival times follow a Cox proportional hazards model with
#' Weibull baseline hazard \eqn{h_0(t) = \lambda \nu t^{\nu - 1}}, simulated by
#' inverting the survival function (Bender, Augustin and Blettner, 2005):
#' \eqn{T = (-\log U / (\lambda e^{\eta}))^{1/\nu}} with \eqn{U \sim U(0, 1)}.
#' Times are censored by independent exponential times with rate
#' \code{censor_rate}. \code{shape} = 1 gives an exponential baseline.
#'
#' @param X A numeric design matrix.
#' @param true_beta A numeric vector or matrix of true coefficients.
#' @param family A character string specifying the distribution family.
#' @param n_trials Optional integer, number of trials for the Binomial family.
#' @param std Optional numeric, standard deviation for the Normal family.
#' @param shape Optional numeric shape parameter (Gamma, InverseGaussian).
#' @param theta Optional numeric size parameter for the NegativeBinomial family.
#' @param cov_matrix Optional covariance matrix for multivariate families.
#' @param n_categories Optional integer, number of categories for Multinomial family.
#' @param baseline_rate Optional baseline hazard rate \eqn{\lambda} for "Cox" (default 1);
#'   \code{shape} is its Weibull shape \eqn{\nu} (default 1).
#' @param censor_rate Optional rate of the exponential censoring times for "Cox"
#'   (default 0.3; 0 means no censoring).
#'
#' @return A numeric vector of responses (integer categories for "Multinomial"), an
#'   \eqn{n \times q} matrix for "MultivariateNormal", or an \eqn{n \times 2}
#'   matrix with columns \code{time} and \code{status} (1 = event) for "Cox".
#'
#' @references
#' Bender, R., Augustin, T. and Blettner, M. (2005). Generating survival times to
#' simulate Cox proportional hazards models. \emph{Statistics in Medicine},
#' 24(11), 1713--1723. \doi{10.1002/sim.2059}
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
#'     set.seed(123)
#'     n <- 100
#'     p <- 5
#'     X <- matrix(rnorm(n * p), n, p)
#'     beta <- c(1.5, -2, 0, 0, 3)
#'     Y <- LP_to_Y(X, beta, family = "Normal", std = 1.0)
#'     head(Y)
#'   }
#' }
#' }
#'
#' @export
#' @importFrom JuliaCall julia_call julia_eval julia_assign julia_source julia_exists
LP_to_Y <- function(X, true_beta, family = "Normal",
                    n_trials = NULL, std = NULL, shape = NULL, theta = NULL,
                    cov_matrix = NULL, n_categories = NULL,
                    baseline_rate = NULL, censor_rate = NULL) {
  .source_julia_backend_once()

  X <- as.matrix(X)
  storage.mode(X) <- "double"
  true_beta <- if (is.matrix(true_beta)) true_beta else as.numeric(true_beta)

  args <- list("LP_to_Y", X, true_beta, family)
  if (!is.null(n_trials))     args$n_trials <- as.integer(n_trials)
  if (!is.null(std))          args$std <- as.numeric(std)
  if (!is.null(shape))        args$shape <- as.numeric(shape)
  if (!is.null(theta))        args$theta <- as.numeric(theta)
  if (!is.null(cov_matrix))   args$cov_matrix <- as.matrix(cov_matrix)
  if (!is.null(n_categories)) args$n_categories <- as.integer(n_categories)
  if (!is.null(baseline_rate)) args$baseline_rate <- as.numeric(baseline_rate)
  if (!is.null(censor_rate))  args$censor_rate <- as.numeric(censor_rate)

  out <- tryCatch(do.call(JuliaCall::julia_call, args), error = function(e) {
    stop("Julia backend error in 'LP_to_Y': ", conditionMessage(e), call. = FALSE)
  })
  if (family == "Cox") colnames(out) <- c("time", "status")
  out
}
