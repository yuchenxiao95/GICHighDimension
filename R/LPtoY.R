#' @title Transform Linear Predictor to Response
#'
#' @description
#' Converts a linear predictor to observed responses using canonical link functions from
#' generalized linear models (GLMs). Supports univariate and multivariate distributions.
#'
#' @details
#' This function uses the Julia backend via the 'JuliaCall' interface to compute responses
#' from linear predictors efficiently. Supported families include:
#' \itemize{
#'   \item "Normal" (Gaussian)
#'   \item "Poisson"
#'   \item "Bernoulli"
#'   \item "Gamma"
#'   \item "MultivariateNormal"
#'   \item "Multinomial" (optional)
#' }
#' GLM stands for *generalized linear model*.
#'
#' @param X A numeric design matrix.
#' @param true_beta A numeric vector or matrix of true coefficients.
#' @param family A character string specifying the distribution family.
#' @param n_trials Optional integer, number of trials for the Binomial family.
#' @param std Optional numeric, standard deviation for the Normal family.
#' @param shape Optional numeric, shape parameter for the Gamma family.
#' @param cov_matrix Optional covariance matrix for multivariate families.
#' @param n_categories Optional integer, number of categories for Multinomial family.
#'
#' @return A numeric vector or matrix of responses generated from the linear predictor.
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
                    n_trials = NULL, std = NULL, shape = NULL,
                    cov_matrix = NULL, n_categories = NULL) {

  if (!requireNamespace("JuliaCall", quietly = TRUE)) {
    stop("Package 'JuliaCall' is required. Install it with install.packages('JuliaCall')", call. = FALSE)
  }

  # Ensure Julia is already initialized (do NOT install in CRAN context)
  if (!JuliaCall::julia_exists("LP_to_Y")) {
    julia_script <- system.file("julia", "LP_to_Y.jl", package = "GICHighDimension")
    if (julia_script == "") {
      stop("Julia script 'LP_to_Y.jl' not found in the package.", call. = FALSE)
    }
    JuliaCall::julia_source(julia_script)
  }

  # Input preparation
  X_julia <- as.matrix(X)
  true_beta_julia <- if (is.matrix(true_beta)) true_beta else as.vector(true_beta)

  args <- list(X_julia, true_beta_julia, family)
  if (!is.null(n_trials)) args$n_trials <- as.integer(n_trials)
  if (!is.null(std)) args$std <- as.numeric(std)
  if (!is.null(shape)) args$shape <- as.numeric(shape)
  if (!is.null(cov_matrix)) args$cov_matrix <- as.matrix(cov_matrix)
  if (!is.null(n_categories)) args$n_categories <- as.integer(n_categories)

  result <- tryCatch({
    do.call(JuliaCall::julia_call, c(list("LP_to_Y"), args))
  }, error = function(e) {
    stop("Julia backend error in 'LP_to_Y': ", e$message, call. = FALSE)
  })

  JuliaCall::julia_assign("jl_result", result)
  JuliaCall::julia_eval("converted = reduce(hcat, jl_result)'")
  JuliaCall::julia_eval("converted")
}
