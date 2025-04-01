#' @title Transform Outcome Y to Linear Predictor
#'
#' @description
#' This function uses Julia's `Y_to_LP` function to transform observed responses
#' into linear predictors using canonical link functions for various GLM families.
#'
#' @param Y Observed response (vector for univariate, matrix for multivariate).
#' @param family Distribution family. Supported options:
#'               "Bernoulli", "Binomial", "Normal", "Poisson", "Gamma", "Exponential", "MultivariateNormal".
#' @param shape (Optional) Shape parameter for Gamma family (default: NULL).
#' @param n_trials (Optional) Number of trials for Binomial family (default: NULL).
#' @param std (Optional) Standard deviation for Normal family (default: NULL).
#'
#' @return Transformed linear predictor values.
#'
#' @examples
#' # Bernoulli example
#' Y_bernoulli <- sample(c(0,1), 100, replace = TRUE)
#' lp_bernoulli <- YtoLP(Y_bernoulli, family = "Bernoulli")
#'
#' # Multivariate Normal example
#' Y_mvnorm <- matrix(rnorm(200), ncol = 2)
#' lp_mvnorm <- YtoLP(Y_mvnorm, family = "MultivariateNormal")
#'
#' @export
Y_to_LP <- function(Y, family, shape = NULL, n_trials = NULL, std = NULL) {

  # Ensure JuliaCall package is available
  if (!requireNamespace("JuliaCall", quietly = TRUE)) {
    stop("JuliaCall required. Install with install.packages('JuliaCall')")
  }

  # Initialize Julia
  JuliaCall::julia_setup()

  # Convert inputs to appropriate types
  if (is.matrix(Y)) {
    Y_julia <- Y  # Keep as matrix for multivariate cases
  } else {
    Y_julia <- as.vector(Y)
  }

  # Prepare arguments list
  args <- list(
    Y = Y_julia,
    family = family
  )

  # Add optional parameters if provided
  if (!is.null(shape)) args$shape <- as.numeric(shape)
  if (!is.null(n_trials)) args$n_trials <- as.integer(n_trials)
  if (!is.null(std)) args$std <- as.numeric(std)

  # Locate and source Julia script
  julia_script <- system.file("julia", "Y_to_LP.jl", package = "GICHighDimension")
  if (julia_script == "") stop("Julia script 'Y_to_LP.jl' not found in package")
  JuliaCall::julia_source(julia_script)

  # Call Julia function with appropriate arguments
  result <- tryCatch({
    do.call(JuliaCall::julia_call, c("Y_to_LP", args))
  }, error = function(e) {
    stop("Julia error: ", e$message)
  })

  return(result)
}
