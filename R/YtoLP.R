#' @title Transform Outcome to Linear Predictor
#'
#' @description
#' Uses Julia's `Y_to_LP` function to transform observed responses into linear predictors
#' using canonical link functions for various GLM families.
#'
#' @details
#' Supported distribution families include:
#' \itemize{
#'   \item "Bernoulli"
#'   \item "Binomial"
#'   \item "Normal"
#'   \item "Poisson"
#'   \item "Gamma"
#'   \item "Exponential"
#'   \item "MultivariateNormal"
#' }
#'
#' @param Y Observed response (numeric vector or matrix).
#' @param family Character string specifying the GLM family.
#' @param shape Optional shape parameter (for Gamma family).
#' @param n_trials Optional number of trials (for Binomial family).
#' @param std Optional standard deviation (for Normal family).
#'
#' @return A numeric vector or matrix of linear predictors, matching the input structure.
#'
#' @examples
#' \donttest{
#' julia_ready <- FALSE
#' if (requireNamespace("JuliaCall", quietly = TRUE)) {
#'   try({
#'     JuliaCall::julia_setup(installJulia = FALSE)
#'     JuliaCall::julia_eval("1 + 1")
#'     julia_ready <- TRUE
#'   }, silent = TRUE)
#' }
#'
#' if (julia_ready) {
#'   Y_bernoulli <- sample(c(0, 1), 100, replace = TRUE)
#'   lp_bernoulli <- Y_to_LP(Y_bernoulli, family = "Bernoulli")
#'
#'   Y_poisson <- rpois(100, lambda = 5)
#'   lp_poisson <- Y_to_LP(Y_poisson, family = "Poisson")
#'
#'   print(head(lp_bernoulli))
#'   print(head(lp_poisson))
#' } else {
#'   message("Julia not available - examples skipped.")
#' }
#' }
#'
#' @export
Y_to_LP <- function(Y, family, shape = NULL, n_trials = NULL, std = NULL) {
  if (!requireNamespace("JuliaCall", quietly = TRUE)) {
    stop("JuliaCall is required. Please install it with install.packages('JuliaCall')", call. = FALSE)
  }

  # Initialize Julia if needed
  try({
    JuliaCall::julia_setup(installJulia = FALSE)
  }, silent = TRUE)

  # Source the Julia script
  if (!JuliaCall::julia_exists("Y_to_LP")) {
    julia_script <- system.file("julia", "Y_to_LP.jl", package = "GICHighDimension")
    if (julia_script == "") {
      stop("Required Julia script 'Y_to_LP.jl' not found in package", call. = FALSE)
    }
    JuliaCall::julia_source(julia_script)
  }

  # Build keyword argument list
  kwargs <- list()
  if (!is.null(shape)) kwargs$shape <- as.numeric(shape)
  if (!is.null(n_trials)) kwargs$n_trials <- as.integer(n_trials)
  if (!is.null(std)) kwargs$std <- as.numeric(std)

  # Combine with positional args and call
  tryCatch({
    do.call(JuliaCall::julia_call, c(list("Y_to_LP", Y, family), kwargs))
  }, error = function(e) {
    stop("Julia execution failed: ", e$message, call. = FALSE)
  })
}
