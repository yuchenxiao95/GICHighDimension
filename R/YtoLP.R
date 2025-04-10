#' @title Transform Outcome to Linear Predictor
#'
#' @description
#' Uses the Julia function `'Y_to_LP'` to convert observed responses to linear predictors
#' using canonical link functions from generalized linear models (GLMs).
#'
#' @details
#' This function supports canonical link transformations for the following distribution families:
#' \itemize{
#'   \item "Bernoulli"
#'   \item "Binomial"
#'   \item "Normal"
#'   \item "Poisson"
#'   \item "Gamma"
#'   \item "Exponential"
#'   \item "MultivariateNormal"
#' }
#' GLM stands for *generalized linear model*, a common framework for regression models.
#'
#' The underlying Julia logic is sourced from `'Y_to_LP.jl'`, and relies on the 'JuliaCall' interface.
#'
#' @param Y A numeric vector or matrix of observed responses.
#' @param family A character string indicating the GLM family to use.
#' @param shape Optional shape parameter (used for the Gamma family).
#' @param n_trials Optional number of trials (used for the Binomial family).
#' @param std Optional standard deviation (used for the Normal family).
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
Y_to_LP <- function(Y, family, shape = NULL, n_trials = NULL, std = NULL) {
  if (!requireNamespace("JuliaCall", quietly = TRUE)) {
    stop("Package 'JuliaCall' is required. Please install it using install.packages('JuliaCall').", call. = FALSE)
  }

  # Safe Julia setup (no install to comply with CRAN)
  tryCatch({
    JuliaCall::julia_setup(installJulia = FALSE)
  }, error = function(e) {
    stop("Julia initialization failed. Please run setup_julia() manually before using this function.", call. = FALSE)
  })

  # Source the Julia function if not already available
  if (!JuliaCall::julia_exists("Y_to_LP")) {
    julia_script <- system.file("julia", "Y_to_LP.jl", package = "GICHighDimension")
    if (julia_script == "") {
      stop("The Julia script 'Y_to_LP.jl' is not found in the installed package.", call. = FALSE)
    }
    JuliaCall::julia_source(julia_script)
  }

  # Build keyword argument list
  kwargs <- list()
  if (!is.null(shape))     kwargs$shape <- as.numeric(shape)
  if (!is.null(n_trials)) kwargs$n_trials <- as.integer(n_trials)
  if (!is.null(std))      kwargs$std <- as.numeric(std)

  # Call the Julia function
  tryCatch({
    do.call(JuliaCall::julia_call, c(list("Y_to_LP", Y, family), kwargs))
  }, error = function(e) {
    stop("Error in calling Julia function 'Y_to_LP': ", e$message, call. = FALSE)
  })
}
