#' @title Transform Linear Predictor to Response
#' @description Convert observed responses to linear predictor scale
#' @details Uses canonical link functions appropriate for each distribution family
#' @param X Design matrix
#' @param true_beta True coefficient vector
#' @param family Distribution family ("Normal", "Bernoulli", etc.)
#' @param cov_matrix Optional covariance matrix
#' @param n_categories Optional number of categories
#' @return Numeric vector/matrix of transformed values
#' @examples
#' # Normal/Gaussian family example
#' set.seed(123)
#' n <- 100
#' p <- 5
#' X <- matrix(rnorm(n * p), n, p)
#' true_beta <- c(1.5, -2, 0, 0, 3)  # True coefficients with some zeros
#' Y <- LP_to_Y(X, true_beta, family = "Normal")
#' head(Y)
#' @export
#' @importFrom JuliaCall julia_eval
LP_to_Y <- function(X, true_beta, family = "Normal",
                    n_trials = NULL, std = NULL, shape = NULL,
                    cov_matrix = NULL, n_categories = NULL) {

  # Ensure JuliaCall is available
  if (!requireNamespace("JuliaCall", quietly = TRUE)) {
    stop("Please install JuliaCall: install.packages('JuliaCall')")
  }

  # Initialize Julia environment
  JuliaCall::julia_setup()

  # Convert inputs to appropriate types
  X_julia <- as.matrix(X)
  if (is.matrix(true_beta)) {
    true_beta_julia <- true_beta
  } else {
    true_beta_julia <- as.vector(true_beta)
  }

  # Source Julia script
  julia_script <- system.file("julia", "LP_to_Y.jl", package = "GICHighDimension")
  if (julia_script == "") {
    stop("Julia script 'LP_to_Y.jl' not found in package")
  }
  JuliaCall::julia_source(julia_script)

  # Prepare arguments list
  args <- list(
    X_julia,
    true_beta_julia,
    family
  )

  # Add optional parameters if specified
  if (!is.null(n_trials)) args$n_trials <- as.integer(n_trials)
  if (!is.null(std)) args$std <- as.numeric(std)
  if (!is.null(shape)) args$shape <- as.numeric(shape)
  if (!is.null(cov_matrix)) args$cov_matrix <- as.matrix(cov_matrix)
  if (!is.null(n_categories)) args$n_categories <- as.integer(n_categories)

  # Call Julia function
  result <- tryCatch({
    do.call(JuliaCall::julia_call, c("LP_to_Y", args))
  }, error = function(e) {
    stop("Julia error: ", e$message)
  })

  return(result)
}

#' @param family character string specifying distribution family
#' @param shape optional shape parameter for Gamma family
#' @param n_trials optional number of trials for Binomial family
#' @param std optional standard deviation for Normal family
#' @return numeric vector/matrix of linear predictors
#' @examples
#' # Normal/Gaussian family example
#' set.seed(123)
#' n <- 100
#' p <- 5
#' X <- matrix(rnorm(n * p), n, p)
#' true_beta <- c(1.5, -2, 0, 0, 3)  # True coefficients with some zeros
#' Y <- LP_to_Y(X, true_beta, family = "Normal")
#' head(Y)
#' @export
LP_to_Y <- function(X, true_beta, family = "Normal",
                    n_trials = NULL, std = NULL, shape = NULL,
                    cov_matrix = NULL, n_categories = NULL) {

  # Ensure JuliaCall is available
  if (!requireNamespace("JuliaCall", quietly = TRUE)) {
    stop("Please install JuliaCall: install.packages('JuliaCall')")
  }

  # Initialize Julia environment
  JuliaCall::julia_setup()

  # Convert inputs to appropriate types
  X_julia <- as.matrix(X)
  if (is.matrix(true_beta)) {
    true_beta_julia <- true_beta
  } else {
    true_beta_julia <- as.vector(true_beta)
  }

  # Source Julia script
  julia_script <- system.file("julia", "LP_to_Y.jl", package = "GICHighDimension")
  if (julia_script == "") {
    stop("Julia script 'LP_to_Y.jl' not found in package")
  }
  JuliaCall::julia_source(julia_script)

  # Prepare arguments list
  args <- list(
    X_julia,
    true_beta_julia,
    family
  )

  # Add optional parameters if specified
  if (!is.null(n_trials)) args$n_trials <- as.integer(n_trials)
  if (!is.null(std)) args$std <- as.numeric(std)
  if (!is.null(shape)) args$shape <- as.numeric(shape)
  if (!is.null(cov_matrix)) args$cov_matrix <- as.matrix(cov_matrix)
  if (!is.null(n_categories)) args$n_categories <- as.integer(n_categories)

  # Call Julia function
  result <- tryCatch({
    do.call(JuliaCall::julia_call, c("LP_to_Y", args))
  }, error = function(e) {
    stop("Julia error: ", e$message)
  })

  return(result)
}
