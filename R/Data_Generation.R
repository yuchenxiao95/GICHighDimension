#' @title Generate Synthetic Data for Linear Models
#' @description Creates simulated datasets for testing and demonstrating linear models and GLMs.
#' @details Generates design matrix from multivariate normal distribution and response variable
#'          with specified signal-to-noise ratio. Supports multiple distribution families.
#' @param N integer number of observations (must be positive)
#' @param P integer number of predictors (must be positive)
#' @param family character string specifying distribution family
#' @param SNR numeric signal-to-noise ratio (must be positive)
#' @param rho numeric correlation between predictors (between -1 and 1)
#' @param beta numeric vector of true coefficients (length must match P)
#' @return A list containing:
#'   \item{X}{Design matrix (N x P)}
#'   \item{Y}{Response vector (length N)}
#'   \item{cov}{Predictor covariance matrix (P x P)}
#'   \item{variance}{Calculated error variance}
#' @examples
#' # Normal data example
#' dat <- Generate_Data(N = 100, P = 5, family = "Normal",
#'                     SNR = 2, rho = 0.3, beta = c(1, 0.5, 0, -0.5, 1))
#' @export
#' @importFrom MASS mvrnorm
#' @importFrom stats rnorm
Generate_Data <- function(N, P, family, SNR, rho, beta) {

  # Input validation
  if (!is.numeric(N) || N <= 0 || !is.numeric(P) || P <= 0) {
    stop("N and P must be positive integers")
  }
  if (length(beta) != P) {
    stop("beta must be a vector of length P")
  }

  # Set mean vector and covariance matrix
  mu <- rep(0, P)  # Mean vector
  cov <- matrix(rho, P, P)
  diag(cov) <- 1  # Covariance matrix with specified correlation

  # Generate the design matrix X
  X <- MASS::mvrnorm(n = N, mu = mu, Sigma = cov)

  # Calculate the variance of Y based on the signal-to-noise ratio
  variance <- as.numeric(t(beta) %*% cov %*% beta) / SNR
  Std <- sqrt(variance)  # Standard deviation

  # Generate the outcome variable Y
  Y <- stats::rnorm(N, X %*% beta, Std)

  # Return the results in a list
  return(list(X = X, Y = Y, cov = cov, variance = variance))
}
