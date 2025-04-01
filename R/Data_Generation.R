#' @title Data Generation for Linear Model
#'
#' @description
#' This function generates synthetic dataset for linear model and Generalized linear model.
#'
#'
#' @param N Numeric. The number of observations.
#' @param P Numeric. The number of predictors.
#' @param family Character string specifying the distribution of the outcome variable.
#'               One of "Binomial", "Bernoulli", "Normal", "Poisson", "Gamma", or "Exponential".
#' @param SNR Numeric The signal-to-noise ratio. Controls the variance of the outcome variable.
#' @param rho Numeric The correlation coefficient between predictors in the design matrix.
#' @param beta Numeric vector. The true coefficient vector used to generate the outcome variable.
#'
#' @return A list containing the following elements:
#'   \item{X}{Numeric matrix. The design matrix.}
#'   \item{Y}{Numeric vector. The outcome vector.}
#'   \item{cov}{Numeric matrix. The covariance matrix of the predictors.}
#'   \item{variance}{Numeric. The variance of the outcome variable.}
#'
#' @details
#' The design matrix is generated from a multivariate normal distribution with mean zero and a specified correlation structure.
#' The true coefficient vector  is used to compute the linear predictor, and the outcome vector is generated
#' as a linear combination of the predictors with added noise, controlled by the signal-to-noise ratio (SNR).
#'
#' @examples
#' # Example of generating a dataset with 100 observations and 50 predictors
#' result <- Generation(N = 100, P = 50, family = "Normal", SNR = 10, rho = 0.5, beta = rep(1, 20))
#' X <- result[[1]]
#' Y <- result[[2]]
#'
#' @export
Data_Generation <- function(N, P, family, SNR, rho, beta) {

  # Set mean vector and covariance matrix
  mu <- rep(0, P)  # Mean vector
  cov <- matrix(rho, P, P)
  diag(cov) <- 1  # Covariance matrix with specified correlation

  # Generate the design matrix X
  X <- mvrnorm(N, mu, cov)

  # Calculate the variance of Y based on the signal-to-noise ratio
  variance <- as.numeric(t(beta) %*% cov %*% beta) / SNR
  Std <- sqrt(variance)  # Standard deviation

  # Generate the outcome variable Y
  Y <- rnorm(N, X %*% beta, Std)

  # Return the results in a list
  return(list(X = X, Y = Y, cov = cov, variance = variance))
}
