#' @title Generate True Coefficient Vector
#' @description Creates a sparse coefficient vector with specified pattern of non-zero values
#' for simulation studies.
#' @param P Number of predictors (positive integer)
#' @param k Number of non-zero coefficients (positive integer <= P)
#' @param type Pattern of non-zero coefficients (1-3, see Details)
#' @return A list containing:
#' \describe{
#'   \item{beta}{Numeric vector of length P with specified coefficient pattern}
#'   \item{indices}{Integer vector of indices where beta is non-zero}
#' }
#'
#' @details
#' \itemize{
#'   \item \code{type = 1}: Equally spaced signals
#'   \item \code{type = 2}: First \code{k} positions
#'   \item \code{type = 3}: Decaying values from 10 down to 0.5
#' }
#'
#' @examples
#' # Equally spaced signals
#' beta1 <- Generate_Beta(P = 50, k = 5, type = 1)
#'
#' # Block of signals at beginning
#' beta2 <- Generate_Beta(P = 100, k = 10, type = 2)
#'
#' # Decaying signals
#' beta3 <- Generate_Beta(P = 200, k = 15, type = 3)
#'
#' @export
Generate_Beta <- function(P, k, type = 1) {
  if (!is.numeric(P) || P <= 0 || P %% 1 != 0) {
    stop("P must be a positive integer")
  }
  if (!is.numeric(k) || k <= 0 || k > P || k %% 1 != 0) {
    stop("k must be a positive integer <= P")
  }
  if (!type %in% 1:3) {
    stop("type must be 1, 2, or 3")
  }

  beta <- numeric(P)

  switch(type,
         "1" = {
           indices <- floor(seq(1, P, length.out = k))
           beta[indices] <- 1
         },
         "2" = {
           indices <- seq_len(k)
           beta[indices] <- 1
         },
         "3" = {
           indices <- seq_len(k)
           beta[indices] <- seq(10, 0.5, length.out = k)
         }
  )

  return(list(beta = beta, indices = indices))
}
