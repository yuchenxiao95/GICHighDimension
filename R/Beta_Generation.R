#' @title Generate Sparse Coefficient Vector
#'
#' @description
#' Creates a sparse coefficient vector with a specified pattern of non-zero values
#' for use in simulation studies, such as benchmarking variable selection methods.
#'
#' @param P Integer. Total number of predictors (must be a positive integer).
#' @param k Integer. Number of non-zero coefficients (must be between 1 and \code{P}).
#' @param type Integer. Pattern type for non-zero coefficients:
#' \itemize{
#'   \item \code{1}: Equally spaced non-zero values
#'   \item \code{2}: First \code{k} positions are non-zero
#'   \item \code{3}: Decaying values from 10 to 0.5
#' }
#'
#' @return A list with components:
#' \describe{
#'   \item{\code{beta}}{A numeric vector of length \code{P} with the specified sparsity pattern.}
#'   \item{\code{indices}}{An integer vector of indices where \code{beta} is non-zero.}
#' }
#'
#' @examples
#' # Equally spaced non-zero coefficients
#' Generate_Beta(P = 50, k = 5, type = 1)
#'
#' # First k positions non-zero
#' Generate_Beta(P = 100, k = 10, type = 2)
#'
#' # Decaying pattern
#' Generate_Beta(P = 200, k = 15, type = 3)
#'
#' @export
Generate_Beta <- function(P, k, type = 1) {
  if (!is.numeric(P) || length(P) != 1 || P <= 0 || P %% 1 != 0) {
    stop("'P' must be a single positive integer.")
  }
  if (!is.numeric(k) || length(k) != 1 || k <= 0 || k > P || k %% 1 != 0) {
    stop("'k' must be a single positive integer less than or equal to 'P'.")
  }
  if (!type %in% c(1, 2, 3)) {
    stop("'type' must be 1, 2, or 3.")
  }

  beta <- numeric(P)

  switch(as.character(type),
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

  list(beta = beta, indices = indices)
}
