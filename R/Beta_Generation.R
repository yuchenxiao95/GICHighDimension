#' @title True Beta Generation
#'
#' @description
#' This function generates a true coefficient vector based on the specified type.
#' The vector can represent different patterns of sparsity, such as coefficients that are equal to 1 at equally spaced indices,
#' or decaying exponentially, among other types.
#'
#' @param P Numeric. The number of predictors in the model.
#' @param k Numeric. The number of non-zero coefficients in the true coefficient vector.
#' @param type Numeric (optional). A parameter to specify the structure of the true coefficient vector.
#'             Possible values:
#'             \itemize{
#'               \item 1: Sparse coefficients equal to 1 at roughly equally spaced indices.
#'               \item 2: First \( k \) components equal to 1.
#'               \item 3: First \( k \) components with values equally spaced between 10 and 0.5.
#'               \item 5: First \( k \) components equal to 1, then exponentially decaying coefficients.
#'             }
#' @return Numeric vector. The generated true coefficient vector.
#'
#' @examples
#' # Example of generating a beta vector with 50 predictors and 10 non-zero coefficients
#' beta <- generate_beta(P = 50, k = 10, type = 1)
#' print(beta)
#'
#' @export
Beta_Generation <- function(P, k, type = 1) {

  beta <- rep(0, P)  # Initialize beta with zeros

  # Generate beta based on the specified type
  if (type == 1) {
    # Type 1: Sparse coefficients equal to 1 at equally spaced indices
    indices <- floor(seq(1, P, length.out = k))
    beta[indices] <- 1
  } else if (type == 2) {
    # Type 2: First k components equal to 1
    indices <- 1:k
    beta[indices] <- 1
  } else if (type == 3) {
    # Type 3: Equally spaced indices with values equally spaced between 10 and 0.5
    indices <- floor(seq(1, P, length.out = k))
    beta[indices] <- seq(10, 0.5, length.out = k)
  } else {
    stop("Invalid type specified.")
  }

  return (list(beta = beta, indices = indices))
}
