#' @title GIC-Based Variable Selection
#'
#' @description
#' Performs variable selection using the Generalized Information Criterion (GIC)
#' with Hopfield Network optimization. The computationally intensive components
#' are implemented in 'Julia' for performance, while an 'R' interface provides user-friendly access.
#'
#' @details
#' This function implements a variable selection algorithm that:
#' \itemize{
#'   \item Uses customizable information criteria (such as AIC, BIC, and ICOMP)
#'   \item Leverages a 'Julia' backend for computational efficiency
#'   \item Supports various model families, including generalized linear models (GLMs) such as the normal and Poisson families
#'   \item Accepts both univariate and multivariate responses
#' }
#'
#' @param X Numeric design matrix (n x p), where n is the number of observations and
#'          p is the number of predictors.
#' @param Y Numeric response (either a vector of length n or a matrix with n rows).
#' @param Initial_Column Integer vector of initial feature indices to consider.
#' @param Calculate_GIC Character name of the 'Julia' function for full GIC calculation.
#' @param Calculate_GIC_short Character name of the matching \code{_short} 'Julia' function.
#'   Kept for backward compatibility; the built-in criteria use fast rank-one updates instead.
#' @param k Integer model-size cap: a move that would give a model with \code{k}
#'   or more variables is skipped. Models also never exceed \eqn{n - 1} variables
#'   (\eqn{n} = sample size), so selected models have at most
#'   \code{min(k, n) - 1} variables.
#' @param Nsim Integer number of passes over the variables, each in a new random order (default: 1).
#' @param gamma EBIC tuning constant \eqn{\gamma} (default 0.6); ignored by other criteria.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{GIC_values}: Calculated GIC values for all candidate models
#'   \item \code{selected_coeffs}: Indices of selected variables
#'   \item \code{elapsed}: Elapsed time in seconds since the search started,
#'     after each step (the Julia search only, excluding data transfer)
#' }
#'
#' @usage
#' GICSelection(X, Y, Initial_Column, Calculate_GIC, Calculate_GIC_short, k,
#'              Nsim = 1L, gamma = 0.6)
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("JuliaCall", quietly = TRUE)) {
#'   julia_available <- FALSE
#'   tryCatch({
#'     JuliaCall::julia_setup(installJulia = FALSE)
#'     julia_available <- TRUE
#'   }, error = function(e) {
#'     message("Julia not available: ", e$message)
#'   })
#'
#'   if (julia_available) {
#'     set.seed(123)
#'     n <- 100; p <- 10; k <- 3
#'     X <- matrix(rnorm(n * p), n, p)
#'     beta <- c(rep(1.5, k), rep(0, p - k))
#'     Y_uni <- LP_to_Y(X, beta, family = "Normal", std = 1.0)
#'
#'     result_uni <- GICSelection(
#'       X = X,
#'       Y = Y_uni,
#'       Initial_Column = 1:p,
#'       Calculate_GIC = "Calculate_SIC",
#'       Calculate_GIC_short = "Calculate_SIC_short",
#'       k = 3,
#'       Nsim = 3
#'     )
#'     print(result_uni$selected_coeffs)
#'
#'     m <- 3
#'     multi_beta <- matrix(0, p, m)
#'     multi_beta[1:3, ] <- 1
#'     rho <- 0.2
#'     cov_p <- matrix(rho, nrow = m, ncol = m)
#'     diag(cov_p) <- 1.0
#'     Y_multi <- LP_to_Y(X, multi_beta, family = "MultivariateNormal", cov_matrix = cov_p)
#'
#'     result_multi <- GICSelection(
#'       X = X,
#'       Y = Y_multi,
#'       Initial_Column = 1:p,
#'       Calculate_GIC = "Calculate_SIC",
#'       Calculate_GIC_short = "Calculate_SIC_short",
#'       k = 3,
#'       Nsim = 3
#'     )
#'     print(result_multi$selected_coeffs)
#'   }
#' }
#' }
#' @export
#' @importFrom JuliaCall julia_setup julia_source julia_call julia_eval julia_exists
#' @references Xiao, Y. and Walker, S. G. (2026). Fast variable selection under
#'   \eqn{\ell_0} regularization in high-dimensions. \emph{Statistics and Computing},
#'   36(3), 100. \doi{10.1007/s11222-026-10853-5}
GICSelection <- function(X, Y, Initial_Column,
                         Calculate_GIC,
                         Calculate_GIC_short,
                         k,
                         Nsim = 1L,
                         gamma = 0.6) {

  ## ---- prechecks & coercions ----
  if (!is.matrix(X)) stop("X must be a matrix", call. = FALSE)
  storage.mode(X) <- "double"
  if (!(is.numeric(Y) && (is.vector(Y) || is.matrix(Y)))) {
    stop("Y must be either a numeric vector or a numeric matrix", call. = FALSE)
  }
  if (is.vector(Y)) Y <- as.numeric(Y)
  if (is.matrix(Y)) storage.mode(Y) <- "double"
  if (nrow(X) != nrow(as.matrix(Y))) stop("X and Y must have the same number of rows", call. = FALSE)
  if (any(Initial_Column < 1L) || any(Initial_Column > ncol(X))) {
    stop("Invalid column indices in Initial_Column", call. = FALSE)
  }
  if (missing(k)) stop("`k` (maximum model size) must be supplied", call. = FALSE)

  .source_julia_backend_once()
  for (fn in c(Calculate_GIC, Calculate_GIC_short)) {
    if (!JuliaCall::julia_exists(fn)) stop("Julia function ", fn, " not found", call. = FALSE)
  }

  julia_result <- tryCatch(
    JuliaCall::julia_call(
      "_GIC_Variable_Selection_flat",
      X, Y, as.integer(Initial_Column),
      Calculate_GIC, Calculate_GIC_short,   # by name: see _GIC_Variable_Selection_flat
      as.integer(k),
      Nsim = as.integer(Nsim), gamma = as.numeric(gamma)
    ),
    error = function(e) stop("Julia error in GIC_Variable_Selection: ", conditionMessage(e),
                             call. = FALSE)
  )

  # The selected sets come back packed into one vector plus their lengths
  lens <- as.integer(julia_result[[3]])
  steps <- factor(rep.int(seq_along(lens), lens), levels = seq_along(lens))
  list(
    GIC_values = as.numeric(unlist(julia_result[[1]])),
    selected_coeffs = unname(split(as.integer(julia_result[[2]]), steps)),
    elapsed = as.numeric(unlist(julia_result[[4]]))
  )
}
