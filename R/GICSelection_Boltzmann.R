#' @title GIC-Based Variable Selection Using Boltzmann Machine
#'
#' @description
#' Performs variable selection using the Generalized Information Criterion ('GIC')
#' combined with a Boltzmann-like simulated annealing approach. The optimization logic
#' is implemented in 'Julia' and accessed from 'R' through the 'JuliaCall' interface.
#'
#' @details
#' This method uses temperature-based control to introduce stochasticity in
#' model exploration. It supports custom GIC formulations by passing the names of
#' 'Julia' functions and works with both univariate and multivariate responses.
#'
#' @param X A numeric design matrix with \code{n} rows (observations) and \code{p} columns (predictors).
#' @param Y A numeric response vector (length \code{n}) or an \code{n x q} matrix.
#' @param Init_Columns Integer vector of indices specifying the initial set of predictors.
#' @param Calculate_GIC Character string giving the name of the 'Julia' function to compute full GIC values.
#' @param Calculate_GIC_short Character string giving the name of the 'Julia' function to compute approximate GIC values.
#' @param T Numeric scalar. The temperature parameter controlling stochasticity in the simulated annealing process (default: 0.1).
#' @param Nsim Integer. Number of passes over the (once-shuffled) variables (default: 5).
#' @param k Optional integer model-size cap, as in \code{GICSelection()}; \code{NULL} (default) means no cap.
#' @param gamma EBIC tuning constant (default 0.6); ignored by other criteria.
#'
#' @return A list with the following components:
#' \describe{
#'   \item{\code{GIC_list}}{List of GIC values returned for each run.}
#'   \item{\code{GIC_coeff}}{Indices of the variables selected in the final run.}
#' }
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
#'     Y <- X %*% beta + rnorm(n)
#'
#'     result <- GICSelectionBoltzmann(
#'       X = X,
#'       Y = Y,
#'       Init_Columns = 1:p,
#'       Calculate_GIC = "Calculate_SIC",
#'       Calculate_GIC_short = "Calculate_SIC_short",
#'       Nsim = 3
#'     )
#'     print(result$GIC_coeff)
#'   }
#' }
#' }
#'
#' @export
#' @importFrom JuliaCall julia_setup julia_source julia_call julia_eval
GICSelectionBoltzmann <- function(X, Y, Init_Columns,
                                  Calculate_GIC,
                                  Calculate_GIC_short,
                                  T = 0.1,
                                  Nsim = 5,
                                  k = NULL,
                                  gamma = 0.6) {
  if (!is.matrix(X)) X <- as.matrix(X)
  storage.mode(X) <- "double"
  if (is.matrix(Y)) storage.mode(Y) <- "double" else Y <- as.numeric(Y)
  if (nrow(X) != nrow(as.matrix(Y))) stop("X and Y must have the same number of rows", call. = FALSE)

  .source_julia_backend_once()

  args <- list("GIC_Variable_Selection_Boltzmann",
               X, Y, as.integer(Init_Columns),
               JuliaCall::julia_eval(Calculate_GIC), JuliaCall::julia_eval(Calculate_GIC_short),
               T = as.numeric(T), Nsim = as.integer(Nsim), gamma = as.numeric(gamma))
  if (!is.null(k)) args$k <- as.integer(k)

  result <- tryCatch(do.call(JuliaCall::julia_call, args), error = function(e) {
    stop("Error occurred while calling the Julia function: ", conditionMessage(e), call. = FALSE)
  })

  list(
    GIC_list = as.numeric(unlist(result[[1]])),
    GIC_coeff = lapply(result[[2]], as.integer)
  )
}
