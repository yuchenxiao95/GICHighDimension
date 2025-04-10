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
#' @param Y A numeric response vector (length \code{n}) or matrix compatible with \code{X}.
#' @param Init_Columns Integer vector of indices specifying the initial set of predictors.
#' @param Calculate_GIC Character string giving the name of the 'Julia' function to compute full GIC values.
#' @param Calculate_GIC_short Character string giving the name of the 'Julia' function to compute approximate GIC values.
#' @param T Numeric scalar. The temperature parameter controlling stochasticity in the simulated annealing process (default: 0.1).
#' @param Nsim Integer. Number of repeated runs for the optimization procedure (default: 5).
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
                                  Nsim = 5) {
  if (!requireNamespace("JuliaCall", quietly = TRUE)) {
    stop("The 'JuliaCall' package is required. Please install it with install.packages('JuliaCall').")
  }

  JuliaCall::julia_setup(installJulia = FALSE)

  X_julia <- as.matrix(X)
  Y_julia <- as.vector(Y)
  Nsim <- as.integer(Nsim)
  T <- as.numeric(T)

  penalty_script <- system.file("julia", "penalty_selection.jl", package = "GICHighDimension")
  if (penalty_script == "") stop("penalty_selection.jl not found.")
  JuliaCall::julia_source(penalty_script)

  gic_boltzmann_script <- system.file("julia", "GIC_Model_Selection_Boltzmann.jl", package = "GICHighDimension")
  if (gic_boltzmann_script == "") stop("GIC_Model_Selection_Boltzmann.jl not found.")
  JuliaCall::julia_source(gic_boltzmann_script)

  Calculate_GIC_func <- JuliaCall::julia_eval(Calculate_GIC)
  Calculate_GIC_short_func <- JuliaCall::julia_eval(Calculate_GIC_short)

  result <- tryCatch({
    JuliaCall::julia_call(
      "GIC_Variable_Selection_Boltzmann",
      X_julia, Y_julia, Init_Columns,
      Calculate_GIC_func, Calculate_GIC_short_func,
      T = T, Nsim = Nsim
    )
  }, error = function(e) {
    stop("Error occurred while calling the Julia function: ", e$message)
  })

  list(
    GIC_list = result[[1]],
    GIC_coeff = result[[2]]
  )
}
