#' @title GIC-based Variable Selection
#' @description
#' Perform variable selection using the Generalized Information Criterion (GIC)
#' with Hopfield network optimization. The computationally intensive components are
#' implemented in Julia for performance.
#' @details This function implements a variable selection algorithm that:
#' \itemize{
#'   \item Uses customizable information criteria for model selection
#'   \item Leverages Julia backend for computational efficiency
#'   \item Supports various model families through the underlying Julia implementation
#' }
#' @param X Numeric design matrix (n x p), where n is the number of observations and
#'          p is the number of predictors.
#' @param Y Numeric response vector of length n.
#' @param Initial_Column Integer vector of initial feature indices to consider.
#' @param Calculate_GIC Character name of the Julia function for full GIC calculation.
#' @param Calculate_GIC_short Character name of the Julia shortcut function for approximate GIC.
#' @param Nsim Integer number of simulations to run (default: 1).
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{GIC_values}: Calculated GIC values for all candidate models
#'   \item \code{selected_coeffs}: Indices of selected variables
#' }
#'
#' @usage
#' GICSelection(X, Y, Initial_Column, Calculate_GIC, Calculate_GIC_short, Nsim = 1L)
#'
#' @examples
#' \donttest{
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
#'     # Generate synthetic data
#'     set.seed(123)
#'     n <- 100
#'     p <- 10
#'     k <- 3
#'
#'     X <- matrix(rnorm(n * p), n, p)
#'     true_beta <- c(rep(1.5, k), rep(0, p - k))
#'     Y <- X %*% true_beta + rnorm(n)
#'
#'     # Run selection with Schwarz Information Criterion
#'     result <- GICSelection(
#'       X = X,
#'       Y = Y,
#'       Initial_Column = 1:p,
#'       Calculate_GIC = "Calculate_SIC",
#'       Calculate_GIC_short = "Calculate_SIC_short",
#'       Nsim = 3
#'     )
#'
#'     # Print selected coefficients
#'     print(result$selected_coeffs)
#'   }
#' }
#' }
#'
#' @export
#' @importFrom JuliaCall julia_setup julia_source julia_call julia_assign julia_eval julia_exists
#' @importFrom stats rnorm
#' @importFrom utils packageVersion
GICSelection <- function(X, Y, Initial_Column,
                         Calculate_GIC,
                         Calculate_GIC_short,
                         Nsim = 1L) {

  if (!is.matrix(X)) stop("X must be a matrix")
  if (!is.numeric(Y)) stop("Y must be numeric")
  if (nrow(X) != length(Y)) stop("X and Y dimensions don't match")
  if (any(Initial_Column > ncol(X))) stop("Invalid column indices")

  if (!requireNamespace("JuliaCall", quietly = TRUE)) {
    stop("JuliaCall required. Install with install.packages('JuliaCall')")
  }

  tryCatch({
    JuliaCall::julia_setup(installJulia = FALSE)

    JuliaCall::julia_library("Distributions")
    JuliaCall::julia_library("LinearAlgebra")
    JuliaCall::julia_library("Statistics")

    script_dir <- system.file("julia", package = "GICHighDimension")
    if (script_dir == "") stop("Julia scripts directory not found")

    JuliaCall::julia_source(file.path(script_dir, "penalty_selection.jl"))
    JuliaCall::julia_source(file.path(script_dir, "GIC_Model_Selection.jl"))

    if (!JuliaCall::julia_exists(Calculate_GIC)) {
      stop("Julia function ", Calculate_GIC, " not found")
    }
    if (!JuliaCall::julia_exists(Calculate_GIC_short)) {
      stop("Julia function ", Calculate_GIC_short, " not found")
    }

    JuliaCall::julia_assign("X_matrix", X)
    JuliaCall::julia_assign("Y_vector", Y)
    JuliaCall::julia_assign("init_cols", as.integer(Initial_Column))

    X_jl <- JuliaCall::julia_eval("convert(Matrix{Float64}, X_matrix)")
    Y_jl <- JuliaCall::julia_eval("convert(Vector{Float64}, Y_vector)")
    InitCol_jl <- JuliaCall::julia_eval("convert(Vector{Int64}, init_cols)")

    julia_result <- JuliaCall::julia_call(
      "GIC_Variable_Selection",
      X_jl, Y_jl, InitCol_jl,
      JuliaCall::julia_eval(Calculate_GIC),
      JuliaCall::julia_eval(Calculate_GIC_short),
      Nsim = as.integer(Nsim)
    )

    list(
      GIC_values = julia_result[[1]],
      selected_coeffs = julia_result[[2]]
    )

  }, error = function(e) {
    warning("Julia error occurred: ", e$message)
    list(
      GIC_values = NA,
      selected_coeffs = NA
    )
  })
}
