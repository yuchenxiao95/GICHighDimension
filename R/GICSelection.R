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
#' @param Calculate_GIC_short Character name of the 'Julia' shortcut function for approximate GIC.
#' @param Nsim Integer number of simulations to run (default: 1).
#' @param n Optional integer sample size to forward to the Julia backend (e.g., for EBIC).
#' @param gamma Optional numeric EBIC/penalty tuning parameter to forward to the Julia backend.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{GIC_values}: Calculated GIC values for all candidate models
#'   \item \code{selected_coeffs}: Indices of selected variables
#' }
#'
#' @usage
#' GICSelection(X, Y, Initial_Column, Calculate_GIC, Calculate_GIC_short, Nsim = 1L, n = NULL, gamma = NULL)
#'
#' @examples
#' \dontrun{
#' # (examples unchanged)
#' }
#' @export
#' @importFrom JuliaCall julia_setup julia_source julia_call julia_assign julia_eval julia_exists
#' @importFrom stats rnorm
#' @importFrom utils packageVersion
GICSelection <- function(X, Y, Initial_Column,
                         Calculate_GIC,
                         Calculate_GIC_short,
                         Nsim = 1L,
                         n = NULL,
                         gamma = NULL) {

  if (!is.matrix(X)) stop("X must be a matrix")
  if (!(is.numeric(Y) && (is.vector(Y) || is.matrix(Y)))) {
    stop("Y must be either a numeric vector or a numeric matrix")
  }
  if (nrow(X) != nrow(as.matrix(Y))) stop("X and Y must have the same number of rows")
  if (any(Initial_Column > ncol(X))) stop("Invalid column indices in Initial_Column")

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
    Y_jl <- if (is.matrix(Y)) {
      JuliaCall::julia_eval("convert(Matrix{Float64}, Y_vector)")
    } else {
      JuliaCall::julia_eval("convert(Vector{Float64}, Y_vector)")
    }
    InitCol_jl <- JuliaCall::julia_eval("convert(Vector{Int64}, init_cols)")

    # Build argument list so we only pass n/gamma if provided
    jl_args <- list(
      "GIC_Variable_Selection",
      X_jl, Y_jl, InitCol_jl,
      JuliaCall::julia_eval(Calculate_GIC),
      JuliaCall::julia_eval(Calculate_GIC_short),
      Nsim = as.integer(Nsim)
    )
    if (!is.null(n)) jl_args$n <- as.integer(n)
    if (!is.null(gamma)) jl_args$gamma <- as.numeric(gamma)

    julia_result <- do.call(JuliaCall::julia_call, jl_args)

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
