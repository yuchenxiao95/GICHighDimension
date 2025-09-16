#' @title GIC-Based Variable Selection
#'
#' @description
#' Performs variable selection using the Generalized Information Criterion (GIC)
#' with a fast 'Julia' backend. Supports custom ICs (AIC/BIC/ICOMP/EBIC/etc.) and
#' univariate or multivariate Y.
#'
#' @details
#' The Julia backend implements an iterative add/remove search over features.
#' You provide two Julia functions:
#' \itemize{
#'   \item \code{Calculate_GIC(Y, X_subset, P, gamma, Huber)} returning \code{(criterion, inverse)}
#'   \item \code{Calculate_GIC_short(Y, X_subset, Inverse, P, gamma, Huber)} returning \code{criterion}
#' }
#' where \code{P} is the total number of predictors in the full design.
#'
#' @param X Numeric design matrix (n × p).
#' @param Y Numeric response (vector length n, or matrix with n rows).
#' @param Initial_Column Integer vector of initial feature indices.
#' @param Calculate_GIC Character name of the Julia full-IC function.
#' @param Calculate_GIC_short Character name of the Julia short-IC function.
#' @param Nsim Integer number of shuffled passes of features (default 1L).
#' @param gamma Numeric EBIC/penalty tuning parameter forwarded to Julia keywords (default 1.0).
#' @param huber Logical; if TRUE, forwards \code{Huber = true} to Julia (default FALSE).
#'
#' @return A list with:
#' \itemize{
#'   \item \code{GIC_values}: numeric vector trace of criterion values
#'   \item \code{selected_coeffs}: list of integer vectors of chosen columns per step
#' }
#'
#' @usage
#' GICSelection(X, Y, Initial_Column,
#'              Calculate_GIC, Calculate_GIC_short,
#'              Nsim = 1L, gamma = 1.0, huber = FALSE)
#'
#' @examples
#' \dontrun{
#'   if (requireNamespace("JuliaCall", quietly = TRUE)) {
#'     set.seed(1)
#'     X <- matrix(rnorm(50*10), 50, 10)
#'     Y <- rnorm(50)
#'     GICSelection(
#'       X, Y, Initial_Column = 1:3,
#'       Calculate_GIC = "Calculate_EBIC",
#'       Calculate_GIC_short = "Calculate_EBIC_short",
#'       Nsim = 2L, gamma = 0.5, huber = FALSE
#'     )
#'   }
#' }
#' @export
#' @importFrom JuliaCall julia_setup julia_source julia_call julia_assign julia_eval julia_exists
GICSelection <- function(X, Y, Initial_Column,
                         Calculate_GIC,
                         Calculate_GIC_short,
                         Nsim = 1L,
                         gamma = 1.0,
                         huber = FALSE) {

  if (!is.matrix(X)) stop("X must be a matrix")
  if (!(is.numeric(Y) && (is.vector(Y) || is.matrix(Y))))
    stop("Y must be either a numeric vector or a numeric matrix")
  if (nrow(X) != nrow(as.matrix(Y)))
    stop("X and Y must have the same number of rows")
  if (any(Initial_Column > ncol(X)))
    stop("Invalid column indices in Initial_Column")

  if (!requireNamespace("JuliaCall", quietly = TRUE)) {
    stop("JuliaCall required. Install with install.packages('JuliaCall')")
  }

  tryCatch({
    JuliaCall::julia_setup(installJulia = FALSE)

    # Ensure core libs (optional but harmless if already loaded)
    JuliaCall::julia_library("LinearAlgebra")
    JuliaCall::julia_library("Statistics")

    script_dir <- system.file("julia", package = "GICHighDimension")
    if (script_dir == "") stop("Julia scripts directory not found")

    # Load your Julia definitions
    JuliaCall::julia_source(file.path(script_dir, "penalty_selection.jl"))
    JuliaCall::julia_source(file.path(script_dir, "GIC_Model_Selection.jl"))

    if (!JuliaCall::julia_exists(Calculate_GIC))
      stop("Julia function ", Calculate_GIC, " not found")
    if (!JuliaCall::julia_exists(Calculate_GIC_short))
      stop("Julia function ", Calculate_GIC_short, " not found")

    # Convert inputs to Julia
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

    # Build call: positional + keyword args (names are case-sensitive for Julia keywords)
    jl_args <- list(
      "GIC_Variable_Selection",
      X_jl, Y_jl, InitCol_jl,
      JuliaCall::julia_eval(Calculate_GIC),
      JuliaCall::julia_eval(Calculate_GIC_short),
      gamma = as.numeric(gamma),
      Huber = as.logical(huber),
      Nsim  = as.integer(Nsim)
    )

    julia_result <- do.call(JuliaCall::julia_call, jl_args)

    list(
      GIC_values      = julia_result[[1]],
      selected_coeffs = julia_result[[2]]
    )

  }, error = function(e) {
    warning("Julia error occurred: ", e$message)
    list(GIC_values = NA, selected_coeffs = NA)
  })
}
