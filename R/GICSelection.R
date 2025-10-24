#' @title GIC-Based Variable Selection
#' @description Performs variable selection using the Generalized Information Criterion (GIC)
#' with Hopfield Network optimization. Heavy computations are delegated to Julia (via {JuliaCall}).
#' @details You pass Julia criterion names via `Calculate_GIC` and `Calculate_GIC_short`.
#' The candidate-pool size `P` is computed internally in Julia. The tuning integer `k`
#' (e.g. target sparsity) is passed positionally to Julia.
#' @param X Numeric matrix (n × p) of predictors.
#' @param Y Numeric response (vector length n, or matrix with n rows).
#' @param Initial_Column Integer vector of initial feature indices (1-based).
#' @param Calculate_GIC Character: name of the Julia full criterion, e.g. "Calculate_BIC".
#' @param Calculate_GIC_short Character: name of the Julia short criterion, e.g. "Calculate_BIC_short".
#' @param k Integer control parameter consumed by the Julia driver (e.g., target sparsity).
#' @param Nsim Integer simulations to run (default 1).
#' @return A list with \code{GIC_values} and \code{selected_coeffs}.
#' @export
#' @importFrom JuliaCall julia_setup julia_source julia_call julia_assign julia_eval julia_exists julia_library
GICSelection <- function(X, Y, Initial_Column,
                         Calculate_GIC,
                         Calculate_GIC_short,
                         k,
                         Nsim = 1L) {
  
  ## --------- Basic checks ---------
  if (!is.matrix(X)) stop("X must be a matrix")
  if (!(is.numeric(Y) && (is.vector(Y) || is.matrix(Y))))
    stop("Y must be either a numeric vector or a numeric matrix")
  if (nrow(X) != nrow(as.matrix(Y)))
    stop("X and Y must have the same number of rows")
  
  if (!is.numeric(Initial_Column))
    stop("Initial_Column must be numeric/integer indices (1-based)")
  Initial_Column <- as.integer(Initial_Column)
  if (any(Initial_Column < 1L | Initial_Column > ncol(X)))
    stop("Invalid column indices in Initial_Column")
  
  if (missing(k) || length(k) != 1L || !is.numeric(k))
    stop("Argument 'k' must be a single numeric/integer value")
  k <- as.integer(k)
  if (k < 1L) stop("'k' must be >= 1")
  
  if (!requireNamespace("JuliaCall", quietly = TRUE))
    stop("Package 'JuliaCall' is required. Install with install.packages('JuliaCall').")
  
  ## --------- Start Julia and source package Julia files ---------
  JuliaCall::julia_setup(installJulia = FALSE)
  JuliaCall::julia_library("LinearAlgebra")
  JuliaCall::julia_library("Statistics")
  JuliaCall::julia_library("Distributions")
  
  jl_dir <- system.file("julia", package = "GICHighDimension")
  if (jl_dir == "" || !dir.exists(jl_dir))
    stop("Julia scripts directory not found in the package (inst/julia).")
  
  for (f in c("penalty_selection.jl", "GIC_Calculation.jl", "GIC_Model_Selection.jl")) {
    fp <- file.path(jl_dir, f)
    if (file.exists(fp)) JuliaCall::julia_source(fp)
  }
  
  ## --------- Verify Julia criterion functions exist ---------
  if (!JuliaCall::julia_exists(Calculate_GIC))
    stop("Julia function '", Calculate_GIC, "' not found. Is it defined/loaded?")
  if (!JuliaCall::julia_exists(Calculate_GIC_short))
    stop("Julia function '", Calculate_GIC_short, "' not found. Is it defined/loaded?")
  
  ## --------- Build Julia anonymous functions (avoid RCall.RFunction) ---------
  make_full_fun <- function(jl_name) {
    JuliaCall::julia_eval(
      sprintf('(Y,X)->(getfield(Main, Symbol("%s")))(Y, X)', jl_name)
    )
  }
  make_short_fun <- function(jl_name) {
    JuliaCall::julia_eval(
      sprintf('(Y,X,Inv)->(getfield(Main, Symbol("%s")))(Y, X, Inv)', jl_name)
    )
  }
  
  f_full  <- make_full_fun(Calculate_GIC)
  f_short <- make_short_fun(Calculate_GIC_short)
  
  ## --------- Move data to Julia (typed) ---------
  JuliaCall::julia_assign("X_matrix", X)
  JuliaCall::julia_assign("Y_vector", Y)
  JuliaCall::julia_assign("init_cols", Initial_Column)
  
  X_jl       <- JuliaCall::julia_eval("convert(Matrix{Float64}, X_matrix)")
  Y_jl       <- if (is.matrix(Y)) JuliaCall::julia_eval("convert(Matrix{Float64}, Y_vector)")
  else               JuliaCall::julia_eval("convert(Vector{Float64}, Y_vector)")
  InitCol_jl <- JuliaCall::julia_eval("convert(Vector{Int64}, init_cols)")
  
  ## --------- Call Julia driver ---------
  jr <- JuliaCall::julia_call(
    "GIC_Variable_Selection",
    X_jl, Y_jl, InitCol_jl,
    f_full, f_short,                # pass Julia Functions, not R closures
    k,                              # positional k :: Int
    Nsim = as.integer(Nsim)         # keyword Nsim
  )
  
  ## --------- Return ---------
  list(
    GIC_values      = jr[[1]],
    selected_coeffs = jr[[2]]
  )
}
