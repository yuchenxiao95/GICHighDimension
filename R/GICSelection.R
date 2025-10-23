#' @title GIC-Based Variable Selection
#'
#' @description
#' Performs variable selection using the Generalized Information Criterion (GIC)
#' with Hopfield Network optimization. Heavy computations are delegated to Julia
#' (via {JuliaCall}); this R wrapper prepares data and function handles.
#'
#' @details
#' You may pass any Julia criterion pair by name via `Calculate_GIC` and
#' `Calculate_GIC_short`. This function automatically detects whether the
#' criterion needs the candidate-pool size `P` (true for EBIC and GIC2–GIC6),
#' and, if so, wraps the Julia functions in closures that capture `P`
#' (default `P = ncol(X)`).
#'
#' @param X Numeric matrix (n × p) of predictors.
#' @param Y Numeric response (vector length n, or matrix with n rows).
#' @param Initial_Column Integer vector of initial feature indices.
#' @param Calculate_GIC Character: name of the Julia **full** criterion, e.g. "Calculate_BIC".
#' @param Calculate_GIC_short Character: name of the Julia **short** criterion, e.g. "Calculate_BIC_short".
#' @param Nsim Integer simulations to run (default 1).
#' @param P Optional integer total number of candidate predictors used by EBIC and GIC2–GIC6
#'   (defaults to `ncol(X)`). Ignored for criteria that do not use `P`.
#'
#' @return A list with
#' \itemize{
#'   \item \code{GIC_values}: criterion trajectory (as returned by Julia)
#'   \item \code{selected_coeffs}: indices of selected variables
#' }
#'
#' @export
#' @importFrom JuliaCall julia_setup julia_source julia_call julia_assign julia_eval julia_exists julia_library
GICSelection <- function(X, Y, Initial_Column,
                         Calculate_GIC,
                         Calculate_GIC_short,
                         Nsim = 1L,
                         P = ncol(X)) {
  
  ## --------- Basic checks ---------
  if (!is.matrix(X)) stop("X must be a matrix")
  if (!(is.numeric(Y) && (is.vector(Y) || is.matrix(Y)))) {
    stop("Y must be either a numeric vector or a numeric matrix")
  }
  if (nrow(X) != nrow(as.matrix(Y))) stop("X and Y must have the same number of rows")
  if (any(Initial_Column > ncol(X))) stop("Invalid column indices in Initial_Column")
  
  if (!requireNamespace("JuliaCall", quietly = TRUE)) {
    stop("Package 'JuliaCall' is required. Install with install.packages('JuliaCall').")
  }
  
  ## --------- Start Julia and source package Julia files ---------
  JuliaCall::julia_setup(installJulia = FALSE)
  JuliaCall::julia_library("LinearAlgebra")
  JuliaCall::julia_library("Statistics")
  JuliaCall::julia_library("Distributions")
  
  jl_dir <- system.file("julia", package = "GICHighDimension")
  if (jl_dir == "" || !dir.exists(jl_dir)) {
    stop("Julia scripts directory not found in the package (inst/julia).")
  }
  # Load in a safe order so all symbols are defined
  for (f in c("penalty_selection.jl", "GIC_Calculation.jl", "GIC_Model_Selection.jl")) {
    fp <- file.path(jl_dir, f)
    if (file.exists(fp)) JuliaCall::julia_source(fp)
  }
  
  ## --------- Helper: does this criterion need P? ---------
  needs_P <- function(fname) {
    # EBIC and GIC2..GIC6 (case-insensitive) require P
    fn <- tolower(fname)
    any(fn %in% c("calculate_ebic",
                  "calculate_gic2","calculate_gic3","calculate_gic4","calculate_gic5","calculate_gic6"))
  }
  
  ## --------- Build Julia callables (wrap with P if needed) ---------
  # Verify the base functions exist in Julia
  if (!JuliaCall::julia_exists(Calculate_GIC)) {
    stop("Julia function '", Calculate_GIC, "' not found. Is it defined/loaded?")
  }
  if (!JuliaCall::julia_exists(Calculate_GIC_short)) {
    stop("Julia function '", Calculate_GIC_short, "' not found. Is it defined/loaded?")
  }
  
  # Construct a Julia anonymous function that either calls f(Y,X)
  # or f(Y,X,P), and similarly for the _short(Y,X,Inv) or _short(Y,X,Inv,P).
  make_full_fun <- function(fname, P, useP) {
    if (useP) {
      JuliaCall::julia_eval(sprintf("(Y,X)->%s(Y, X, %d)", fname, as.integer(P)))
    } else {
      JuliaCall::julia_eval(sprintf("(Y,X)->%s(Y, X)", fname))
    }
  }
  make_short_fun <- function(fname_short, P, useP) {
    if (useP) {
      JuliaCall::julia_eval(sprintf("(Y,X,Inv)->%s(Y, X, Inv, %d)", fname_short, as.integer(P)))
    } else {
      JuliaCall::julia_eval(sprintf("(Y,X,Inv)->%s(Y, X, Inv)", fname_short))
    }
  }
  
  useP_full  <- needs_P(Calculate_GIC)
  useP_short <- needs_P(gsub("_short$", "", Calculate_GIC_short, ignore.case = TRUE)) ||
    grepl("ebic|gic[2-6]", Calculate_GIC_short, ignore.case = TRUE)
  
  f_full  <- make_full_fun(Calculate_GIC,       P, useP_full)
  f_short <- make_short_fun(Calculate_GIC_short, P, useP_short)
  
  ## --------- Move data to Julia (typed) ---------
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
  
  ## --------- Call Julia driver ---------
  jr <- JuliaCall::julia_call(
    "GIC_Variable_Selection",
    X_jl, Y_jl, InitCol_jl,
    f_full, f_short,
    Nsim = as.integer(Nsim)
  )
  
  ## --------- Return ---------
  list(
    GIC_values     = jr[[1]],
    selected_coeffs = jr[[2]]
  )
}
