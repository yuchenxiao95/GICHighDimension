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
#' @param k Integer tuning/control parameter passed to the Julia driver.
#' @param Nsim Integer number of simulations to run (default: 1).
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{GIC_values}: Calculated GIC values for all candidate models
#'   \item \code{selected_coeffs}: Indices of selected variables
#' }
#'
#' @usage
#' GICSelection(X, Y, Initial_Column, Calculate_GIC, Calculate_GIC_short, k, Nsim = 1L)
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
GICSelection <- function(X, Y, Initial_Column,
                         Calculate_GIC,
                         Calculate_GIC_short,
                         k,
                         Nsim = 1L) {
  
  ## ---- fast, strict prechecks & coercions ----
  if (!is.matrix(X)) stop("X must be a matrix")
  storage.mode(X) <- "double"
  if (!(is.numeric(Y) && (is.vector(Y) || is.matrix(Y)))) {
    stop("Y must be either a numeric vector or a numeric matrix")
  }
  if (is.vector(Y)) Y <- as.numeric(Y)           # keep vector if univariate
  if (is.matrix(Y)) storage.mode(Y) <- "double"
  if (nrow(X) != nrow(as.matrix(Y))) stop("X and Y must have the same number of rows")
  if (any(Initial_Column < 1L) || any(Initial_Column > ncol(X))) {
    stop("Invalid column indices in Initial_Column")
  }
  Initial_Column <- as.integer(Initial_Column)
  k   <- as.integer(k)
  Nsim <- as.integer(Nsim)
  
  if (!requireNamespace("JuliaCall", quietly = TRUE)) {
    stop("JuliaCall required. Install with install.packages('JuliaCall')")
  }
  
  tryCatch({
    ## ---- initialize Julia (no auto-install to keep it fast/predictable) ----
    JuliaCall::julia_setup(installJulia = FALSE)
    
    ## Light libs; avoid loading too much
    JuliaCall::julia_library("LinearAlgebra")
    JuliaCall::julia_library("Statistics")
    # Distributions only needed if your LP_to_Y examples are run, otherwise skip
    # JuliaCall::julia_library("Distributions")
    
    ## ---- source your package's Julia backend ----
    script_dir <- system.file("julia", package = "GICHighDimension")
    if (script_dir == "") stop("Julia scripts directory not found")
    JuliaCall::julia_source(file.path(script_dir, "penalty_selection.jl"))
    JuliaCall::julia_source(file.path(script_dir, "GIC_Model_Selection.jl"))
    
    ## ---- install arity adapters once (idempotent) ----
    JuliaCall::julia_eval(
      "
      if !isdefined(Main, :_gic_wrappers_installed) || !_gic_wrappers_installed
          for f in (:Calculate_AIC, :Calculate_AIC_c, :Calculate_SIC, :Calculate_BIC,
                    :Calculate_CAIC, :Calculate_CAICF, :Calculate_AttIC)
              if isdefined(Main, f) && !hasmethod(getfield(Main, f),
                       Tuple{Union{AbstractVector,AbstractMatrix}, AbstractMatrix, Integer})
                  @eval begin
                      $f(Y::Union{AbstractVector,AbstractMatrix}, X::AbstractMatrix, ::Integer) = $f(Y, X)
                  end
              end
          end

          for f in (:Calculate_AIC_short, :Calculate_AIC_c_short, :Calculate_SIC_short, :Calculate_BIC_short,
                    :Calculate_CAIC_short, :Calculate_CAICF_short, :Calculate_AttIC_short)
              if isdefined(Main, f) && !hasmethod(getfield(Main, f),
                       Tuple{Union{AbstractVector,AbstractMatrix}, AbstractMatrix, AbstractMatrix, Integer})
                  @eval begin
                      $f(Y::Union{AbstractVector,AbstractMatrix}, X::AbstractMatrix, Inv::AbstractMatrix, ::Integer) = $f(Y, X, Inv)
                  end
              end
          end

          const _gic_wrappers_installed = true
      end
      "
    )
    
    ## ---- resolve metric functions by name once, as callables ----
    # Validate existence in Julia Main
    if (!JuliaCall::julia_exists(Calculate_GIC)) {
      stop("Julia function ", Calculate_GIC, " not found")
    }
    if (!JuliaCall::julia_exists(Calculate_GIC_short)) {
      stop("Julia function ", Calculate_GIC_short, " not found")
    }
    # Get callable function objects
    f_full  <- JuliaCall::julia_eval(Calculate_GIC)
    f_short <- JuliaCall::julia_eval(Calculate_GIC_short)
    
    ## ---- call Julia driver directly with R objects (no manual assign/convert) ----
    # JuliaCall converts R matrix/vector -> Julia Array{Float64} eagerly & reuses the object during the call
    julia_result <- JuliaCall::julia_call(
      "GIC_Variable_Selection",
      X, Y, Initial_Column,
      f_full, f_short, k,
      Nsim = Nsim
    )
    
    list(
      GIC_values = julia_result[[1]],
      selected_coeffs = julia_result[[2]]
    )
    
  }, error = function(e) {
    warning("Julia error occurred: ", e$message)
    list(GIC_values = NA, selected_coeffs = NA)
  })
}
