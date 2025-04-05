#' @title GIC Variable Selection with Boltzmann Machine
#' @description This function performs GIC-based variable selection using a Boltzmann-like
#' simulated annealing approach, which is implemented in Julia.
#' @details The method uses temperature control for randomness in the variable selection process.
#' @param X Design matrix (numeric).
#' @param Y Outcome vector (numeric).
#' @param Init_Columns Initial subset of feature indices (integer vector).
#' @param Calculate_GIC Name of the Julia function to calculate GIC (string).
#' @param Calculate_GIC_short Name of the Julia function for short GIC updates (string).
#' @param T Temperature parameter controlling the randomness of updates (default 0.2).
#' @param Nsim Number of times to repeat the selection process (default 2).
#'
#' @return A list containing GIC values and corresponding coefficients.
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
#'     # Run Boltzmann GIC selection
#'     result <- GICSelectionBoltzmann(
#'       X = X,
#'       Y = Y,
#'       Init_Columns = 1:p,
#'       Calculate_GIC = "Calculate_SIC",
#'       Calculate_GIC_short = "Calculate_SIC_short",
#'       Nsim = 3
#'     )
#'
#'     # Print selected coefficients
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
                                  T = 0.2,
                                  Nsim = 2) {
  if (!requireNamespace("JuliaCall", quietly = TRUE)) {
    stop("The JuliaCall package is required but not installed. Please install it using install.packages('JuliaCall').")
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
