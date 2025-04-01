#' Perform Boltzmann-based Variable Selection
#'
#' This function performs GIC-based variable selection using a Boltzmann-like
#' simulated annealing approach, which is implemented in Julia. The method
#' uses temperature control for randomness in the variable selection process.
#'
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
#' # Example usage:
#' X <- matrix(rnorm(100), ncol = 10)  # Random design matrix with 10 features
#' Y <- rnorm(10)  # Random outcome vector
#' result <- GICSelectionBoltzmann(X, Y, Init_Columns = 1:5,
#'                                  Calculate_GIC = "Calculate_BIC",
#'                                  Calculate_GIC_short = "Calculate_BIC_short")
#' print(result)
#'
#' @export
GICSelectionBoltzmann <- function(X, Y, Init_Columns, Calculate_GIC, Calculate_GIC_short, T = 0.2, Nsim = 2) {

  # Ensure JuliaCall package is installed and loaded
  if (!requireNamespace("JuliaCall", quietly = TRUE)) {
    stop("The JuliaCall package is required but not installed. Please install it using install.packages('JuliaCall').")
  }
  library(JuliaCall)

  # Initialize Julia environment
  julia_setup()

  # Convert R inputs to appropriate Julia types
  X_julia <- as.matrix(X)  # Convert design matrix to Julia-compatible format
  Y_julia <- as.vector(Y)  # Convert outcome vector to Julia-compatible format
  Nsim <- as.integer(Nsim)  # Ensure Nsim is an integer
  T <- as.numeric(T)  # Ensure T is numeric for the temperature parameter

  # Source the necessary Julia scripts
  penalty_script <- system.file("julia", "penalty_selection.jl", package = "GICModelSelection")
  if (penalty_script == "") stop("penalty_selection.jl not found. Ensure it's included in the package.")
  julia_source(penalty_script)

  gic_boltzmann_script <- system.file("julia", "GIC_Model_Selection_Boltzmann.jl", package = "GICModelSelection")
  if (gic_boltzmann_script == "") stop("GIC_Model_Selection_Boltzmann.jl not found. Ensure it's included in the package.")
  julia_source(gic_boltzmann_script)


  # Define Julia functions to calculate GIC and GIC_short
  Calculate_GIC_func <- julia_eval(Calculate_GIC)
  Calculate_GIC_short_func <- julia_eval(Calculate_GIC_short)

  # Try to call the Julia function for GIC variable selection via Boltzmann approach
  result <- tryCatch({
    julia_call(
      "GIC_Variable_Selection_Boltzmann",
      X_julia, Y_julia, Init_Columns,
      Calculate_GIC_func, Calculate_GIC_short_func,
      T = T, Nsim = Nsim
    )
  }, error = function(e) {
    stop("Error occurred while calling the Julia function: ", e$message)
  })

  # Convert the result from Julia to R
  GIC_list <- result[[1]]  # GIC values
  GIC_coeff <- result[[2]]  # Corresponding coefficients

  # Return the result as a list
  list(
    GIC_list = GIC_list,  # GIC values
    GIC_coeff = GIC_coeff  # Corresponding coefficients
  )
}

