#' Perform GIC-based Variable Selection Using Julia
#'
#' This function calculates GIC values and corresponding non-zero coefficient indices using
#' Julia functions for performance and flexibility.
#'
#' @param X Design matrix (numeric).
#' @param Y Outcome vector (numeric).
#' @param Initial_Column Initial subset of feature indices (integer vector).
#' @param Calculate_GIC Name of the Julia function to calculate GIC (string).
#' @param Calculate_GIC_short Name of the Julia function for short GIC updates (string).
#' @param Nsim Number of times to repeat the selection process (default is 1).
#'
#' @return A list containing GIC values and corresponding non-zero coefficient indices.
#'
#' @examples
#' # Example usage:
#' X <- matrix(rnorm(100), ncol = 10)  # Random design matrix with 10 features
#' Y <- rnorm(10)  # Random outcome vector
#' result <- GICSelection(X, Y, Initial_Column = 1:5,
#'                        Calculate_GIC = "calculate_GIC",
#'                        Calculate_GIC_short = "calculate_GIC_short")
#' print(result)
#'
#' @export
GICSelection <- function(X, Y, Initial_Column, Calculate_GIC, Calculate_GIC_short, Nsim = 1) {

  # Ensure JuliaCall package is installed and loaded
  if (!requireNamespace("JuliaCall", quietly = TRUE)) {
    stop("The JuliaCall package is required but not installed. Please install it using install.packages('JuliaCall').")
  }
  library(JuliaCall)

  # Initial_Column = init_cols
  # Calculate_GIC = 'Calculate_SIC'
  # Calculate_GIC_short = 'Calculate_SIC_short'

  # Initialize Julia environment
  julia_setup()

  # Convert R inputs to appropriate Julia types
  X_julia <- as.matrix(X)  # Convert design matrix to a Julia-compatible format
  Y_julia <- as.vector(Y)  # Convert outcome vector to a Julia-compatible format
  Nsim <- as.integer(Nsim)  # Ensure Nsim is an integer

  # Source the necessary Julia scripts
  penalty_script <- system.file("julia", "penalty_selection.jl", package = "GICHighDimension")
  if (penalty_script == "") stop("penalty_selection.jl not found. Ensure it's included in the package.")
  julia_source(penalty_script)

  gic_script <- system.file("julia", "GIC_Model_Selection.jl", package = "GICHighDimension")
  if (gic_script == "") stop("GIC_Model_Selection.jl not found. Ensure it's included in the package.")
  julia_source(gic_script)


  # Define Julia functions to calculate GIC and GIC_short
  Calculate_GIC_func <- julia_eval(Calculate_GIC)
  Calculate_GIC_short_func <- julia_eval(Calculate_GIC_short)

  # Try to call the Julia function for GIC variable selection
  result <- tryCatch({
    julia_call(
      "GIC_Variable_Selection",
      X_julia, Y_julia, Initial_Column,
      Calculate_GIC_func, Calculate_GIC_short_func,
      Nsim = Nsim
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

