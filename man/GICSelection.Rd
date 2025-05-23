% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GICSelection.R
\name{GICSelection}
\alias{GICSelection}
\title{GIC-Based Variable Selection}
\usage{
GICSelection(X, Y, Initial_Column, Calculate_GIC, Calculate_GIC_short, Nsim = 1L)
}
\arguments{
\item{X}{Numeric design matrix (n x p), where n is the number of observations and
p is the number of predictors.}

\item{Y}{Numeric response (either a vector of length n or a matrix with n rows).}

\item{Initial_Column}{Integer vector of initial feature indices to consider.}

\item{Calculate_GIC}{Character name of the 'Julia' function for full GIC calculation.}

\item{Calculate_GIC_short}{Character name of the 'Julia' shortcut function for approximate GIC.}

\item{Nsim}{Integer number of simulations to run (default: 1).}
}
\value{
A list containing:
\itemize{
\item \code{GIC_values}: Calculated GIC values for all candidate models
\item \code{selected_coeffs}: Indices of selected variables
}
}
\description{
Performs variable selection using the Generalized Information Criterion (GIC)
with Hopfield Network optimization. The computationally intensive components
are implemented in 'Julia' for performance, while an 'R' interface provides user-friendly access.
}
\details{
This function implements a variable selection algorithm that:
\itemize{
\item Uses customizable information criteria (such as AIC, BIC, and ICOMP)
\item Leverages a 'Julia' backend for computational efficiency
\item Supports various model families, including generalized linear models (GLMs) such as the normal and Poisson families
\item Accepts both univariate and multivariate responses
}
}
\examples{
\dontrun{
if (requireNamespace("JuliaCall", quietly = TRUE)) {
  julia_available <- FALSE
  tryCatch({
    JuliaCall::julia_setup(installJulia = FALSE)
    julia_available <- TRUE
  }, error = function(e) {
    message("Julia not available: ", e$message)
  })

  if (julia_available) {
    set.seed(123)
    n <- 100; p <- 10; k <- 3
    X <- matrix(rnorm(n * p), n, p)
    beta <- c(rep(1.5, k), rep(0, p - k))
    Y_uni <- LP_to_Y(X, beta, family = "Normal", std = 1.0)

    result_uni <- GICSelection(
      X = X,
      Y = Y_uni,
      Initial_Column = 1:p,
      Calculate_GIC = "Calculate_SIC",
      Calculate_GIC_short = "Calculate_SIC_short",
      Nsim = 3
    )
    print(result_uni$selected_coeffs)

    m <- 3
    multi_beta <- matrix(0, p, m)
    multi_beta[1:3, ] <- 1
    rho <- 0.2
    cov_p <- matrix(rho, nrow = m, ncol = m)
    diag(cov_p) <- 1.0
    Y_multi <- LP_to_Y(X, multi_beta, family = "MultivariateNormal", cov_matrix = cov_p)

    result_multi <- GICSelection(
      X = X,
      Y = Y_multi,
      Initial_Column = 1:p,
      Calculate_GIC = "Calculate_SIC",
      Calculate_GIC_short = "Calculate_SIC_short",
      Nsim = 3
    )
    print(result_multi$selected_coeffs)
  }
}
}
}
