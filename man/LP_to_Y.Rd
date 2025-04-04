% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LPtoY.R
\name{LP_to_Y}
\alias{LP_to_Y}
\title{Transform Linear Predictor to Response}
\usage{
LP_to_Y(
  X,
  true_beta,
  family = "Normal",
  n_trials = NULL,
  std = NULL,
  shape = NULL,
  cov_matrix = NULL,
  n_categories = NULL
)
}
\arguments{
\item{X}{Design matrix}

\item{true_beta}{True coefficient vector}

\item{family}{Character string specifying distribution family
(e.g., "Normal", "Poisson", "MultivariateNormal", etc.)}

\item{n_trials}{Optional number of trials for Binomial family}

\item{std}{Optional standard deviation for Normal family}

\item{shape}{Optional shape parameter for Gamma family}

\item{cov_matrix}{Optional covariance matrix (for Multivariate Normal)}

\item{n_categories}{Optional number of categories (for Multinomial family)}
}
\value{
Numeric vector or matrix of transformed responses
}
\description{
Convert observed responses to the appropriate distribution scale
using canonical link functions (e.g., Gaussian, Poisson).
}
\details{
Uses a Julia backend for fast transformation. Handles:
\itemize{
\item Gaussian ("Normal")
\item Bernoulli
\item Poisson
\item Gamma
\item Multivariate Normal
\item (Optionally) Multinomial
}
}
\examples{
\dontrun{
if (requireNamespace("JuliaCall", quietly = TRUE)) {
  julia_available <- FALSE
  tryCatch({
    JuliaCall::julia_setup()
    julia_available <- TRUE
  }, error = function(e) {
    message("Julia not available: ", e$message)
  })

  if (julia_available) {
    set.seed(123)
    n <- 100
    p <- 5
    X <- matrix(rnorm(n * p), n, p)
    true_beta <- c(1.5, -2, 0, 0, 3)
    Y <- LP_to_Y(X, true_beta, family = "Normal")
    head(Y)
  }
}
}

}
