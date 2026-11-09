# Regenerates the search-path figures in man/figures/ used by the
# "More examples" section of README.md and by vignettes/examples.Rmd
# (README-search.png, for the quick-start example, is not made here). Run from the package root:
#   Rscript data-raw/readme_figures.R
# The code repeats the README examples in the same order with the same seeds,
# so the figures match the printed output.

devtools::load_all(".", quiet = TRUE)
setup_julia()

set_seeds <- function(seed) {
  set.seed(seed)
  JuliaCall::julia_command(sprintf("import Random; Random.seed!(%d);", seed))
}

# Two panels: criterion and model size after each search step. Dotted lines
# mark the start of each new pass over the predictors.
plot_search <- function(fit, p, true_size, criterion, name) {
  file <- file.path("man", "figures", paste0("README-", name, ".png"))
  png(file, width = 8, height = 3.1, units = "in", res = 200)
  on.exit(dev.off())
  op <- par(mfrow = c(1, 2), mar = c(4.2, 4.4, 2.2, 1), las = 1,
            cex.axis = 0.95, cex.lab = 1.1, cex.main = 1.1)
  on.exit(par(op), add = TRUE)

  steps  <- seq_along(fit$GIC_values)
  size   <- lengths(fit$selected_coeffs)
  passes <- seq(p, length(steps) - 1, by = p)

  plot(steps, fit$GIC_values, type = "s", lwd = 2.2, col = "#3B6FB6",
       xlab = "Search step", ylab = "", main = "Criterion along the search")
  title(ylab = criterion, line = 3.3)
  abline(v = passes, lty = 3, col = "grey55")

  # Log scale when the search starts from a large model, so the end of the
  # path near the true size stays visible.
  log_y <- max(size) > 50
  plot(steps, size, type = "s", lwd = 2.2, col = "#B5443A",
       ylim = range(c(size, true_size)), log = if (log_y) "y" else "",
       xlab = "Search step", ylab = "",
       main = sprintf("Model size (true size = %d)", true_size))
  title(ylab = if (log_y) "Variables in model (log scale)" else "Variables in model",
        line = 3.3)
  abline(v = passes, lty = 3, col = "grey55")
  abline(h = true_size, lty = 2, col = "grey40")
  message("wrote ", file)
}

## ---- Linear model, p = 500 ------------------------------------------------
set_seeds(1)
n <- 1000; p <- 500
X <- matrix(rnorm(n * p), n, p)
true_cols <- sort(sample(p, 5))
beta <- numeric(p); beta[true_cols] <- 2
Y <- LP_to_Y(X, beta, family = "Normal", std = sqrt(sum(beta^2) / 0.71))

fit <- GICSelection(X, Y, 1:p, "Calculate_BIC", "Calculate_BIC_short",
                    k = p, Nsim = 5)
plot_search(fit, p, 5, "BIC", "linear-bic")

fit_ebic <- GICSelection(X, Y, 1:10, "Calculate_EBIC", "Calculate_EBIC_short",
                         k = 50, Nsim = 5)
plot_search(fit_ebic, p, 5, "EBIC", "linear-ebic")

## ---- Multivariate normal ---------------------------------------------------
m <- 5
B <- matrix(0, p, m)
true_by_response <- vector("list", m)
for (i in 1:m) {
  cols <- sort(sample(p, 5))
  true_by_response[[i]] <- cols
  B[cols, i] <- seq(10, 0.1, length.out = 5)
}
Y_multi <- LP_to_Y(X, B, family = "MultivariateNormal", cov_matrix = diag(m))
fit_multi <- GICSelection(X, Y_multi, 1:p, "Calculate_SIC", "Calculate_SIC_short",
                          k = p, Nsim = 5)
plot_search(fit_multi, p, length(unique(unlist(true_by_response))), "SIC",
            "multivariate")

## ---- GLM families -----------------------------------------------------------
set_seeds(2)
n <- 1000; p <- 200
X <- matrix(rnorm(n * p), n, p)
true_cols <- c(3, 17, 42, 88, 150)
beta <- numeric(p); beta[true_cols] <- c(1, -1, 0.8, -0.8, 0.6)

glm_fit <- function(Y, ...) {
  GICSelection(X, Y_to_LP(Y, ...), 1:10,
               "Calculate_EBIC", "Calculate_EBIC_short", k = 50, Nsim = 3)
}

y <- LP_to_Y(X, beta, family = "Bernoulli")
plot_search(glm_fit(y, family = "Bernoulli"), p, 5, "EBIC", "bernoulli")

y <- LP_to_Y(X, beta, family = "Binomial", n_trials = 10)
plot_search(glm_fit(y, family = "Binomial", n_trials = 10), p, 5, "EBIC", "binomial")

y <- LP_to_Y(X, beta, family = "Poisson")
plot_search(glm_fit(y, family = "Poisson"), p, 5, "EBIC", "poisson")

y <- LP_to_Y(X, beta, family = "NegativeBinomial", theta = 2)
plot_search(glm_fit(y, family = "NegativeBinomial"), p, 5, "EBIC", "negbin")

y <- LP_to_Y(X, beta, family = "Gamma", shape = 2)
plot_search(glm_fit(y, family = "Gamma"), p, 5, "EBIC", "gamma")

y <- LP_to_Y(X, beta, family = "InverseGaussian", shape = 2)
plot_search(glm_fit(y, family = "InverseGaussian"), p, 5, "EBIC", "invgauss")

B_mn <- matrix(0, p, 2)
B_mn[true_cols, 1] <- 1.5
B_mn[true_cols, 2] <- c(-1.5, 1.5, -1.5, 1.5, -1.5)
y <- LP_to_Y(X, B_mn, family = "Multinomial", n_categories = 3)
plot_search(glm_fit(y, family = "Multinomial", n_categories = 3), p, 5, "EBIC",
            "multinomial")
