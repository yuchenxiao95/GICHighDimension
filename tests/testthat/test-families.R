test_that("LP_to_Y / Y_to_LP / Beta_estimate cover every family", {
  skip_if_no_julia()
  set.seed(5)
  JuliaCall::julia_command("import Random; Random.seed!(5);")  # LP_to_Y draws in Julia
  n <- 10000; p <- 4
  X <- matrix(rnorm(n * p), n, p) / 2
  beta <- c(0.8, -0.5, 0.3, 0)

  fams <- list(Normal = list(), Bernoulli = list(), Binomial = list(n_trials = 5L),
               Poisson = list(), Gamma = list(shape = 3), Exponential = list(),
               InverseGaussian = list(shape = 3), NegativeBinomial = list(theta = 2),
               Geometric = list())
  for (fam in names(fams)) {
    y <- do.call(LP_to_Y, c(list(X, beta, family = fam), fams[[fam]]))
    expect_length(y, n)
    eta <- if (fam == "Binomial") Y_to_LP(y, family = fam, n_trials = 5L) else Y_to_LP(y, family = fam)
    expect_length(eta, n)
    b <- Beta_estimate(X, y, family = fam, n_trials = fams[[fam]]$n_trials)
    expect_equal(b[-1], beta, tolerance = 0.15, info = fam)
  }

  Sig <- matrix(c(1, 0.3, 0.3, 1), 2)
  Ym <- LP_to_Y(X, cbind(beta, beta), family = "MultivariateNormal", cov_matrix = Sig)
  expect_equal(dim(Ym), c(n, 2))

  cats <- LP_to_Y(X, cbind(c(1, 0, 0, 0), c(0, -1, 0, 0)), family = "Multinomial", n_categories = 3)
  expect_true(all(cats %in% 1:3))
  expect_equal(dim(Y_to_LP(cats, family = "Multinomial")), c(n, 2))
})
