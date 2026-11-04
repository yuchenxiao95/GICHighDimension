test_that("GICSelection recovers the true support (univariate)", {
  skip_if_no_julia()
  set.seed(123)
  n <- 300; p <- 30
  X <- matrix(rnorm(n * p), n, p)
  beta <- c(2, -1.5, 1, rep(0, p - 3))
  Y <- as.numeric(X %*% beta + rnorm(n))

  res <- GICSelection(X, Y, Initial_Column = 1:5,
                      Calculate_GIC = "Calculate_EBIC",
                      Calculate_GIC_short = "Calculate_EBIC_short",
                      k = 15, Nsim = 3)

  expect_named(res, c("GIC_values", "selected_coeffs"))
  expect_length(res$GIC_values, 3 * p)
  expect_true(all(diff(res$GIC_values) >= -1e-10))      # greedy: never decreases
  expect_setequal(res$selected_coeffs[[length(res$selected_coeffs)]], 1:3)
})

test_that("GICSelection handles a matrix response and respects k", {
  skip_if_no_julia()
  set.seed(1)
  n <- 200; p <- 20
  X <- matrix(rnorm(n * p), n, p)
  B <- matrix(0, p, 3); B[1:4, ] <- 1
  Y <- X %*% B + matrix(rnorm(n * 3), n, 3)

  res <- GICSelection(X, Y, 1:2, "Calculate_SIC", "Calculate_SIC_short", k = 4, Nsim = 2)
  sizes <- vapply(res$selected_coeffs, length, integer(1))
  expect_true(all(sizes <= 3))                           # models of size >= k are skipped
})

test_that("GICSelection fails loudly without k", {
  skip_if_no_julia()
  X <- matrix(rnorm(40), 20, 2)
  expect_error(GICSelection(X, rnorm(20), 1, "Calculate_AIC", "Calculate_AIC_short"), "k")
})

test_that("GICSelectionBoltzmann runs with criteria that use P", {
  skip_if_no_julia()
  set.seed(2)
  X <- matrix(rnorm(200 * 10), 200, 10)
  Y <- as.numeric(X[, 1:2] %*% c(2, 2) + rnorm(200))
  res <- GICSelectionBoltzmann(X, Y, 1:2, "Calculate_EBIC", "Calculate_EBIC_short",
                               T = 0.01, Nsim = 2)
  expect_length(res$GIC_list, 20)
})

test_that("GICSelection works when p > n and never exceeds n - 1 variables", {
  skip_if_no_julia()
  set.seed(6)
  n <- 40; p <- 200
  X <- matrix(rnorm(n * p), n, p)
  Y <- as.numeric(X[, 1:3] %*% c(3, -2, 2) + rnorm(n))
  res <- GICSelection(X, Y, 1:10, "Calculate_AIC", "Calculate_AIC_short", k = 1000, Nsim = 2)
  expect_type(res$selected_coeffs, "list")
  sizes <- vapply(res$selected_coeffs, length, integer(1))
  expect_true(all(sizes <= n - 1))
  expect_true(all(is.finite(res$GIC_values)))
})
