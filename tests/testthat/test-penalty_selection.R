library(testthat)
library(JuliaCall)
library(MASS)

test_that("calculate_aic works with example inputs", {
  Y <- rnorm(10)
  X <- matrix(rnorm(100), nrow = 10)
  result <- calculate_aic(Y, X)
  expect_true(is.list(result))
  expect_true("AIC" %in% names(result))
  expect_true("Inverse" %in% names(result))
})

test_that("calculate_bic works with example inputs", {
  Y <- rnorm(10)
  X <- matrix(rnorm(100), nrow = 10)
  result <- calculate_bic(Y, X)
  expect_true(is.list(result))
  expect_true("BIC" %in% names(result))
  expect_true("Inverse" %in% names(result))
})

test_that("calculate_sic works with example inputs", {
  Y <- rnorm(10)
  X <- matrix(rnorm(100), nrow = 10)
  result <- calculate_sic(Y, X)
  expect_true(is.list(result))
  expect_true("SIC" %in% names(result))
  expect_true("Inverse" %in% names(result))
})

test_that("calculate_caicf works with example inputs", {
  Y <- rnorm(10)
  X <- matrix(rnorm(100), nrow = 10)
  result <- calculate_caicf(Y, X)
  expect_true(is.list(result))
  expect_true("CAICF" %in% names(result))
  expect_true("Inverse" %in% names(result))
})

test_that("calculate_caic works with example inputs", {
  Y <- rnorm(10)
  X <- matrix(rnorm(100), nrow = 10)
  result <- calculate_caic(Y, X)
  expect_true(is.list(result))
  expect_true("CAIC" %in% names(result))
  expect_true("Inverse" %in% names(result))
})
