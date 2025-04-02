library(testthat)
library(JuliaCall)
library(MASS)

test_that("Calculate_AIC works with example inputs", {
  Y <- rnorm(20)
  X <- matrix(rnorm(100), nrow = 20)
  result <- Calculate_AIC(Y, X)
  expect_true(is.list(result))
})

test_that("Calculate_AICc works with example inputs", {
  Y <- rnorm(20)
  X <- matrix(rnorm(100), nrow = 20)
  result <- Calculate_AICc(Y, X)
  expect_true(is.list(result))
})

test_that("Calculate_AttIC works with example inputs", {
  Y <- rnorm(20)
  X <- matrix(rnorm(100), nrow = 20)
  result <- Calculate_AttIC(Y, X)
  expect_true(is.list(result))
})

test_that("Calculate_SIC works with example inputs", {
  Y <- rnorm(20)
  X <- matrix(rnorm(100), nrow = 20)
  result <- Calculate_SIC(Y, X)
  expect_true(is.list(result))
})

test_that("Calculate_BIC works with example inputs", {
  Y <- rnorm(20)
  X <- matrix(rnorm(100), nrow = 20)
  result <- Calculate_BIC(Y, X)
  expect_true(is.list(result))
})

test_that("Calculate_CAIC works with example inputs", {
  Y <- rnorm(20)
  X <- matrix(rnorm(100), nrow = 20)
  result <- Calculate_CAIC(Y, X)
  expect_true(is.list(result))
})

test_that("Calculate_CAICF works with example inputs", {
  Y <- rnorm(20)
  X <- matrix(rnorm(100), nrow = 20)
  result <- Calculate_CAICF(Y, X)
  expect_true(is.list(result))
})

test_that("Calculate_GIC2 works with example inputs", {
  Y <- rnorm(20)
  X <- matrix(rnorm(100), nrow = 20)
  result <- Calculate_GIC2(Y, X)
  expect_true(is.list(result))
})

test_that("Calculate_GIC3 works with example inputs", {
  Y <- rnorm(20)
  X <- matrix(rnorm(100), nrow = 20)
  result <- Calculate_GIC3(Y, X)
  expect_true(is.list(result))
})

test_that("Calculate_GIC4 works with example inputs", {
  Y <- rnorm(20)
  X <- matrix(rnorm(100), nrow = 20)
  result <- Calculate_GIC4(Y, X)
  expect_true(is.list(result))
})

test_that("Calculate_GIC5 works with example inputs", {
  Y <- rnorm(20)
  X <- matrix(rnorm(100), nrow = 20)
  result <- Calculate_GIC5(Y, X)
  expect_true(is.list(result))
})

test_that("Calculate_GIC6 works with example inputs", {
  Y <- rnorm(20)
  X <- matrix(rnorm(100), nrow = 20)
  result <- Calculate_GIC6(Y, X)
  expect_true(is.list(result))
})
