library(testthat)
library(JuliaCall)
library(MASS)

test_that("Calculate_AIC works with example inputs", {
  Y <- rnorm(10)
  X <- matrix(rnorm(100), nrow = 10)
  result <- Calculate_AIC(Y, X)
  expect_true(is.list(result))
  expect_true("AIC" %in% names(result))
  expect_true("Inverse" %in% names(result))
  expect_true(is.numeric(result$AIC))
  expect_true(is.matrix(result$Inverse))
})

test_that("Calculate_AICc works with example inputs", {
  Y <- rnorm(10)
  X <- matrix(rnorm(100), nrow = 10)
  result <- Calculate_AICc(Y, X)
  expect_true(is.list(result))
  expect_true("AIC_c" %in% names(result))
  expect_true("Inverse" %in% names(result))
  expect_true(is.numeric(result$AIC_c))
})

test_that("Calculate_AttIC works with example inputs", {
  Y <- rnorm(10)
  X <- matrix(rnorm(100), nrow = 10)
  result <- Calculate_AttIC(Y, X)
  expect_true(is.list(result))
  expect_true("AttIC" %in% names(result))
  expect_true("Inverse" %in% names(result))
  expect_true(is.numeric(result$AttIC))
})

test_that("Calculate_SIC works with example inputs", {
  Y <- rnorm(10)
  X <- matrix(rnorm(100), nrow = 10)
  result <- Calculate_SIC(Y, X)
  expect_true(is.list(result))
  expect_true("SIC" %in% names(result))
  expect_true("Inverse" %in% names(result))
  expect_true(is.numeric(result$SIC))
})

test_that("Calculate_BIC works with example inputs", {
  Y <- rnorm(10)
  X <- matrix(rnorm(100), nrow = 10)
  result <- Calculate_BIC(Y, X)
  expect_true(is.list(result))
  expect_true("BIC" %in% names(result))
  expect_true("Inverse" %in% names(result))
  expect_true(is.numeric(result$BIC))
})

test_that("Calculate_CAIC works with example inputs", {
  Y <- rnorm(10)
  X <- matrix(rnorm(100), nrow = 10)
  result <- Calculate_CAIC(Y, X)
  expect_true(is.list(result))
  expect_true("CAIC" %in% names(result))
  expect_true("Inverse" %in% names(result))
  expect_true(is.numeric(result$CAIC))
})

test_that("Calculate_CAICF works with example inputs", {
  Y <- rnorm(10)
  X <- matrix(rnorm(100), nrow = 10)
  result <- Calculate_CAICF(Y, X)
  expect_true(is.list(result))
  expect_true("CAICF" %in% names(result))
  expect_true("Inverse" %in% names(result))
  expect_true(is.numeric(result$CAICF))
})

test_that("Calculate_GIC2 works with example inputs", {
  Y <- rnorm(10)
  X <- matrix(rnorm(100), nrow = 10)
  result <- Calculate_GIC2(Y, X)
  expect_true(is.list(result))
  expect_true("GIC2" %in% names(result))
  expect_true("Inverse" %in% names(result))
  expect_true(is.numeric(result$GIC2))
})

test_that("Calculate_GIC3 works with example inputs", {
  Y <- rnorm(10)
  X <- matrix(rnorm(100), nrow = 10)
  result <- Calculate_GIC3(Y, X)
  expect_true(is.list(result))
  expect_true("GIC3" %in% names(result))
  expect_true("Inverse" %in% names(result))
  expect_true(is.numeric(result$GIC3))
})

test_that("Calculate_GIC4 works with example inputs", {
  Y <- rnorm(10)
  X <- matrix(rnorm(100), nrow = 10)
  result <- Calculate_GIC4(Y, X)
  expect_true(is.list(result))
  expect_true("GIC4" %in% names(result))
  expect_true("Inverse" %in% names(result))
  expect_true(is.numeric(result$GIC4))
})

test_that("Calculate_GIC5 works with example inputs", {
  Y <- rnorm(10)
  X <- matrix(rnorm(100), nrow = 10)
  result <- Calculate_GIC5(Y, X)
  expect_true(is.list(result))
  expect_true("GIC5" %in% names(result))
  expect_true("Inverse" %in% names(result))
  expect_true(is.numeric(result$GIC5))
})

test_that("Calculate_GIC6 works with example inputs", {
  Y <- rnorm(10)
  X <- matrix(rnorm(100), nrow = 10)
  result <- Calculate_GIC6(Y, X)
  expect_true(is.list(result))
  expect_true("GIC6" %in% names(result))
  expect_true("Inverse" %in% names(result))
  expect_true(is.numeric(result$GIC6))
})
