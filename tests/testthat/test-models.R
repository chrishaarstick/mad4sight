
# models unit testing -----------------------------------------------------


# R Libraries
library(mad4sight)
library(testthat)
library(checkmate)
library(dplyr)


context("models unit tests")



test_that("models constructor works as expected", {
  
  m1 <- models("auto.arima")
  expect_class("model")
  
  expect_error(models("not-model"))
})
