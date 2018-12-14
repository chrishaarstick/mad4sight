
# models unit testing -----------------------------------------------------


# R Libraries
library(mad4sight)
library(testthat)
library(checkmate)
library(dplyr)


context("models unit tests")



test_that("models constructor works as expected", {
  
  m1 <- model("auto.arima")
  expect_class(m1, "model")
  
  expect_error(model("not-model"))
})
