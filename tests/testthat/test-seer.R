
# seer class unit tests ---------------------------------------------------

# R Libraries
library(mad4sight)
library(purrr)
library(testthat)
library(checkmate)
library(lubridate)
library(dplyr)

context("seer unit tests")

# Create Dataset
set.seed(319)
n <- 100
df <- tibble(index = 1:n,
             y = as.numeric(arima.sim(model = list(0,0,0), n)))



# seer unit tests ---------------------------------------------------------

test_that("neophyte function works as expected", {
  
  s1 <- neophyte(df,
                 y_var = "y",
                 x_vars = NULL,
                 sampling = samples(method = "single", args = list()),
                 models = list(model(algo = "auto.arima")),
                 measure = "rmse",
                 confidence_levels = c(.8, .95),
                 horizon = 1,
                 forecast_xreg = NULL,
                 backend = "sequential")
  
  expect_class(s1, "seer")
  expect_data_frame(s1$df)
  expect_data_frame(s1$fits)
  expect_data_frame(s1$performance)
  expect_data_frame(s1$forecast)
  expect_list(s1$models)
  expect_list(s1$indices)
})
