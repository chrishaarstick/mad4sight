
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
df <- tibble(y = as.numeric(arima.sim(model = list(0,0,0), n)))



# seer unit tests ---------------------------------------------------------

test_that("neophyte function works as expected", {
  
  s1 <- neophyte(df,
                 y_var = "y",
                 sampling = samples(method = "single", args = list()),
                 models = list(model(algo = "auto.arima")),
                 measure = "rmse",
                 confidence_levels = c(80, 95),
                 horizon = 1,
                 forecast_xreg = NULL,
                 backend = "sequential")
  
  expect_class(s1, "seer")
  expect_data_frame(s1$df)
  expect_data_frame(s1$fit)
  expect_data_frame(s1$performance)
  expect_data_frame(s1$forecast)
  expect_list(s1$models)
  expect_list(s1$indices)
})


test_that("fit_models function works as expected", {
  
  m1 <- neophyte(df,
                 y_var = "y",
                 sampling = samples(method = "single", args = list()),
                 models = list(model(algo = "auto.arima")),
                 measure = "rmse",
                 confidence_levels = c(80, 95),
                 horizon = 1,
                 forecast_xreg = NULL,
                 backend = "sequential") %>% 
    fit_models(.)
  
  expect_data_frame(m1)
  expect_equal(colnames(m1), c("uid", "index", "fit"))
  expect_class(m1$fit[[1]], "ARIMA")
  
})



test_that("get_model_predictions function works as expected", {
  
  obj <- neophyte(df,
                 y_var = "y",
                 sampling = samples(method = "split", args = list(ratio = .9)),
                 models = list(model(algo = "auto.arima")),
                 measure = "rmse",
                 confidence_levels = c(80, 95),
                 horizon = 1,
                 forecast_xreg = NULL,
                 backend = "sequential")
  
  fits <- fit_models(obj)
  
  predictions <- get_model_predictions(obj, fits)
  expect_data_frame(predictions, ncols = 4)
  expect_data_frame(predictions$data[[1]])
  expect_true("predicted" %in% colnames(predictions$data[[1]]))
})


