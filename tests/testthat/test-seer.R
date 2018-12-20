
# seer class unit tests ---------------------------------------------------

# R Libraries
library(mad4sight)
library(madutils)
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
                 sampling = samples(method = "split", args = list(ratio=.9)),
                 models = list(model(algo = "auto.arima")),
                 selection = model_selection(),
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
  
  fits <- neophyte(df,
                 y_var = "y",
                 sampling = samples(method = "split", args = list(ratio=.9)),
                 models = list(model(algo = "auto.arima")),
                 selection = model_selection(),
                 confidence_levels = c(80, 95),
                 horizon = 1,
                 forecast_xreg = NULL,
                 backend = "sequential") %>% 
    fit_models(.)
  
  expect_data_frame(fits)
  expect_equal(colnames(fits), c("uid", "index", "fit"))
  expect_class(fits$fit[[1]], "ARIMA")
  
})



test_that("get_model_predictions function works as expected", {
  
  obj <- neophyte(df,
                 y_var = "y",
                 sampling = samples(method = "split", args = list(ratio = .9)),
                 models = list(model(algo = "auto.arima")),
                 selection = model_selection(),
                 confidence_levels = c(80, 95),
                 horizon = 1,
                 forecast_xreg = NULL,
                 backend = "sequential")
  obj$fits <- fit_models(obj)  
  predictions <- get_model_predictions(obj)
  
  expect_data_frame(predictions, ncols = 4)
  expect_data_frame(predictions$data[[1]])
  expect_true("predicted" %in% colnames(predictions$data[[1]]))
})


test_that("get_model_performance function works as expected", {
  
  obj <- neophyte(df,
                  y_var = "y",
                  sampling = samples(method = "split", args = list(ratio = .9)),
                  models = list(model(algo = "auto.arima")),
                  selection = model_selection(),
                  confidence_levels = c(80, 95),
                  horizon = 1,
                  forecast_xreg = NULL,
                  backend = "sequential")
  obj$fits <- fit_models(obj)  
  obj$predictions <- get_model_predictions(obj)
  performance <- get_model_performance(obj)
  
  
  expect_data_frame(performance, ncols = 7, nrows = 2)
  expect_true(all(c("train", "validation") %in% performance$sample))
})



test_that("get_final_model function works as expected", {
  
  obj <- neophyte(df,
                  y_var = "y",
                  sampling = samples(method = "split", args = list(ratio = .9)),
                  models = list(model(algo = "auto.arima")),
                  selection = model_selection(measure = "RMSE", n = 1, weights = "equal"),
                  confidence_levels = c(80, 95),
                  horizon = 1,
                  forecast_xreg = NULL,
                  backend = "sequential")
  obj$fits <- fit_models(obj)  
  obj$predictions <- get_model_predictions(obj)
  obj$performance <- get_model_performance(obj)
  final_model <- get_final_model(obj)
  
  expect_data_frame(final_model, ncols = 5, nrows = 1)
  expect_class(final_model$model[[1]], "model")
  expect_class(final_model$fit[[1]], "ARIMA")
  expect_class(final_model$forecasts[[1]], "data.frame")
  expect_true(sum(final_model$forecast_wt) == 1)
})




test_that("make_final_forecasts function works as expected", {
  
  obj <- neophyte(df,
                  y_var = "y",
                  sampling = samples(method = "split", args = list(ratio = .9)),
                  models = list(model(algo = "auto.arima")),
                  selection = model_selection(measure = "RMSE", n = 1, weights = "equal"),
                  confidence_levels = c(80, 95),
                  horizon = 1,
                  forecast_xreg = NULL,
                  backend = "sequential")
  obj$fits <- fit_models(obj)  
  obj$predictions <- get_model_predictions(obj)
  obj$performance <- get_model_performance(obj)
  obj$final_model <- get_final_model(obj)
  forecasts <- make_final_forecasts(obj)
  
  expect_data_frame(forecasts, ncols = 6, nrows = 1)
  expect_equal(forecasts$index[1], nrow(df) + 1)
})



test_that("seer model with arguments works as expected", {
  
  m1 <- model(algo = "auto.arima", args = list(lambda = "auto"))
  
  s1 <- seer(df,
               y_var  = "y",
               sampling = samples(method = "split", args = list(ratio = .9)),
               models = list(m1),
               selection = model_selection(),
               confidence_levels = c(80, 95),
               horizon = 1,
               forecast_xreg = NULL,
               backend = "sequential",
               user = NULL,
               uid = madutils::random_string("seer"),
               desc = "")
  
  expect_class(s1, "seer")
  expect_data_frame(s1$forecast, nrows = 1)
})



test_that("selection with multiple models works as expected", {
  
  m1 <- model(algo = "auto.arima", args = list(lambda = "auto"))
  m2 <- model(algo = "ets")
  
  selection <- model_selection(measure = "RMSE", n = 2, weights = "weighted")
  
  s1 <- seer(df,
             y_var  = "y",
             sampling = samples(method = "split", args = list(ratio = .9)),
             models = list(m1, m2),
             selection = selection,
             confidence_levels = c(80, 95),
             horizon = 1,
             forecast_xreg = NULL,
             backend = "sequential",
             user = NULL,
             uid = madutils::random_string("seer"),
             desc = "")
  
  expect_class(s1, "seer")
  expect_data_frame(s1$forecast, nrows = 1)
  expect_data_frame(s1$final_model, nrows = 2)
  expect_equal(sum(s1$final_model$forecast_wt), 1)
})


test_that("covariate model works as expected", {
  
  df1 <- mutate(df, x = rnorm(n()))
  
  p1 <- pipeline(expr = function(df) {select(df, y)})
  p2 <- pipeline(expr = function(df) {select(df, y, x)})
  
  m1 <- model(algo = "auto.arima", pipe = p1)
  m2 <- model(algo = "auto.arima", pipe = p2)
  
  selection <- model_selection(measure = "RMSE", n = 2, weights = "weighted")
  
  xreg <- tibble(x = rnorm(5))
  
  s1 <- seer(df1,
             y_var  = "y",
             sampling = samples(method = "split", args = list(ratio = .9)),
             models = list(m1, m2),
             selection = selection,
             confidence_levels = c(80, 95),
             horizon = 5,
             forecast_xreg = xreg,
             backend = "sequential",
             user = NULL,
             uid = madutils::random_string("seer"),
             desc = "")
  
  expect_class(s1, "seer")
  expect_data_frame(s1$forecast, nrows = 5)
  expect_data_frame(s1$final_model, nrows = 2)
  expect_equal(sum(s1$final_model$forecast_wt), 1)
})

