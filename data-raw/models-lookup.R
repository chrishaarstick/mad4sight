# model_algo look up table -------------------------------------------------

# R Libraries
library(tibble)


# create `model_algo` look up table

# Forecast modeling algorithms currently supported
model_algos <- tibble(
  algorithm =  c(
    "Arima",
    "arfima",
    "auto.arima",
    "bats",
    "ets",
    "nnetar",
    "tbats"
  ),
  name = c(
    "Arima",
    "Fractionally Differenced Arima",
    "Auto Arima",
    "ETS with boxcox, ARIMA errors, Trend and Seasonal",
    "Exponentially Smoothing State Space",
    "Neural Network Time Series",
    "BATS with Trigonometric Seasonal"
  ),
  class = "model",
  package = c(
    "forecast",
    "forecast",
    "forecast",
    "forecast",
    "forecast",
    "forecast",
    "forecast"
  )
)


save(model_algos, file = "data/model_algos.rdata")


