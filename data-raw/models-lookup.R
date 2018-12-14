# models look up table ----------------------------------------------------

# R Libraries
library(tibble)


# create `models` look up table

# Forecast models currently supported
models <- tibble(
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


save(models, file = "data/models.rdata")


