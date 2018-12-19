
# seer class object functions and methods ---------------------------------


#' Seer Constructor Function
#'
#' Creates seer class object
#'
#' @param df data.frame input
#' @param y_var column name of target variable
#' @param sampling sample constructor function. see `samples` function for
#'   details
#' @param models list of model constructor functions. see `model` function for
#'   details
#' @param selection model_selection constructor function result. see
#'   `model_selection` for details
#' @param confidence_levels numeric vector with one or two confidence levels
#'   used in the forecast predictions
#' @param horizon forecast horizon
#' @param forecast_xreg optional foreacast covariate data
#' @param backend future backend mode. `sequential` creates single threaded
#'   execution. `multisession` creates parrallel backend
#'
#' @return seer class object
#' @export
neophyte <- function(df,
                     y_var,
                     sampling,
                     models,
                     selection,
                     confidence_levels,
                     horizon,
                     forecast_xreg,
                     backend) {
  
  checkmate::assert_data_frame(df, min.cols = 1)
  checkmate::assert_string(y_var)
  checkmate::assert_class(sampling, "samples")
  checkmate::assert_choice(sampling$method, c("split", "slice"))
  checkmate::assert_list(models)
  checkmate::assert_class(selection, "model_selection")
  checkmate::assert_numeric(confidence_levels, lower = 50, upper = 100, min.len = 1, max.len = 2)
  checkmate::assert_numeric(horizon, lower = 1)
  checkmate::assert_data_frame(forecast_xreg, null.ok = TRUE)
  checkmate::assert_choice(backend, choices = c("sequential", "multisession"))
  
  
  # create samples
  sampling_fun <- match.fun(sampling$method)
  sampling_args <- modifyList(sampling$args, list(df = df))
  indices <- do.call(sampling_fun, sampling_args)
  
  # create intial seer object
  obj <- structure(
    list(
      df = df,
      y_var = y_var,
      sampling = sampling,
      indices = indices,
      models = purrr::modify(models, ~set_target(.x, y_var)),
      confidence_levels = confidence_levels,
      fits = tibble::tibble(),
      predictions = tibble::tibble(),
      performance = tibble::tibble(),
      selection = selection,
      final_model = NULL,
      forecast = tibble::tibble(),
      horizon = horizon,
      forecast_xreg = forecast_xreg,
      created_at = Sys.time(),
      backend = backend
    ),
    class = "seer"
  )
}





#' Seer function to create forecasts
#'
#' @inheritParams neophyte
#' @param user optional string input for user. defaults to sys.user
#' @param uid optional string for uid. defaults to random string
#' @param desc optional string input for seer description. defaults to empty
#'   string
#'
#' @return
#' @export
#'
#' @examples
seer <- function(df,
                 y_var,
                 sampling = samples(method = "split", args = list(ratio = .9)),
                 models = list(model(algo = "auto.arima")),
                 selection = model_selection(),
                 confidence_levels = c(80, 95),
                 horizon = 1,
                 forecast_xreg = NULL,
                 backend = "sequential",
                 user = NULL,
                 uid = madutils::random_string("seer"),
                 desc = "") {
  
  
  # Create neophypte
  obj <- neophyte(df,
                  y_var,
                  sampling,
                  models,
                  selection,
                  confidence_levels,
                  horizon,
                  forecast_xreg,
                  backend)
  obj$user <- ifelse(is.null(user), as.character(Sys.info()["user"]), user)
  obj$uid <- uid
  obj$desc <- desc
  
  
  # set backend execution
  future::plan(strategy = get(obj$backend, asNamespace("future"))())
  
  # fit models
  obj$fits <- fit_models(obj)
  
  # get predictions
  obj$predictions <- get_model_predictions(obj)
  
  # get model performance
  obj$performance <- get_model_performance(obj$predictions, dplyr::select(df, y_var))
    
  # select final model
  obj$final_model <- get_final_model(obj)
  
  # create forecasts
  obj$forecast <- make_final_forecasts(obj)
  
  obj
}




