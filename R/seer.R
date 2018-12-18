
# seer class object functions and methods ---------------------------------


#' Seer Constructor Function
#'
#' Creates seer class object
#'
#' @param df data.frame input
#' @param y_var column name of target variable
#' @param x_vars optional column names of covariates
#' @param sampling sample constructor function. see `samples` function for
#'   details
#' @param models list of model constructor functions. see `model` function for
#'   details
#' @param measure string input of error metric to use for model performance
#'   evaluation
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
                     measure,
                     confidence_levels,
                     horizon,
                     forecast_xreg,
                     backend) {
  
  checkmate::assert_data_frame(df, min.cols = 1)
  checkmate::assert_string(y_var)
  checkmate::assert_class(sampling, "samples")
  checkmate::assert_list(models)
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
      models = models,
      fit = tibble::tibble(),
      performance = tibble::tibble(),
      measure = measure,
      confidence_levels = confidence_levels,
      created_at = Sys.time(),
      final_model = NULL,
      forecast = tibble::tibble(),
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
                 sampling = samples(method = "single", args = list()),
                 models = list(model(algo = "auto.arima")),
                 measure = "rmse",
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
                  measure,
                  confidence_levels,
                  horizon,
                  forecast_xreg,
                  backend)
  obj$user <- ifelse(is.null(user), as.character(Sys.info()["user"]), user)
  obj$uid <- uid
  obj$desc <- desc
  
  
  # fit models
  fits <- fit_models(obj)
  
  # get predictions
  predictions <- get_model_predictions(obj, fits)
  
  # get model performance
  performance <- predictions %>% 
    group_by(sample, uid, index) %>%
    unnest() %>% 
    inner_join(df %>%
                 select(y_var) %>% 
                 mutate(rn = dplyr::row_number()),
               by = "rn") %>% 
    do(get_accuracy(.$predicted, .$y)) %>% 
    ungroup()
  
  # save models and performance
  
}




