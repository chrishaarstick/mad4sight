

# Get Model Predictions ---------------------------------------------------



#' Get Model Predictions
#'
#' Function to get all seer model predictions for train and validation samples
#'
#' Returned nested tibble has column for model uid, sample, index, and nested
#' data column with rn (row number), predicted and any confidence levels columns
#'
#' @param obj seer object
#' @param fits tibble with uid, index, and fit. Result of fit_models function
#'
#' @return nested tibble with model predictions
#' @export
get_model_predictions <- function(obj) {
  
  checkmate::assert_class(obj, "seer")
  checkmate::assert_data_frame(obj$fits)
  checkmate::assert_set_equal(names(obj$fits), c("uid", "index", "fit"))
  
  
  # convert idicies to joinable df
  indices_df <- obj$indices %>% 
    dplyr::bind_rows(. , .id = "sample") %>%
    tidyr::gather(index, rn, -sample)
  
  # get train predictions
  train_preds <- obj$fits %>%
    dplyr::mutate(sample = "train") %>% 
    dplyr::group_by(sample, uid, index) %>%
    dplyr::do(get_fitted(.$fit[[1]])) 
   
  
  # get validation predictions
  val_preds <- obj$fits %>% 
    dplyr::mutate(sample = "validation") %>% 
    dplyr::filter(index != "single") %>% 
    dplyr::group_by(sample, uid, index) %>% 
    dplyr::mutate(model = purrr::keep(obj$models, ~.x$uid == uid)) %>% 
    dplyr::mutate(df = list(obj$df[obj$indices$validation[[index]], ])) %>% 
    dplyr::do(make_predictions(.$fit[[1]], .$model[[1]], .$df[[1]], obj$confidence_levels)) 
  
  
  # Combine output, nest and returnve
  dplyr::bind_rows(train_preds, val_preds) %>% 
    dplyr::inner_join(indices_df, by = c("sample","index")) %>% 
    tidyr::nest()
}


# Internal make predictions function for use in evaluate models step
make_predictions <- function(fit, model, df, confidence_levels) {
  
  # apply pipeline
  df <- madutils::flow(df, model$pipeline)
  
  # set validation data
  y <- df[[model$y_var]]
  x_vars <- setdiff(colnames(df), model$y_var)
  
  # set forecast args
  forecast_args <- list(object = fit, h = length(y), level = confidence_levels)
  
  # check for predictors
  if(! is.null(fit$xreg)) {
    xreg <- df[, x_vars, drop=FALSE] 
    forecast_args <- modifyList(forecast_args, list(xreg = xreg))
  }
  
  # make forecasts
  get_forecasts(forecast_args)
}

