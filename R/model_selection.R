
# final model selection ---------------------------------------------------


#' Model Selection Constructor
#'
#' Helper function to create final model selection criteria for forecasting
#'
#' @param measure accuracy metric to arrange and weight by. Choices are RMSE and
#'   MAE. Default is RMSE
#' @param n top_n number of models to select. Default is 1
#' @param weights option to weight model forcasts by inverse of measure or
#'   equally. Options are equal for equal weighting or weighted for inverse
#'   weighting. Default is equal
#'
#' @return model selection object
#' @export
#'
#' @examples
#' # default
#' model_selection()
#' 
#' # weighted selection
#' model_selection(measure = "RMSE", n = 2, weights = "weighted")
model_selection <- function(measure = "RMSE", n = 1, weights = "equal") {
  
  checkmate::assert_choice(toupper(measure), c("RMSE", "MAE"))
  checkmate::assert_numeric(n, lower = 1)
  checkmate::assert_choice(weights, c("equal", "weighted"))
  
  structure(
    list(
      measure = toupper(measure),
      n = n,
      weights = weights
    ),
    class = "model_selection"
  )
}


#' Get Final Model
#'
#' Fits final models on full training sample based on selection criterion.
#' Forecasts on refit models
#'
#' @param obj seer object
#'
#' @return nested data.frame with new model fit, forecasts, and forecast weight 
#' @export
#'
#' @examples
get_final_model <- function(obj) {
  
  checkmate::assert_class(obj, "seer")
  
  # filter to top models and calculate forecast weight
  uids <- obj$performance %>% 
    dplyr::filter(sample == "validation") %>% 
    dplyr::top_n(obj$selection$n, dplyr::desc(!!rlang::sym(obj$selection$measure))) %>% 
    dplyr::mutate(forecast_wt = dplyr::case_when(
      obj$selection$weights == "equal" ~ 1/n(),
      TRUE ~ (1/!!rlang::sym(obj$selection$measure))/sum(1/!!rlang::sym(obj$selection$measure))
    )) %>% 
    dplyr::select(uid, forecast_wt) 
    
  
  # add model, data, refit and make predictions
  models_df <- uids %>% 
    dplyr::group_by(uid, forecast_wt) %>% 
    dplyr::mutate(model = purrr::keep(obj$models, ~.x$uid == uid)) %>% 
    dplyr::mutate(df = list(obj$df)) %>% 
    dplyr::ungroup()
  
  fit_df <- models_df %>% 
    dplyr::group_by(uid) %>% 
    dplyr::do(fit = fit_model(.$model[[1]], .$df[[1]])) %>% 
    dplyr::ungroup()
  
  forecast_df <- fit_df %>%
    dplyr::inner_join(models_df, by = "uid") %>% 
    dplyr::group_by(uid) %>% 
    dplyr::do(make_forecasts(.$fit[[1]],
                             .$model[[1]],
                             obj$horizon,
                             obj$forecast_xreg,
                             obj$confidence_levels)) %>% 
    dplyr::mutate(index = (nrow(obj$df)+1) : (nrow(obj$df)+obj$horizon)) %>% 
    dplyr::select(uid, index, predicted, low80, high80, low95, high95) %>% 
    tidyr::nest()
  
  
  models_df %>% 
    dplyr::select(uid, model, forecast_wt) %>% 
    dplyr::inner_join(fit_df, by = "uid") %>% 
    dplyr::inner_join(forecast_df, by = "uid") %>% 
    dplyr::rename(forecasts = data)
}



# Internal make forecast function for use in final model step
make_forecasts <- function(fit, model, h, xreg = NULL, confidence_levels) {
  
  # set forecast args
  forecast_args <- list(object = fit, h = h, level = confidence_levels)
  
  # set algorithm & arguments
  algo_pack <- mad4sight::model_algos %>% 
    dplyr::filter(algorithm == model$algo) %>% 
    dplyr::pull(package)
  
  algo <- get(as.character(model$algo), asNamespace(algo_pack))
  
  if(!is.null(fit$xreg) & "xreg" %in% names(formals(algo))) {
    
    # apply pipeline
    xreg <- dplyr::mutate(xreg, !!rlang::sym(model$y_var) := 1)
    xreg <- madutils::flow(xreg, model$pipeline)
    x_vars <- setdiff(colnames(xreg), model$y_var)
    xreg <- xreg[, x_vars, drop=FALSE] 
    
    forecast_args <- modifyList(forecast_args, list(xreg = xreg))
  }
  
  # make forecasts
  get_forecasts(forecast_args)
}


