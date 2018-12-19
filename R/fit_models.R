
# Fit Models Function -----------------------------------------------------

#' Fit Seer Models
#' 
#' Fits all models on all training samples in seer object
#'
#' @param obj seer object
#'
#' @return list with model algo, index, and fit in each element
#' @export
fit_models <- function(obj) {
  
  checkmate::assert_class(obj, "seer")
  
  # create expanded list of model, index combos and fit model
  list(uid = purrr::map_chr(obj$models, "uid"),
                   index = as.character(names(obj$indices$train))) %>%
    purrr::cross_df() %>% 
    dplyr::group_by(uid, index) %>% 
    dplyr::mutate(model = purrr::keep(obj$models, ~.x$uid == uid)) %>% 
    dplyr::mutate(df = list(obj$df[obj$indices$train[[index]], ])) %>% 
    dplyr::do(fit = fit_model(.$model[[1]], .$df[[1]])) %>% 
    dplyr::ungroup()
}



# Internal fit model helper function
fit_model <- function(model, df) {
  
  # apply pipeline
  df <- madutils::flow(df, model$pipeline)
  
  # get numeric time series target
  y <- df[[model$y_var]]
  x_vars <- setdiff(colnames(df), model$y_var)
  
  # set algorithm & arguments
  algo_pack <- mad4sight::model_algos %>% 
    dplyr::filter(algorithm == model$algo) %>% 
    dplyr::pull(package)
  
  algo <- get(as.character(model$algo), asNamespace(algo_pack))
  algo_args <- modifyList(model$args, list(y = y))
  
  # check for xreg predictors
  if(length(x_vars) > 0 & "xreg" %in% names(formals(algo))) {
    xreg <- df[, x_vars, drop=FALSE] 
    algo_args <- modifyList(algo_args, list(xreg = xreg))
  }
  
  # fit model
  do.call(algo, algo_args)
}


# Internal get fitted helper function
get_fitted <- function(model) {
  tibble::tibble(predicted= as.numeric(model$fitted))
}