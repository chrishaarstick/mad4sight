
# Fit Models Function -----------------------------------------------------

#' Fit Seer Models
#' 
#' Fits all models on all training samples in seer object
#'
#' @param obj seer object
#'
#' @return list with model algo, index, and fit in each element
#' @importFrom foreach %dopar%
#' @export
fit_models <- function(obj) {
  
  checkmate::assert_class(obj, "seer")
  
  # set backend execution
  future::plan(strategy = get(obj$backend, asNamespace("future"))())
  doFuture::registerDoFuture()
  
  
  # create expanded list of model, index combos
  mod_list <- list(uid = purrr::map_chr(obj$models, "uid"),
                   index = as.character(names(obj$indices$train))) %>%
    purrr::cross() 
  
  # get all fits
  fits <- mod_list %>% 
    purrr::map(., ~ get_train_model_args(.$uid, .$index, obj = obj)) %>%
    furrr::future_map(., ~ fit_model(.$model, .$df))
  
  # creat flattened output structure
  purrr::flatten_dfr(mod_list) %>% 
    dplyr:::mutate(fit = fits)
}


# Internal get model arguments function 
get_train_model_args <- function(uid, index, obj) {
  
  # get model
  model <- purrr::keep(obj$models, ~.x$uid == uid)[[1]]
  model$y_var <- obj$y_var
  
  # get row index
  rn_index <- obj$indices$train[[index]]
  
  # get training data
  train_df <- obj$df[rn_index,]
  
  list(model = model, df = train_df)
}


# Internal fit model helper function
fit_model <- function(model, df) {
  
  # apply pipeline
  df <- madutils::flow(df, model$pipeline)
  
  # get numeric time series target
  y <- df[[model$y_var]]
  x_vars <- setdiff(colnames(df), model$y_var)
  
  # set algorithm & arguments
  algo_pack <- model_algos %>% 
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