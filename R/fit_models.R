
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
  
  
  # create model grid
  model_grid <- tibble::as.tibble(
    expand.grid(uid = purrr::map_chr(obj$models, "uid"), 
                algo = purrr::map_chr(obj$models, "algo"), 
                index  = as.character(names(obj$indices$train)))) %>% 
    dplyr::mutate_if(., is.factor, as.character)
  
  
  # fit models to training samples
  foreach::foreach(
    iter = 1:nrow(model_grid),
    .packages = c("forecast", "dplyr"),
    .export = c("model_grid", "obj", "model_algos"),
    .errorhandling = "pass") %dopar% {
      
      mg <- model_grid[iter, ]                     
      
      # get model
      mod <- purrr::keep(obj$models, ~.x$uid == mg$uid)[[1]]
      
      # set algorithm & arguments
      algo_pack <- model_algos %>% 
        dplyr::filter(algorithm == mg$algo) %>% 
        dplyr::pull(package)
      
      algo <- get(as.character(mg$algo), asNamespace(algo_pack))
      algo_args <- modifyList(mod$args, list(y = y))
      
      # set index
      index <- obj$indices$train[[mg$index]]
      
      # apply model pipeline and get training dataset
      train_df <- obj$df %>% 
        dplyr::slice(index) %>%  
        madutils::flow(., mod$pipeline)
      
      # get numeric time series target
      y <- train_df[[obj$y_var]]
      x_vars <- setdiff(colnames(train_df), obj$y_var)
      
      if(length(x_vars) > 0 & "xreg" %in% names(formals(algo))) {
        xreg <- train_df[, obj$x_vars, drop=FALSE] 
        algo_args <- modifyList(algo_args, list(xreg = xreg))
      }
      
      # fit model
      c(as.list(mg), list(fit = do.call(algo, algo_args)))
    }
}



get_model_args <- function(uid, index, obj) {
  
  # get model
  model <- purrr::keep(obj$models, ~.x$uid == uid)[[1]]
  model$y_var <- obj$y_var
  
  # get row index
  rn_index <- obj$indices$train[[index]]
  
  # get training data
  train_df <- obj$df[rn_index,]
  
  list(model = model, df = train_df)
}


fit_model <- function(model, df) {
  
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
    xreg <- df[, obj$x_vars, drop=FALSE] 
    algo_args <- modifyList(algo_args, list(xreg = xreg))
  }
  
  # fit model
  do.call(algo, algo_args)
}

