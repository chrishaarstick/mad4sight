
# seer class object functions and methods ---------------------------------


#' Seer function to create forecasts
#'
#' @param df 
#' @param index_var 
#' @param y_var 
#' @param x_vars 
#' @param samples 
#' @param measure 
#' @param models 
#' @param backend 
#'
#' @return
#' @importFrom foreach %dopar%
#' @export
#'
#' @examples
seer <- function(df,
                 index_var,
                 y_var,
                 x_vars = NULL,
                 samples = list(strategy = "none"),
                 measure = "rmse",
                 models = list(list(algo = "auto.arima")),
                 backend = "sequential") {
  
  checkmate::assert_data_frame(df, min.cols = 2)
  checkmate::assert_string(index_var)
  checkmate::assert_string(y_var)
  checkmate::assert_character(x_vars, null.ok = TRUE)
  checkmate::assert_list(samples)
  checkmate::assert_subset("strategy", names(samples))
  checkmate::assert_choice(samples$strategy, c("none", "slice", "split"))
  checkmate::assert_list(models)
  lapply(models, function(x) checkmate::assert_choice( "algo", names(x)))
  checkmate::assert_choice(backend, choices = c("sequential", "multisession"))
  
  
  # set backend execution
  future::plan(strategy = get(backend, asNamespace("future"))())
  
  
  # create samples
  if(samples$strategy == "none") {
    indices <- list(train = list(train = 1:nrow(df)), validation = NULL)
  } else {
    sample_args <- modifyList(samples[names(samples) != "strategy"], list(df = df))
    indices <- do.call(samples$strategy, sample_args)
  }
 
  
  # iterate through models
  fits <- furrr::future_map(models, .f = function(m){
    
    # set algorithm
    algo <- get(as.character(m$algo), asNamespace("forecast"))
    x_var_flag <- (! is.null(x_vars) & "xreg" %in% names(formals(algo)))
    
    
    perf <- tibble()
    
    # iterate through indicies
    for(i in names(indices$train)) {
   
      # set index
      train_index <- indices$train[[i]]
      
      # get numeric time series target
      y <- df[[y_var]][train_index]
      
      # set model arguments
      algo_args <- modifyList(m[names(m) != "algo"], list(y = y))
      
      if(x_var_flag) {
        xreg <- df[train_index, x_var, drop=FALSE]
        algo_args <- modifyList(algo_args, list(xreg = xreg))
      }
      
      # fit model
      model <- do.call(algo, algo_args)
      
      # get train performance
      train_perf <- forecast::accuracy(model) %>% 
        tibble::as.tibble() %>% 
        dplyr::mutate(algo = m$algo, sample = "train", index = i)
      
      # get validation index forecast
      if(! is.null(indices$validation)) {
        val_index <- indices$validation[[i]]
        
        if(x_var_flag) {
          xreg <- df[val_index, x_var, drop=FALSE]
        } else {
          xreg <- NULL
        }
        
        
        forecast::forecast(fit, h = length(val_index))
      }
    }
    
   
  },
  .options = furrr::future_options(packages = c("forecast", "purrr", "dplyr")))
  
  
  # create model grid
  model_grid <- expand.grid(algo = purrr::map_chr(models, "algo"), 
                            index  = as.character(names(indices$train)))
  
  
  # fit models to training samples
  fits <- foreach::foreach(
    iter = 1:nrow(model_grid),
    .packages = c("forecast", "dplyr"),
    .export = c("model_grid", "df", "indices", "x_vars", "y_var"),
    .errorhandling = "pass") %dopar% {
      
      mg <- model_grid[iter, ]                     
      
      # set algorithm
      algo <- get(as.character(mg$algo), asNamespace("forecast"))
      
      # set index
      index <- indices[[as.character(mg$index)]]
      
      # get numeric time series target
      y <- df[[y_var]][index[[1]]]
      
      # set model arguments
      mod <- purrr::keep(models, ~.x$algo == mg$algo)
      algo_args <- modifyList(mod[names(mod) != "algo"], list(y = y))
      
      if(! is.null(x_vars) & "xreg" %in% names(formals(algo))) {
        xreg <- dplyr::select(df, x_vars)
        algo_args <- modifyList(algo_args, list(xreg = xreg))
      }
      
      # fit model
      do.call(algo, algo_args)
    }
  
                    
  
  # evaluate models on validation samples
  
  
  # save models and performance
  
}
