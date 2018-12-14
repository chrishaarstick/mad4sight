
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
                     x_vars,
                     sampling,
                     models,
                     measure,
                     confidence_levels,
                     horizon,
                     forecast_xreg,
                     backend) {
  
  checkmate::assert_data_frame(df, min.cols = 1)
  checkmate::assert_string(y_var)
  checkmate::assert_character(x_vars, null.ok = TRUE)
  checkmate::assert_class(sampling, "samples")
  checkmate::assert_list(models)
  checkmate::assert_numeric(confidence_levels, lower = .5, upper = 1, min.len = 1, max.len = 2)
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
      x_vars = x_vars,
      sampling = sampling,
      indices = indices,
      models = list(),
      fits = tibble::tibble(),
      performance = tibble::tibble(),
      measure = measure,
      confidence_levels = confidence_levels,
      created_at = Sys.time(),
      final_model = NULL,
      forecast = tibble::tibble()
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
                 x_vars = NULL,
                 sampling = samples(method = "single", args = list()),
                 models = list(model(algo = "auto.arima")),
                 measure = "rmse",
                 confidence_levels = c(.8, .95),
                 horizon = 1,
                 forecast_xreg = NULL,
                 backend = "sequential",
                 user = NULL,
                 uid = madutils::random_string("seer"),
                 desc = "") {
  
  
  # Create neophypte
  obj <- neophyte(df,
                  y_var,
                  x_vars,
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
          xreg <- df[val_index, x_vars, drop=FALSE]
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




train_models <- function(obj) {
  checkmate::assert_class(obj, "seer")
  
  # set backend execution
  future::plan(strategy = get(obj$backend, asNamespace("future"))())
  doFuture::registerDoFuture()
  
}
