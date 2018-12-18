
# Evaluate Models Function ------------------------------------------------



evaluate_models <- function(obj, fits) {
  
  checkmate::assert_class(obj, "seer")
  checkmate::assert_list(fits)
  checkmate::assert_set_equal(names(fits), c("algo", "index", "fit"))
  
  # set backend execution
  future::plan(strategy = get(obj$backend, asNamespace("future"))())
  doFuture::registerDoFuture()
  
  # training Data Fit
  fits %>% 
    purrr::map(., ~ purrr::map_at(., "fit", function(x) as.numeric(fitted(x)))) %>%
    purrr::map(., as_tibble) %>%
    dplyr::bind_rows(.) %>% 
    dplyr::mutate_if(., is.factor, as.character) %>% 
    dplyr::mutate(sample = "train")
  
  
  # validation Data Fit
  val_fits <- purrr::keep(fits, ~ !purrr::has_element(., "single"))
  foreach::foreach(
    iter = 1:length(val_fits),
    .packages = c("forecast", "dplyr"),
    .export = c("obj"),
    .errorhandling = "pass") %dopar% {
      
      sample <- "validation"
      algo   <- val_fits[[iter]]$algo
      model  <- val_fits[[iter]]$fit
      
      # set index
      index <- obj$indices$validation[[val_fits[[iter]]$index]]
      
      # check for validation index
      if(is.null(index)) pass 
      
      # set validation data
      y <- obj$df[[obj$y_var]][index]
      
      # set forecast args
      forecast_args <- list(object = model, h = length(y), level = obj$confidence_levels)
      
      # check for predictors
      if(! is.null(val_fits[[iter]]$fit$xreg)) {
        xreg <- obj$df[index, obj$x_vars, drop=FALSE] 
        forecast_args <- modifyList(forecast_args, list(xreg = xreg))
      }
      
      # make forecasts
      forecasts <- get_forecasts(forecast_args)
    }
}