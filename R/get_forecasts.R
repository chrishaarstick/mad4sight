
#' Get Forecasts
#' 
#' Wrapper function for making forecasts using forecast function
#'
#' @param ... list of named arguments
#'
#' @return tibble with forecast predictions
#' @export
get_forecasts <- function(...) {
  
  # make forecasts
  fun <- get("forecast", asNamespace("forecast"))
  forecasts <- do.call(fun, ...) 
  tibble::as.tibble(forecasts) %>% 
    setNames(c("predicted", "low80", "high80", "low95", "high95"))
}