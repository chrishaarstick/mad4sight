
# make final forecasts ----------------------------------------------------



#' Make Final Forecasts
#'
#' Makes final forecasts based on models and forecasting weight as a result of
#' model selection criteria
#'
#' Confidence intervals are calculated by taking widest interval
#'
#' @param obj seer object
#'
#' @return tibble with final forecasts. Adds index column to forecast ouput
#'   which starts at 1 plus the number of rows in the input dataset
#' @export
make_final_forecasts <- function(obj) {
  
  checkmate::assert_class(obj, "seer")
  
  obj$final_model %>% 
    tidyr::unnest(forecasts) %>% 
    dplyr::group_by(index) %>% 
    dplyr::summarise(predicted = sum(predicted * forecast_wt)) %>% 
    dplyr::inner_join(
      obj$final_model %>% 
        tidyr::unnest(forecasts) %>% 
        dplyr::group_by(index) %>% 
        dplyr::select(index, dplyr::contains("low")) %>% 
        dplyr::summarise_all(min),
      by = "index"
    ) %>% 
    dplyr::inner_join(
      obj$final_model %>% 
        tidyr::unnest(forecasts) %>% 
        dplyr::group_by(index) %>% 
        dplyr::select(index, dplyr::contains("high")) %>% 
        dplyr::summarise_all(max),
      by = "index"
    )
}