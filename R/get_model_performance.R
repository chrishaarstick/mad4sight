

#' Get Model Performance
#'
#' Function calculates model performance on all uid, sample, and index
#' predictions then averages by uid and sample. Function uses forecast
#' `accuracy` function to calculate metrics. Used internally by `seer` function
#'
#' @param predictions nested data.frame with predictions. output of
#'   `get_model_predictions` function
#' @param actuals data.frame with only one column that is the actual y_var
#'
#' @return data.frame with ME, RMSE, MAE, MPE, and MAPE calculated for all model
#'   uid and sample combinations
#' @export
get_model_performance <- function(obj) {
  
  checkmate::assert_data_frame(obj$predictions)
  checkmate::assert_set_equal(colnames(obj$predictions), c("uid", "sample", "index", "data"))
  checkmate::assert_subset("predicted", colnames(obj$predictions$data[[1]]))

  actuals <- obj$df %>% 
    dplyr::select_at(obj$y_var) %>% 
    dplyr::mutate(rn = dplyr::row_number())
  
  obj$predictions %>% 
    dplyr::group_by(uid, sample, index) %>%
    tidyr::unnest() %>% 
    dplyr::inner_join(actuals, by = "rn") %>% 
    dplyr::rename(actual = !!rlang::sym(obj$y_var)) %>% 
    dplyr::do(get_accuracy(.$predicted, .$actual)) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(uid, sample) %>% 
    dplyr::select_if(is.numeric) %>% 
    dplyr::summarise_all(mean) %>% 
    dplyr::ungroup()
}



# Internal get accuracy wrapper function
get_accuracy <- function(predicted, actual) {
  tibble::as_tibble(forecast::accuracy(predicted, actual))
}