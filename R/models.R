
# model helper functions --------------------------------------------------

#' Create Forecast Model
#'
#' Constructor function to create a model class object for use with seer
#' function. Creates an model object after assertions on the algorithm input.
#' Used to standarize the model inputs
#'
#' @param algo model algorithm string. ex - `auto.arima`
#' @param args list of model arguments. default is empty list
#' @param pipe model pipeline. default is identity
#' @param uid optional sting uid argument. default is random_string
#' @param desc optional description
#'
#' @return model class object
#' @importFrom magrittr %>% 
#' @export
#'
#' @examples
#' model(algo = "auto.arima", args = list(), uid = "my-model", desc = "example description")
model <- function(algo,
                  args = list(),
                  pipe = madutils::pipeline(),
                  uid = madutils::random_string("model"),
                  desc = "") {
  
  checkmate::assert_choice(algo, model_algos$algorithm)
  checkmate::assert_list(args)
  checkmate::assert_class(pipe, "pipeline")
  checkmate::assert_string(uid)
  checkmate::assert_string(desc)
  
  
  # check for supported algorithm input
  algo_pack <- model_algos %>% 
    dplyr::filter(algorithm == algo) %>% 
    dplyr::pull(package)
  checkmate::assert_subset(names(args), names(formals(get(algo, asNamespace(algo_pack)))))
      
  
  structure(
    list(
      algo = algo,
      args = args,
      pipeline = pipe,
      uid = uid,
      desc = desc,
      created_on = Sys.time()
    ),
    class = c("model")
  )                     
}


# Internal set model target helper function
set_target <- function(model, y_var) {
  checkmate::assert_class(model, "model")
  model$y_var <- y_var
  model
}
