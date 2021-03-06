
# seer sampling -----------------------------------------------------------


#' Samples Constructor
#'
#' Function to create samples class object. Validates inputs with supported
#' sampling functions in mad4sight. Used internally within `seer` function
#'
#' @param method sampling method
#' @param args list of sampling method arguments
#'
#' @return samples class object
#' @export
#'
#' @examples
#' samples(method = "split", args = list(ratio = .5))
samples <- function(method, args = list()) {
  
  checkmate::assert_choice(method, c("single", "slices", "split"))
  
  sample_fun <- match.fun(method)
  
  checkmate::assert_list(args)
  checkmate::assert_subset(names(args), names(formals(sample_fun)))
  
  required_args <- sample_fun %>% 
    formals() %>% 
    as.list() %>% 
    purrr::keep(., ~ . == "") %>% 
    names() %>%
    purrr::keep( ~ . != "df")
  checkmate::assert_subset(required_args, names(args))
  
  structure(
    list(
      method = method,
      args = args
    ),
    class = "samples"
  )
}




#' Create Time Slice Samples
#'
#' Function to create time slice indices. Based on
#' \link[caret]{createTimeSlices} from Caret package
#'
#' @param df data.frame to sample from
#' @param width number of records per training sample
#' @param horizon number of recoreds per validation sample
#' @param skip number of samples skipped. default is 0
#' @param fixed_width logical option to keep all training samples the same
#'   width. default is TRUE
#' @param label string label for samples. default is `slice`
#'
#' @return list with train and validation samples
#' @export
#'
#' @examples
#' library(tibble)
#' df <- tibble(i = 1:20)
#' slices(df, width = 15, horizon = 1, skip = 0)
slices <- function(df, 
                   width,
                   horizon,
                   skip = 0,
                   fixed_width = TRUE,
                   label = "slice") {
  
  n <- nrow(df)
  checkmate::assert_data_frame(df)
  checkmate::assert_numeric(width, lower = 1, upper = n - 1)
  checkmate::assert_numeric(horizon, lower = 1, upper = n - width)
  checkmate::assert_numeric(skip, lower = 0, upper = n - 1)
  checkmate::assert_flag(fixed_width)
  checkmate::assert_string(label)
  
  # calculate sequence stops and starts
  stops <- seq(width, (n - horizon), by = skip + 1)
  
  if(fixed_width) {
    starts <- stops - width + 1
  } else {
    starts <- rep(1, length(stops))
  }
  
  
  # create sequences
  s1 <- purrr::map2(starts, stops, seq)
  s2 <- purrr::map2(stops + 1, stops + horizon, seq)
  
  
  # create sample output
  labels <- paste0(label, as.character(stops))
  names(s1) <- names(s2) <- labels
  list(train = s1, validation = s2)  
}



#' Create Split Samples
#' 
#' Function to create a split train and validation samples
#'
#' @param df data.frame to sample
#' @param ratio ratio of data to use for train sample. 1-ratio used for validation
#'
#' @return list with train and validation samples
#' @export
#'
#' @examples
#' library(tibble)
#' df <- tibble(i = 1:20)
#' split(df, ratio = .5)
split <- function(df, ratio) {
  
  checkmate::assert_data_frame(df)
  checkmate::assert_numeric(ratio, lower = 0, upper = 1)
  
  # calculate split
  n <- nrow(df)
  s <- floor(nrow(df) * ratio)
  
  list(train = list(split = 1:s), validation = list(split = (s+1):n))
}



#' Create Single Sample
#' 
#' Function to create a single training sample formatted for mad4sight use 
#'
#' @param df data.frame to sample
#'
#' @return list with train and validation named indices
#' @export
#'
#' @examples
#' library(tibble)
#' df <- tibble(i = 1:20)
#' single(df)
single <- function(df) {
  
  checkmate::assert_data_frame(df)
  
  list(train = list(single = 1:nrow(df)), validation = list(single = NULL))
}

