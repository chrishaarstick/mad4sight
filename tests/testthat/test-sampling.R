
# sampling unit tests -----------------------------------------------------


# R Libraries
library(mad4sight)
library(purrr)
library(testthat)
library(checkmate)
library(lubridate)
library(dplyr)

context("sampling unit tests")


# Create simulated data
set.seed(319)
n <- 20
df <- tibble(index = 1:n)



# slices unit tests -------------------------------------------------------

test_that("slices creates samples as expected", {
  s1 <- slices(df, width = 15, horizon = 1, skip = 0, label = "slice")
  
  expect_list(s1)
  expect_subset(names(s1), c("train", "validation"))
  expect_equal(length(s1$train), length(s1$validation))
  expect_equal(names(s1$train), names(s1$validation))
  expect_true(all(grepl("slice", names(s1$training))))
})


test_that("slices width works as expected", {
 
  .width <- 15
   s1 <- slices(df, width = .width , horizon = 1, skip = 0, label = "slice")
  
  expect_true(pluck(s1, "train") %>% 
                map(., ~length(.x)) %>%
                map_lgl(~.x == .width) %>%
                all())
})



test_that("slices horizon works as expected", {
  
  .horizon <- 2
  s1 <- slices(df, width = 10 , horizon = .horizon, skip = 0, label = "slice")
  
  expect_true(pluck(s1, "validation") %>% 
                map(., ~length(.x)) %>%
                map_lgl(~.x == .horizon) %>%
                all())
})



test_that("slices skip works as expected", {
  
  .skip <- 1
  s1 <- slices(df, width = 10 , horizon = 1, skip = .skip, label = "slice")
  
  n1 <- pluck(s1, "validation") %>%
    as.numeric() %>% 
    diff()
  
  expect_true(all(n1 == .skip + 1))
})



# split unit tests --------------------------------------------------------


test_that("split works as expected", {
  
  .ratio <- .5
  s1 <- split(df, ratio = .ratio)
  
  expect_list(s1)
  expect_subset(names(s1), c("train", "validation"))
  expect_equal(length(s1$train), length(s1$validation))
  expect_equal(length(s1$train$split), floor(.ratio * nrow(df)))
  expect_equal(length(s1$validation$split), nrow(df) - length(s1$train$split))
})



# single unit tests -------------------------------------------------------

test_that("single works as expected", {
  s1 <- single(df)
  
  expect_list(s1)
  expect_subset(names(s1), c("train", "validation"))
  expect_equal(length(s1$train), length(s1$validation))
  expect_equal(length(s1$train$single), nrow(df))
  expect_null(s1$validation$single)
})


# samples unit tests ------------------------------------------------------

test_that("samples works as expected", {
  
  s1 <- samples(method = "single", args = list(df = df))
  s2 <- samples(method = "split", args = list(df = df, ratio = .5))
  s3 <- samples(method = "slices", args = list(df = df, width = 15, horizon = 1))
  
  expect_class(s1, "samples")
  expect_class(s2, "samples")
  expect_class(s3, "samples")
  
  expect_error(samples("nosamplefun"))
  expect_error(samples(method = "slices", args = list(size = 5)))
  expect_error(samples(method = "slices", args = list(df = df, width = 15)))
})
