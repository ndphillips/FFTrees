context("Handle NA data")

# NA values in predictors:

test_that("FFTrees works with NA values in categorical predictors", {

  # Training data with NA values in 3 categorical predictors:
  data_NA_categorical <- data.frame(p_1 = factor(c(1, 1, 1, 2, 2, 2, 3, 3, 3, NA)),                         # factor
                                    p_2 = c("A", "A", "A", "B", "B", "B", "C", "C", "C", NA),               # character
                                    p_3 = c(NA, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE),  # logical
                                    crit = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE))

  # Main: Create an FFTrees object:
  fft_NA_1 <- FFTrees(crit ~ .,
                 data = data_NA_categorical,
                 do.comp = TRUE,
                 quiet = TRUE)

  testthat::expect_is(fft_NA_1, "FFTrees")

})



test_that("FFTrees works with NA values in 2 numeric predictors", {

  # Training data with NA values in numeric predictors:
  # Putting two predictors here because of another bug (https://github.com/ndphillips/FFTrees/issues/170)
  data_NA_numeric <- data.frame(p_1 = c(1, 2, 3, 4, NA),
                                p_2 = c(NA, 6, 3, 5, 3),
                                crit = c(TRUE, TRUE, FALSE, TRUE, TRUE))

  # Create an FFTrees object:
  fft_NA_2 <- FFTrees(crit ~ .,
                 data = data_NA_numeric,
                 do.comp = TRUE,
                 quiet = TRUE)

  testthat::expect_is(fft_NA_2, "FFTrees")

})


# eof.
