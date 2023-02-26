test_that("FFTrees works with NA numeric features", {

  # Training data with an NA numeric feature
  # Putting two features here because of another bug (https://github.com/ndphillips/FFTrees/issues/170)
  data_missing_numeric <- data.frame(x = c(1, 2, 3, 4, NA),
                                     y = c(4, 6, 3, 5, 3),
                                     crit = c(TRUE, TRUE, FALSE, TRUE, TRUE))

  fft <- FFTrees(crit ~ .,
                 data = data_missing_numeric,
                 comp = FALSE)

  testthat::expect_is(fft, "FFTrees")

})
