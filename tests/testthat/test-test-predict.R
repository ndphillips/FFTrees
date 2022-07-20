context("test-predictFFTrees")

test_that("predict.FFTrees() works", {
  data_train <- heart.train
  data_test <- heart.test


  x <- FFTrees(diagnosis ~ .,
    data = data_train
  )


  class_test_pred <- predict(x, newdata = data_test)
  prob_test_pred <- predict(x, newdata = data_test, type = "prob")
  both_test_pred <- predict(x, newdata = data_test, type = "both")


  # Raw predictions are a logical vector
  testthat::expect_is(class_test_pred, "logical")

  # Raw predictions have same length as heart.test
  testthat::expect_length(class_test_pred, nrow(data_test))

  testthat::expect_equal(nrow(prob_test_pred), nrow(data_test))

  # Cases predicted TRUE have higher overall probability than those predicted FALSE
  out <- aggregate(prob_1 ~ class, FUN = mean, data = both_test_pred)
  testthat::expect_true(out$prob_1[out$class == TRUE] > out$prob_1[out$class == FALSE])

  # No variation in class outcomes when prob_1 is the same
  out <- aggregate(class ~ prob_1, FUN = sd, data = both_test_pred)
  testthat::expect_true(all(out$class == 0))
})
