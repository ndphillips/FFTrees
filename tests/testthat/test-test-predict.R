context("test-predictFFTrees")

test_that("predict.FFTrees() works", {
  x <- FFTrees(diagnosis ~ .,
    data = heart.train
  )

  # Raw predictions are a logical vector
  expect_is(predict(x, newdata = heart.test), "logical")

  # Raw predictions have same length as heart.test
  expect_length(predict(x, newdata = heart.test), nrow(heart.test))

  expect_is(predict(x, newdata = heart.test, type = "prob"), "matrix")

  expect_true(nrow(predict(x, newdata = heart.test, type = "prob")) == nrow(heart.test))
})
