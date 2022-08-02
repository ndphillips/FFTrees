context("test-show_cues")

test_that("show_cues() works", {
  set.seed(100)
  x <- FFTrees(
    formula = diagnosis ~ .,
    data = heartdisease,
    train.p = .5
  )
  expect_silent(showcues(x, quiet = TRUE))
})
