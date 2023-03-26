context("Verify graphics: Plot cues")


test_that("Plotting cues by showcues() works", {

  set.seed(100)

  # Create an FFTrees object:
  x <- FFTrees(
    formula = diagnosis ~ .,
    data = heartdisease,
    train.p = .50,
    quiet = TRUE
  )

  # Main:
  expect_silent(showcues(x, quiet = TRUE))

})


# eof.
