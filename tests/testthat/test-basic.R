context("Creating basic FFTrees objects")

test_that("Can create FFTrees object", {
  object <- FFTrees(diagnosis ~ .,
    data = heartdisease
  )

  expect_is(object = object, class = "FFTrees")
})



test_that("Can create FFTrees object with one cue", {
  object <- FFTrees(diagnosis ~ age, data = heartdisease)

  expect_is(object = object, class = "FFTrees")
})
