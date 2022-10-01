context("Creating basic FFTrees objects")

test_that("Can create FFTrees object", {

  object <- FFTrees(diagnosis ~ .,
                    data = heartdisease
  )

  expect_s3_class(object = object, class = "FFTrees")

})



test_that("Can create FFTrees object with one cue", {

  object <- FFTrees(diagnosis ~ age, data = heartdisease)

  expect_s3_class(object = object, class = "FFTrees")

})

# eof.
