context("Create basic FFTrees objects")

test_that("Can create an FFTrees object", {

  object <- FFTrees(diagnosis ~ .,
                    data = heartdisease,
                    quiet = TRUE
  )

  expect_s3_class(object = object, class = "FFTrees")

})


test_that("Can create an FFTrees object with only one cue", {

  object <- FFTrees(diagnosis ~ age,
                    data = heartdisease,
                    quiet = TRUE)

  expect_s3_class(object = object, class = "FFTrees")

})

# eof.
