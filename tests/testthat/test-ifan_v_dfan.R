context("ifan vs dfan")

test_that("Can create FFTrees object with dfan", {
  object <- FFTrees(diagnosis ~ ., data = heartdisease, algorithm = "dfan")

  expect_is(object = object, class = "FFTrees")
})

test_that("Different results with ifan and dfan", {
  trees_ifan <- FFTrees(diagnosis ~ .,
    data = heartdisease,
    algorithm = "ifan"
  )

  trees_dfan <- FFTrees(diagnosis ~ .,
    data = heartdisease,
    algorithm = "dfan"
  )

  expect_false(identical(
    trees_ifan$trees$definitions,
    trees_dfan$trees$definitions
  ))
})
