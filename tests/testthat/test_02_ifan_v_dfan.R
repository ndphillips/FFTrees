context("Check tree algorithms: ifan vs dfan")


test_that("Can create an FFTrees object with 'dfan' algorithm", {

  hd_dfan <- FFTrees(diagnosis ~ .,
                     data = heartdisease,  # full set
                     algorithm = "dfan",
                     quiet = TRUE)

  expect_s3_class(object = hd_dfan, class = "FFTrees")

})



test_that("Different results by 'ifan' vs. 'dfan' algorithms", {

  trees_ifan <- FFTrees(diagnosis ~ .,
                        # data = heartdisease,  # full set
                        data = utils::head(heartdisease, 100),  # reduced set
                        algorithm = "ifan",
                        quiet = TRUE
  )

  trees_dfan <- FFTrees(diagnosis ~ .,
                        # data = heartdisease,  # full set
                        data = utils::head(heartdisease, 100),  # reduced set
                        algorithm = "dfan",
                        quiet = TRUE
  )

  expect_false(identical(
    trees_ifan$trees$definitions,
    trees_dfan$trees$definitions
  ))

})


# eof.
