context("Verify level summary statistics")


test_that("Train summary statistics are correct", {

  # Create an FFTrees object:
  x <- FFTrees(diagnosis ~ .,
    data = heart.train,
    data.test = heart.test,
    quiet = TRUE
  )


  # mcu:

  levelstats_mcu <- as.numeric(sapply(x$trees$decisions$train, FUN = function(x) {
    mean(x$levelout)
  }))

  results_mcu <- as.numeric(x$trees$stats$train$mcu)

  testthat::expect_equal(levelstats_mcu, results_mcu)


  # cost:

  levelstats_cost <- as.numeric(sapply(x$trees$decisions$train, FUN = function(x) {
    mean(x$cost)
  }))

  results_cost <- as.numeric(x$trees$stats$train$cost)

  testthat::expect_equal(levelstats_cost, results_cost)

})


# eof.
