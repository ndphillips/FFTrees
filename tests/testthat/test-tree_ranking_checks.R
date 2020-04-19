test_that("train summary statistics are correct", {


  x <- FFTrees(diagnosis ~.,
               data = heart.train,
               data.test = heart.test)

  # column means of levelout matches results

  levelstats_mcu <- as.numeric(colMeans(x$trees$results$train$levelout))
  results_mcu <- as.numeric(x$trees$results$train$stats$mcu)

  testthat::expect_true(all(levelstats_mcu == results_mcu))

  # column means of cost matches results

  levelstats_cost <- as.numeric(colMeans(x$trees$results$train$cost))
  results_cost <- as.numeric(x$trees$results$train$stats$cost)

  testthat::expect_true(all(levelstats_mcu == results_mcu))





})
