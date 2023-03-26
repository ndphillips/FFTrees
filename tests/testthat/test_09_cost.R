context("Costs work")


test_that("Using goal = 'cost' kills a high cost cue", {

  # Create FFTs with cue costs for goal 'bacc':
  trees_bacc <- FFTrees(diagnosis ~ .,
                        data = heartdisease,
                        cost.cues = list(ca = 100),
                        goal = "bacc",
                        quiet = TRUE
  )

  # Create FFTs with cue costs for goal 'cost':
  trees_cost <- FFTrees(diagnosis ~ .,
                        data = heartdisease,
                        cost.cues = list(ca = 100),
                        goal = "cost",
                        quiet = TRUE
  )

  # Compare:
  testthat::expect_gt(
    trees_bacc$trees$stats$train$cost[1],
    trees_cost$trees$stats$train$cost[1]
  )

})


test_that("Changing costs without changing goal does NOT affect FFT creation", {

  # Create FFTs with outcome costs 1 for goal 'bacc':
  trees_bacc <- FFTrees(diagnosis ~ .,
                        data = heartdisease,
                        cost.outcomes = list(hi = 0, fa = 1, mi = 1, cr = 0),
                        goal = "bacc",
                        quiet = TRUE
  )

  # Create FFTs with outcome costs 2 for goal 'bacc':
  trees_cost <- FFTrees(diagnosis ~ .,
                        data = heartdisease,
                        cost.outcomes = list(hi = 0, fa = 10, mi = 1, cr = 0),
                        cost.cues = list(ca = 100),
                        goal = "bacc",
                        quiet = TRUE
  )

  # Compare:
  testthat::expect_equal(
    object = trees_bacc$trees$definitions,
    expected = trees_cost$trees$definitions
  )

})


test_that("Changing costs and goal = 'cost' DOES affect FFT creation", {

  # Create FFTs with outcome costs and goal 'cost':
  trees_cost_1 <- FFTrees(diagnosis ~ .,
                          data = heartdisease,
                          cost.outcomes = list(hi = 0, fa = 1, mi = 1, cr = 0),
                          goal = "cost",
                          quiet = TRUE
  )

  # Create FFTs with different costs and goal 'cost':
  trees_cost_2 <- FFTrees(diagnosis ~ .,
                          data = heartdisease,
                          cost.outcomes = list(hi = 0, fa = 2, mi = 4, cr = 0),
                          goal = "cost",
                          quiet = TRUE
  )

  # Compare:
  testthat::expect_false(object = identical(
    trees_cost_1$trees$definitions,
    trees_cost_2$trees$definitions
  ))

})


# eof.
