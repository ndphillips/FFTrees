context("Costs work")


test_that("Using goal = 'cost' kills a high cost cue", {
  trees_bacc <- FFTrees(diagnosis ~ .,
    data = heartdisease,
    cost.cues = list(ca = 100),
    goal = "bacc"
  )

  trees_cost <- FFTrees(diagnosis ~ .,
    data = heartdisease,
    cost.cues = list(ca = 100),
    goal = "cost"
  )

  testthat::expect_gt(
    trees_bacc$trees$stats$train$cost[1],
    trees_cost$trees$stats$train$cost[1]
  )
})

test_that("Changing costs without changing goal does not affect tree", {
  trees_bacc <- FFTrees(diagnosis ~ .,
    data = heartdisease,
    cost.outcomes = list(hi = 0, fa = 1, mi = 1, cr = 0),
    goal = "bacc"
  )

  trees_cost <- FFTrees(diagnosis ~ .,
    data = heartdisease,
    cost.outcomes = list(hi = 0, fa = 1, mi = 10, cr = 0),
    goal = "bacc"
  )

  testthat::expect_equal(
    object = trees_bacc$trees$definitions,
    expected = trees_cost$trees$definitions
  )
})

test_that("Changing costs and goal = 'cost' does affect tree", {
  trees_cost1 <- FFTrees(diagnosis ~ .,
    data = heartdisease,
    cost.outcomes = list(hi = 0, fa = 1, mi = 1, cr = 0),
    goal = "cost"
  )

  trees_cost2 <- FFTrees(diagnosis ~ .,
    data = heartdisease,
    cost.outcomes = list(hi = 0, fa = 1, mi = 10, cr = 0),
    goal = "cost"
  )

  testthat::expect_false(object = identical(
    trees_cost1$trees$definitions,
    trees_cost2$trees$definitions
  ))
})
