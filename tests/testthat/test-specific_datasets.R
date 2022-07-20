context("test-specific_datasets")

test_that("Can replicate Phillips et al.2017 heartdisease trees", {
  phillips_trees <- FFTrees(diagnosis ~ .,
    data = heart.train,
    data.test = heart.test,
    cost.outcomes = list(hi = 0, mi = 1, fa = 1, cr = 0),
    goal = "bacc"
  )

  expect_true(object = identical(
    phillips_trees$trees$definitions[1, ],
    structure(list(
      tree = 1L,
      nodes = 3L,
      classes = "c;c;n",
      cues = "thal;cp;ca",
      directions = "=;=;>",
      thresholds = "rd,fd;a;0",
      exits = "1;0;0.5"
    ),
    row.names = c(NA, -1L),
    class = c("tbl_df", "tbl", "data.frame")
    )
  ))
})
