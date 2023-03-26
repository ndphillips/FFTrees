context("Replicate published FFTs (Phillips et al., 2017)")

test_that("Can replicate the heartdisease FFTs of Phillips et al. (2017)", {

  # Phillips, N. D., Neth, H., Woike, J. K. & Gaissmaier, W. (2017).
  # FFTrees: A toolbox to create, visualize, and evaluate fast-and-frugal decision trees.
  # Judgment and Decision Making, 12 (4), 344–368.
  # URL/doi <https://doi.org/10.1017/S1930297500006239>

  # Create an FFTrees object:
  pub_ffts <- FFTrees(diagnosis ~ .,
    data = heart.train,
    data.test = heart.test,
    cost.outcomes = list(hi = 0, fa = 1, mi = 1, cr = 0),
    goal = "bacc",
    quiet = TRUE
  )

  # Compare:
  expect_true(object = identical(
    pub_ffts$trees$definitions[1, ],
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


# eof.
