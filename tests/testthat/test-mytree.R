context("test-mytree")


test_that("Can build tree based off of auto-generated tree in words", {
  x <- FFTrees(diagnosis ~ .,
    data = heartdisease,
    quiet = TRUE
  )

  best_tree_in_words <- x$trees$inwords[[1]]

  x <- FFTrees(diagnosis ~ .,
    data = heart.train,
    data.test = heart.test,
    my.tree = best_tree_in_words
  )

  expect_s3_class(object = x, class = "FFTrees")

})


test_that("Can build tree based off of custom tree in words (v1)", {

  best_tree_in_words <- "If thalach > 170, decide True.
   If slope = {flat}, decide False.
   If ca <= 0, decide False, otherwise, decide True."

  x <- FFTrees(diagnosis ~ .,
    data = heart.train,
    data.test = heart.test,
    my.tree = best_tree_in_words
  )

  expect_s3_class(object = x, class = "FFTrees")

  expect_identical(
    object = x$trees$definitions,
    expected = structure(list(
      tree = 1L, nodes = 3L, classes = "n;c;n", cues = "thalach;slope;ca",
      directions = ">;!=;>", thresholds = "170;flat;0", exits = "1;0;.5"
    ),
    row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")
    )
  )

})


test_that("Can build tree based off of custom tree in words (v2)", {
  best_tree_in_words <- "If thal = {fd}, decide False.
   If age > 40, decide False.
   If ca > 0, decide False, otherwise, decide True."

  x <- FFTrees(diagnosis ~ .,
    data = heart.train,
    data.test = heart.test,
    my.tree = best_tree_in_words
  )

  expect_s3_class(object = x, class = "FFTrees")

  expect_identical(
    object = x$trees$definitions,
    expected = structure(list(
      tree = 1L, nodes = 3L, classes = "c;n;n", cues = "thal;age;ca",
      directions = "!=;<=;<=", thresholds = "fd;40;0", exits = "0;0;.5"
    ),
    row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")
    )
  )

})


test_that("A custom tree in my.tree is built successfully (v3)", {

  # Create my.fft (from a verbal FFT description,
  # with the final node predicting the True (1:right) criterion value first:

  my.fft <- FFTrees(
    formula = diagnosis ~ .,
    data = heart.train,
    data.test = heart.test,
    decision.labels = c("Healthy", "Disease"),
    my.tree = "If sex = 1, predict Disease.
               If age < 45, predict Healthy.
               If thal = {fd, normal}, predict Healthy,
               (and ignore the rest of this sentence)."
    )

  expect_identical(
    object = my.fft$trees$definitions,
    expected = structure(list(
      tree = 1L, nodes = 3L, classes = "n;n;c", cues = "sex;age;thal",
      directions = "=;>=;!=", thresholds = "1;45;fd,normal", exits = "1;0;.5"
    ), class = c(
      "tbl_df",
      "tbl", "data.frame"
    ), row.names = c(NA, -1L))
  )

})


# eof.
