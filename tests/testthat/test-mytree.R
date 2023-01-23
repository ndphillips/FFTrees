context("test-mytree")


test_that("Can build tree based on best auto-generated tree in words (v1)", {

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


test_that("Can build tree based on custom tree in words (v2)", {

  my_tree_in_words <- "If thalach > 170, decide True.
   If slope = {flat}, decide False.
   If ca <= 0, decide False, otherwise, decide True."

  x <- FFTrees(diagnosis ~ .,
    data = heart.train,
    data.test = heart.test,
    my.tree = my_tree_in_words
  )

  expect_s3_class(object = x, class = "FFTrees")

  expect_identical(
    object = x$trees$definitions,
    expected = structure(list(
      tree = 1L,
      nodes = 3L,
      classes = paste(c("n", "c", "n"),         collapse = fft_node_sep), # "n;c;n",
      cues = paste(c("thalach", "slope", "ca"), collapse = fft_node_sep), # "thalach;slope;ca",
      directions = paste(c(">", "!=", ">"),     collapse = fft_node_sep), # ">;!=;>",
      thresholds = paste(c("170", "flat", "0"), collapse = fft_node_sep), # "170;flat;0",
      exits = paste(c("1", "0", ".5"),          collapse = fft_node_sep)  # "1;0;.5"
    ),
    row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")
    )
  )

})


test_that("Can build tree based on custom tree in words (v3)", {

  my_tree_2 <- "If thal = {fd}, decide False.
   If age > 40, decide False.
   If ca > 0, decide False, otherwise, decide True."

  x <- FFTrees(diagnosis ~ .,
    data = heart.train,
    data.test = heart.test,
    my.tree = my_tree_2
  )

  expect_s3_class(object = x, class = "FFTrees")

  expect_identical(
    object = x$trees$definitions,
    expected = structure(list(
      tree = 1L,
      nodes = 3L,
      classes = paste(c("c", "n", "n"),       collapse = fft_node_sep), # "c;n;n",
      cues = paste(c("thal", "age", "ca"),    collapse = fft_node_sep), # "thal;age;ca",
      directions = paste(c("!=", "<=", "<="), collapse = fft_node_sep), # "!=;<=;<=",
      thresholds = paste(c("fd", "40", "0"),  collapse = fft_node_sep), # "fd;40;0",
      exits = paste(c("0", "0", ".5"),        collapse = fft_node_sep)  # "0;0;.5"
    ),
    row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")
    )
  )

})


test_that("A custom tree in my.tree is built successfully (v4)", {

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
      tree = 1L,
      nodes = 3L,
      classes = paste(c("n", "n", "c"),             collapse = fft_node_sep), # "n;n;c",
      cues = paste(c("sex", "age", "thal"),         collapse = fft_node_sep), # "sex;age;thal",
      directions = paste(c("=", ">=", "!="),        collapse = fft_node_sep), # "=;>=;!=",
      thresholds = paste(c("1", "45", "fd,normal"), collapse = fft_node_sep), # "1;45;fd,normal",
      exits = paste(c("1", "0", ".5"),              collapse = fft_node_sep)  # "1;0;.5"
    ), class = c(
      "tbl_df",
      "tbl", "data.frame"
    ), row.names = c(NA, -1L))
  )

})


# eof.
