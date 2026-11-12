context("Rounding numeric cue thresholds by 'digits'")

# Verify that 'digits' enables rounding of cue threshold values
# (and older 'rounding' argument is deprecated):


test_that("digits is stored as x$params$digits (default = 2)", {

  x <- FFTrees(diagnosis ~ .,
               data = heartdisease,
               quiet = TRUE)

  expect_equal(x$params$digits, 2L)

})


test_that("digits is stored when set by the user", {

  x <- FFTrees(diagnosis ~ .,
               data = heartdisease,
               digits = 0,
               quiet = TRUE)

  expect_equal(x$params$digits, 0L)

})


test_that("digits = 0 rounds numeric cue thresholds to integers", {

  x <- FFTrees(diagnosis ~ .,
               data = heartdisease,
               digits = 0,
               quiet = TRUE)

  cue_stats <- x$cues$stats$train

  # Consider only numeric/integer cues (which can have non-integer thresholds):
  num_cues <- cue_stats[substr(cue_stats$class, 1, 1) %in% c("n", "i"), ]
  thresh   <- suppressWarnings(as.numeric(num_cues$threshold))
  thresh   <- thresh[!is.na(thresh)]

  # With digits = 0, all numeric thresholds should be whole numbers:
  expect_true(all(thresh == round(thresh, 0)))

})


test_that("FFT-in-words uses x$params$digits for rounding", {

  x1 <- FFTrees(diagnosis ~ .,
                data = heartdisease,
                digits = 1,
                quiet = TRUE)

  # Re-expressing in words should not error and should respect stored digits:
  words <- inwords(x1, data = "train")

  expect_type(words, "character")

})


test_that("deprecated 'rounding' argument warns and maps to 'digits'", {

  expect_warning(
    x <- FFTrees(diagnosis ~ .,
                 data = heartdisease,
                 rounding = 0,
                 quiet = TRUE),
    regexp = "rounding"
  )

  # 'rounding' value should have been used as 'digits':
  expect_equal(x$params$digits, 0L)

})

# eof.
