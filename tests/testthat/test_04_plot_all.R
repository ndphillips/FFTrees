context("Verify graphics: Plot all")

test_that("Graphics parameters are not changed by plotting", {

  par_0 <- par(no.readonly = TRUE)

  # Create FFTrees object:
  heart_fft <- FFTrees(
    formula = diagnosis ~ .,
    data = utils::head(heartdisease, 25),  # reduced set
    quiet = TRUE
  )

  # Main:
  plot(heart_fft,
       what = "all",
       main = "Heart disease diagnosis",
       decision.labels = c("absent", "present")
  )

  testthat::expect_equal(par_0, par(no.readonly = TRUE))

  par(par_0)

})

# eof.
