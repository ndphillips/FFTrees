context("Verify graphics: Plot all")

test_that("Graphics parameters not changed in function", {

  par0 <- par(no.readonly = TRUE)

  heart_fft <- FFTrees(
    formula = diagnosis ~ .,
    data = utils::head(heartdisease, 100)
  )

  plot(heart_fft,
       what = "all",
       main = "Heart Disease Diagnosis",
       decision.labels = c("Absent", "Present")
  )

  expect_equal(par0, par(no.readonly = TRUE))
  par(par0)

})

# eof.
