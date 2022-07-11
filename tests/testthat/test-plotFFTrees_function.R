test_that("graphic parameters not changed in function", {
  par0 <- par(no.readonly = TRUE)

  heart.fft <- FFTrees(
    formula = diagnosis ~ .,
    data = utils::head(heartdisease, 100)
  )
  plot(heart.fft,
    main = "Heart Disease Diagnosis",
    decision.labels = c("Absent", "Present")
  )

  expect_equal(par0, par(no.readonly = TRUE))
  par(par0)
})
