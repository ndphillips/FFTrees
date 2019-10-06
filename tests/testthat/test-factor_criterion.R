context("test-factor criterion")

library(dplyr)

test_that("FFTrees runs with binary factor criterion", {

  data.train <- heart.train
  data.test <- heart.test

data.train.fac <- data.train %>%
  mutate(diagnosis = factor(diagnosis, levels = c(TRUE, FALSE),
                            labels = c("Healthy", "Sick")))


data.test.fac <- data.test %>%
  mutate(diagnosis = factor(diagnosis, levels = c(TRUE, FALSE),
                            labels = c("Healthy", "Sick")))

out <- FFTrees(formula = diagnosis ~.,
               data = data.train.fac,
               data.test = data.test.fac)


testthat::expect_is(out, class = "FFTrees")

# class prediction output is factor

testthat::expect_is(predict(out, newdata = data.test.fac), "factor")

# Probability prediction is matrix

testthat::expect_is(predict(out, newdata = data.test.fac, type = "prob"), "matrix")

# probability prediciton matrix contains decision labels as column names
testthat::expect_true(setequal(colnames(predict(out, newdata = data.test.fac, type = "prob")),
                               out$metadata$decision.labels))

})

