## ---- echo = F, message = F, results = 'hide'----------------------------
library(fft)

## ------------------------------------------------------------------------
set.seed(100) # For reproducability due to training / testing data split

heart.fft <- fft(
  train.cue.df = heartdisease[,names(heartdisease) != "diagnosis"],
  train.criterion.v = heartdisease$diagnosis,
  train.p = .5,
  max.levels = 4
  )

## ---- fig.width = 6, fig.height = 6--------------------------------------
plot(heart.fft,
     which.tree = "best.train",
     which.data = "test",
     description = "Heart Disease",
     decision.names = c("Healthy", "Disease")
     )

## ---- fig.width = 6, fig.height = 6--------------------------------------
plot(heart.fft,
     which.tree = 1,
     which.data = "test",
     description = "Heart Disease",
     decision.names = c("Healthy", "Disease")
     )

## ---- fig.width = 6, fig.height = 6--------------------------------------
plot(heart.fft,
     roc = T
     )

## ---- fig.width = 6, fig.height = 6--------------------------------------
plot(heart.fft,
     roc = T,
     lr = T,
     cart = T
     )

