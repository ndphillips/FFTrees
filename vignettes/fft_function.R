## ---- echo = F, message = F, results = 'hide'----------------------------
library(fft)

## ------------------------------------------------------------------------
head(heartdisease)

## ------------------------------------------------------------------------
set.seed(100) # For reproducability

heart.fft <- fft(
  train.cue.df = heartdisease[,names(heartdisease) != "diagnosis"],
  train.criterion.v = heartdisease$diagnosis,
  train.p = .5,
  max.levels = 4
  )

## ------------------------------------------------------------------------
class(heart.fft)

## ------------------------------------------------------------------------
names(heart.fft)

## ------------------------------------------------------------------------
heart.fft$cue.accuracies

## ------------------------------------------------------------------------
heart.fft$trees[,1:6]   # Tree info are in columns 1:6

## ------------------------------------------------------------------------
heart.fft$trees[,7:15]   # Training stats are in columns 7:15

## ------------------------------------------------------------------------
heart.fft$trees[,16:24]   # Test stats are in columns 16:24

## ------------------------------------------------------------------------
# which tree had the best training statistics?
heart.fft$best.train.tree

## ------------------------------------------------------------------------
# Which tree had the best testing statistics?
heart.fft$best.test.tree

## ------------------------------------------------------------------------
heart.fft$train.decision.df[1:5,]

## ------------------------------------------------------------------------
heart.fft$train.levelout.df[1:5,]

## ------------------------------------------------------------------------
heart.fft$cart

## ------------------------------------------------------------------------
heart.fft$lr

## ---- fig.width = 6, fig.height = 6--------------------------------------
plot(heart.fft,
     which.tree = "best.train",
     which.data = "test",
     description = "Heart Disease",
     decision.names = c("Healthy", "Disease")
     )

