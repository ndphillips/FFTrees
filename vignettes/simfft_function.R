## ---- echo = F, message = F, results = 'hide'----------------------------
library(fft)

## ------------------------------------------------------------------------
head(breastcancer)

## ---- results = 'hide'---------------------------------------------------
set.seed(100) # For reproducability

bcancer.fft.sim <- simfft(
  train.cue.df = breastcancer[,names(breastcancer) != "diagnosis"],
  train.criterion.v =  breastcancer$diagnosis == "M",
  train.p = .1,
  sim.n = 10
)

## ------------------------------------------------------------------------
bcancer.fft.sim

## ---- fig.width = 5, fig.height = 5--------------------------------------
simfftplot(bcancer.fft.sim,
           roc = F
           )

## ---- fig.width = 5, fig.height = 5--------------------------------------
simfftplot(bcancer.fft.sim,
           roc = T,
           which.data = "train"
           )

## ---- fig.width = 5, fig.height = 5--------------------------------------
simfftplot(bcancer.fft.sim,
           roc = T,
           which.data = "test"
           )

## ---- fig.width = 5, fig.height = 5--------------------------------------
simfftplot(bcancer.fft.sim,
           roc = T,
           lr = T,
           cart = T,
           which.data = "train"
           )

## ---- fig.width = 5, fig.height = 5--------------------------------------
simfftplot(bcancer.fft.sim,
           roc = T,
           lr = T,
           cart = T,
           which.data = "test"
           )

