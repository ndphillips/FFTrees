## ---- echo = F, message = F, results = 'hide'----------------------------
library(fft)

## ---- fig.width = 6, fig.height = 6, echo = F, fig.align='center'--------
heart.fft <- fft(heartdisease[,names(heartdisease) != "diagnosis"],
                 heartdisease$diagnosis,
                 train.p = .5
                 )

plot(heart.fft,
     description = "Heart Disease",
     decision.names = c("Healthy", "Disease")
)

