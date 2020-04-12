[![Build Status](https://travis-ci.org/ndphillips/FFTrees.svg?branch=master)](https://travis-ci.org/ndphillips/FFTrees)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/FFTrees)](https://CRAN.R-project.org/package=FFTrees)
[![Rdoc](http://www.rdocumentation.org/badges/version/FFTrees)](http://www.rdocumentation.org/packages/FFTrees)
[![Downloads](http://cranlogs.r-pkg.org/badges/FFTrees?color=brightgreen)](http://www.r-pkg.org/pkg/FFTrees)

# FFTrees

- FFTrees is an R package to create and visualize fast-and-frugal decision trees (FFTs) like the one below that predicts heart disease.

- Additional information about FFTs, and the FFTrees package can be found at [Phillips, Neth, Woike & Gaissmaier, 2017](http://journal.sjdm.org/17/17217/jdm17217.pdf). For seminal papers on FFTs, consult [Martignon, Katsikopoulos & Woike,  2008](http://www.sciencedirect.com/science/article/pii/S0022249608000370) and [Martignon, Vitouch, Takezawa & Forster, 2003 ](https://books.google.ch/books?hl=en&lr=&id=J9DdqEFo29AC&oi=fnd&pg=PA189&dq=martignon+and+yet+enlightened&ots=u9nvGtvQdz&sig=OFkVfi8xLDIzE1Lecb5HQnYA6Fo#v=onepage&q=martignon%20and%20yet%20enlightened&f=false)


```R
# Install FFTrees from CRAN
install.packages("FFTrees")

# Load package
library(FFTrees)

# Create an FFTrees object from the heartdisease data
heart.fft <- FFTrees(formula = diagnosis ~., 
                     data = heart.train,
                     data.test = heart.test)
                       
# Plot the best tree applied to the test data
plot(heart.fft,
     data = "test",
     main = "Heart Disease", 
     decision.labels = c("Healthy", "Disease"))
```

![](inst/HeartFFT.jpeg)


