
<!-- README.md is generated from README.Rmd. Please edit that file -->

# FFTrees

[![Build
Status](https://travis-ci.org/ndphillips/FFTrees.svg?branch=master)](https://travis-ci.org/ndphillips/FFTrees)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/FFTrees)](https://CRAN.R-project.org/package=FFTrees)
[![Rdoc](http://www.rdocumentation.org/badges/version/FFTrees)](http://www.rdocumentation.org/packages/FFTrees)
[![Downloads](http://cranlogs.r-pkg.org/badges/FFTrees?color=brightgreen)](http://www.r-pkg.org/pkg/FFTrees)

The goal of FFTrees is to create and visualize fast-and-frugal decision
trees (FFTs) like the one below that predicts heart disease.

## Installation

You can install the released version of FFTrees from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("FFTrees")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ndphillips/FFTrees")
```

## Examples

``` r
# Load package
library(FFTrees)
#> 
#>    O
#>   / \
#>  F   O
#>     / \
#>    F   Trees 1.5.2
#> 
#> Nathaniel.D.Phillips.is@gmail.com
#> FFTrees.guide() opens the guide.

# Create an FFTrees object from the heartdisease data
heart.fft <- FFTrees(formula = diagnosis ~., 
                     data = heart.train,
                     data.test = heart.test, 
                     decision.labels = c("Healthy", "Disease"))
#> Setting goal = 'wacc'
#> Setting goal.chase = 'waccc'
#> Setting cost.outcomes = list(hi = 0, mi = 1, fa = 1, cr = 0)
#> Growing FFTs with ifan
#> Fitting other algorithms for comparison (disable with do.comp = FALSE) ...

# See the print method
heart.fft
#> FFTrees 
#> - Trees: 7 fast-and-frugal trees predicting diagnosis
#> - Outcome costs: [hi = 0, mi = 1, fa = 1, cr = 0]
#> 
#> FFT #1: Definition
#> [1] If thal = {rd,fd}, decide Disease.
#> [2] If cp != {a}, decide Healthy.
#> [3] If ca <= 0, decide Healthy, otherwise, decide Disease.
#> 
#> FFT #1: Prediction Accuracy
#> Prediction Data: N = 153, Pos (+) = 73 (48%) 
#> 
#> |         | True + | True - |
#> |---------|--------|--------|
#> |Decide + | hi 64  | fa 19  | 83
#> |Decide - | mi 9   | cr 61  | 70
#> |---------|--------|--------|
#>             73       80       N = 153
#> 
#> acc  = 81.7%  ppv  = 77.1%  npv  = 87.1%
#> bacc = 82.0%  sens = 87.7%  spec = 76.2%
#> E(cost) = 0.183
#> 
#> FFT #1: Prediction Speed and Frugality
#> mcu = 1.73, pci = 0.87

# Plot the best tree applied to the test data
plot(heart.fft,
     data = "test",
     main = "Heart Disease")
```

<img src="man/figures/README-example-1.png" width="80%" />

``` r

# Create your own custom FFT 'in words' and apply it to data

# Create my own fft
my.fft <- FFTrees(formula = diagnosis ~., 
                  data = heart.train,
                  data.test = heart.test, 
                  decision.labels = c("Healthy", "Disease"),
                  my.tree = "If sex = 1, predict Disease.
                             If age < 45, predict Healthy.
                             If thal = {fd, normal}, predict Disease. 
                             Otherwise, predict Healthy")
#> Setting goal = 'wacc'
#> Setting goal.chase = 'waccc'
#> Setting cost.outcomes = list(hi = 0, mi = 1, fa = 1, cr = 0)
#> Fitting other algorithms for comparison (disable with do.comp = FALSE) ...


# See the print method
heart.fft
#> FFTrees 
#> - Trees: 7 fast-and-frugal trees predicting diagnosis
#> - Outcome costs: [hi = 0, mi = 1, fa = 1, cr = 0]
#> 
#> FFT #1: Definition
#> [1] If thal = {rd,fd}, decide Disease.
#> [2] If cp != {a}, decide Healthy.
#> [3] If ca <= 0, decide Healthy, otherwise, decide Disease.
#> 
#> FFT #1: Prediction Accuracy
#> Prediction Data: N = 153, Pos (+) = 73 (48%) 
#> 
#> |         | True + | True - |
#> |---------|--------|--------|
#> |Decide + | hi 64  | fa 19  | 83
#> |Decide - | mi 9   | cr 61  | 70
#> |---------|--------|--------|
#>             73       80       N = 153
#> 
#> acc  = 81.7%  ppv  = 77.1%  npv  = 87.1%
#> bacc = 82.0%  sens = 87.7%  spec = 76.2%
#> E(cost) = 0.183
#> 
#> FFT #1: Prediction Speed and Frugality
#> mcu = 1.73, pci = 0.87

# Plot my custom fft and see how it did
plot(my.fft,
     data = "test",
     main = "Custom FFT")
```

<img src="man/figures/README-example-2.png" width="80%" />
