---
title: "The fft package"
author: "Nathaniel Phillips"
date: "2016-07-19"
output: rmarkdown::html_vignette
bibliography: fft.bib
csl: apa.csl
vignette: >
  %\VignetteIndexEntry{Overview of the fft package}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

## Overview

The purpose of this package is to produce, compare, and display Fast and Frugal Decision Trees (FFTs) like the one below. FFTs are simple, transparent decision strategies that use minimal information to make decisions [see @gigerenzer1999fast;@gigerenzer1999good]. They are frequently prefereable to more complex decision strategies (such as Logistic Regression) because they rarely overfit data [@gigerenzer2009homo] and are easy to interpret and impliment in real-world decision tasks [@marewski2012heuristic].




<img src="figure/unnamed-chunk-2-1.png" title="plot of chunk unnamed-chunk-2" alt="plot of chunk unnamed-chunk-2" style="display: block; margin: auto;" />

The main function in the package is `fft()` which takes a formula `formula` and a training dataset `data` arguments and returns several FFTs which attempt to classify training cases into criterion classes. The `fft()` function returns a list object with the "fft" class which can then be passed to other functions such as `plot()` (which plots the FFTs), and `predict()` which applies an existing set of FFTs to new datasets.

## Datasets

The package contains several datasets taken from the [UCI Machine Learning Repository](http://archive.ics.uci.edu/ml/).

- `heartdisease` -- patients suspected of having heart disease [source](http://archive.ics.uci.edu/ml/datasets/Heart+Disease)
- `breastcancer` -- patients suspected of having breast cancer [source](https://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Wisconsin+(Diagnostic))
- `titanic` -- records of which passengers on the Titanic survived
- `forestfires` -- forest fire statistics [source](http://archive.ics.uci.edu/ml/datasets/Forest+Fires)
- `wine` -- ratings of wine quality [source](http://archive.ics.uci.edu/ml/datasets/Wine)
- `income` -- Census data from > 30,000 US residents [source](http://archive.ics.uci.edu/ml/datasets/Adult)
- `bank` -- Bank marketing dataset [source](http://archive.ics.uci.edu/ml/datasets/Bank+Marketing)

## Guides

To learn more about the package, click the following guides:

- [Creating FFTs with fft()](fft_function.html)
- [Visualizing FFTs with plot()](fft_plot.html)
- [Lots of example FFTs](fft_example.html)

## Bibliography
