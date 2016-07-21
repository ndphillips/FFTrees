---
title: "Example FFTs"
author: "Nathaniel Phillips"
date: "2016-07-21"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Examples of FFTs}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---




## Mushrooms

The `mushrooms` dataset contains data about mushrooms. The goal of our model is to predict which mushrooms are poisonous.

First, we'll create a training and test dataset:


```r
set.seed(100)
train.cases <- sample(c(T, F), size = nrow(mushrooms), replace = T)
mushrooms.train <- mushrooms[train.cases,]
mushrooms.test <- mushrooms[train.cases == F,]
```



```r
mushrooms.fft <- fft(formula = poisonous ~.,
                     data = mushrooms.train,
                     data.test = mushrooms.test)
```

Here is the best training tree applied to the test data:


```r
plot(mushrooms.fft, data = "test")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)


## Iris

The `iris` dataset contains data about 150 flowers. Our goal is to predict which flowers are of the class Virginica


```r
head(iris)
```

```
##   sep.len sep.wid pet.len pet.wid virginica
## 1     5.1     3.5     1.4     0.2     FALSE
## 2     4.9     3.0     1.4     0.2     FALSE
## 3     4.7     3.2     1.3     0.2     FALSE
## 4     4.6     3.1     1.5     0.2     FALSE
## 5     5.0     3.6     1.4     0.2     FALSE
## 6     5.4     3.9     1.7     0.4     FALSE
```

We'll create a new fft object called `iris.fft` using the `fft()` function. In this case, we won't create a separate training and test dataset, we'll just fit the tree to the entire dataset:


```r
iris.fft <- fft(
  formula = virginica ~.,
  data = iris
  )
```

Here is the final best training tree (tree 6):


```r
plot(iris.fft)
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

Here is tree number 2, which is a bit more conservative than tree 6.


```r
plot(iris.fft, which.tree = 2)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)
