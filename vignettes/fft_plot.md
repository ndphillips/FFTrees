---
title: "plot.fft() function"
author: "Nathaniel Phillips"
date: "2016-07-19"
output: rmarkdown::html_vignette
bibliography: fft.bib
csl: apa.csl
vignette: >
  %\VignetteIndexEntry{Visualizing FFTs with plot()}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---




Applying the `plot()` function to an `fft` object will visualize the tree.

## Creating an fft object

Let's create an fft object called `titanic.fft` from the `titanic` dataset.


```r
titanic.fft <- fft(
  formula = survived ~.,
  data = titanic
  )
```


## Plotting a tree

To plot the tree, use `plot()`:


```r
plot(titanic.fft)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

You can specify additional arguments to the `plot()` command that will change what is displayed

- `which.tree`: Which tree do you want to plot? You can specify an integer such as `which.tree = 2` will plot the tree #2 in the fft object, or `which.tree = "best.train"` which will use the best training tree.

- `data`: Which data do you want to apply the tree to? You can specify `data = "train"` or `data = "test"` to use the training or testing datasets stored in the `fft` object. Alternatively, you can specify a new dataset (e.g.; `data = test.data`

For example, let's plot tree # 4 with `which.tree = 4`.


```r
plot(titanic.fft, 
     which.tree = 4)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

Now let's apply this tree to a new dataset. I'll apply the tree to only data from children:


```r
plot(titanic.fft,
     which.tree = 2,
     data = subset(titanic, age == "child")
     )
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)
