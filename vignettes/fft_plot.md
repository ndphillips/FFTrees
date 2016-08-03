---
title: "plot.fft() function"
author: "Nathaniel Phillips"
date: "2016-07-21"
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

To plot the tree from an fft object, use `plot()`. You can add some stylistic arguments like `description` and `decision.names`:


```r
plot(titanic.fft, 
     description = "Titanic", 
     decision.names = c("Died", "Survived"))
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

## Elements

The top row of the plot shows the main dataset information. 

The middle row shows the tree as well as how many examples were classified at each level in the tree. For example, this tree could be understood as: "If the person is *not* male, predict they survived Then, if the person is neither in first nor second class, predict they died. Finally, if the person is a child, predict they survived."

The bottom row shows general performance statistics of the tree. If fitting data (i.e.; data used to build the tree) are displayed, you'll see a "Data Fitting" label. If a testing dataset separate from the one used to build the tree is used, you'll see a "Prediction" label. The classification table on the left side shows the relationship between tree decisions and the truth. CR (Correct Rejection) and H (Hit) are correct decisions. MI (Miss) and FA (False-alarm) are incorrect decisions.

The next three levels show performance in terms of Specificity, Hit Rate, and D-prime. Finally, the plot on the right shows an ROC curve comparing the performance of all trees in the fft object. Additionally, the performance of logistic regression (blue) and CART (red) are shown. The tree plotted in the middle row is highlighted in a solid green color (i the case above, tree # 3).

## Additional arguments

You can specify additional arguments to the `plot()` command that will change what is displayed

- `which.tree`: Which tree do you want to plot? You can specify an integer such as `which.tree = 2` will plot the tree #2 in the fft object, or `which.tree = "best.train"` which will use the best training tree.

- `data`: Which data do you want to apply the tree to? You can specify `data = "train"` or `data = "test"` to use the training or testing datasets stored in the `fft` object. Alternatively, you can specify a new dataset (e.g.; `data = test.data`. If you specify a new dataset, the function will automatically apply the tree to the new data and calculate performance statitics (using the `predict.fft()` function).

For example, let's repeat the previous analysis, but now we'll create separate training and test datasets:


```r
set.seed(100)
train.cases <- sample(c(T, F), size = nrow(titanic), replace = T, prob = c(.05, .95))
titanic.train <- titanic[train.cases,]
titanic.test <- titanic[train.cases == F,]

titanic.pred.fft <- fft(formula = survived ~.,
                        data = titanic.train,
                        data.test = titanic.test)
```

Here is the best training tree applied to the training data:


```r
plot(titanic.pred.fft,
     which.tree = "best.train")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

The best training tree (tree #3) had a high specificity of 93%, but a low hit rate of just 67%. However, as we can see in the ROC table, LR didn't perform much better, and CART did even worse than tree #3.

Now let's apply the same tree to the test data:


```r
plot(titanic.pred.fft,
     which.tree = "best.train",
     data = "test")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

Performance has decreased in this test data (e.g.; the hit-rate is down to a very poor 50%). However, both logistic regression and CART did similarly. Let's see how tree # 4, the most liberal tree, did:


```r
plot(titanic.pred.fft,
     which.tree = 4,
     data = "test")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

Tree #4 was able to increase the testing hit-rate up to 62%, but at a cost of a lower specificity of 71%.
