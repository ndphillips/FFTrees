---
title: "Example FFTs"
author: "Nathaniel Phillips"
date: "2016-07-19"
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

Here is a summary of the final 8 trees:


```r
summary(mushrooms.fft)
```

```
##   tree.num                   level.name                 level.class
## 1        1 odor;sporepc;gcolor;ringtype factor;factor;factor;factor
## 3        2                 odor;sporepc               factor;factor
## 5        3          odor;sporepc;gcolor        factor;factor;factor
## 7        4                 odor;sporepc               factor;factor
## 2        5                 odor;sporepc               factor;factor
## 6        6                 odor;sporepc               factor;factor
## 4        7          odor;sporepc;gcolor        factor;factor;factor
## 8        8 odor;sporepc;gcolor;ringtype factor;factor;factor;factor
##   level.exit                         level.threshold level.sigdirection
## 1  0;0;0;0.5 n,l,a;n,k,o,u,y,b;n,w,u,k,p,e,y,o;l,e,n         !=;!=;!=;=
## 3      0;0.5                       n,l,a;n,k,o,u,y,b              !=;!=
## 5    0;0;0.5       n,l,a;n,k,o,u,y,b;n,w,u,k,p,e,y,o           !=;!=;!=
## 7      0;0.5                       n,l,a;n,k,o,u,y,b              !=;!=
## 2      1;0.5                       n,l,a;n,k,o,u,y,b              !=;!=
## 6      1;0.5                       n,l,a;n,k,o,u,y,b              !=;!=
## 4    1;1;0.5       n,l,a;n,k,o,u,y,b;n,w,u,k,p,e,y,o           !=;!=;!=
## 8  1;1;1;0.5 n,l,a;n,k,o,u,y,b;n,w,u,k,p,e,y,o;l,e,n         !=;!=;!=;=
##   n.train hi.train mi.train fa.train cr.train  hr.train far.train
## 1    4093     1297      661        0     2135 0.6624106 0.0000000
## 3    4093     1670      288        0     2135 0.8529111 0.0000000
## 5    4093     1339      619        0     2135 0.6838611 0.0000000
## 7    4093     1670      288        0     2135 0.8529111 0.0000000
## 2    4093     1958        0      344     1791 1.0000000 0.1611241
## 6    4093     1958        0      344     1791 1.0000000 0.1611241
## 4    4093     1958        0      514     1621 1.0000000 0.2407494
## 8    4093     1958        0      790     1345 1.0000000 0.3700234
##     v.train dprime.train n.test hi.test mi.test fa.test cr.test   hr.test
## 1 0.6622521     2.048976   4031    1295     663       0    2073 0.6613892
## 3 0.8527040     2.363812   4031    1678     280       0    2073 0.8569969
## 5 0.6836971     2.078703   4031    1349     609       0    2073 0.6889683
## 7 0.8527040     2.363812   4031    1678     280       0    2073 0.8569969
## 2 0.8386689     2.323205   4031    1958       0     280    1793 1.0000000
## 6 0.8386689     2.323205   4031    1958       0     280    1793 1.0000000
## 4 0.7590622     2.180293   4031    1958       0     454    1619 1.0000000
## 8 0.6298185     1.994299   4031    1958       0     754    1319 1.0000000
##    far.test    v.test dprime.test
## 1 0.0000000 0.6612274    2.043819
## 3 0.0000000 0.8567852    2.369006
## 5 0.0000000 0.6887995    2.082142
## 7 0.0000000 0.8567852    2.369006
## 2 0.1350699 0.8647144    2.379611
## 6 0.1350699 0.8647144    2.379611
## 4 0.2190063 0.7807983    2.216105
## 8 0.3637241 0.6361154    2.002660
```

Here is the best training tree applied to the test data:


```r
plot(mushrooms.fft, data = "test")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)


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

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)

Here is tree number 2, which is a bit more conservative than tree 6


```r
plot(iris.fft, which.tree = 2)
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)
