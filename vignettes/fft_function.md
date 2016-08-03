---
title: "fft() function"
author: "Nathaniel Phillips"
date: "2016-07-19"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Creating FFTs with fft()}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---




This function is at the heart of the `FFTrees` package. The function takes a training dataset as an argument, and generates several FFT (more details about the algorithms coming soon...)

## heartdisease example

Let's start with an example, we'll create FFTs fitted to the `heartdisease` dataset. This dataset contains data from 202 patients suspected of having heart disease. Here's how the dataset looks:


```r
head(heartdisease)
```

```
##   age sex cp trestbps chol fbs restecg thalach exang oldpeak slope ca thal
## 1  63   1  1      145  233   1       2     150     0     2.3     3  0    6
## 2  67   1  4      160  286   0       2     108     1     1.5     2  3    3
## 3  67   1  4      120  229   0       2     129     1     2.6     2  2    7
## 4  37   1  3      130  250   0       0     187     0     3.5     3  0    3
## 5  41   0  2      130  204   0       2     172     0     1.4     1  0    3
## 6  56   1  2      120  236   0       0     178     0     0.8     1  0    3
##   diagnosis
## 1         0
## 2         1
## 3         1
## 4         0
## 5         0
## 6         0
```

The critical dependent variable is `diagnosis` which indicates whether a patient has heart diesease or not. The other variables in the dataset (e.g.; sex, age, and several biological measurements) will be used as predictors.

Now we'll split the original dataset into a *training* dataset, and a *testing* dataset. We will create the trees with the training set, then test its performance in the test dataset:


```r
set.seed(100)
samples <- sample(c(T, F), size = nrow(heartdisease), replace = T)
heartdisease.train <- heartdisease[samples,]
heartdisease.test <- heartdisease[samples == 0,]
```

We'll create a new fft object called `heart.fft` using the `fft()` function. We'll specify `diagnosis` as the (binary) dependent variable with `formula = diagnosis ~ .`:


```r
heart.fft <- fft(
  formula = diagnosis ~.,
  data = heartdisease.train,
  data.test = heartdisease.test
  )
```

## Elements of an fft object

As you can see, `fft()` returns an object with the fft class


```r
class(heart.fft)
```

```
## [1] "fft"
```

There are many elements in an fft object:


```r
names(heart.fft)
```

```
##  [1] "formula"        "data.train"     "data.test"      "cue.accuracies"
##  [5] "trees"          "trees.auc"      "cart"           "lr"            
##  [9] "decision.train" "decision.test"  "levelout.train" "levelout.test"
```

### cue.accuracies

The `cue.accuracies` dataframe contains the original, marginal cue accuracies. That is, for each cue, the threshold that maximizes v (hr - far) is chosen (this is done using the `cuerank()` function):


```r
heart.fft$cue.accuracies
```

```
##    cue.name cue.class level.threshold level.sigdirection hi mi fa cr
## 4        cp   numeric               4                 >= 50 14 22 63
## 2     exang   numeric               1                 >= 40 24 11 74
## 21     thal   numeric               6                 >= 47 17 17 68
## 22    slope   numeric               2                 >= 52 12 31 54
##          hr       far         v    dprime correction hr.weight
## 4  0.781250 0.2588235 0.5224265 0.7116992       0.25       0.5
## 2  0.625000 0.1294118 0.4955882 0.7239078       0.25       0.5
## 21 0.734375 0.2000000 0.5343750 0.7338601       0.25       0.5
## 22 0.812500 0.3647059 0.4477941 0.6165273       0.25       0.5
```

Here, we can see that the `thal` cue had the highest v value of 0.5344 while `cp` had the second highest v value of 0.5224.


### trees

The `trees` dataframe contains all tree definitions and training (and possibly test) statistics for all ($2^{max.levels - 1}$) trees. For our `heart.fft` example, there are $2^{4 - 1} = 8$ trees.

Tree definitions (exit directions, cue order, and cue thresholds) are contained in columns 1 through 6:


```r
heart.fft$trees
```

```
##   tree.num          level.name                     level.class level.exit
## 1        1 thal;cp;exang;slope numeric;numeric;numeric;numeric  0;0;0;0.5
## 5        2       thal;cp;exang         numeric;numeric;numeric    0;0;0.5
## 3        3       thal;cp;exang         numeric;numeric;numeric    0;1;0.5
## 7        4 thal;cp;exang;slope numeric;numeric;numeric;numeric  0;1;1;0.5
## 2        5       thal;cp;exang         numeric;numeric;numeric    1;0;0.5
## 6        6 thal;cp;exang;slope numeric;numeric;numeric;numeric  1;0;1;0.5
## 4        7       thal;cp;exang         numeric;numeric;numeric    1;1;0.5
## 8        8 thal;cp;exang;slope numeric;numeric;numeric;numeric  1;1;1;0.5
##   level.threshold level.sigdirection n.train hi.train mi.train fa.train
## 1         3;3;0;2           >;>;>;>=     149       22       42        2
## 5           3;3;0              >;>;>     149       26       38        4
## 3           3;3;0              >;>;>     149       39       25        7
## 7         3;3;0;2           >;>;>;>=     149       46       18       14
## 2           3;3;0              >;>;>     149       58        6       20
## 6         3;3;0;2           >;>;>;>=     149       58        6       24
## 4           3;3;0              >;>;>     149       61        3       36
## 8         3;3;0;2           >;>;>;>=     149       63        1       50
##   cr.train hr.train  far.train   v.train dprime.train n.test hi.test
## 1       83 0.343750 0.02352941 0.3202206    0.7917602    154      21
## 5       81 0.406250 0.04705882 0.3591912    0.7184319    154      28
## 3       78 0.609375 0.08235294 0.5270221    0.8335539    154      47
## 7       71 0.718750 0.16470588 0.5540441    0.7772158    154      52
## 2       65 0.906250 0.23529412 0.6709559    1.0197666    154      59
## 6       61 0.906250 0.28235294 0.6238971    0.9469384    154      63
## 4       49 0.953125 0.42352941 0.5295956    0.9344061    154      65
## 8       35 0.984375 0.58823529 0.3961397    0.9654334    154      69
##   mi.test fa.test cr.test   hr.test   far.test    v.test dprime.test
## 1      54       0      79 0.2800000 0.00000000 0.2783123   1.0768926
## 5      47       0      79 0.3733333 0.00000000 0.3710275   1.2057404
## 3      28       6      73 0.6266667 0.07594937 0.5507173   0.8779473
## 7      23      11      68 0.6933333 0.13924051 0.5540928   0.7945295
## 2      16      20      59 0.7866667 0.25316456 0.5335021   0.7297365
## 6      12      25      54 0.8400000 0.31645570 0.5235443   0.7360455
## 4      10      37      42 0.8666667 0.46835443 0.3983122   0.5950893
## 8       6      48      31 0.9200000 0.60759494 0.3124051   0.5660078
```

You can also use the generic `summary()` function to get the trees dataframe


```r
summary(heart.fft)
```

Training statistics are contained in columns 7:15 and have the `.train` suffix. 


```r
heart.fft$trees[,7:15]   # Training stats are in columns 7:15
```

```
##   n.train hi.train mi.train fa.train cr.train hr.train  far.train
## 1     149       22       42        2       83 0.343750 0.02352941
## 5     149       26       38        4       81 0.406250 0.04705882
## 3     149       39       25        7       78 0.609375 0.08235294
## 7     149       46       18       14       71 0.718750 0.16470588
## 2     149       58        6       20       65 0.906250 0.23529412
## 6     149       58        6       24       61 0.906250 0.28235294
## 4     149       61        3       36       49 0.953125 0.42352941
## 8     149       63        1       50       35 0.984375 0.58823529
##     v.train dprime.train
## 1 0.3202206    0.7917602
## 5 0.3591912    0.7184319
## 3 0.5270221    0.8335539
## 7 0.5540441    0.7772158
## 2 0.6709559    1.0197666
## 6 0.6238971    0.9469384
## 4 0.5295956    0.9344061
## 8 0.3961397    0.9654334
```

For our heart disease dataset, it looks like tree 2 had the highest training v (HR - FAR) values.

Test statistics are contained in columns 16:24 and have the `.test` suffix. 


```r
heart.fft$trees[,16:24]   # Test stats are in columns 16:24
```

```
##   n.test hi.test mi.test fa.test cr.test   hr.test   far.test    v.test
## 1    154      21      54       0      79 0.2800000 0.00000000 0.2783123
## 5    154      28      47       0      79 0.3733333 0.00000000 0.3710275
## 3    154      47      28       6      73 0.6266667 0.07594937 0.5507173
## 7    154      52      23      11      68 0.6933333 0.13924051 0.5540928
## 2    154      59      16      20      59 0.7866667 0.25316456 0.5335021
## 6    154      63      12      25      54 0.8400000 0.31645570 0.5235443
## 4    154      65      10      37      42 0.8666667 0.46835443 0.3983122
## 8    154      69       6      48      31 0.9200000 0.60759494 0.3124051
##   dprime.test
## 1   1.0768926
## 5   1.2057404
## 3   0.8779473
## 7   0.7945295
## 2   0.7297365
## 6   0.7360455
## 4   0.5950893
## 8   0.5660078
```

It looks like trees 2 and 6 also had the highest test v (HR - FAR) values. 


### AUC

AUC statistics are in `auc`


```r
heart.fft$auc
```

```
## NULL
```


### Other information

#### train.decision.df, test.decision.df

The `train.decision.df` and `test.decision.df` contain the raw classification decisions for each tree for each training (and test) case.

Here are each of the 8 tree decisions for the first 5 training cases.


```r
heart.fft$decision.train[1:5,]
```

```
##   tree.1 tree.2 tree.3 tree.4 tree.5 tree.6 tree.7 tree.8
## 1      0      0      0      1      1      1      1      1
## 2      0      0      0      0      1      1      1      1
## 3      0      0      0      0      0      0      0      1
## 4      0      0      0      0      0      0      0      0
## 5      0      0      0      0      0      0      0      0
```


#### train.levelout.df, test.levelout.df

The `train.levelout.df` and `test.levelout.df` contain the levels at which each case was classified for each tree.

Here are the levels at which the first 5 training cases were classified:


```r
heart.fft$levelout.train[1:5,]
```

```
##   tree.1 tree.2 tree.3 tree.4 tree.5 tree.6 tree.7 tree.8
## 1      2      2      3      4      1      1      1      1
## 2      1      1      1      1      3      3      2      2
## 3      1      1      1      1      2      2      3      4
## 4      1      1      1      1      2      2      3      4
## 5      1      1      1      1      2      2      3      4
```



## Plotting trees

Once you've created an fft object using `fft()` you can visualize the tree (and ROC curves) using `plot()`. The following code will visualize the best training tree (tree 2) applied to the test data:


```r
plot(heart.fft,
     description = "Heart Disease",
     decision.names = c("Healthy", "Disease")
     )
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15-1.png)

See the vignette on `plot.fft` `vignette("fft_plot", package = "fft")` for more details.

## Additional arguments

The `fft()` function has several additional arguments than change how trees are built. Note: Not all of these arguments have fully tested yet!

- `train.p`: What percent of the data should be used for training? `train.p = .1` will randomly select 10% of the data for training and leave the remaining 90% for testing. Settting `train.p = 1` will fit the trees to the entire dataset (with no testing).

- `n.sim`: If `train.p < 1`, how many different training sets should be generated to create the trees?

- `rank.method`: As trees are being built, should cues be selected based on their marginal accuracy (`rank.method = "m"`) applied to the entire dataset, or on their conditional accuracy (`rank.method = "c"`) applied to all cases that have not yet been classified? Each method has potential pros and cons. The marginal method is much faster to implement and may be prone to less over-fitting. However, the conditional method could capture important conditional dependencies between cues that the marginal method misses.
