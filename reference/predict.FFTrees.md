# Predict classification outcomes or probabilities from data

`predict.FFTrees` predicts binary classification outcomes or their
probabilities from `newdata` for an `FFTrees` object.

## Usage

``` r
# S3 method for class 'FFTrees'
predict(
  object = NULL,
  newdata = NULL,
  tree = 1,
  type = "class",
  sens.w = NULL,
  method = "laplace",
  data = NULL,
  ...
)
```

## Arguments

- object:

  An `FFTrees` object created by the
  [`FFTrees`](https://www.nathanieldphillips.co/FFTrees/reference/FFTrees.md)
  function.

- newdata:

  dataframe. A data frame of test data.

- tree:

  integer. Which tree in the object should be used? By default,
  `tree = 1` is used.

- type:

  string. What should be predicted? Can be `"class"`, which returns a
  vector of class predictions, `"prob"` which returns a matrix of class
  probabilities, or `"both"` which returns a matrix with both class and
  probability predictions.

- sens.w, data:

  deprecated

- method:

  string. Method of calculating class probabilities. Either 'laplace',
  which applies the Laplace correction, or 'raw' which applies no
  correction.

- ...:

  Additional arguments passed on to `predict`.

## Value

Either a logical vector of predictions, or a matrix of class
probabilities.

## See also

[`print.FFTrees`](https://www.nathanieldphillips.co/FFTrees/reference/print.FFTrees.md)
for printing FFTs;
[`plot.FFTrees`](https://www.nathanieldphillips.co/FFTrees/reference/plot.FFTrees.md)
for plotting FFTs;
[`summary.FFTrees`](https://www.nathanieldphillips.co/FFTrees/reference/summary.FFTrees.md)
for summarizing FFTs;
[`FFTrees`](https://www.nathanieldphillips.co/FFTrees/reference/FFTrees.md)
for creating FFTs from and applying them to data.

## Examples

``` r
# Create training and test data:
set.seed(100)
breastcancer <- breastcancer[sample(nrow(breastcancer)), ]
breast.train <- breastcancer[1:150, ]
breast.test  <- breastcancer[151:303, ]

# Create an FFTrees object from the training data:
breast.fft <- FFTrees(
  formula = diagnosis ~ .,
  data = breast.train
)
#> ✔ Created an FFTrees object.
#>   Ranking 9 cues:  ■■■■■■■■                          22% | ETA:  0s
#>   Ranking 9 cues:  ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s
#> 
#> ✔ Ranked 9 cues (optimizing 'bacc').
#> ✔ Created 6 FFTs with 'ifan' algorithm (chasing 'bacc').
#> ✔ Defined 6 FFTs.
#> ✔ Applied 6 FFTs to 'train' data.
#> ✔ Ranked 6 FFTs by 'train' data.
#> ✔ Expressed 6 FFTs in words.

# Predict classification outcomes for test data:
breast.fft.pred <- predict(breast.fft,
  newdata = breast.test
)
#> ✔ Applied 6 FFTs to 'test' data.
#> ✔ Generated predictions for tree 1.

# Predict class probabilities for test data:
breast.fft.pred <- predict(breast.fft,
  newdata = breast.test,
  type = "prob"
)
#> ✔ Applied 6 FFTs to 'test' data.
#> ✔ Generated predictions for tree 1.
```
