# Apply an FFT to data and generate accuracy statistics

`fftrees_apply` applies a fast-and-frugal tree (FFT, as an `FFTrees`
object) to a dataset (of type `mydata`) and generates corresponding
accuracy statistics (on cue levels and for trees).

`fftrees_apply` is called internally by the main
[`FFTrees`](https://www.nathanieldphillips.co/FFTrees/reference/FFTrees.md)
function (with `mydata = "train"` and — if test data exists —
`mydata = "test"`). Alternatively, `fftrees_apply` is called when
predicting outcomes for new data by
[`predict.FFTrees`](https://www.nathanieldphillips.co/FFTrees/reference/predict.FFTrees.md).

## Usage

``` r
fftrees_apply(x, mydata = NULL, newdata = NULL, fin_NA_pred = "majority")
```

## Arguments

- x:

  An object with FFT definitions which are to be applied to current data
  (as an `FFTrees` object).

- mydata:

  The type of data to which the FFT should be applied (as character,
  either `"train"` or `"test"`).

- newdata:

  New data to which an FFT should be applied (as a data frame).

- fin_NA_pred:

  What outcome should be predicted if the *final* node in a tree has a
  cue value of `NA` (as character)? Valid options are:

  'noise'

  :   predict `FALSE` (0/left/signal) for all corresponding cases

  'signal'

  :   predict `TRUE` (1/right/noise) for all corresponding cases

  'majority'

  :   predict the more common criterion value (i.e., `TRUE` if base rate
      `p(TRUE) > .50` in 'train' data) for all corresponding cases

  'baseline'

  :   flip a random coin that is biased by the criterion baseline
      `p(TRUE)` (in 'train' data) for all corresponding cases

  'dnk'

  :   yet ToDo: abstain from classifying / decide to 'do not know' /
      defer (i.e., tertium datur)

  Default: `fin_NA_pred = "majority"`.

## Value

A modified `FFTrees` object (with lists in `x$trees` containing
information on FFT decisions and statistics).

## See also

[`FFTrees`](https://www.nathanieldphillips.co/FFTrees/reference/FFTrees.md)
for creating FFTs from and applying them to data.
