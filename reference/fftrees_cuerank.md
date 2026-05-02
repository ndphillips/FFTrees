# Calculate thresholds that optimize some statistic (goal) for cues in data

`fftrees_cuerank` takes an `FFTrees` object `x` and optimizes its
`goal.threshold` (from `x$params`) for all cues in `newdata` (of type
`data`).

## Usage

``` r
fftrees_cuerank(x = NULL, newdata = NULL, data = "train", rounding = NULL)
```

## Arguments

- x:

  An `FFTrees` object.

- newdata:

  A dataset with cues to be ranked (as data frame).

- data:

  The type of data with cues to be ranked (as character: `'train'`,
  `'test'`, or `'dynamic'`). Default: `data = 'train'`.

- rounding:

  integer. An integer value indicating the decimal digit to which
  non-integer numeric cue thresholds are to be rounded. Default:
  `rounding = NULL` (i.e., no rounding).

## Value

A modified `FFTrees` object (with cue rank information for the current
`data` type in `x$cues$stats`).

## Details

`fftrees_cuerank` creates a data frame `cuerank_df` that is added to
`x$cues$stats`.

Note that the cue directions and thresholds computed by **FFTrees**
always predict positive criterion values (i.e., `TRUE` or signal, rather
than `FALSE` or noise). Using these thresholds for negative exits (i.e.,
for predicting instances of `FALSE` or noise) usually requires a
reversal (e.g., negating cue direction).

`fftrees_cuerank` is called (twice) by the `fftrees_grow_fan` algorithm
to grow fast-and-frugal trees (FFTs).
