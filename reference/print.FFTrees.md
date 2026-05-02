# Print basic information of fast-and-frugal trees (FFTs)

`print.FFTrees` prints basic information on FFTs for an `FFTrees` object
`x`.

As `x` may not contain test data, `print.FFTrees` by default prints the
performance characteristics for training data (i.e., fitting), rather
than for test data (i.e., for prediction). When test data is available,
specify `data = "test"` to print prediction performance.

## Usage

``` r
# S3 method for class 'FFTrees'
print(x = NULL, tree = 1, data = "train", ...)
```

## Arguments

- x:

  An `FFTrees` object created by
  [`FFTrees`](https://www.nathanieldphillips.co/FFTrees/reference/FFTrees.md).

- tree:

  The tree to be printed (as an integer, only valid when the
  corresponding tree argument is non-empty). Default: `tree = 1`. To
  print the best training or best test tree with respect to the `goal`
  specified during FFT construction, use `"best.train"` or
  `"best.test"`, respectively.

- data:

  The type of data in `x` to be printed (as a string) or a test dataset
  (as a data frame).

  - A valid data string must be either `'train'` (for fitting
    performance) or `'test'` (for prediction performance).

  - For a valid data frame, the specified tree is evaluated and printed
    for this data (as 'test' data), but the global `FFTrees` object `x`
    remains unchanged unless it is re-assigned.

  By default, `data = 'train'` (as `x` may not contain test data).

- ...:

  additional arguments passed to `print`.

## Value

An invisible `FFTrees` object `x` and summary information on an FFT
printed to the console (as side effect).

## See also

[`plot.FFTrees`](https://www.nathanieldphillips.co/FFTrees/reference/plot.FFTrees.md)
for plotting FFTs;
[`summary.FFTrees`](https://www.nathanieldphillips.co/FFTrees/reference/summary.FFTrees.md)
for summarizing FFTs;
[`inwords`](https://www.nathanieldphillips.co/FFTrees/reference/inwords.md)
for obtaining a verbal description of FFTs;
[`FFTrees`](https://www.nathanieldphillips.co/FFTrees/reference/FFTrees.md)
for creating FFTs from and applying them to data.
