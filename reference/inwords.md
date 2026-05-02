# Provide a verbal description of an FFT

`inwords` generates and provides a verbal description of a
fast-and-frugal tree (FFT) from an `FFTrees` object.

When `data` remains unspecified, `inwords` will only look up
`x$trees$inwords`. When `data` is set to either "train" or "test",
`inwords` first employs
[`fftrees_ffttowords`](https://www.nathanieldphillips.co/FFTrees/reference/fftrees_ffttowords.md)
to re-generate the verbal descriptions of FFTs in `x`.

## Usage

``` r
inwords(x, data = NULL, tree = 1)
```

## Arguments

- x:

  An `FFTrees` object.

- data:

  The type of data to which a tree is being applied (as character string
  "train" or "test"). Default: `data = NULL` will only look up
  `x$trees$inwords`.

- tree:

  The tree to display (as an integer).

## Value

A verbal description of an FFT (as a character string).

## See also

[`fftrees_ffttowords`](https://www.nathanieldphillips.co/FFTrees/reference/fftrees_ffttowords.md)
for converting FFTs into verbal descriptions;
[`print.FFTrees`](https://www.nathanieldphillips.co/FFTrees/reference/print.FFTrees.md)
for printing FFTs;
[`plot.FFTrees`](https://www.nathanieldphillips.co/FFTrees/reference/plot.FFTrees.md)
for plotting FFTs;
[`summary.FFTrees`](https://www.nathanieldphillips.co/FFTrees/reference/summary.FFTrees.md)
for summarizing FFTs;
[`FFTrees`](https://www.nathanieldphillips.co/FFTrees/reference/FFTrees.md)
for creating FFTs from and applying them to data.
