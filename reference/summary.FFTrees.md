# Summarize an `FFTrees` object

`summary.FFTrees` summarizes key contents of an `FFTrees` object.

## Usage

``` r
# S3 method for class 'FFTrees'
summary(object, tree = NULL, ...)
```

## Arguments

- object:

  An `FFTrees` object.

- tree:

  The tree to summarize (as an integer, but may be a vector). If
  `tree = NULL` (as per default) or exceeding the possible range
  `1:object$trees$n`, information on all trees in `object` is returned.

- ...:

  Additional arguments (currently ignored).

## Value

An invisible list with elements containing the `definitions` and
performance `stats` of the FFT(s) specified by `tree`(s).

## Details

Given an `FFTrees` object `x`, `summary.FFTrees` selects key parameters
from `x$params` and provides the definitions and performance statistics
for `tree` from `x$trees`. Inspect and query `x` for additional details.

`summary.FFTrees` returns an invisible list containing two elements:

1.  `definitions` and corresponding performance measures of `tree`s;

2.  `stats` on decision frequencies, derived probabilities, and costs
    (separated by `train` and `test`).

A header prints descriptive information of the `FFTrees` object (to the
console): Its `main` title, number of trees (`object$trees$n`), and the
name of the criterion variable (`object$criterion_name`).

Per default, information on all available trees is shown and returned.
Specifying `tree` filters the output list elements for the corresponding
tree(s). When only a single `tree` is specified, the printed header
includes a verbal description of the corresponding tree.

While `summary.FFTrees` provides key details about the specified
`tree`(s), the individual decisions (stored in `object$trees$decisions`)
are not shown or returned.

## See also

[`print.FFTrees`](https://www.nathanieldphillips.co/FFTrees/reference/print.FFTrees.md)
for printing FFTs;
[`plot.FFTrees`](https://www.nathanieldphillips.co/FFTrees/reference/plot.FFTrees.md)
for plotting FFTs;
[`inwords`](https://www.nathanieldphillips.co/FFTrees/reference/inwords.md)
for obtaining a verbal description of FFTs;
[`FFTrees`](https://www.nathanieldphillips.co/FFTrees/reference/FFTrees.md)
for creating FFTs from and applying them to data.
