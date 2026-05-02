# Rank FFTs by current goal

`fftrees_ranktrees` ranks trees in an `FFTrees` object `x` based on the
current goal (either `"cost"` or as specified in `x$params$goal`).

`fftrees_ranktrees` is called by the main
[`FFTrees`](https://www.nathanieldphillips.co/FFTrees/reference/FFTrees.md)
function when creating FFTs from and applying them to (training) data.

## Usage

``` r
fftrees_ranktrees(x, data = "train")
```

## Arguments

- x:

  An `FFTrees` object.

- data:

  The type of data to be used (as character). Default: `data = "train"`.

## See also

[`FFTrees`](https://www.nathanieldphillips.co/FFTrees/reference/FFTrees.md)
for creating FFTs from and applying them to data.
