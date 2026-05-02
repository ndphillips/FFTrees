# Get FFT definitions (from an `FFTrees` object `x`)

`get_fft_df` gets the FFT definitions of an `FFTrees` object `x` (as a
`data.frame`).

## Usage

``` r
get_fft_df(x)
```

## Arguments

- x:

  An `FFTrees` object.

## Value

A set of FFT definitions (as a `data.frame`/`tibble`, in the one-line
per FFT definition format used by an `FFTrees` object).

## Details

The FFTs in the `data.frame` returned are represented in the one-line
per FFT definition format used by an `FFTrees` object.

In addition to looking up `x$trees$definitions`, `get_fft_df` verifies
that the FFT definitions are valid (given current settings).

## See also

[`read_fft_df`](https://www.nathanieldphillips.co/FFTrees/reference/read_fft_df.md)
for reading one FFT definition from tree definitions;
[`write_fft_df`](https://www.nathanieldphillips.co/FFTrees/reference/write_fft_df.md)
for writing one FFT to tree definitions;
[`add_fft_df`](https://www.nathanieldphillips.co/FFTrees/reference/add_fft_df.md)
for adding FFTs to tree definitions;
[`FFTrees`](https://www.nathanieldphillips.co/FFTrees/reference/FFTrees.md)
for creating FFTs from and applying them to data.

Other utility functions:
[`get_best_tree()`](https://www.nathanieldphillips.co/FFTrees/reference/get_best_tree.md),
[`get_exit_type()`](https://www.nathanieldphillips.co/FFTrees/reference/get_exit_type.md)

Other tree definition and manipulation functions:
[`add_fft_df()`](https://www.nathanieldphillips.co/FFTrees/reference/add_fft_df.md),
[`add_nodes()`](https://www.nathanieldphillips.co/FFTrees/reference/add_nodes.md),
[`drop_nodes()`](https://www.nathanieldphillips.co/FFTrees/reference/drop_nodes.md),
[`edit_nodes()`](https://www.nathanieldphillips.co/FFTrees/reference/edit_nodes.md),
[`flip_exits()`](https://www.nathanieldphillips.co/FFTrees/reference/flip_exits.md),
[`read_fft_df()`](https://www.nathanieldphillips.co/FFTrees/reference/read_fft_df.md),
[`reorder_nodes()`](https://www.nathanieldphillips.co/FFTrees/reference/reorder_nodes.md),
[`select_nodes()`](https://www.nathanieldphillips.co/FFTrees/reference/select_nodes.md),
[`write_fft_df()`](https://www.nathanieldphillips.co/FFTrees/reference/write_fft_df.md)
