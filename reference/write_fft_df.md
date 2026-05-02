# Write an FFT definition to tree definitions

`write_fft_df` writes the definition of a single FFT (as a tidy data
frame) into the one-line FFT definition used by an `FFTrees` object.

`write_fft_df` allows turning individual tree definitions into the
one-line FFT definition format used by an `FFTrees` object.

[`read_fft_df`](https://www.nathanieldphillips.co/FFTrees/reference/read_fft_df.md)
provides the inverse functionality.

## Usage

``` r
write_fft_df(fft, tree = -99L)
```

## Arguments

- fft:

  One FFT definition (as a data frame in tidy format, with one row per
  node).

- tree:

  The ID of the to-be-written FFT (as an integer). Default:
  `tree = -99L`.

## Value

An FFT definition in the one line FFT definition format used by an
`FFTrees` object (as a data frame).

## See also

[`get_fft_df`](https://www.nathanieldphillips.co/FFTrees/reference/get_fft_df.md)
for getting the FFT definitions of an `FFTrees` object;
[`read_fft_df`](https://www.nathanieldphillips.co/FFTrees/reference/read_fft_df.md)
for reading one FFT definition from tree definitions;
[`add_fft_df`](https://www.nathanieldphillips.co/FFTrees/reference/add_fft_df.md)
for adding FFTs to tree definitions;
[`FFTrees`](https://www.nathanieldphillips.co/FFTrees/reference/FFTrees.md)
for creating FFTs from and applying them to data.

Other tree definition and manipulation functions:
[`add_fft_df()`](https://www.nathanieldphillips.co/FFTrees/reference/add_fft_df.md),
[`add_nodes()`](https://www.nathanieldphillips.co/FFTrees/reference/add_nodes.md),
[`drop_nodes()`](https://www.nathanieldphillips.co/FFTrees/reference/drop_nodes.md),
[`edit_nodes()`](https://www.nathanieldphillips.co/FFTrees/reference/edit_nodes.md),
[`flip_exits()`](https://www.nathanieldphillips.co/FFTrees/reference/flip_exits.md),
[`get_fft_df()`](https://www.nathanieldphillips.co/FFTrees/reference/get_fft_df.md),
[`read_fft_df()`](https://www.nathanieldphillips.co/FFTrees/reference/read_fft_df.md),
[`reorder_nodes()`](https://www.nathanieldphillips.co/FFTrees/reference/reorder_nodes.md),
[`select_nodes()`](https://www.nathanieldphillips.co/FFTrees/reference/select_nodes.md)
