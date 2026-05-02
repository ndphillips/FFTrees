# Flip exits in an FFT definition

`flip_exits` reverses the exits of one or more `nodes` from an existing
FFT definition (in the tidy data frame format).

`flip_exits` alters the value(s) of the non-final exits specified in
`nodes` (from 0 to 1, or from 1 to 0). By contrast, exits of final
`nodes` remain unchanged.

Duplicates in `nodes` are flipped only once (rather than repeatedly) and
`nodes` not in the range `1:nrow(fft)` are ignored.

`flip_exits` is a more specialized function than
[`edit_nodes`](https://www.nathanieldphillips.co/FFTrees/reference/edit_nodes.md).

## Usage

``` r
flip_exits(fft, nodes = NA, quiet = FALSE)
```

## Arguments

- fft:

  One FFT definition (as a data frame in tidy format, with one row per
  node).

- nodes:

  The FFT nodes whose exits are to be flipped (as an integer vector).
  Default: `nodes = NA`.

- quiet:

  Hide feedback messages (as logical)? Default: `quiet = FALSE`.

## Value

One FFT definition (as a data frame in tidy format, with one row per
node).

## See also

[`add_nodes`](https://www.nathanieldphillips.co/FFTrees/reference/add_nodes.md)
for adding nodes to an FFT definition;
[`edit_nodes`](https://www.nathanieldphillips.co/FFTrees/reference/edit_nodes.md)
for editing nodes in an FFT definition;
[`drop_nodes`](https://www.nathanieldphillips.co/FFTrees/reference/drop_nodes.md)
for deleting nodes from an FFT definition;
[`reorder_nodes`](https://www.nathanieldphillips.co/FFTrees/reference/reorder_nodes.md)
for reordering nodes of an FFT definition;
[`select_nodes`](https://www.nathanieldphillips.co/FFTrees/reference/select_nodes.md)
for selecting nodes in an FFT definition;
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
[`get_fft_df()`](https://www.nathanieldphillips.co/FFTrees/reference/get_fft_df.md),
[`read_fft_df()`](https://www.nathanieldphillips.co/FFTrees/reference/read_fft_df.md),
[`reorder_nodes()`](https://www.nathanieldphillips.co/FFTrees/reference/reorder_nodes.md),
[`select_nodes()`](https://www.nathanieldphillips.co/FFTrees/reference/select_nodes.md),
[`write_fft_df()`](https://www.nathanieldphillips.co/FFTrees/reference/write_fft_df.md)
