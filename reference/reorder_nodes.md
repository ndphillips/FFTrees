# Reorder nodes in an FFT definition

`reorder_nodes` allows reordering the `nodes` in an existing FFT
definition (in the tidy data frame format).

`reorder_nodes` allows to directly set and change the node order in an
FFT definition by specifying `nodes`.

When a former non-final node becomes a final node, the exit type of the
former final node is set to the signal value (i.e., `exit_types[2]`).

## Usage

``` r
reorder_nodes(fft, order = NA, quiet = FALSE)
```

## Arguments

- fft:

  One FFT definition (as a data frame in tidy format, with one row per
  node).

- order:

  The desired node order (as an integer vector). The values of `order`
  must be a permutation of `1:nrow(fft)`. Default: `order = NA`.

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
[`flip_exits`](https://www.nathanieldphillips.co/FFTrees/reference/flip_exits.md)
for reversing exits in an FFT definition;
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
[`flip_exits()`](https://www.nathanieldphillips.co/FFTrees/reference/flip_exits.md),
[`get_fft_df()`](https://www.nathanieldphillips.co/FFTrees/reference/get_fft_df.md),
[`read_fft_df()`](https://www.nathanieldphillips.co/FFTrees/reference/read_fft_df.md),
[`select_nodes()`](https://www.nathanieldphillips.co/FFTrees/reference/select_nodes.md),
[`write_fft_df()`](https://www.nathanieldphillips.co/FFTrees/reference/write_fft_df.md)
