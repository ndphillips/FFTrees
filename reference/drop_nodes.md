# Drop a node from an FFT definition

`drop_nodes` deletes one or more `nodes` from an existing FFT definition
(by removing the corresponding rows from the FFT definition in the tidy
data frame format).

When dropping the final node, the last remaining node becomes the new
final node (i.e., gains a second exit).

Duplicates in `nodes` are dropped only once (rather than incrementally)
and `nodes` not in the range `1:nrow(fft)` are ignored. Dropping all
nodes yields an error.

`drop_nodes` is the inverse function of
[`select_nodes`](https://www.nathanieldphillips.co/FFTrees/reference/select_nodes.md).
Inserting new nodes is possible by
[`add_nodes`](https://www.nathanieldphillips.co/FFTrees/reference/add_nodes.md).

## Usage

``` r
drop_nodes(fft, nodes = NA, quiet = FALSE)
```

## Arguments

- fft:

  One FFT definition (as a data frame in tidy format, with one row per
  node).

- nodes:

  The FFT nodes to drop (as an integer vector). Default: `nodes = NA`.

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
[`edit_nodes()`](https://www.nathanieldphillips.co/FFTrees/reference/edit_nodes.md),
[`flip_exits()`](https://www.nathanieldphillips.co/FFTrees/reference/flip_exits.md),
[`get_fft_df()`](https://www.nathanieldphillips.co/FFTrees/reference/get_fft_df.md),
[`read_fft_df()`](https://www.nathanieldphillips.co/FFTrees/reference/read_fft_df.md),
[`reorder_nodes()`](https://www.nathanieldphillips.co/FFTrees/reference/reorder_nodes.md),
[`select_nodes()`](https://www.nathanieldphillips.co/FFTrees/reference/select_nodes.md),
[`write_fft_df()`](https://www.nathanieldphillips.co/FFTrees/reference/write_fft_df.md)
