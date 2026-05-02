# Read an FFT definition from tree definitions

`read_fft_df` reads and returns the definition of a single FFT (as a
tidy data frame) from the multi-line FFT definitions of an `FFTrees`
object.

`read_fft_df` allows reading individual tree definitions to manipulate
them with other tree trimming functions.

[`write_fft_df`](https://www.nathanieldphillips.co/FFTrees/reference/write_fft_df.md)
provides the inverse functionality.

## Usage

``` r
read_fft_df(ffts_df, tree = 1)
```

## Arguments

- ffts_df:

  A set of FFT definitions (as a data frame, usually from an `FFTrees`
  object, with suitable variable names to pass `verify_ffts_df`.

- tree:

  The ID of the to-be-selected FFT (as an integer), corresponding to a
  tree in `ffts_df`. Default: `tree = 1`.

## Value

One FFT definition (as a data frame in tidy format, with one row per
node).

## See also

[`get_fft_df`](https://www.nathanieldphillips.co/FFTrees/reference/get_fft_df.md)
for getting the FFT definitions of an `FFTrees` object;
[`write_fft_df`](https://www.nathanieldphillips.co/FFTrees/reference/write_fft_df.md)
for writing one FFT to tree definitions;
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
[`reorder_nodes()`](https://www.nathanieldphillips.co/FFTrees/reference/reorder_nodes.md),
[`select_nodes()`](https://www.nathanieldphillips.co/FFTrees/reference/select_nodes.md),
[`write_fft_df()`](https://www.nathanieldphillips.co/FFTrees/reference/write_fft_df.md)
