# Add an FFT definition to tree definitions

`add_fft_df` adds the definition(s) of one or more FFT(s) (in the
multi-line format of an `FFTrees` object) or a single FFT (as a tidy
data frame) to the multi-line FFT definitions of an `FFTrees` object.

`add_fft_df` allows for collecting and combining (sets of) tree
definitions after manipulating them with other tree trimming functions.

## Usage

``` r
add_fft_df(fft, ffts_df = NULL, quiet = FALSE)
```

## Arguments

- fft:

  A (set of) FFT definition(s) (in the multi-line format of an `FFTrees`
  object) or one FFT definition (as a data frame in tidy format, with
  one row per node).

- ffts_df:

  A set of FFT definitions (as a data frame, usually from an `FFTrees`
  object, with suitable variable names to pass `verify_ffts_df`.
  Default: `ffts_df = NULL`.

- quiet:

  Hide feedback messages (as logical)? Default: `quiet = FALSE`.

## Value

A (set of) FFT definition(s) in the one line FFT definition format used
by an `FFTrees` object (as a data frame).

## See also

[`get_fft_df`](https://www.nathanieldphillips.co/FFTrees/reference/get_fft_df.md)
for getting the FFT definitions of an `FFTrees` object;
[`read_fft_df`](https://www.nathanieldphillips.co/FFTrees/reference/read_fft_df.md)
for reading one FFT definition from tree definitions;
[`write_fft_df`](https://www.nathanieldphillips.co/FFTrees/reference/write_fft_df.md)
for writing one FFT to tree definitions;
[`FFTrees`](https://www.nathanieldphillips.co/FFTrees/reference/FFTrees.md)
for creating FFTs from and applying them to data.

Other tree definition and manipulation functions:
[`add_nodes()`](https://www.nathanieldphillips.co/FFTrees/reference/add_nodes.md),
[`drop_nodes()`](https://www.nathanieldphillips.co/FFTrees/reference/drop_nodes.md),
[`edit_nodes()`](https://www.nathanieldphillips.co/FFTrees/reference/edit_nodes.md),
[`flip_exits()`](https://www.nathanieldphillips.co/FFTrees/reference/flip_exits.md),
[`get_fft_df()`](https://www.nathanieldphillips.co/FFTrees/reference/get_fft_df.md),
[`read_fft_df()`](https://www.nathanieldphillips.co/FFTrees/reference/read_fft_df.md),
[`reorder_nodes()`](https://www.nathanieldphillips.co/FFTrees/reference/reorder_nodes.md),
[`select_nodes()`](https://www.nathanieldphillips.co/FFTrees/reference/select_nodes.md),
[`write_fft_df()`](https://www.nathanieldphillips.co/FFTrees/reference/write_fft_df.md)
