# Edit nodes in an FFT definition

`edit_nodes` allows manipulating one or more `nodes` from an existing
FFT definition (in the tidy data frame format).

`edit_nodes` allows to directly set and change the value(s) of `class`,
`cue`, `direction`, `threshold`, and `exit`, in an FFT definition for
the specified `nodes`.

There is only rudimentary verification for plausible entries.
Importantly, however, as `edit_nodes` is ignorant of `data`, the values
of its variables are not validated for a specific set of data.

Repeated changes of a node are possible (by repeating the corresponding
integer value in `nodes`).

## Usage

``` r
edit_nodes(
  fft,
  nodes = NA,
  class = NA,
  cue = NA,
  direction = NA,
  threshold = NA,
  exit = NA,
  quiet = FALSE
)
```

## Arguments

- fft:

  One FFT definition (as a data frame in tidy format, with one row per
  node).

- nodes:

  The FFT nodes to be edited (as an integer vector). Default:
  `nodes = NA`.

- class:

  The class values of `nodes` (as character).

- cue:

  The cue names of `nodes` (as character).

- direction:

  The direction values of `nodes` (as character).

- threshold:

  The threshold values of `nodes` (as character).

- exit:

  The exit values of `nodes` (as values from `exit_types`).

- quiet:

  Hide feedback messages (as logical)? Default: `quiet = FALSE`.

## Value

One FFT definition (as a data frame in tidy format, with one row per
node).

## See also

[`add_nodes`](https://www.nathanieldphillips.co/FFTrees/reference/add_nodes.md)
for adding nodes to an FFT definition;
[`drop_nodes`](https://www.nathanieldphillips.co/FFTrees/reference/drop_nodes.md)
for deleting nodes from an FFT definition;
[`flip_exits`](https://www.nathanieldphillips.co/FFTrees/reference/flip_exits.md)
for reversing exits in an FFT definition;
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
[`flip_exits()`](https://www.nathanieldphillips.co/FFTrees/reference/flip_exits.md),
[`get_fft_df()`](https://www.nathanieldphillips.co/FFTrees/reference/get_fft_df.md),
[`read_fft_df()`](https://www.nathanieldphillips.co/FFTrees/reference/read_fft_df.md),
[`reorder_nodes()`](https://www.nathanieldphillips.co/FFTrees/reference/reorder_nodes.md),
[`select_nodes()`](https://www.nathanieldphillips.co/FFTrees/reference/select_nodes.md),
[`write_fft_df()`](https://www.nathanieldphillips.co/FFTrees/reference/write_fft_df.md)
