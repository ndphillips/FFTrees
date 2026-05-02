# Select the best tree (from current set of FFTs)

`get_best_tree` selects (looks up and identifies) the best tree (as an
integer) from the set (or “fan”) of FFTs contained in the current
`FFTrees` object `x`, an existing type of `data` ('train' or 'test'),
and a `goal` for which corresponding statistics are available in the
designated `data` type (in `x$trees$stats`).

## Usage

``` r
get_best_tree(x, data, goal, my.goal.max = TRUE)
```

## Arguments

- x:

  An `FFTrees` object.

- data:

  The type of data to consider (as character: either 'train' or 'test').

- goal:

  A goal (as character) to be maximized or minimized when selecting a
  tree from an existing `FFTrees` object `x` (with existing
  `x$trees$stats`).

- my.goal.max:

  Default direction for user-defined `my.goal` (as logical): Should
  `my.goal` be maximized? Default: `my.goal.max = TRUE`.

## Value

An integer denoting the `tree` that maximizes/minimizes `goal` in
`data`.

## Details

Importantly, `get_best_tree` only identifies and selects the \`tree\`
*identifier* (as an integer) from the set of *existing* trees with known
statistics, rather than creating new trees or computing new cue
thresholds. More specifically, `goal` is used for identifying and
selecting the \`tree\` identifier (as an integer) of the best FFT from
an existing set of FFTs, but not for computing new cue thresholds (see
`goal.threshold` and
[`fftrees_cuerank()`](https://www.nathanieldphillips.co/FFTrees/reference/fftrees_cuerank.md))
or creating new trees (see `goal.chase` and
[`fftrees_ranktrees()`](https://www.nathanieldphillips.co/FFTrees/reference/fftrees_ranktrees.md)).

## See also

[`FFTrees`](https://www.nathanieldphillips.co/FFTrees/reference/FFTrees.md)
for creating FFTs from and applying them to data.

Other utility functions:
[`get_exit_type()`](https://www.nathanieldphillips.co/FFTrees/reference/get_exit_type.md),
[`get_fft_df()`](https://www.nathanieldphillips.co/FFTrees/reference/get_fft_df.md)
