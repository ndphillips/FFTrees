# Perform a grid search over thresholds and return accuracy statistics for a given numeric cue

Perform a grid search over thresholds and return accuracy statistics for
a given numeric cue

## Usage

``` r
fftrees_threshold_numeric_grid(
  thresholds,
  cue_v,
  criterion_v,
  directions = c(">", "<="),
  goal.threshold = NULL,
  sens.w = NULL,
  my.goal = NULL,
  my.goal.fun = NULL,
  cost.each = NULL,
  cost.outcomes = NULL
)
```

## Arguments

- thresholds:

  numeric. A vector of thresholds to consider.

- cue_v:

  numeric. Feature values.

- criterion_v:

  logical. A logical vector of (TRUE) criterion values.

- directions:

  character. Possible directions to consider.

- goal.threshold:

  A character string indicating the criterion to maximize when
  *optimizing cue thresholds*: `"acc"` = overall accuracy, `"bacc"` =
  balanced accuracy, `"wacc"` = weighted accuracy, `"dprime"` =
  discriminability, `"cost"` = costs (based only on `cost.outcomes`, as
  `cost.cues` are constant per cue). Default: `goal.threshold = "bacc"`.

- sens.w:

  numeric. Sensitivity weight parameter (from `0` to `1`, for computing
  `wacc`). Default: `sens.w = .50`.

- my.goal:

  Name of an optional, user-defined goal (as character string). Default:
  `my.goal = NULL`.

- my.goal.fun:

  User-defined goal function (with 4 arguments `hi fa mi cr`). Default:
  `my.goal.fun = NULL`.

- cost.each:

  numeric. A constant cost value to add to each value (e.g., the cost of
  the cue).

- cost.outcomes:

  list. A list of length 4 with names 'hi', 'fa', 'mi', and 'cr'
  specifying the costs of a hit, false alarm, miss, and correct
  rejection, respectively, in some common currency. For instance,
  `cost.outcomes = listc("hi" = 0, "fa" = 10, "mi" = 20, "cr" = 0)`
  means that a false alarm and miss cost `10` and `20` units,
  respectively, while correct decisions have no cost.

## Value

A data frame containing accuracy statistics for numeric thresholds.

## See also

[`fftrees_threshold_factor_grid`](https://www.nathanieldphillips.co/FFTrees/reference/fftrees_threshold_factor_grid.md)
for factor cues.
