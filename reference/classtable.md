# Compute classification statistics for binary prediction and criterion (e.g.; truth) vectors

The main input are 2 logical vectors of prediction and criterion values.

## Usage

``` r
classtable(
  prediction_v = NULL,
  criterion_v = NULL,
  correction = 0.25,
  sens.w = NULL,
  cost.outcomes = NULL,
  cost_v = NULL,
  my.goal = NULL,
  my.goal.fun = NULL,
  quiet_mis = FALSE,
  na_prediction_action = "ignore"
)
```

## Arguments

- prediction_v:

  logical. A logical vector of predictions.

- criterion_v:

  logical. A logical vector of (TRUE) criterion values.

- correction:

  numeric. Correction added to all counts for calculating `dprime`.
  Default: `correction = .25`.

- sens.w:

  numeric. Sensitivity weight parameter (from 0 to 1, for computing
  `wacc`). Default: `sens.w = NULL` (to ensure that values are passed by
  calling function).

- cost.outcomes:

  list. A list of length 4 with names 'hi', 'fa', 'mi', and 'cr'
  specifying the costs of a hit, false alarm, miss, and correct
  rejection, respectively. For instance,
  `cost.outcomes = listc("hi" = 0, "fa" = 10, "mi" = 20, "cr" = 0)`
  means that a false alarm and miss cost 10 and 20, respectively, while
  correct decisions have no cost. Default: `cost.outcomes = NULL` (to
  ensure that values are passed by calling function).

- cost_v:

  numeric. Additional cost value of each decision (as an optional vector
  of numeric values). Typically used to include the cue cost of each
  decision (as a constant for the current level of an FFT). Default:
  `cost_v = NULL` (to ensure that values are passed by calling
  function).

- my.goal:

  Name of an optional, user-defined goal (as character string). Default:
  `my.goal = NULL`.

- my.goal.fun:

  User-defined goal function (with 4 arguments `hi fa mi cr`). Default:
  `my.goal.fun = NULL`.

- quiet_mis:

  A logical value passed to hide/show `NA` user feedback (usually
  `x$params$quiet$mis` of the calling function). Default:
  `quiet_mis = FALSE` (i.e., show user feedback).

- na_prediction_action:

  What happens when no prediction is possible? (Experimental and
  currently unused.)

## Details

The primary confusion matrix is computed by
[`confusionMatrix`](https://rdrr.io/pkg/caret/man/confusionMatrix.html).
