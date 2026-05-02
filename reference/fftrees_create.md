# Create an object of class `FFTrees`

`fftrees_create` creates an `FFTrees` object.

`fftrees_create` is called internally by the main
[`FFTrees`](https://www.nathanieldphillips.co/FFTrees/reference/FFTrees.md)
function. Its main purpose is to verify and store various parameters
(e.g., to denote algorithms, goals, thresholds) to be used in
maximization processes and for evaluation purposes (e.g., `sens.w` and
cost values).

## Usage

``` r
fftrees_create(
  formula = NULL,
  data = NULL,
  data.test = NULL,
  algorithm = NULL,
  goal = NULL,
  goal.chase = NULL,
  goal.threshold = NULL,
  max.levels = NULL,
  numthresh.method = NULL,
  numthresh.n = NULL,
  repeat.cues = NULL,
  stopping.rule = NULL,
  stopping.par = NULL,
  sens.w = NULL,
  cost.outcomes = NULL,
  cost.cues = NULL,
  main = NULL,
  decision.labels = NULL,
  my.goal = NULL,
  my.goal.fun = NULL,
  my.tree = NULL,
  quiet = NULL
)
```

## Arguments

- formula:

  A formula (with a binary criterion variable).

- data:

  Training data (as data frame).

- data.test:

  Data for testing models/prediction (as data frame).

- algorithm:

  Algorithm for growing FFTs (`"ifan"` or `"dfan"`) (as character
  string).

- goal:

  Measure used to select FFTs (as character string).

- goal.chase:

  Measure used to optimize FFT creation (as character string).

- goal.threshold:

  Measure used to optimize cue thresholds (as character string).

- max.levels:

  integer.

- numthresh.method:

  string.

- numthresh.n:

  integer.

- repeat.cues:

  logical.

- stopping.rule:

  string.

- stopping.par:

  numeric.

- sens.w:

  numeric.

- cost.outcomes:

  list.

- cost.cues:

  list.

- main:

  string.

- decision.labels:

  string.

- my.goal:

  The name of an optimization measure defined by `my.goal.fun` (as a
  character string). Example: `my.goal = "my_acc"` (see `my.goal.fun`
  for corresponding function). Default: `my.goal = NULL`.

- my.goal.fun:

  The definition of an outcome measure to optimize, defined as a
  function of the frequency counts of the 4 basic classification
  outcomes `hi, fa, mi, cr` (i.e., an R function with 4 arguments
  `hi, fa, mi, cr`). Example:
  `my.goal.fun = function(hi, fa, mi, cr){(hi + cr)/(hi + fa + mi + cr)}`
  (i.e., accuracy). Default: `my.goal.fun = NULL`.

- my.tree:

  A verbal description of an FFT, i.e., an "FFT in words" (as character
  string). For example,
  `my.tree = "If age > 20, predict TRUE. If sex = {m}, predict FALSE. Otherwise, predict TRUE."`.

- quiet:

  A list of logical elements.

## Value

A new `FFTrees` object.

## See also

[`fftrees_define`](https://www.nathanieldphillips.co/FFTrees/reference/fftrees_define.md)
for defining FFTs;
[`FFTrees`](https://www.nathanieldphillips.co/FFTrees/reference/FFTrees.md)
for creating FFTs from and applying them to data.
