# Add decision statistics to data (based on frequency counts of a 2x2 matrix of classification outcomes)

`add_stats` assumes the input of the 4 essential classification outcomes
(as frequency counts in a data frame `"data"` with variable names
`"hi"`, `"fa"`, `"mi"`, and `"cr"`) and uses them to compute various
decision accuracy measures.

## Usage

``` r
add_stats(
  data,
  correction = 0.25,
  sens.w = NULL,
  my.goal = NULL,
  my.goal.fun = NULL,
  cost.outcomes = NULL,
  cost.each = NULL
)
```

## Arguments

- data:

  A data frame with 4 frequency counts (as integer values, named `"hi"`,
  `"fa"`, `"mi"`, and `"cr"`).

- correction:

  numeric. Correction added to all counts for calculating `dprime`.
  Default: `correction = .25`.

- sens.w:

  numeric. Sensitivity weight (for computing weighted accuracy, `wacc`).
  Default: `sens.w = NULL` (to ensure that values are passed by calling
  function).

- my.goal:

  Name of an optional, user-defined goal (as character string). Default:
  `my.goal = NULL`.

- my.goal.fun:

  User-defined goal function (with 4 arguments `hi fa mi cr`). Default:
  `my.goal.fun = NULL`.

- cost.outcomes:

  list. A list of length 4 named `"hi"`, `"fa"`, `"mi"`, `"cr"`, and
  specifying the costs of a hit, false alarm, miss, and correct
  rejection, respectively. E.g.;
  `cost.outcomes = listc("hi" = 0, "fa" = 10, "mi" = 20, "cr" = 0)`
  means that a false alarm and miss cost 10 and 20 units, respectively,
  while correct decisions incur no costs. Default:
  `cost.outcomes = NULL` (to ensure that values are passed by calling
  function).

- cost.each:

  numeric. An optional fixed cost added to all outputs (e.g., the cost
  of using the cue). Default: `cost.each = NULL` (to ensure that values
  are passed by calling function).

## Value

A data frame with variables of computed accuracy and cost measures (but
dropping inputs).

## Details

Providing numeric values for `cost.each` (as a vector) and
`cost.outcomes` (as a named list) allows computing cost information for
the counts of corresponding classification decisions.
