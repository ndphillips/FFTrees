# Visualize cue accuracies (as points in ROC space)

`showcues` plots the cue accuracies of an `FFTrees` object created by
the
[`FFTrees`](https://www.nathanieldphillips.co/FFTrees/reference/FFTrees.md)
function (as points in ROC space).

If the optional arguments `cue.accuracies` and `alt.goal` are specified,
their values take precedence over the corresponding settings of an
`FFTrees` object `x` (but do not change `x`).

`showcues` is called when the main
[`plot.FFTrees`](https://www.nathanieldphillips.co/FFTrees/reference/plot.FFTrees.md)
function is set to `what = "cues"`.

## Usage

``` r
showcues(
  x = NULL,
  cue.accuracies = NULL,
  alt.goal = NULL,
  main = NULL,
  top = 5,
  quiet = list(ini = TRUE, fin = FALSE, set = TRUE),
  ...
)
```

## Arguments

- x:

  An `FFTrees` object created by the
  [`FFTrees`](https://www.nathanieldphillips.co/FFTrees/reference/FFTrees.md)
  function.

- cue.accuracies:

  An optional data frame specifying cue accuracies directly (without
  specifying `FFTrees` object `x`).

- alt.goal:

  An optional alternative goal to sort the current cue accuracies
  (without using the goal of `FFTrees` object `x`).

- main:

  A main plot title (as character string).

- top:

  How many of the top cues should be highlighted (as an integer)?

- quiet:

  Should user feedback messages be suppressed (as a list of 3 logical
  arguments)? Default:
  `quiet = list(ini = TRUE, fin = FALSE, set = FALSE)`.

- ...:

  Graphical parameters (passed to
  [`plot`](https://rdrr.io/r/graphics/plot.default.html)).

## Value

A plot showing cue accuracies (of an `FFTrees` object) (as points in ROC
space).

## See also

[`print.FFTrees`](https://www.nathanieldphillips.co/FFTrees/reference/print.FFTrees.md)
for printing FFTs;
[`plot.FFTrees`](https://www.nathanieldphillips.co/FFTrees/reference/plot.FFTrees.md)
for plotting FFTs;
[`summary.FFTrees`](https://www.nathanieldphillips.co/FFTrees/reference/summary.FFTrees.md)
for summarizing FFTs;
[`FFTrees`](https://www.nathanieldphillips.co/FFTrees/reference/FFTrees.md)
for creating FFTs from and applying them to data.

Other plot functions:
[`plot.FFTrees()`](https://www.nathanieldphillips.co/FFTrees/reference/plot.FFTrees.md)

## Examples

``` r
# Create fast-and-frugal trees (FFTs) for heart disease:
heart.fft <- FFTrees(formula = diagnosis ~ .,
                     data = heart.train,
                     data.test = heart.test,
                     main = "Heart Disease",
                     decision.labels = c("Healthy", "Diseased")
                     )
#> ✔ Created an FFTrees object.
#>   Ranking 13 cues:  ■■■■■■■■■■■■■■■■■■■■■■■■■■        85% | ETA:  0s
#>   Ranking 13 cues:  ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s
#> 
#> ✔ Ranked 13 cues (optimizing 'bacc').
#> ✔ Created 7 FFTs with 'ifan' algorithm (chasing 'bacc').
#> ✔ Defined 7 FFTs.
#> ✔ Applied 7 FFTs to 'train' data.
#> ✔ Ranked 7 FFTs by 'train' data.
#> ✔ Applied 7 FFTs to 'test' data.
#> ✔ Expressed 7 FFTs in words.

# Show cue accuracies (in ROC space):
showcues(heart.fft,
         main = "Predicting heart disease")
#> Plotting cue training statistics:
#> — Cue accuracies ranked by bacc
#> 

```
