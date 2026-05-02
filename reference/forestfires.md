# Forest fires data

A dataset of forest fire statistics.

## Usage

``` r
forestfires
```

## Format

A data frame containing 517 rows and 13 columns.

- X:

  Integer -x-axis spatial coordinate within the Montesinho park map: 1
  to 9

- Y:

  Integer - y-axis spatial coordinate within the Montesinho park map: 2
  to 9

- month:

  Factor - month of the year: "jan" to "dec"

- day:

  Factor -day of the week: "mon" to "sun"

- FFMC:

  Numeric -FFMC index from the FWI system: 18.7 to 96.20

- DMC:

  Numeric - DMC index from the FWI system: 1.1 to 291.3

- DC:

  Numeric - DC index from the FWI system: 7.9 to 860.6

- ISI:

  Numeric - ISI index from the FWI system: 0.0 to 56.10

- temp:

  Numeric - temperature in Celsius degrees: 2.2 to 33.30

- RH:

  Numeric - relative humidity in percent: 15.0 to 100

- wind:

  Numeric - wind speed in km/h: 0.40 to 9.40

- rain:

  Numeric - outside rain in mm/m2 : 0.0 to 6.4

- fire.crit:

  *Criterion*: Was there a fire (greater than 1.00 ha)?

  Values: `TRUE` (yes) vs. `FALSE` (no) (47.0% vs. 53.0%).

## Source

<http://archive.ics.uci.edu/ml/datasets/Forest+Fires>

Original creator: Prof. Paulo Cortez and Aníbal Morais Department of
Information Systems University of Minho, Portugal

## Details

We made the following enhancements to the original data for improved
usability:

- The criterion was redefined from a numeric variable that indicated the
  number of hectares that burned in a fire into a logical variable
  (`TRUE` (for values \>1) vs. `FALSE` (for values \<=1)).

Other than that, the data remains consistent with the original dataset.

## See also

Other datasets:
[`blood`](https://www.nathanieldphillips.co/FFTrees/reference/blood.md),
[`breastcancer`](https://www.nathanieldphillips.co/FFTrees/reference/breastcancer.md),
[`car`](https://www.nathanieldphillips.co/FFTrees/reference/car.md),
[`contraceptive`](https://www.nathanieldphillips.co/FFTrees/reference/contraceptive.md),
[`creditapproval`](https://www.nathanieldphillips.co/FFTrees/reference/creditapproval.md),
[`fertility`](https://www.nathanieldphillips.co/FFTrees/reference/fertility.md),
[`heart.cost`](https://www.nathanieldphillips.co/FFTrees/reference/heart.cost.md),
[`heart.test`](https://www.nathanieldphillips.co/FFTrees/reference/heart.test.md),
[`heart.train`](https://www.nathanieldphillips.co/FFTrees/reference/heart.train.md),
[`heartdisease`](https://www.nathanieldphillips.co/FFTrees/reference/heartdisease.md),
[`iris.v`](https://www.nathanieldphillips.co/FFTrees/reference/iris.v.md),
[`mushrooms`](https://www.nathanieldphillips.co/FFTrees/reference/mushrooms.md),
[`sonar`](https://www.nathanieldphillips.co/FFTrees/reference/sonar.md),
[`titanic`](https://www.nathanieldphillips.co/FFTrees/reference/titanic.md),
[`voting`](https://www.nathanieldphillips.co/FFTrees/reference/voting.md),
[`wine`](https://www.nathanieldphillips.co/FFTrees/reference/wine.md)
