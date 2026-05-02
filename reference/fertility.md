# Fertility data

This dataset describes a sample of 100 volunteers providing a semen
sample that was analyzed according to the WHO 2010 criteria.

## Usage

``` r
fertility
```

## Format

A data frame containing 100 rows and 10 columns.

- season:

  Season in which the analysis was performed. (winter, spring, summer,
  fall)

- age:

  Age at the time of analysis

- child.dis:

  Childish diseases (ie , chicken pox, measles, mumps, polio) (yes(1),
  no(0))

- trauma:

  Accident or serious trauma (yes(1), no(0))

- surgery:

  Surgical intervention (yes(1), no(0))

- fevers:

  High fevers in the last year (less than three months ago(-1), more
  than three months ago (0), no. (1))

- alcohol:

  Frequency of alcohol consumption (several times a day, every day,
  several times a week, once a week, hardly ever or never)

- smoking:

  Smoking habit (never(-1), occasional (0)) daily (1))

- sitting:

  Number of hours spent sitting per day

- diagnosis:

  *Criterion*: Diagnosis normal (TRUE) vs. altered (FALSE) (88.0% vs.\\
  22.0%).

## Source

<https://archive.ics.uci.edu/ml/datasets/Fertility>

Original contributors:

David Gil Lucentia Research Group Department of Computer Technology
University of Alicante

Jose Luis Girela Department of Biotechnology University of Alicante

## Details

Sperm concentration are related to socio-demographic data, environmental
factors, health status, and life habits.

We made the following enhancements to the original data for improved
usability:

- The criterion was redefined from a factor variable with two levels
  (`N = Normal`, `O = Altered`) into a logical variable (`TRUE` vs.
  `FALSE`).

Other than that, the data remains consistent with the original dataset.

## See also

Other datasets:
[`blood`](https://www.nathanieldphillips.co/FFTrees/reference/blood.md),
[`breastcancer`](https://www.nathanieldphillips.co/FFTrees/reference/breastcancer.md),
[`car`](https://www.nathanieldphillips.co/FFTrees/reference/car.md),
[`contraceptive`](https://www.nathanieldphillips.co/FFTrees/reference/contraceptive.md),
[`creditapproval`](https://www.nathanieldphillips.co/FFTrees/reference/creditapproval.md),
[`forestfires`](https://www.nathanieldphillips.co/FFTrees/reference/forestfires.md),
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
