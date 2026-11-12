# Contraceptive use data

A subset of the 1987 National Indonesia Contraceptive Prevalence Survey.

## Usage

``` r
contraceptive
```

## Format

A data frame containing 1473 cases (rows) and 10 variables (columns).

- wife.age:

  Wife's age, Numeric

- wife.edu:

  Wife's education, Nummeric, (1=low, 2, 3, 4=high)

- hus.ed:

  Husband's education, Nummeric, (1=low, 2, 3, 4=high)

- children:

  Number of children ever born, Numeric

- wife.rel:

  Wife's religion, Numeric, (0=Non-Islam, 1=Islam)

- wife.work:

  Wife's now working?, Nummeric, (0=Yes, 1=No)

- hus.occ:

  Husband's occupation, Nummeric, (1, 2, 3, 4)

- sol:

  Standard-of-living index, Nummeric, (1=low, 2, 3, 4=high)

- media:

  Media exposure, Numeric, (0=Good, 1=Not good)

- cont.crit:

  *Criterion*: Use of a contraceptive (as logical).

  Values: `FALSE` vs. `TRUE` (42.7% vs. 57.3%).

## Source

<https://archive.ics.uci.edu/ml/datasets/Contraceptive+Method+Choice>

Original creator and donor: Tjen-Sien Lim

## Details

The samples describe married women who were either not pregnant or do
not know if they were pregnant at the time of the interview.

The problem consists in predicting a woman's current contraceptive
method choice (here: binarized `cont.crit`) based on her demographic and
socio-economic characteristics.

We made the following enhancements to the original data for improved
usability:

- The criterion was binarized from a class attribute variable with three
  levels (`1 = No-use`, `2 = Long-term`, `3 = Short-term`), into a
  logical variable (`TRUE` vs. `FALSE`).

Other than that, the data remains consistent with the original dataset.

## See also

Other datasets:
[`blood`](https://www.nathanieldphillips.co/FFTrees/dev/reference/blood.md),
[`breastcancer`](https://www.nathanieldphillips.co/FFTrees/dev/reference/breastcancer.md),
[`car`](https://www.nathanieldphillips.co/FFTrees/dev/reference/car.md),
[`creditapproval`](https://www.nathanieldphillips.co/FFTrees/dev/reference/creditapproval.md),
[`fertility`](https://www.nathanieldphillips.co/FFTrees/dev/reference/fertility.md),
[`forestfires`](https://www.nathanieldphillips.co/FFTrees/dev/reference/forestfires.md),
[`heart.cost`](https://www.nathanieldphillips.co/FFTrees/dev/reference/heart.cost.md),
[`heart.test`](https://www.nathanieldphillips.co/FFTrees/dev/reference/heart.test.md),
[`heart.train`](https://www.nathanieldphillips.co/FFTrees/dev/reference/heart.train.md),
[`heartdisease`](https://www.nathanieldphillips.co/FFTrees/dev/reference/heartdisease.md),
[`iris.v`](https://www.nathanieldphillips.co/FFTrees/dev/reference/iris.v.md),
[`mushrooms`](https://www.nathanieldphillips.co/FFTrees/dev/reference/mushrooms.md),
[`sonar`](https://www.nathanieldphillips.co/FFTrees/dev/reference/sonar.md),
[`titanic`](https://www.nathanieldphillips.co/FFTrees/dev/reference/titanic.md),
[`voting`](https://www.nathanieldphillips.co/FFTrees/dev/reference/voting.md),
[`wine`](https://www.nathanieldphillips.co/FFTrees/dev/reference/wine.md)
