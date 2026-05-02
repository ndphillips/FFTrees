# Credit approval data

This data reports predictors and the result of credit card applications.
Its attribute names and values have been changed to symbols to protect
confidentiality.

## Usage

``` r
creditapproval
```

## Format

A data frame containing 690 cases (rows) and 15 variables (columns).

- c.1:

  categorical: b, a

- c.2:

  continuous

- c.3:

  continuous

- c.4:

  categorical: u, y, l, t

- c.5:

  categorical: g, p, gg

- c.6:

  categorical: c, d, cc, i, j, k, m, r, q, w, x, e, aa, ff

- c.7:

  categorical: v, h, bb, j, n, z, dd, ff, o

- c.8:

  continuous

- c.9:

  categorical: t, f

- c.10:

  categorical: t, f

- c.11:

  continuous

- c.12:

  categorical: t, f

- c.13:

  categorical: g, p, s

- c.14:

  continuous

- c.15:

  continuous

- crit:

  *Criterion*: Credit approval.

  Values: `TRUE` (+) vs. `FALSE` (-) (44.5% vs. 55.5%).

## Source

<https://archive.ics.uci.edu/ml/datasets/Credit+Approval>

## Details

This dataset contains a mix of attributes – continuous, nominal with
small sample sizes, and nominal with larger sample sizes. There are also
a few missing values.

We made the following enhancements to the original data for improved
usability:

- Any missing values, denoted as "?" in the dataset, were transformed
  into `NA` values.

- Binary factor variables with exclusive "t" and "f" values were
  converted to logical vectors (`TRUE`/`FALSE`).

Other than that, the data remains consistent with the original dataset.

## See also

Other datasets:
[`blood`](https://www.nathanieldphillips.co/FFTrees/reference/blood.md),
[`breastcancer`](https://www.nathanieldphillips.co/FFTrees/reference/breastcancer.md),
[`car`](https://www.nathanieldphillips.co/FFTrees/reference/car.md),
[`contraceptive`](https://www.nathanieldphillips.co/FFTrees/reference/contraceptive.md),
[`fertility`](https://www.nathanieldphillips.co/FFTrees/reference/fertility.md),
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
