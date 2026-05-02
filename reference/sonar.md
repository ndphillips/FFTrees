# Sonar data

The file contains patterns of sonar signals bounced off a metal cylinder
or bounced off a roughly cylindrical rock at various angles and under
various conditions. The transmitted sonar signal is a
frequency-modulated chirp, rising in frequency.

## Usage

``` r
sonar
```

## Format

A data frame containing 208 rows and 60 columns.

- V1:

  Number in the range 0.0 to 1.0 that represents the energy within a
  particular frequency band, integrated over a certain period of time.

- V2:

  (see `V1`)

- V3:

  (see `V1`)

- V4:

  (see `V1`)

- V5:

  (see `V1`)

- V6:

  (see `V1`)

- V7:

  (see `V1`)

- V8:

  (see `V1`)

- V9:

  (see `V1`)

- V10:

  (see `V1`)

- V11:

  (see `V1`)

- V12:

  (see `V1`)

- V13:

  (see `V1`)

- V14:

  (see `V1`)

- V15:

  (see `V1`)

- V16:

  (see `V1`)

- V17:

  (see `V1`)

- V18:

  (see `V1`)

- V19:

  (see `V1`)

- V20:

  (see `V1`)

- V21:

  (see `V1`)

- V22:

  (see `V1`)

- V23:

  (see `V1`)

- V24:

  (see `V1`)

- V25:

  (see `V1`)

- V26:

  (see `V1`)

- V27:

  (see `V1`)

- V28:

  (see `V1`)

- V29:

  (see `V1`)

- V30:

  (see `V1`)

- V31:

  (see `V1`)

- V32:

  (see `V1`)

- V33:

  (see `V1`)

- V34:

  (see `V1`)

- V35:

  (see `V1`)

- V36:

  (see `V1`)

- V37:

  (see `V1`)

- V38:

  (see `V1`)

- V39:

  (see `V1`)

- V40:

  (see `V1`)

- V41:

  (see `V1`)

- V42:

  (see `V1`)

- V43:

  (see `V1`)

- V44:

  (see `V1`)

- V45:

  (see `V1`)

- V46:

  (see `V1`)

- V47:

  (see `V1`)

- V48:

  (see `V1`)

- V49:

  (see `V1`)

- V50:

  (see `V1`)

- V51:

  (see `V1`)

- V52:

  (see `V1`)

- V53:

  (see `V1`)

- V54:

  (see `V1`)

- V55:

  (see `V1`)

- V56:

  (see `V1`)

- V57:

  (see `V1`)

- V58:

  (see `V1`)

- V59:

  (see `V1`)

- V60:

  (see `V1`)

- mine.crit:

  *Criterion*: Did a sonar signal bounce off a metal cylinder (or a
  rock)?

  Values: `TRUE` (metal cylinder) vs. `FALSE` (rock) (53.37% vs.\\
  46.63%).

## Source

<https://archive.ics.uci.edu/ml/datasets/Connectionist+Bench+(Sonar,+Mines+vs.+Rocks)>

## Details

We made the following enhancements to the original data for improved
usability:

- The binary factor *criterion* variable with exclusive "m" and "r"
  values was converted to a logical `TRUE/FALSE` vector.

Other than that, the data remains consistent with the original dataset.

## References

Gorman, R. P., and Sejnowski, T. J. (1988). Analysis of hidden units in
a layered network trained to classify sonar targets. *Neural Networks*,
*1*, pp. 75–89.

## See also

Other datasets:
[`blood`](https://www.nathanieldphillips.co/FFTrees/reference/blood.md),
[`breastcancer`](https://www.nathanieldphillips.co/FFTrees/reference/breastcancer.md),
[`car`](https://www.nathanieldphillips.co/FFTrees/reference/car.md),
[`contraceptive`](https://www.nathanieldphillips.co/FFTrees/reference/contraceptive.md),
[`creditapproval`](https://www.nathanieldphillips.co/FFTrees/reference/creditapproval.md),
[`fertility`](https://www.nathanieldphillips.co/FFTrees/reference/fertility.md),
[`forestfires`](https://www.nathanieldphillips.co/FFTrees/reference/forestfires.md),
[`heart.cost`](https://www.nathanieldphillips.co/FFTrees/reference/heart.cost.md),
[`heart.test`](https://www.nathanieldphillips.co/FFTrees/reference/heart.test.md),
[`heart.train`](https://www.nathanieldphillips.co/FFTrees/reference/heart.train.md),
[`heartdisease`](https://www.nathanieldphillips.co/FFTrees/reference/heartdisease.md),
[`iris.v`](https://www.nathanieldphillips.co/FFTrees/reference/iris.v.md),
[`mushrooms`](https://www.nathanieldphillips.co/FFTrees/reference/mushrooms.md),
[`titanic`](https://www.nathanieldphillips.co/FFTrees/reference/titanic.md),
[`voting`](https://www.nathanieldphillips.co/FFTrees/reference/voting.md),
[`wine`](https://www.nathanieldphillips.co/FFTrees/reference/wine.md)
