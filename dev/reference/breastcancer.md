# Breast cancer data

Physiological data of patients tested for breast cancer.

## Usage

``` r
breastcancer
```

## Format

A data frame containing 699 patients (rows) and 9 variables (columns).

- thickness:

  Clump Thickness

- cellsize.unif:

  Uniformity of Cell Size

- cellshape.unif:

  Uniformity of Cell Shape

- adhesion:

  Marginal Adhesion

- epithelial:

  Single Epithelial Cell Size

- nuclei.bare:

  Bare Nuclei

- chromatin:

  Bland Chromatin

- nucleoli:

  Normal Nucleoli

- mitoses:

  Mitoses

- diagnosis:

  *Criterion*: Absence/presence of breast cancer.

  Values: `FALSE` vs. `TRUE` (65.0% vs.\\ 35.0%).

## Source

<https://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Wisconsin+(Original)>

Original creator:

Dr. William H. Wolberg (physician) University of Wisconsin Hospitals
Madison, Wisconsin, USA

## Details

We made the following enhancements to the original data for improved
usability:

- The ID number of the cases was excluded.

- The numeric criterion with value `2` for benign and `4` for malignant
  was converted to logical (i.e., `TRUE`/`FALSE`).

- 16 cases were excluded because they contained `NA` values.

Other than that, the data remains consistent with the original dataset.

## See also

Other datasets:
[`blood`](https://www.nathanieldphillips.co/FFTrees/dev/reference/blood.md),
[`car`](https://www.nathanieldphillips.co/FFTrees/dev/reference/car.md),
[`contraceptive`](https://www.nathanieldphillips.co/FFTrees/dev/reference/contraceptive.md),
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
