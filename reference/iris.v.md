# Iris data

A famous dataset from R.A. Fisher (1936) simplified to predict only the
virginica class (i.e., as a binary classification problem).

## Usage

``` r
iris.v
```

## Format

A data frame containing 150 rows and 4 columns.

- sep.len:

  sepal length in cm

- sep.wid:

  sepal width in cm

- pet.len:

  petal length in cm

- pet.wid:

  petal width in cm

- virginica:

  *Criterion*: Does an iris belong to the class "virginica"?

  Values: `TRUE` vs. `FALSE` (33.33% vs.66.67%).

## Source

<https://archive.ics.uci.edu/ml/datasets/Iris>

## Details

To improve usability, we made the following changes:

- The criterion was binarized from a factor variable with three levels
  (`Iris-setosa`, `Iris-versicolor`, `Iris-virginica`), into a logical
  variable (i.e., `TRUE` for all instances of `Iris-virginica` and
  `FALSE` for the two other levels).

Other than that, the data remains consistent with the original dataset.

## References

Fisher, R.A. (1936): The use of multiple measurements in taxonomic
problems. Annual Eugenics, 7, Part II, pp. 179–188.

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
[`mushrooms`](https://www.nathanieldphillips.co/FFTrees/reference/mushrooms.md),
[`sonar`](https://www.nathanieldphillips.co/FFTrees/reference/sonar.md),
[`titanic`](https://www.nathanieldphillips.co/FFTrees/reference/titanic.md),
[`voting`](https://www.nathanieldphillips.co/FFTrees/reference/voting.md),
[`wine`](https://www.nathanieldphillips.co/FFTrees/reference/wine.md)
