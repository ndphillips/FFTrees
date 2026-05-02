# Car acceptability data

A dataset on car evaluations based on basic features, derived from a
simple hierarchical decision model.

## Usage

``` r
car
```

## Format

A data frame containing 1728 cars (rows) and 7 variables (columns).

- buying.price:

  price for buying the car, Factor (high, low, med, vhigh)

- maint.price:

  price of the maintenance, Factor (high, low, med, vhigh)

- doors:

  number of doors, Factor (2, 3, 4, 5more)

- persons:

  capacity in terms of persons to carry, Factor (2, 4, more)

- luggage:

  the size of luggage boot, Factor (big, med, small)

- safety:

  estimated safety of the car, Factor (high, low, med)

- acceptability:

  *Criterion*: Category of acceptability rating.

  Values: `unacc`/ `vgood`/ `good`/ `acc`

## Source

<http://archive.ics.uci.edu/ml/datasets/Car+Evaluation>

Original creators and donors: Marko Bohanec and Blaz Zupan

## Details

The criterion variable is a car's `acceptability` rating.

The *criterion* for this dataset has not yet been binarized. Before
using it with **FFTrees**, this prerequisite step should be completed
based on individual preferences.

## References

Bohanec, M., Rajkovic, V. (1990): Expert system for decision making.
*Sistemica*, *1* (1), 145–157.

## See also

Other datasets:
[`blood`](https://www.nathanieldphillips.co/FFTrees/reference/blood.md),
[`breastcancer`](https://www.nathanieldphillips.co/FFTrees/reference/breastcancer.md),
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
[`sonar`](https://www.nathanieldphillips.co/FFTrees/reference/sonar.md),
[`titanic`](https://www.nathanieldphillips.co/FFTrees/reference/titanic.md),
[`voting`](https://www.nathanieldphillips.co/FFTrees/reference/voting.md),
[`wine`](https://www.nathanieldphillips.co/FFTrees/reference/wine.md)
