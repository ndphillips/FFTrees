# Voting data

A dataset of votes for each of the U.S. House of Representatives
Congressmen on the 16 key votes identified by the CQA.

## Usage

``` r
voting
```

## Format

A data frame containing 435 rows and 16 columns.

- handicapped:

  handicapped-infants, logical (TRUE, FALSE)

- water:

  water-project-cost-sharing, logical (TRUE, FALSE)

- adoption:

  adoption-of-the-budget-resolution, logical (TRUE, FALSE)

- physician:

  physician-fee-freeze, logical (TRUE, FALSE)

- elsalvador:

  el-salvador-aid, logical (TRUE, FALSE)

- religionschool:

  religious-groups-in-schools, logical (TRUE, FALSE)

- satellite:

  anti-satellite-test-ban, logical (TRUE, FALSE)

- nicaraguan:

  aid-to-nicaraguan-contras, logical (TRUE, FALSE)

- mxmissile:

  mxmissile, logical (TRUE, FALSE)

- immigration:

  immigration, logical (TRUE, FALSE)

- synfuels:

  synfuels-corporation-cutback, logical (TRUE, FALSE)

- education:

  education-spending, logical (TRUE, FALSE)

- superfund:

  superfund-right-to-sue, logical (TRUE, FALSE)

- crime:

  crime, logical (TRUE, FALSE)

- dutyfree:

  duty-free-exports, logical (TRUE, FALSE)

- southafrica:

  export-administration-act-south-africa, logical (TRUE, FALSE)

- party.crit:

  *Criterion*: Where the voters democratic (or republican) congressmen?

  Values: `TRUE` (democrat) / `FALSE` (republican) (61.52% vs. 38.48%).

## Source

<https://archive.ics.uci.edu/ml/datasets/Congressional+Voting+Records>

## Details

The CQA lists nine different types of votes: Voted for, paired for, and
announced for (these three simplified to yea), voted against, paired
against, and announced against (these three simplified to nay), voted
present, voted present to avoid conflict of interest, and did not vote
or otherwise make a position known (these three simplified to an unknown
disposition).

We made the following enhancements to the original data for improved
usability:

- Any missing values, denoted as "?" in the dataset, were transformed
  into NAs.

- Binary factor variables with exclusive "y" and "n" values were
  converted to logical TRUE/FALSE vectors.

- The binary character *criterion* variable with exclusive "democrat"
  and "republican" values was converted to a logical `TRUE/FALSE`
  vector.

Other than that, the data remains consistent with the original dataset.

## References

Congressional Quarterly Almanac, 98th Congress, 2nd session 1984,
*Congressional Quarterly Inc.*, Volume XL. Washington, D.C., 1985.

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
[`sonar`](https://www.nathanieldphillips.co/FFTrees/reference/sonar.md),
[`titanic`](https://www.nathanieldphillips.co/FFTrees/reference/titanic.md),
[`wine`](https://www.nathanieldphillips.co/FFTrees/reference/wine.md)
