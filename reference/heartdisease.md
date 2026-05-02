# Heart disease data

A dataset predicting the `diagnosis` of 303 patients tested for heart
disease.

## Usage

``` r
heartdisease
```

## Format

A data frame containing 303 rows and 14 columns, with the following
variables:

- diagnosis:

  True value of binary criterion: TRUE = Heart disease, FALSE = No heart
  disease

- age:

  Age (in years)

- sex:

  Sex, 1 = male, 0 = female

- cp:

  Chest pain type: ta = typical angina, aa = atypical angina, np =
  non-anginal pain, a = asymptomatic

- trestbps:

  Resting blood pressure (in mm Hg on admission to the hospital)

- chol:

  Serum cholestoral in mg/dl

- fbs:

  Fasting blood sugar \> 120 mg/dl: 1 = true, 0 = false

- restecg:

  Resting electrocardiographic results. "normal" = normal, "abnormal" =
  having ST-T wave abnormality (T wave inversions and/or ST elevation or
  depression of \> 0.05 mV), "hypertrophy" = showing probable or
  definite left ventricular hypertrophy by Estes' criteria.

- thalach:

  Maximum heart rate achieved

- exang:

  Exercise induced angina: 1 = yes, 0 = no

- oldpeak:

  ST depression induced by exercise relative to rest

- slope:

  The slope of the peak exercise ST segment.

- ca:

  Number of major vessels (0-3) colored by flouroscopy

- thal:

  "normal" = normal, "fd" = fixed defect, "rd" = reversible defect

## Source

<https://archive.ics.uci.edu/ml/datasets/Heart+Disease>

## Details

Note that this is a simplified version of the 303 cases of the Cleveland
Clinic Foundation (V.A. Medical Center, Long Beach and Cleveland Clinic
Foundation; Principal investigator: Robert Detrano, MD, PhD).

The original dataset contains 3 further subsets (from Budapest, Hungary;
Long Beach CA; and Zurich, Switzerland), a total of 76 raw attributes,
and some missing values.

The original criterion variable `num` is integer valued from 0 (no
presence) to 4 (maximum). To obtain a binary criterion `diagnosis`,
values from 1 to 3 have been collapsed to `TRUE`.

## See also

[`heart.cost`](https://www.nathanieldphillips.co/FFTrees/reference/heart.cost.md)
dataset for cost information.

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
[`iris.v`](https://www.nathanieldphillips.co/FFTrees/reference/iris.v.md),
[`mushrooms`](https://www.nathanieldphillips.co/FFTrees/reference/mushrooms.md),
[`sonar`](https://www.nathanieldphillips.co/FFTrees/reference/sonar.md),
[`titanic`](https://www.nathanieldphillips.co/FFTrees/reference/titanic.md),
[`voting`](https://www.nathanieldphillips.co/FFTrees/reference/voting.md),
[`wine`](https://www.nathanieldphillips.co/FFTrees/reference/wine.md)
