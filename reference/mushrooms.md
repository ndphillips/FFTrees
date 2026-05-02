# Mushrooms data

Data describing poisonous vs. non-poisonous mushrooms.

## Usage

``` r
mushrooms
```

## Format

A data frame containing 8,124 rows and 23 columns.

See
<http://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.names>
for column descriptions.

- poisonous:

  *Criterion*: Is the mushroom poisonous?

  Values: `TRUE` (poisonous) vs. `FALSE` (eatable) (48.2% vs.\\ 52.8%).

- cshape:

  cap-shape, character (bell=b, conical=c, convex=x, flat=f, knobbed=k,
  sunken=s)

- csurface:

  cap-surface, character (fibrous=f, grooves=g, scaly=y, smooth=s)

- ccolor:

  cap-color, character (brown=n, buff=b, cinnamon=c, gray=g, green=r,
  pink=p, purple=u, red=e, white=w, yellow=y)

- bruises:

  Are there bruises? logical (TRUE/FALSE)

- odor:

  character (almond=a, anise=l, creosote=c, fishy=y, foul=f, musty=m,
  none=n, pungent=p, spicy=s)

- gattach:

  gill-attachment, character (attached=a, descending=d, free=f,
  notched=n)

- gspace:

  gill-spacing, character (close=c, crowded=w, distant=d)

- gsize:

  gill-size, character (broad=b, narrow=n)

- gcolor:

  gill-color, character (black=k, brown=n, buff=b, chocolate=h, gray=g,
  green=r, orange=o, pink=p, purple=u, red=e, white=w, yellow=y)

- sshape:

  stalk-shape, character (enlarging=e, tapering=t)

- sroot:

  stalk-root, character (bulbous=b ,club=c, cup=u, equal=e,
  rhizomorphs=z, rooted=r)

- ssaring:

  stalk-surface-above-ring, character (fibrous=f, scaly=y, silky=k,
  smooth=s)

- ssbring:

  stalk-surface-below-ring, character (fibrous=f, scaly=y, silky=k,
  smooth=s)

- scaring:

  stalk-color-above-ring, character (brown=n, buff=b, cinnamon=c,
  gray=g, orange=o, pink=p, red=e, white=w, yellow=y)

- scbring:

  stalk-color-below-ring, character (brown=n, buff=b, cinnamon=c,
  gray=g, orange=o, pink=p, red=e, white=w, yellow=y)

- vtype:

  veil-type, character (partial=p, universal=u)

- vcolor:

  veil-color, character (brown=n, orange=o, white=w, yellow=y)

- ringnum:

  character (none=n, one=o, two=t)

- ringtype:

  character (cobwebby=c, evanescent=e, flaring=f, large=l, none=n,
  pendant=p, sheathing=s, zone=z)

- sporepc:

  spore-print-color, character (black=k, brown=n, buff=b, chocolate=h,
  green=r, orange=o, purple=u, white=w, yellow=y)

- population:

  character(abundant=a, clustered=c, numerous=n, scattered=s, several=v,
  solitary=y)

- habitat:

  character (grasses=g, leaves=l, meadows=m, paths=p, urban=u, waste=w,
  woods=d)

## Source

<https://archive.ics.uci.edu/ml/datasets/Mushroom>

## Details

This dataset includes descriptions of hypothetical samples corresponding
to 23 species of gilled mushrooms in the Agaricus and Lepiota Family.
Each species is classified as `poisonous` (True or False). The Guide
clearly states that there is no simple rule for determining the
edibility of a mushroom; no rule like “leaflets three, let it be” for
Poisonous Oak and Ivy.

We made the following enhancements to the original data for improved
usability:

- Any missing values, denoted as "?" in the dataset, were transformed
  into NAs.

- Binary factor variables with exclusive "t" and "f" values were
  converted to logical `TRUE/FALSE` vectors.

- The binary factor *criterion* variable with exclusive "p" and "e"
  values was converted to a logical `TRUE/FALSE` vector.

Other than that, the data remains consistent with the original dataset.

## References

Mushroom records drawn from The Audubon Society Field Guide to North
American Mushrooms (1981). G.H. Lincoff (Pres.), New York: A.A. Knopf.

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
[`sonar`](https://www.nathanieldphillips.co/FFTrees/reference/sonar.md),
[`titanic`](https://www.nathanieldphillips.co/FFTrees/reference/titanic.md),
[`voting`](https://www.nathanieldphillips.co/FFTrees/reference/voting.md),
[`wine`](https://www.nathanieldphillips.co/FFTrees/reference/wine.md)
