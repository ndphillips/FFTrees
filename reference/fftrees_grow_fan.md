# Grow fast-and-frugal trees (FFTs) using the `fan` algorithms

`fftrees_grow_fan` is called by
[`fftrees_define`](https://www.nathanieldphillips.co/FFTrees/reference/fftrees_define.md)
to create new FFTs by applying the `fan` algorithms (specifically,
either `ifan` or `dfan`) to data.

## Usage

``` r
fftrees_grow_fan(x, repeat.cues = TRUE)
```

## Arguments

- x:

  An `FFTrees` object.

- repeat.cues:

  Can cues be considered/used repeatedly (as logical)? Default:
  `repeat.cues = TRUE`, but only relevant when using the `dfan`
  algorithm.

## See also

[`fftrees_create`](https://www.nathanieldphillips.co/FFTrees/reference/fftrees_create.md)
for creating `FFTrees` objects;
[`fftrees_define`](https://www.nathanieldphillips.co/FFTrees/reference/fftrees_define.md)
for defining FFTs; `fftrees_grow_fan` for creating FFTs by applying
algorithms to data;
[`fftrees_wordstofftrees`](https://www.nathanieldphillips.co/FFTrees/reference/fftrees_wordstofftrees.md)
for creating FFTs from verbal descriptions;
[`FFTrees`](https://www.nathanieldphillips.co/FFTrees/reference/FFTrees.md)
for creating FFTs from and applying them to data.
