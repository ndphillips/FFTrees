# Create FFT definitions

`fftrees_define` defines fast-and-frugal trees (FFTs) either from the
definitions provided or by applying algorithms (when no definitions are
provided), and returns a modified `FFTrees` object that contains those
definitions.

In most use cases, `fftrees_define` passes a new `FFTrees` object `x`
either to
[`fftrees_grow_fan`](https://www.nathanieldphillips.co/FFTrees/reference/fftrees_grow_fan.md)
(to create new FFTs by applying algorithms to data) or to
[`fftrees_wordstofftrees`](https://www.nathanieldphillips.co/FFTrees/reference/fftrees_wordstofftrees.md)
(if `my.tree` is specified).

If an existing `FFTrees` object `object` or `tree.definitions` are
provided as inputs, no new FFTs are created. When both arguments are
provided, `tree.definitions` take priority over the FFTs in an existing
`object`. Specifically,

- If `tree.definitions` are provided, these are assigned to the FFTs of
  `x`.

- If no `tree.definitions` are provided, but an existing `FFTrees`
  object `object` is provided, the trees from `object` are assigned to
  the FFTs of `x`.

## Usage

``` r
fftrees_define(x, object = NULL, tree.definitions = NULL)
```

## Arguments

- x:

  The current `FFTrees` object (to be changed and returned).

- object:

  An existing `FFTrees` object (with tree definitions).

- tree.definitions:

  A `data.frame`. An optional hard-coded definition of FFTs (in the same
  format as in an `FFTrees` object). If specified, no new FFTs are
  created, but the tree definitions in `object` or `x` are replaced by
  the tree definitions provided and the current object is re-evaluated.

## Value

An `FFTrees` object with tree definitions.

## See also

[`fftrees_create`](https://www.nathanieldphillips.co/FFTrees/reference/fftrees_create.md)
for creating `FFTrees` objects;
[`fftrees_grow_fan`](https://www.nathanieldphillips.co/FFTrees/reference/fftrees_grow_fan.md)
for creating FFTs by applying algorithms to data;
[`fftrees_wordstofftrees`](https://www.nathanieldphillips.co/FFTrees/reference/fftrees_wordstofftrees.md)
for creating FFTs from verbal descriptions;
[`FFTrees`](https://www.nathanieldphillips.co/FFTrees/reference/FFTrees.md)
for creating FFTs from and applying them to data.
