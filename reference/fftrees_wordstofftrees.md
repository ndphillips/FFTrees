# Convert a verbal description of an FFT into an `FFTrees` object

`fftrees_wordstofftrees` converts a verbal description of an FFT
(provided as a string of text) into a tree definition (of an `FFTrees`
object). Thus, `fftrees_wordstofftrees` provides a simple natural
language parser for FFTs.

`fftrees_wordstofftrees` is the complement function to
[`fftrees_ffttowords`](https://www.nathanieldphillips.co/FFTrees/reference/fftrees_ffttowords.md),
which converts an abstract tree definition (of an `FFTrees` object) into
a verbal description (i.e., provides natural language output).

To increase robustness, the parsing of `fftrees_wordstofftrees` allows
for lower- or uppercase spellings (but not typographical variants) and
ignores the else-part of the final sentence (i.e., the part beginning
with "otherwise").

## Usage

``` r
fftrees_wordstofftrees(x, my.tree)
```

## Arguments

- x:

  An `FFTrees` object.

- my.tree:

  A character string. A verbal description (as a string of text)
  defining an FFT.

## Value

An `FFTrees` object with a new tree definition as described by
`my.tree`.

## See also

[`fftrees_ffttowords`](https://www.nathanieldphillips.co/FFTrees/reference/fftrees_ffttowords.md)
for converting FFTs into verbal descriptions;
[`print.FFTrees`](https://www.nathanieldphillips.co/FFTrees/reference/print.FFTrees.md)
for printing FFTs;
[`plot.FFTrees`](https://www.nathanieldphillips.co/FFTrees/reference/plot.FFTrees.md)
for plotting FFTs;
[`summary.FFTrees`](https://www.nathanieldphillips.co/FFTrees/reference/summary.FFTrees.md)
for summarizing FFTs;
[`FFTrees`](https://www.nathanieldphillips.co/FFTrees/reference/FFTrees.md)
for creating FFTs from and applying them to data.
