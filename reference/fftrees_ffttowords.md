# Describe a fast-and-frugal tree (FFT) in words

`fftrees_ffttowords` provides a verbal description of tree definition
(as defined in an `FFTrees` object). Thus, `fftrees_ffttowords`
translates an abstract FFT definition into natural language output.

`fftrees_ffttowords` is the complement function to
[`fftrees_wordstofftrees`](https://www.nathanieldphillips.co/FFTrees/reference/fftrees_wordstofftrees.md),
which parses a verbal description of an FFT into the abstract tree
definition of an `FFTrees` object.

The final sentence (or tree node) of the FFT's description always
predicts positive criterion values (i.e., `TRUE` instances) first,
before predicting negative criterion values (i.e., `FALSE` instances).
Note that this may require a reversal of exit directions, if the final
cue predicted `FALSE` instances.

Note that the cue directions and thresholds computed by **FFTrees**
always predict positive criterion values (i.e., `TRUE` or signal, rather
than `FALSE` or noise). Using these thresholds for negative exits (i.e.,
for predicting instances of `FALSE` or noise) usually requires a
reversal (e.g., negating cue direction).

## Usage

``` r
fftrees_ffttowords(x = NULL, mydata = "train", digits = 2)
```

## Arguments

- x:

  An `FFTrees` object created with
  [`FFTrees`](https://www.nathanieldphillips.co/FFTrees/reference/FFTrees.md).

- mydata:

  The type of data to which a tree is being applied (as character string
  "train" or "test"). Default: `mydata = "train"`.

- digits:

  How many digits to round numeric values (as integer)?

## Value

A modified `FFTrees` object `x` with `x$trees$inwords` containing a list
of string vectors.

## See also

[`fftrees_wordstofftrees`](https://www.nathanieldphillips.co/FFTrees/reference/fftrees_wordstofftrees.md)
for converting a verbal description of an FFT into an `FFTrees` object;
[`fftrees_create`](https://www.nathanieldphillips.co/FFTrees/reference/fftrees_create.md)
for creating `FFTrees` objects;
[`fftrees_grow_fan`](https://www.nathanieldphillips.co/FFTrees/reference/fftrees_grow_fan.md)
for creating FFTs by applying algorithms to data;
[`print.FFTrees`](https://www.nathanieldphillips.co/FFTrees/reference/print.FFTrees.md)
for printing FFTs;
[`plot.FFTrees`](https://www.nathanieldphillips.co/FFTrees/reference/plot.FFTrees.md)
for plotting FFTs;
[`summary.FFTrees`](https://www.nathanieldphillips.co/FFTrees/reference/summary.FFTrees.md)
for summarizing FFTs;
[`FFTrees`](https://www.nathanieldphillips.co/FFTrees/reference/FFTrees.md)
for creating FFTs from and applying them to data.

## Examples

``` r

heart.fft <- FFTrees(diagnosis ~ .,
  data = heartdisease,
  decision.labels = c("Healthy", "Disease")
)
#> ✔ Created an FFTrees object.
#>   Ranking 13 cues:  ■■■■■■■■                          23% | ETA:  1s
#>   Ranking 13 cues:  ■■■■■■■■■■■■■■■                   46% | ETA:  0s
#>   Ranking 13 cues:  ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s
#> 
#> ✔ Ranked 13 cues (optimizing 'bacc').
#> ✔ Created 7 FFTs with 'ifan' algorithm (chasing 'bacc').
#> ✔ Defined 7 FFTs.
#> ✔ Applied 7 FFTs to 'train' data.
#> ✔ Ranked 7 FFTs by 'train' data.
#> ✔ Expressed 7 FFTs in words.

inwords(heart.fft)
#> [1] "If thal = {rd,fd}, decide Disease."                   
#> [2] "If cp != {a}, decide Healthy."                        
#> [3] "If ca > 0, decide Disease, otherwise, decide Healthy."
```
