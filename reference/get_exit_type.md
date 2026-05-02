# Get exit type (from a vector `x` of FFT exit descriptions)

`get_exit_type` checks and converts a vector `x` of FFT exit
descriptions into exits of an FFT that correspond to the current options
of `exit_types` (as a global constant).

## Usage

``` r
get_exit_type(x, verify = TRUE)
```

## Arguments

- x:

  A vector of FFT exit descriptions.

- verify:

  A flag to turn verification on/off (as logical). Default:
  `verify = TRUE`.

## Value

A vector of `exit_types` (or an error).

## Details

`get_exit_type` also verifies that the exit types conform to an FFT
(e.g., only the exits of the final node are bi-directional).

## See also

[`FFTrees`](https://www.nathanieldphillips.co/FFTrees/reference/FFTrees.md)
for creating FFTs from and applying them to data.

Other utility functions:
[`get_best_tree()`](https://www.nathanieldphillips.co/FFTrees/reference/get_best_tree.md),
[`get_fft_df()`](https://www.nathanieldphillips.co/FFTrees/reference/get_fft_df.md)

## Examples

``` r
get_exit_type(c(0, 1, .5))
#> [1] 0.0 1.0 0.5
get_exit_type(c(FALSE,   " True ",  2/4))
#> [1] 0.0 1.0 0.5
get_exit_type(c("noise", "signal", "final"))
#> [1] 0.0 1.0 0.5
get_exit_type(c("left",  "right",  "both"))
#> [1] 0.0 1.0 0.5
```
