# Describe data

Calculate key descriptive statistics for a given set of data.

## Usage

``` r
describe_data(data, data_name, criterion_name, baseline_value)
```

## Arguments

- data:

  A data frame with a criterion variable `criterion_name`.

- data_name:

  A character string specifying a name for the data.

- criterion_name:

  A character string specifying the criterion name.

- baseline_value:

  The value in `criterion_name` denoting the baseline (e.g., `TRUE` or
  `FALSE`).

## Value

A data frame with the descriptive statistics.

## Examples

``` r
data(heartdisease)
describe_data(heartdisease, "heartdisease",
              criterion_name = "diagnosis",
              baseline_value = TRUE)
#> # A tibble: 1 × 7
#>   Name         Cases_n Criterion Baseline_pct Predictors_n NAs_n NAs_pct
#>   <chr>          <int> <chr>            <dbl>        <dbl> <int>   <dbl>
#> 1 heartdisease     303 diagnosis         45.9           13     0       0
```
