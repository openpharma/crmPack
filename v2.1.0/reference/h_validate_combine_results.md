# Combining S4 Class Validation Results

**\[experimental\]**

A simple helper function that combines two outputs from calls to
`result()` function which is placed in a slot of
[`Validate()`](https://openpharma.github.io/crmPack/reference/Validate.md)
reference class.

## Usage

``` r
h_validate_combine_results(v1, v2)
```

## Arguments

- v1:

  (`logical` or `character`)  
  an output from `result()` function from
  [`Validate()`](https://openpharma.github.io/crmPack/reference/Validate.md)
  reference class, to be combined with `v2`.

- v2:

  (`logical` or `character`)  
  an output from `result()` function from
  [`Validate()`](https://openpharma.github.io/crmPack/reference/Validate.md)
  reference class, to be combined with `v1`.

## Examples

``` r
h_validate_combine_results(TRUE, "some_message")
#> [1] "some_message"
```
