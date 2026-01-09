# Extracting Efficacy Responses for Subjects Categorized by the DLT

**\[stable\]**

A method that extracts efficacy responses for subjects and categorizes
it with respect to DLT, i.e. DLT or no DLT. The efficacy responses are
reported together with their corresponding dose levels.

## Usage

``` r
getEff(object, ...)

# S4 method for class 'DataDual'
getEff(object, no_dlt = FALSE)
```

## Arguments

- object:

  (`DataDual`)  
  object from which the responses and dose levels are extracted.

- ...:

  further arguments passed to class-specific methods.

- no_dlt:

  (`flag`)  
  should only no DLT responses be returned? Otherwise, all responses are
  returned.

## Value

`list` with efficacy responses categorized by the DLT value.

## Examples

``` r
# Example data.
data <- DataDual(
  x = c(25, 50, 25, 50, 75, 300, 250, 150),
  y = c(0, 0, 0, 0, 0, 1, 1, 0),
  w = c(0.31, 0.42, 0.59, 0.45, 0.6, 0.7, 0.6, 0.52),
  doseGrid = seq(25, 300, 25)
)
#> Used default patient IDs!
#> Used best guess cohort indices!

# Get the efficacy response and their corresponding dose levels
# categorized by the DLT.
getEff(data)
#> $x_no_dlt
#> [1]  25  50  25  50  75 150
#> 
#> $w_no_dlt
#> [1] 0.31 0.42 0.59 0.45 0.60 0.52
#> 
#> $x_dlt
#> [1] 300 250
#> 
#> $w_dlt
#> [1] 0.7 0.6
#> 
```
