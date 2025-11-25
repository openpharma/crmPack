# Writing JAGS Model to a File

**\[stable\]**

This function converts a R function with JAGS model into a text and then
writes it into a given file. During the "model into text" conversion,
the format of numbers of which absolute value is less than `0.001` or
greater than `10000` is changed. These numbers will be converted into
scientific format with specified number of significant digits using
[`formatC()`](https://rdrr.io/r/base/formatc.html) function.

## Usage

``` r
h_jags_write_model(model, file = NULL, digits = 5)
```

## Arguments

- model:

  (`function`)  
  function containing the JAGS model.

- file:

  (`string` or `NULL`)  
  the name of the file (including the optional path) where the model
  will be saved. If `NULL`, the file will be created in a `R_crmPack`
  folder placed under temporary directory indicated by
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html) function.

- digits:

  (`count`)  
  a desired number of significant digits for for numbers used in JAGS
  input, see [`formatC()`](https://rdrr.io/r/base/formatc.html).

## Value

The name of the file where the model was saved.

## Note

JAGS syntax allows truncation specification like `dnorm(...) I(...)`,
which is illegal in R. To overcome this incompatibility, use dummy
operator `\%_\%` before `I(...)`, i.e. `dnorm(...) \%_\% I(...)` in the
model's code. This dummy operator `\%_\%` will be removed just before
saving the JAGS code into a file. Due to technical issues related to
conversion of numbers to scientific format, it is required that the body
of a model function does not contain `TEMP_NUM_PREF_` or `_TEMP_NUM_SUF`
character constants in its body.

## Examples

``` r
# Some model function
my_model <- function() {
  alpha0 <- mean(1:10)
  alpha1 <- 600000
}

h_jags_write_model(my_model, digits = 5)
#> [1] "/tmp/Rtmpytg9TL/R_crmPack/jags_model_fun2b675200eb6.txt"
```
