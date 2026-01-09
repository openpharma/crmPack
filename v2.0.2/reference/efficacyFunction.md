# Getting the Efficacy Function for a Given Model Type

**\[experimental\]**

A function that returns an
[`efficacy()`](https://openpharma.github.io/crmPack/reference/efficacy.md)
function that computes expected efficacy for a given dose level, based
on the model specific parameters.

## Usage

``` r
efficacyFunction(model, ...)

# S4 method for class 'ModelEff'
efficacyFunction(model, ...)
```

## Arguments

- model:

  (`ModelEff`)  
  the model.

- ...:

  model specific parameters.

## Value

A
[`efficacy()`](https://openpharma.github.io/crmPack/reference/efficacy.md)
function that computes expected efficacy.

## Functions

- `efficacyFunction(ModelEff)`:

## See also

[`efficacy()`](https://openpharma.github.io/crmPack/reference/efficacy.md).

## Examples

``` r
my_data <- DataDual(
  doseGrid = c(0.001, seq(25, 300, 25)),
  placebo = TRUE
)

my_model <- Effloglog(
  eff = c(1.223, 2.513),
  eff_dose = c(25, 300),
  nu = c(a = 1, b = 0.025),
  data = my_data,
  const = 2
)

eff_fun <- efficacyFunction(my_model, theta1 = -4.8, theta2 = 3.7)
eff_fun(30)
#> [1] -0.2011775
```
