# Summarize `ComboSimulations`

**\[experimental\]**

Summarize two-drug combination simulations.

## Usage

``` r
# S4 method for class 'ComboSimulations'
summary(object, truth, target = c(0.2, 0.35), ...)
```

## Arguments

- object:

  (`ComboSimulations`) the object we want to summarize.

- truth:

  (`function`) optional function mapping a dose combination to a
  toxicity probability. It can accept one argument (length-2 numeric
  vector or one-row matrix) or two numeric arguments (`drug1`, `drug2`).

- target:

  (`numeric`) optional target toxicity interval used only when `truth`
  is supplied.

- ...:

  additional arguments can be supplied here for `truth`.

## Value

An object of class
[`ComboSimulationsSummary`](https://docs.crmpack.org/reference/ComboSimulationsSummary-class.md).
