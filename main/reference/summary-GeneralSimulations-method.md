# Summarize the simulations, relative to a given truth

Summarize the simulations, relative to a given truth

## Usage

``` r
# S4 method for class 'GeneralSimulations'
summary(object, truth, target = c(0.2, 0.35), ...)
```

## Arguments

- object:

  the
  [`GeneralSimulations`](https://openpharma.github.io/crmPack/reference/GeneralSimulations-class.md)
  object we want to summarize

- truth:

  a function which takes as input a dose (vector) and returns the true
  probability (vector) for toxicity

- target:

  the target toxicity interval (default: 20-35%) used for the
  computations

- ...:

  Additional arguments can be supplied here for `truth`

## Value

an object of class
[`GeneralSimulationsSummary`](https://openpharma.github.io/crmPack/reference/GeneralSimulationsSummary-class.md)
