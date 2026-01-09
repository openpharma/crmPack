# Summarize the `GeneralSimulations`, Relative to a Given Truth

**\[stable\]**

Summarize simulations relative to a given true dose-toxicity curve.

## Usage

``` r
# S4 method for class 'GeneralSimulations'
summary(object, truth, target = c(0.2, 0.35), ...)
```

## Arguments

- object:

  (`GeneralSimulations`)  
  the object we want to summarize.

- truth:

  (`function`)  
  a function which takes as input a dose (vector) and returns the true
  probability (vector) for toxicity.

- target:

  (`numeric`)  
  the target toxicity interval (default: 20-35%) used for the
  computations.

- ...:

  additional arguments can be supplied here for `truth`.

## Value

An object of class
[`GeneralSimulationsSummary`](https://openpharma.github.io/crmPack/reference/GeneralSimulationsSummary-class.md).
