# Plot `GeneralSimulationsSummary`

**\[stable\]**

Graphical display of the general simulation summary.

This plot method can be applied to
[`GeneralSimulationsSummary`](https://openpharma.github.io/crmPack/reference/GeneralSimulationsSummary-class.md)
objects in order to summarize them graphically. Possible `type`s of
plots at the moment are:

- nObs:

  Distribution of the number of patients in the simulated trials

- doseSelected:

  Distribution of the final selected doses in the trials. Note that this
  can include zero entries, meaning that the trial was stopped because
  all doses in the dose grid appeared too toxic.

- propDLTs:

  Distribution of the proportion of patients with DLTs in the trials

- nAboveTarget:

  Distribution of the number of patients treated at doses which are
  above the target toxicity interval (as specified by the `truth` and
  `target` arguments to
  [`summary,GeneralSimulations-method`](https://openpharma.github.io/crmPack/reference/summary-GeneralSimulations-method.md))

You can specify any subset of these in the `type` argument.

## Usage

``` r
# S4 method for class 'GeneralSimulationsSummary,missing'
plot(x, y, type = c("nObs", "doseSelected", "propDLTs", "nAboveTarget"), ...)
```

## Arguments

- x:

  (`GeneralSimulationsSummary`)  
  the object we want to plot from.

- y:

  (`missing`)  
  not used.

- type:

  (`character`)  
  the types of plots you want to obtain.

- ...:

  not used.

## Value

A single `ggplot` object if a single plot is asked for, otherwise a
`gtable` object.
