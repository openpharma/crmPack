# Obtain the Toxicity Threshold Used for Simulation Overdose Reporting

Obtain the Toxicity Threshold Used for Simulation Overdose Reporting

## Usage

``` r
h_overdose_threshold(next_best)

# S4 method for class 'NextBest'
h_overdose_threshold(next_best)

# S4 method for class 'NextBestDualEndpoint'
h_overdose_threshold(next_best)
```

## Arguments

- next_best:

  ([`NextBest`](https://docs.crmpack.org/reference/NextBest-class.md))\
  next-best rule defining the threshold.

## Value

A single toxicity probability threshold.

## Functions

- `h_overdose_threshold(NextBest)`: use the upper toxicity target
  boundary.

- `h_overdose_threshold(NextBestDualEndpoint)`: use the lower boundary
  of the toxicity overdose interval instead of the biomarker target.
