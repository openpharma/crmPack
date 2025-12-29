# `NextBestInfTheory`

**\[stable\]**

`NextBestInfTheory` is the class for next best dose that is based on
information theory as proposed in https://doi.org/10.1002/sim.8450.

## Usage

``` r
NextBestInfTheory(target, asymmetry)

.DefaultNextBestInfTheory()
```

## Arguments

- target:

  (`proportion`)  
  see slot definition.

- asymmetry:

  (`number`)  
  see slot definition.

## Slots

- `target`:

  (`proportion`)  
  target toxicity probability, except 0 or 1.

- `asymmetry`:

  (`number`)  
  value of the asymmetry exponent in the divergence function that
  describes the rate of penalization for overly toxic does. It must be a
  value from \\(0, 2)\\ interval.

## Note

Typically, end users will not use the `.DefaultNextBestInfTheory()`
function.
