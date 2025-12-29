# Helper Function to Calculate Inverse Dose

**\[stable\]**

Creates an inverse function to find the dose corresponding to a target
probability.

## Usage

``` r
h_pseudo_sim_inverse_dose(f, lower = -100, upper = 100)
```

## Arguments

- f:

  (`function`)  
  the truth function mapping dose to probability.

- lower:

  (`number`)  
  lower bound for root finding.

- upper:

  (`number`)  
  upper bound for root finding.

## Value

A function that takes a probability and returns the corresponding dose.
