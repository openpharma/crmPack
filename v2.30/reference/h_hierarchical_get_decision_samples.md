# Helper function to get the samples used for decision rules in a hierarchical design

In a hierarchical design, the samples used for decision rules in each
arm may either be the overall samples from the hierarchical model (if
borrowing is allowed) or the arm-specific samples (if no borrowing).

## Usage

``` r
h_hierarchical_get_decision_samples(
  samples,
  arm_name,
  arm,
  arm_data,
  mcmcOptions
)
```

## Arguments

- samples:

  the overall samples from the hierarchical model.

- arm_name:

  the name of the arm for which we want to get the samples for decision
  rules.

- arm:

  the `DesignArm` object for this arm.

- arm_data:

  the data for this arm.

- mcmcOptions:

  the MCMC options to use if we need to fit an arm-specific model.

## Value

the samples to be used for decision rules for this arm.
