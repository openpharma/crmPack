# Construct a simplified DesignArm with hardcoded rule objects

Construct a simplified DesignArm with hardcoded rule objects

## Usage

``` r
h_historical_arm_design(data, model)
```

## Arguments

- data:

  (`Data` or `DataCombo`)\
  arm data for historical arms.

- model:

  (`GeneralModel` or `TwoDrugsCombo`)\
  arm model for historical arms.

## Value

A `Design` object with hardcoded rules for historical arms. The rules
don't matter because they won't be used, but they are required to
construct a `Design` object.
