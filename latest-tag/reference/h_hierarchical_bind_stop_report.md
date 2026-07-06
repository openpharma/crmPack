# Bind Hierarchical Stop Reports for One Arm

**\[experimental\]**

Converts the per-simulation list of hierarchical stopping rule results
into the arm-level logical matrix expected by existing simulation
summary classes.

## Usage

``` r
h_hierarchical_bind_stop_report(stop_report, arm_name, nsim)
```

## Arguments

- stop_report:

  (`list`)\
  per-simulation list of named arm stop reports.

- arm_name:

  (`string`)\
  hierarchical arm name to extract.

- nsim:

  (`count`)\
  number of simulations.

## Value

A logical matrix with one row per simulation.
