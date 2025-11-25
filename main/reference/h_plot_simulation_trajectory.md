# Helper Function to Create Trajectory Plot

**\[stable\]**

Creates a trajectory plot showing dose level statistics across patients.

## Usage

``` r
h_plot_simulation_trajectory(sim_doses, max_patients, has_placebo)
```

## Arguments

- sim_doses:

  (`list`)  
  list of simulated doses per trial.

- max_patients:

  (`integer`)  
  maximum number of patients.

- has_placebo:

  (`flag`)  
  whether the design includes placebo.

## Value

A `ggplot` object.
