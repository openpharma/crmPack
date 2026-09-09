# Helper Function to Create Trajectory Plot

**\[stable\]**

Creates a trajectory plot showing dose level statistics across patients.

## Usage

``` r
h_plot_simulation_trajectory(
  sim_doses,
  dose_grid,
  max_patients,
  has_placebo,
  dose_scale = c("auto", "linear", "log"),
  axis_ticks = c("dosegrid", "regular")
)
```

## Arguments

- sim_doses:

  (`list`)\
  list of simulated doses per trial.

- dose_grid:

  (`numeric`)\
  dose grid used for y-axis tick positions.

- max_patients:

  (`integer`)\
  maximum number of patients.

- has_placebo:

  (`flag`)\
  whether the design includes placebo.

- dose_scale:

  (`string`)\
  dose-axis scale, one of `"auto"`, `"linear"`, or `"log"`. `"auto"`
  uses a linear scale for this plot. The log scale requires all doses to
  be strictly positive.

- axis_ticks:

  (`string`)\
  y-axis tick positions, either at each dose-grid value (`"dosegrid"`,
  the default) or at regular positions selected by `ggplot2`
  (`"regular"`).

## Value

A `ggplot` object.
