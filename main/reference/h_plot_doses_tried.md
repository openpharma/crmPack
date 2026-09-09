# Helper Function to Create Doses Tried Plot

**\[stable\]**

Creates a lollipop or bar plot showing average proportions of doses
tested.

## Usage

``` r
h_plot_doses_tried(
  sim_doses,
  dose_grid,
  prob_plot_type = c("lollipop", "bar"),
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
  dose grid.

- prob_plot_type:

  (`string`)\
  plot geometry, either `"lollipop"` or `"bar"`.

- dose_scale:

  (`string`)\
  dose-axis scale, one of `"auto"`, `"linear"`, or `"log"`. With bars,
  `"auto"` switches to a log scale when equal-width bars would overlap.
  The log scale requires all doses to be strictly positive.

- axis_ticks:

  (`string`)\
  x-axis tick positions, either at each dose-grid value (`"dosegrid"`,
  the default) or at regular positions selected by `ggplot2`
  (`"regular"`).

## Value

A `ggplot` object.
