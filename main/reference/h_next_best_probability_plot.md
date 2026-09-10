# Plot a Probability Across a Dose Grid

Creates a one-dimensional probability plot using either lollipops or the
legacy bars.

## Usage

``` r
h_next_best_probability_plot(
  dose_grid,
  probability,
  description,
  colour,
  prob_plot_type = c("lollipop", "bar"),
  dose_scale = c("linear", "log", "factor"),
  axis_ticks = c("dosegrid", "regular"),
  axis_text_angle = ifelse(match.arg(axis_ticks) == "dosegrid", 45, 0),
  base_plot = ggplot()
)
```

## Arguments

- dose_grid:

  (`numeric`) dose grid.

- probability:

  (`proportion`) probability at each dose-grid value.

- description:

  (`string`) y-axis label.

- colour:

  (`string`) colour used for the probability geometry.

- prob_plot_type:

  (`string`) probability geometry, either `"lollipop"` or `"bar"`.

- dose_scale:

  (`string`) dose-axis scale: `"linear"`, `"log"`, or `"factor"` for
  equally spaced dose levels. The log scale requires all doses to be
  strictly positive.

- axis_ticks:

  (`string`) x-axis tick positions, either at each dose-grid value
  (`"dosegrid"`, the default) or at regular positions selected by
  `ggplot2` (`"regular"`).

- axis_text_angle:

  (`number`) rotation angle for x-axis tick labels. Defaults to 45
  degrees for `axis_ticks = "dosegrid"` and 0 degrees for
  `axis_ticks = "regular"`.

- base_plot:

  (`ggplot`) plot containing any background layers to draw below the
  probability geometry.

## Value

A `ggplot2` object.
