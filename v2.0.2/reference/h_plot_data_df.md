# Preparing Data for Plotting

**\[experimental\]**

This helper function prepares a `data.frame` object based on `Data`
class object. The resulting data frame is used by the plot function for
`Data` class objects.

**\[experimental\]**

A method that transforms
[`GeneralData`](https://openpharma.github.io/crmPack/reference/GeneralData-class.md)
objects into a `tibble` suitable for plotting with `ggplot2` methods

## Usage

``` r
h_plot_data_df(data, ...)

h_plot_data_df(data, ...)

# S4 method for class 'Data'
h_plot_data_df(data, blind = FALSE, legend = TRUE, ...)

# S4 method for class 'DataOrdinal'
h_plot_data_df(data, blind = FALSE, legend = TRUE, ...)
```

## Arguments

- data:

  (`Data`)  
  object from which data is extracted and converted into a data frame.

- ...:

  further arguments passed to `data.frame` constructor. It can be e.g.
  an extra `column_name = value` pair based on a slot from `x` (which in
  this case might be a subclass of `Data`) which does not appear in
  `Data`.

- blind:

  (`flag`)  
  should data be blinded? If `TRUE`, then for each cohort, all DLTs are
  assigned to the first subjects in the cohort. In addition, the placebo
  (if any) is set to the active dose level for that cohort.

- legend:

  (`flag`)  
  Display the legend for the toxicity categories

## Value

A [`data.frame`](https://rdrr.io/r/base/data.frame.html) object with
values to plot.

`data.frame` containing columns for patient, cohort, dose and toxicity
grade

A `data.frame` object with columns patient, ID, cohort, dose and
toxicity.

## Methods (by class)

- `h_plot_data_df(Data)`: method for
  [`Data`](https://openpharma.github.io/crmPack/reference/Data-class.md).

- `h_plot_data_df(DataOrdinal)`: Class specific method for
  [`DataOrdinal`](https://openpharma.github.io/crmPack/reference/DataOrdinal-class.md)
