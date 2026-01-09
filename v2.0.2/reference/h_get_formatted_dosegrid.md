# Format a `doseGrid` for Printing

Format a `doseGrid` for Printing

## Usage

``` r
h_get_formatted_dosegrid(grid, units = NA, fmt = NA, ...)
```

## Arguments

- grid:

  (`numeric`)  
  the dose grid

- units:

  (`character`)  
  The units in which the values in `doseGrid` are

- fmt:

  (`character`)  
  The format used to display values in `doseGrid`. If `NA`, grid values
  are not pre-formatted

- ...:

  not used at present measured. Appended to each value in `doseGrid`
  when `knit_print`ed. The default, `NA`, omits the units.

## Value

A character string containing the formatted dose grid. If the grid is
`c(1, 2, 3)` and `units` is `"mg"`, the returned value is
`"1 mg, 2 mg and 3 mg"`.
