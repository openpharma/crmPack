# Set Default Values for kable Parameters

Sets default values for `col.names` and `caption` if not already
provided by the user in the parameter list.

## Usage

``` r
h_kable_param_default(param, col_names = NULL, caption = NULL)
```

## Arguments

- param:

  (`list`)  
  The list of `...` parameters passed to `knit_print`

- col_names:

  (`character`)  
  Default column names for the table

- caption:

  (`character`)  
  Default caption for the table

## Value

The updated parameter list with defaults applied
