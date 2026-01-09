# `StoppingExternal`

**\[experimental\]**

`StoppingExternal` is the class for stopping based on an external flag.

## Usage

``` r
StoppingExternal(report_label = NA_character_)

.DefaultStoppingExternal(report_label = NA_character_)
```

## Arguments

- report_label:

  (`string` or `NA`)  
  see slot definition.

## Note

Typically, end users will not use the `.DefaultStoppingExternal()`
function.

## Examples

``` r
my_stopping <- StoppingExternal()
```
