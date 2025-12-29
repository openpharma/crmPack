# Plot `gtable` Objects

**\[stable\]**

This is needed because `crmPack` uses
[`gridExtra::arrangeGrob()`](https://rdrr.io/pkg/gridExtra/man/arrangeGrob.html)
to combine `ggplot2` plots, and the resulting `gtable` object is not
plotted otherwise when implicitly printing it in the console.

## Usage

``` r
# S3 method for class 'gtable'
plot(x, ...)

# S3 method for class 'gtable'
print(x, ...)
```

## Arguments

- x:

  (`gtable`)  
  object to print.

- ...:

  additional parameters passed to `plot.gtable()`.

## Value

Called for side effects.
