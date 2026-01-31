# `DataMixture`

**\[stable\]**

`DataMixture` is a class for the data with mixture sharing. It inherits
from
[`Data`](https://openpharma.github.io/crmPack/reference/Data-class.md)
and it contains additional information on the mixture sharing.

## Usage

``` r
DataMixture(xshare = numeric(), yshare = integer(), ...)

.DefaultDataMixture()
```

## Arguments

- xshare:

  (`numeric`)  
  the doses for the share patients.

- yshare:

  (`integer`)  
  the vector of toxicity events (0 or 1) for the share patients. You can
  also supply `numeric` vectors, but these will then be converted to
  `integer` internally.

- ...:

  parameters passed to
  [`Data()`](https://openpharma.github.io/crmPack/reference/Data-class.md).

## Slots

- `xshare`:

  (`numeric`)  
  the doses for the share patients.

- `yshare`:

  (`integer`)  
  the vector of toxicity events (0 or 1) for the share patients.

- `nObsshare`:

  (`count`)  
  number of share patients.

## Note

Typically, end users will not use the `.DefaultDataMixture()` function.

## Examples

``` r
my_data <- DataMixture(
  xshare = c(12, 14, 16, 18.0),
  yshare = c(0L, 1L, 1L, 1L),
  nObsshare = 4L,
  x = c(0.1, 0.5, 1.5),
  y = c(0, 0, 0),
  ID = 1:3,
  cohort = 1:3,
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
)
my_data
#> An object of class "DataMixture"
#> Slot "xshare":
#> [1] 12 14 16 18
#> 
#> Slot "yshare":
#> [1] 0 1 1 1
#> 
#> Slot "nObsshare":
#> [1] 4
#> 
#> Slot "x":
#> [1] 0.1 0.5 1.5
#> 
#> Slot "y":
#> [1] 0 0 0
#> 
#> Slot "doseGrid":
#>  [1]  0.1  0.5  1.5  3.0  6.0 10.0 12.0 14.0 16.0 18.0 20.0 22.0 24.0 26.0 28.0
#> [16] 30.0 32.0 34.0 36.0 38.0 40.0 42.0 44.0 46.0 48.0 50.0 52.0 54.0 56.0 58.0
#> [31] 60.0 62.0 64.0 66.0 68.0 70.0 72.0 74.0 76.0 78.0 80.0
#> 
#> Slot "nGrid":
#> [1] 41
#> 
#> Slot "xLevel":
#> [1] 1 2 3
#> 
#> Slot "placebo":
#> [1] FALSE
#> 
#> Slot "backfilled":
#> [1] FALSE FALSE FALSE
#> 
#> Slot "response":
#> [1] NA NA NA
#> 
#> Slot "ID":
#> [1] 1 2 3
#> 
#> Slot "cohort":
#> [1] 1 2 3
#> 
#> Slot "nObs":
#> [1] 3
#> 
```
