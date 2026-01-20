# Tidying `CrmPackClass` objects

**\[experimental\]**

In the spirit of the `broom` package, provide a method to convert a
`CrmPackClass` object to a (list of) tibbles.

Following the principles of the `broom` package, convert a
`CrmPackClass` object to a (list of) tibbles. This is a basic, default
representation.

**\[experimental\]**

A method that tidies a
[`GeneralData`](https://openpharma.github.io/crmPack/reference/GeneralData-class.md)
object.

**\[experimental\]**

A method that tidies a
[`DataGrouped`](https://openpharma.github.io/crmPack/reference/DataGrouped-class.md)
object.

**\[experimental\]**

A method that tidies a
[`DataDA`](https://openpharma.github.io/crmPack/reference/DataDA-class.md)
object.

**\[experimental\]**

A method that tidies a
[`DataDual`](https://openpharma.github.io/crmPack/reference/DataDual-class.md)
object.

**\[experimental\]**

A method that tidies a
[`DataParts`](https://openpharma.github.io/crmPack/reference/DataParts-class.md)
object.

**\[experimental\]**

A method that tidies a
[`DataMixture`](https://openpharma.github.io/crmPack/reference/DataMixture-class.md)
object.

**\[experimental\]**

A method that tidies a
[`DataOrdinal`](https://openpharma.github.io/crmPack/reference/DataOrdinal-class.md)
object.

**\[experimental\]**

A method that tidies a
[`LogisticIndepBeta`](https://openpharma.github.io/crmPack/reference/LogisticIndepBeta-class.md)
object.

**\[experimental\]**

A method that tidies a
[`Effloglog`](https://openpharma.github.io/crmPack/reference/Effloglog-class.md)
object.

## Usage

``` r
tidy(x, ...)

# S4 method for class 'CrmPackClass'
tidy(x, ...)

# S4 method for class 'GeneralData'
tidy(x, ...)

# S4 method for class 'DataGrouped'
tidy(x, ...)

# S4 method for class 'DataDA'
tidy(x, ...)

# S4 method for class 'DataDual'
tidy(x, ...)

# S4 method for class 'DataParts'
tidy(x, ...)

# S4 method for class 'DataMixture'
tidy(x, ...)

# S4 method for class 'DataOrdinal'
tidy(x, ...)

# S4 method for class 'Simulations'
tidy(x, ...)

# S4 method for class 'LogisticIndepBeta'
tidy(x, ...)

# S4 method for class 'Effloglog'
tidy(x, ...)

# S4 method for class 'IncrementsMaxToxProb'
tidy(x, ...)

# S4 method for class 'IncrementsRelative'
tidy(x, ...)

# S4 method for class 'CohortSizeDLT'
tidy(x, ...)

# S4 method for class 'CohortSizeMin'
tidy(x, ...)

# S4 method for class 'CohortSizeMax'
tidy(x, ...)

# S4 method for class 'CohortSizeRange'
tidy(x, ...)

# S4 method for class 'CohortSizeParts'
tidy(x, ...)

# S4 method for class 'IncrementsMin'
tidy(x, ...)

# S4 method for class 'IncrementsRelative'
tidy(x, ...)

# S4 method for class 'IncrementsRelativeDLT'
tidy(x, ...)

# S4 method for class 'IncrementsRelativeParts'
tidy(x, ...)

# S4 method for class 'NextBestNCRM'
tidy(x, ...)

# S4 method for class 'NextBestNCRMLoss'
tidy(x, ...)

# S4 method for class 'DualDesign'
tidy(x, ...)

# S4 method for class 'Samples'
tidy(x, ...)
```

## Arguments

- x:

  (`CrmPackClass`)  
  the object to be tidied.

- ...:

  potentially used by class-specific methods.

## Value

A (list of) tibble(s) representing the object in tidy form.

The
[`tibble::tibble`](https://tibble.tidyverse.org/reference/tibble.html)
object.

The
[`tibble::tibble`](https://tibble.tidyverse.org/reference/tibble.html)
object.

The
[`tibble::tibble`](https://tibble.tidyverse.org/reference/tibble.html)
object.

The
[`tibble::tibble`](https://tibble.tidyverse.org/reference/tibble.html)
object.

The
[`tibble::tibble`](https://tibble.tidyverse.org/reference/tibble.html)
object.

The
[`tibble::tibble`](https://tibble.tidyverse.org/reference/tibble.html)
object.

The
[`tibble::tibble`](https://tibble.tidyverse.org/reference/tibble.html)
object.

The [`list`](https://rdrr.io/r/base/list.html) of
[`tibble::tibble`](https://tibble.tidyverse.org/reference/tibble.html)
objects.

The [`list`](https://rdrr.io/r/base/list.html) of
[`tibble::tibble`](https://tibble.tidyverse.org/reference/tibble.html)
objects.

## Usage Notes

The prior observations are indicated by a `Cohort` value of `0` in the
returned `tibble`.

## Examples

``` r
CohortSizeConst(3) %>% tidy()
#> # A tibble: 1 × 1
#>    size
#>   <int>
#> 1     3
.DefaultData() %>% tidy()
.DefaultDataOrdinal() %>% tidy()
#> # A tibble: 10 × 11
#>       ID Cohort  Dose Placebo  NObs NGrid DoseGrid   XLevel Cat0  Cat1  Cat2 
#>    <int>  <int> <dbl> <lgl>   <int> <int> <list>      <int> <lgl> <lgl> <lgl>
#>  1     1      1    10 FALSE      10    10 <dbl [10]>      1 TRUE  FALSE FALSE
#>  2     2      2    20 FALSE      10    10 <dbl [10]>      2 TRUE  FALSE FALSE
#>  3     3      3    30 FALSE      10    10 <dbl [10]>      3 TRUE  FALSE FALSE
#>  4     4      4    40 FALSE      10    10 <dbl [10]>      4 TRUE  FALSE FALSE
#>  5     5      5    50 FALSE      10    10 <dbl [10]>      5 TRUE  FALSE FALSE
#>  6     6      5    50 FALSE      10    10 <dbl [10]>      5 FALSE TRUE  FALSE
#>  7     7      5    50 FALSE      10    10 <dbl [10]>      5 TRUE  FALSE FALSE
#>  8     8      6    60 FALSE      10    10 <dbl [10]>      6 TRUE  FALSE FALSE
#>  9     9      6    60 FALSE      10    10 <dbl [10]>      6 FALSE TRUE  FALSE
#> 10    10      6    60 FALSE      10    10 <dbl [10]>      6 FALSE FALSE TRUE 
.DefaultDataGrouped() %>% tidy()
#> # A tibble: 3 × 10
#>      ID Cohort  Dose XLevel Tox   Placebo  NObs NGrid DoseGrid   Group
#>   <int>  <int> <dbl>  <int> <lgl> <lgl>   <int> <int> <list>     <fct>
#> 1     1      1     1      1 FALSE FALSE       3    11 <dbl [11]> mono 
#> 2     2      2     3      2 FALSE FALSE       3    11 <dbl [11]> mono 
#> 3     3      3     5      3 FALSE FALSE       3    11 <dbl [11]> combo
.DefaultDataDA() %>% tidy()
#> # A tibble: 8 × 12
#>      ID Cohort  Dose XLevel Tox   Placebo  NObs NGrid DoseGrid     U    T0  TMax
#>   <int>  <int> <dbl>  <int> <lgl> <lgl>   <int> <int> <list>   <dbl> <dbl> <dbl>
#> 1     1      1   0.1      1 FALSE FALSE       8    41 <dbl>       42     0    60
#> 2     2      2   0.5      2 FALSE FALSE       8    41 <dbl>       30    15    60
#> 3     3      3   1.5      3 TRUE  FALSE       8    41 <dbl>       15    30    60
#> 4     4      4   3        4 TRUE  FALSE       8    41 <dbl>        5    40    60
#> 5     5      5   6        5 FALSE FALSE       8    41 <dbl>       20    55    60
#> 6     6      6  10        6 FALSE FALSE       8    41 <dbl>       25    70    60
#> 7     7      6  10        6 TRUE  FALSE       8    41 <dbl>       30    75    60
#> 8     8      6  10        6 FALSE FALSE       8    41 <dbl>       60    85    60
.DefaultData() %>% tidy()
.DefaultDataOrdinal() %>% tidy()
#> # A tibble: 10 × 11
#>       ID Cohort  Dose Placebo  NObs NGrid DoseGrid   XLevel Cat0  Cat1  Cat2 
#>    <int>  <int> <dbl> <lgl>   <int> <int> <list>      <int> <lgl> <lgl> <lgl>
#>  1     1      1    10 FALSE      10    10 <dbl [10]>      1 TRUE  FALSE FALSE
#>  2     2      2    20 FALSE      10    10 <dbl [10]>      2 TRUE  FALSE FALSE
#>  3     3      3    30 FALSE      10    10 <dbl [10]>      3 TRUE  FALSE FALSE
#>  4     4      4    40 FALSE      10    10 <dbl [10]>      4 TRUE  FALSE FALSE
#>  5     5      5    50 FALSE      10    10 <dbl [10]>      5 TRUE  FALSE FALSE
#>  6     6      5    50 FALSE      10    10 <dbl [10]>      5 FALSE TRUE  FALSE
#>  7     7      5    50 FALSE      10    10 <dbl [10]>      5 TRUE  FALSE FALSE
#>  8     8      6    60 FALSE      10    10 <dbl [10]>      6 TRUE  FALSE FALSE
#>  9     9      6    60 FALSE      10    10 <dbl [10]>      6 FALSE TRUE  FALSE
#> 10    10      6    60 FALSE      10    10 <dbl [10]>      6 FALSE FALSE TRUE 
.DefaultDataGrouped() %>% tidy()
#> # A tibble: 3 × 10
#>      ID Cohort  Dose XLevel Tox   Placebo  NObs NGrid DoseGrid   Group
#>   <int>  <int> <dbl>  <int> <lgl> <lgl>   <int> <int> <list>     <fct>
#> 1     1      1     1      1 FALSE FALSE       3    11 <dbl [11]> mono 
#> 2     2      2     3      2 FALSE FALSE       3    11 <dbl [11]> mono 
#> 3     3      3     5      3 FALSE FALSE       3    11 <dbl [11]> combo
.DefaultDataDA() %>% tidy()
#> # A tibble: 8 × 12
#>      ID Cohort  Dose XLevel Tox   Placebo  NObs NGrid DoseGrid     U    T0  TMax
#>   <int>  <int> <dbl>  <int> <lgl> <lgl>   <int> <int> <list>   <dbl> <dbl> <dbl>
#> 1     1      1   0.1      1 FALSE FALSE       8    41 <dbl>       42     0    60
#> 2     2      2   0.5      2 FALSE FALSE       8    41 <dbl>       30    15    60
#> 3     3      3   1.5      3 TRUE  FALSE       8    41 <dbl>       15    30    60
#> 4     4      4   3        4 TRUE  FALSE       8    41 <dbl>        5    40    60
#> 5     5      5   6        5 FALSE FALSE       8    41 <dbl>       20    55    60
#> 6     6      6  10        6 FALSE FALSE       8    41 <dbl>       25    70    60
#> 7     7      6  10        6 TRUE  FALSE       8    41 <dbl>       30    75    60
#> 8     8      6  10        6 FALSE FALSE       8    41 <dbl>       60    85    60
.DefaultData() %>% tidy()
.DefaultDataOrdinal() %>% tidy()
#> # A tibble: 10 × 11
#>       ID Cohort  Dose Placebo  NObs NGrid DoseGrid   XLevel Cat0  Cat1  Cat2 
#>    <int>  <int> <dbl> <lgl>   <int> <int> <list>      <int> <lgl> <lgl> <lgl>
#>  1     1      1    10 FALSE      10    10 <dbl [10]>      1 TRUE  FALSE FALSE
#>  2     2      2    20 FALSE      10    10 <dbl [10]>      2 TRUE  FALSE FALSE
#>  3     3      3    30 FALSE      10    10 <dbl [10]>      3 TRUE  FALSE FALSE
#>  4     4      4    40 FALSE      10    10 <dbl [10]>      4 TRUE  FALSE FALSE
#>  5     5      5    50 FALSE      10    10 <dbl [10]>      5 TRUE  FALSE FALSE
#>  6     6      5    50 FALSE      10    10 <dbl [10]>      5 FALSE TRUE  FALSE
#>  7     7      5    50 FALSE      10    10 <dbl [10]>      5 TRUE  FALSE FALSE
#>  8     8      6    60 FALSE      10    10 <dbl [10]>      6 TRUE  FALSE FALSE
#>  9     9      6    60 FALSE      10    10 <dbl [10]>      6 FALSE TRUE  FALSE
#> 10    10      6    60 FALSE      10    10 <dbl [10]>      6 FALSE FALSE TRUE 
.DefaultDataGrouped() %>% tidy()
#> # A tibble: 3 × 10
#>      ID Cohort  Dose XLevel Tox   Placebo  NObs NGrid DoseGrid   Group
#>   <int>  <int> <dbl>  <int> <lgl> <lgl>   <int> <int> <list>     <fct>
#> 1     1      1     1      1 FALSE FALSE       3    11 <dbl [11]> mono 
#> 2     2      2     3      2 FALSE FALSE       3    11 <dbl [11]> mono 
#> 3     3      3     5      3 FALSE FALSE       3    11 <dbl [11]> combo
.DefaultDataDA() %>% tidy()
#> # A tibble: 8 × 12
#>      ID Cohort  Dose XLevel Tox   Placebo  NObs NGrid DoseGrid     U    T0  TMax
#>   <int>  <int> <dbl>  <int> <lgl> <lgl>   <int> <int> <list>   <dbl> <dbl> <dbl>
#> 1     1      1   0.1      1 FALSE FALSE       8    41 <dbl>       42     0    60
#> 2     2      2   0.5      2 FALSE FALSE       8    41 <dbl>       30    15    60
#> 3     3      3   1.5      3 TRUE  FALSE       8    41 <dbl>       15    30    60
#> 4     4      4   3        4 TRUE  FALSE       8    41 <dbl>        5    40    60
#> 5     5      5   6        5 FALSE FALSE       8    41 <dbl>       20    55    60
#> 6     6      6  10        6 FALSE FALSE       8    41 <dbl>       25    70    60
#> 7     7      6  10        6 TRUE  FALSE       8    41 <dbl>       30    75    60
#> 8     8      6  10        6 FALSE FALSE       8    41 <dbl>       60    85    60
.DefaultData() %>% tidy()
.DefaultDataOrdinal() %>% tidy()
#> # A tibble: 10 × 11
#>       ID Cohort  Dose Placebo  NObs NGrid DoseGrid   XLevel Cat0  Cat1  Cat2 
#>    <int>  <int> <dbl> <lgl>   <int> <int> <list>      <int> <lgl> <lgl> <lgl>
#>  1     1      1    10 FALSE      10    10 <dbl [10]>      1 TRUE  FALSE FALSE
#>  2     2      2    20 FALSE      10    10 <dbl [10]>      2 TRUE  FALSE FALSE
#>  3     3      3    30 FALSE      10    10 <dbl [10]>      3 TRUE  FALSE FALSE
#>  4     4      4    40 FALSE      10    10 <dbl [10]>      4 TRUE  FALSE FALSE
#>  5     5      5    50 FALSE      10    10 <dbl [10]>      5 TRUE  FALSE FALSE
#>  6     6      5    50 FALSE      10    10 <dbl [10]>      5 FALSE TRUE  FALSE
#>  7     7      5    50 FALSE      10    10 <dbl [10]>      5 TRUE  FALSE FALSE
#>  8     8      6    60 FALSE      10    10 <dbl [10]>      6 TRUE  FALSE FALSE
#>  9     9      6    60 FALSE      10    10 <dbl [10]>      6 FALSE TRUE  FALSE
#> 10    10      6    60 FALSE      10    10 <dbl [10]>      6 FALSE FALSE TRUE 
.DefaultDataGrouped() %>% tidy()
#> # A tibble: 3 × 10
#>      ID Cohort  Dose XLevel Tox   Placebo  NObs NGrid DoseGrid   Group
#>   <int>  <int> <dbl>  <int> <lgl> <lgl>   <int> <int> <list>     <fct>
#> 1     1      1     1      1 FALSE FALSE       3    11 <dbl [11]> mono 
#> 2     2      2     3      2 FALSE FALSE       3    11 <dbl [11]> mono 
#> 3     3      3     5      3 FALSE FALSE       3    11 <dbl [11]> combo
.DefaultDataDA() %>% tidy()
#> # A tibble: 8 × 12
#>      ID Cohort  Dose XLevel Tox   Placebo  NObs NGrid DoseGrid     U    T0  TMax
#>   <int>  <int> <dbl>  <int> <lgl> <lgl>   <int> <int> <list>   <dbl> <dbl> <dbl>
#> 1     1      1   0.1      1 FALSE FALSE       8    41 <dbl>       42     0    60
#> 2     2      2   0.5      2 FALSE FALSE       8    41 <dbl>       30    15    60
#> 3     3      3   1.5      3 TRUE  FALSE       8    41 <dbl>       15    30    60
#> 4     4      4   3        4 TRUE  FALSE       8    41 <dbl>        5    40    60
#> 5     5      5   6        5 FALSE FALSE       8    41 <dbl>       20    55    60
#> 6     6      6  10        6 FALSE FALSE       8    41 <dbl>       25    70    60
#> 7     7      6  10        6 TRUE  FALSE       8    41 <dbl>       30    75    60
#> 8     8      6  10        6 FALSE FALSE       8    41 <dbl>       60    85    60
.DefaultData() %>% tidy()
.DefaultDataOrdinal() %>% tidy()
#> # A tibble: 10 × 11
#>       ID Cohort  Dose Placebo  NObs NGrid DoseGrid   XLevel Cat0  Cat1  Cat2 
#>    <int>  <int> <dbl> <lgl>   <int> <int> <list>      <int> <lgl> <lgl> <lgl>
#>  1     1      1    10 FALSE      10    10 <dbl [10]>      1 TRUE  FALSE FALSE
#>  2     2      2    20 FALSE      10    10 <dbl [10]>      2 TRUE  FALSE FALSE
#>  3     3      3    30 FALSE      10    10 <dbl [10]>      3 TRUE  FALSE FALSE
#>  4     4      4    40 FALSE      10    10 <dbl [10]>      4 TRUE  FALSE FALSE
#>  5     5      5    50 FALSE      10    10 <dbl [10]>      5 TRUE  FALSE FALSE
#>  6     6      5    50 FALSE      10    10 <dbl [10]>      5 FALSE TRUE  FALSE
#>  7     7      5    50 FALSE      10    10 <dbl [10]>      5 TRUE  FALSE FALSE
#>  8     8      6    60 FALSE      10    10 <dbl [10]>      6 TRUE  FALSE FALSE
#>  9     9      6    60 FALSE      10    10 <dbl [10]>      6 FALSE TRUE  FALSE
#> 10    10      6    60 FALSE      10    10 <dbl [10]>      6 FALSE FALSE TRUE 
.DefaultDataGrouped() %>% tidy()
#> # A tibble: 3 × 10
#>      ID Cohort  Dose XLevel Tox   Placebo  NObs NGrid DoseGrid   Group
#>   <int>  <int> <dbl>  <int> <lgl> <lgl>   <int> <int> <list>     <fct>
#> 1     1      1     1      1 FALSE FALSE       3    11 <dbl [11]> mono 
#> 2     2      2     3      2 FALSE FALSE       3    11 <dbl [11]> mono 
#> 3     3      3     5      3 FALSE FALSE       3    11 <dbl [11]> combo
.DefaultDataDA() %>% tidy()
#> # A tibble: 8 × 12
#>      ID Cohort  Dose XLevel Tox   Placebo  NObs NGrid DoseGrid     U    T0  TMax
#>   <int>  <int> <dbl>  <int> <lgl> <lgl>   <int> <int> <list>   <dbl> <dbl> <dbl>
#> 1     1      1   0.1      1 FALSE FALSE       8    41 <dbl>       42     0    60
#> 2     2      2   0.5      2 FALSE FALSE       8    41 <dbl>       30    15    60
#> 3     3      3   1.5      3 TRUE  FALSE       8    41 <dbl>       15    30    60
#> 4     4      4   3        4 TRUE  FALSE       8    41 <dbl>        5    40    60
#> 5     5      5   6        5 FALSE FALSE       8    41 <dbl>       20    55    60
#> 6     6      6  10        6 FALSE FALSE       8    41 <dbl>       25    70    60
#> 7     7      6  10        6 TRUE  FALSE       8    41 <dbl>       30    75    60
#> 8     8      6  10        6 FALSE FALSE       8    41 <dbl>       60    85    60
.DefaultData() %>% tidy()
.DefaultDataOrdinal() %>% tidy()
#> # A tibble: 10 × 11
#>       ID Cohort  Dose Placebo  NObs NGrid DoseGrid   XLevel Cat0  Cat1  Cat2 
#>    <int>  <int> <dbl> <lgl>   <int> <int> <list>      <int> <lgl> <lgl> <lgl>
#>  1     1      1    10 FALSE      10    10 <dbl [10]>      1 TRUE  FALSE FALSE
#>  2     2      2    20 FALSE      10    10 <dbl [10]>      2 TRUE  FALSE FALSE
#>  3     3      3    30 FALSE      10    10 <dbl [10]>      3 TRUE  FALSE FALSE
#>  4     4      4    40 FALSE      10    10 <dbl [10]>      4 TRUE  FALSE FALSE
#>  5     5      5    50 FALSE      10    10 <dbl [10]>      5 TRUE  FALSE FALSE
#>  6     6      5    50 FALSE      10    10 <dbl [10]>      5 FALSE TRUE  FALSE
#>  7     7      5    50 FALSE      10    10 <dbl [10]>      5 TRUE  FALSE FALSE
#>  8     8      6    60 FALSE      10    10 <dbl [10]>      6 TRUE  FALSE FALSE
#>  9     9      6    60 FALSE      10    10 <dbl [10]>      6 FALSE TRUE  FALSE
#> 10    10      6    60 FALSE      10    10 <dbl [10]>      6 FALSE FALSE TRUE 
.DefaultDataGrouped() %>% tidy()
#> # A tibble: 3 × 10
#>      ID Cohort  Dose XLevel Tox   Placebo  NObs NGrid DoseGrid   Group
#>   <int>  <int> <dbl>  <int> <lgl> <lgl>   <int> <int> <list>     <fct>
#> 1     1      1     1      1 FALSE FALSE       3    11 <dbl [11]> mono 
#> 2     2      2     3      2 FALSE FALSE       3    11 <dbl [11]> mono 
#> 3     3      3     5      3 FALSE FALSE       3    11 <dbl [11]> combo
.DefaultDataDA() %>% tidy()
#> # A tibble: 8 × 12
#>      ID Cohort  Dose XLevel Tox   Placebo  NObs NGrid DoseGrid     U    T0  TMax
#>   <int>  <int> <dbl>  <int> <lgl> <lgl>   <int> <int> <list>   <dbl> <dbl> <dbl>
#> 1     1      1   0.1      1 FALSE FALSE       8    41 <dbl>       42     0    60
#> 2     2      2   0.5      2 FALSE FALSE       8    41 <dbl>       30    15    60
#> 3     3      3   1.5      3 TRUE  FALSE       8    41 <dbl>       15    30    60
#> 4     4      4   3        4 TRUE  FALSE       8    41 <dbl>        5    40    60
#> 5     5      5   6        5 FALSE FALSE       8    41 <dbl>       20    55    60
#> 6     6      6  10        6 FALSE FALSE       8    41 <dbl>       25    70    60
#> 7     7      6  10        6 TRUE  FALSE       8    41 <dbl>       30    75    60
#> 8     8      6  10        6 FALSE FALSE       8    41 <dbl>       60    85    60
.DefaultData() %>% tidy()
.DefaultDataOrdinal() %>% tidy()
#> # A tibble: 10 × 11
#>       ID Cohort  Dose Placebo  NObs NGrid DoseGrid   XLevel Cat0  Cat1  Cat2 
#>    <int>  <int> <dbl> <lgl>   <int> <int> <list>      <int> <lgl> <lgl> <lgl>
#>  1     1      1    10 FALSE      10    10 <dbl [10]>      1 TRUE  FALSE FALSE
#>  2     2      2    20 FALSE      10    10 <dbl [10]>      2 TRUE  FALSE FALSE
#>  3     3      3    30 FALSE      10    10 <dbl [10]>      3 TRUE  FALSE FALSE
#>  4     4      4    40 FALSE      10    10 <dbl [10]>      4 TRUE  FALSE FALSE
#>  5     5      5    50 FALSE      10    10 <dbl [10]>      5 TRUE  FALSE FALSE
#>  6     6      5    50 FALSE      10    10 <dbl [10]>      5 FALSE TRUE  FALSE
#>  7     7      5    50 FALSE      10    10 <dbl [10]>      5 TRUE  FALSE FALSE
#>  8     8      6    60 FALSE      10    10 <dbl [10]>      6 TRUE  FALSE FALSE
#>  9     9      6    60 FALSE      10    10 <dbl [10]>      6 FALSE TRUE  FALSE
#> 10    10      6    60 FALSE      10    10 <dbl [10]>      6 FALSE FALSE TRUE 
.DefaultDataGrouped() %>% tidy()
#> # A tibble: 3 × 10
#>      ID Cohort  Dose XLevel Tox   Placebo  NObs NGrid DoseGrid   Group
#>   <int>  <int> <dbl>  <int> <lgl> <lgl>   <int> <int> <list>     <fct>
#> 1     1      1     1      1 FALSE FALSE       3    11 <dbl [11]> mono 
#> 2     2      2     3      2 FALSE FALSE       3    11 <dbl [11]> mono 
#> 3     3      3     5      3 FALSE FALSE       3    11 <dbl [11]> combo
.DefaultDataDA() %>% tidy()
#> # A tibble: 8 × 12
#>      ID Cohort  Dose XLevel Tox   Placebo  NObs NGrid DoseGrid     U    T0  TMax
#>   <int>  <int> <dbl>  <int> <lgl> <lgl>   <int> <int> <list>   <dbl> <dbl> <dbl>
#> 1     1      1   0.1      1 FALSE FALSE       8    41 <dbl>       42     0    60
#> 2     2      2   0.5      2 FALSE FALSE       8    41 <dbl>       30    15    60
#> 3     3      3   1.5      3 TRUE  FALSE       8    41 <dbl>       15    30    60
#> 4     4      4   3        4 TRUE  FALSE       8    41 <dbl>        5    40    60
#> 5     5      5   6        5 FALSE FALSE       8    41 <dbl>       20    55    60
#> 6     6      6  10        6 FALSE FALSE       8    41 <dbl>       25    70    60
#> 7     7      6  10        6 TRUE  FALSE       8    41 <dbl>       30    75    60
#> 8     8      6  10        6 FALSE FALSE       8    41 <dbl>       60    85    60
.DefaultSimulations() %>% tidy()
#> $fit
#> $fit[[1]]
#>        middle        lower     upper
#> 1  0.01795659 0.0001356646 0.1095379
#> 2  0.04295474 0.0016321911 0.1754396
#> 3  0.06726322 0.0056169332 0.2226731
#> 4  0.12842478 0.0243503549 0.3058274
#> 5  0.18920026 0.0588209283 0.3636024
#> 6  0.24772849 0.1003384609 0.4208029
#> 7  0.30263888 0.1451465641 0.4929943
#> 8  0.44035461 0.2425361574 0.6499173
#> 9  0.51041108 0.2831712114 0.7257390
#> 10 0.64801756 0.3521703338 0.8682858
#> 11 0.70343588 0.3857129334 0.9146661
#> 
#> 
#> $stop_report
#> # A tibble: 1 × 1
#>   stop_report[,NA] [,NA] [,"≥ 3 cohorts dosed"] [,"P(0.2 ≤ prob(DLE | NBD) ≤ 0…¹
#>   <lgl>            <lgl> <lgl>                  <lgl>                           
#> 1 TRUE             TRUE  TRUE                   TRUE                            
#> # ℹ abbreviated name: ¹​[,"P(0.2 ≤ prob(DLE | NBD) ≤ 0.35) ≥ 0.5"]
#> # ℹ 1 more variable: stop_report[5] <lgl>
#> 
#> $data
#> $data[[1]]
#> # A tibble: 22 × 9
#>       ID Cohort  Dose XLevel Tox   Placebo  NObs NGrid DoseGrid  
#>    <int>  <int> <dbl>  <int> <lgl> <lgl>   <int> <int> <list>    
#>  1     1      1     3      2 FALSE FALSE      22    11 <dbl [11]>
#>  2     2      2     5      3 FALSE FALSE      22    11 <dbl [11]>
#>  3     3      3    10      4 FALSE FALSE      22    11 <dbl [11]>
#>  4     4      4    20      6 TRUE  FALSE      22    11 <dbl [11]>
#>  5     5      5    20      6 FALSE FALSE      22    11 <dbl [11]>
#>  6     6      5    20      6 FALSE FALSE      22    11 <dbl [11]>
#>  7     7      5    20      6 FALSE FALSE      22    11 <dbl [11]>
#>  8     8      6    25      7 FALSE FALSE      22    11 <dbl [11]>
#>  9     9      6    25      7 TRUE  FALSE      22    11 <dbl [11]>
#> 10    10      6    25      7 FALSE FALSE      22    11 <dbl [11]>
#> # ℹ 12 more rows
#> 
#> 
#> $doses
#> # A tibble: 1 × 1
#>   doses
#>   <dbl>
#> 1    20
#> 
#> $seed
#> # A tibble: 1 × 1
#>    seed
#>   <int>
#> 1   819
#> 
#> attr(,"class")
#> [1] "tbl_Simulations" "list"           
.DefaultLogisticIndepBeta() %>% tidy()
#> $pseudoData
#> # A tibble: 2 × 3
#>    Dose     N   Tox
#>   <dbl> <int> <dbl>
#> 1    25     3  1.05
#> 2   300     3  1.8 
#> 
#> $data
#> # A tibble: 0 × 9
#> # ℹ 9 variables: ID <int>, Cohort <int>, Dose <dbl>, XLevel <int>, Tox <lgl>,
#> #   Placebo <lgl>, NObs <int>, NGrid <int>, DoseGrid <list>
#> 
#> $params
#> # A tibble: 2 × 3
#>   Param   mean cov          
#>   <chr>  <dbl> <named list> 
#> 1 Phi1  -1.95  <dbl [2 × 2]>
#> 2 Phi2   0.412 <dbl [2 × 2]>
#> 
#> attr(,"class")
#> [1] "tbl_LogisticIndepBeta" "list"                 
.DefaultEffloglog() %>% tidy()
#> $pseudoData
#> # A tibble: 2 × 2
#>    Dose Response
#>   <dbl>    <dbl>
#> 1    25     1.22
#> 2   300     2.51
#> 
#> $data
#> # A tibble: 8 × 10
#>      ID Cohort  Dose XLevel Tox   Placebo  NObs NGrid DoseGrid       W
#>   <int>  <int> <dbl>  <int> <lgl> <lgl>   <int> <int> <list>     <dbl>
#> 1     1      1    25      1 FALSE FALSE       8    12 <dbl [12]>  0.31
#> 2     2      2    50      2 FALSE FALSE       8    12 <dbl [12]>  0.42
#> 3     3      2    50      2 FALSE FALSE       8    12 <dbl [12]>  0.59
#> 4     4      3    75      3 FALSE FALSE       8    12 <dbl [12]>  0.45
#> 5     5      4   100      4 TRUE  FALSE       8    12 <dbl [12]>  0.6 
#> 6     6      4   100      4 TRUE  FALSE       8    12 <dbl [12]>  0.7 
#> 7     7      5   225      9 TRUE  FALSE       8    12 <dbl [12]>  0.6 
#> 8     8      6   300     12 TRUE  FALSE       8    12 <dbl [12]>  0.52
#> 
#> $params
#> # A tibble: 2 × 3
#>   Param   mean cov          
#>   <chr>  <dbl> <named list> 
#> 1 theta1 -2.82 <dbl [2 × 2]>
#> 2 theta2  2.71 <dbl [2 × 2]>
#> 
#> attr(,"class")
#> [1] "tbl_Effloglog" "list"         
IncrementsMaxToxProb(prob = c("DLAE" = 0.2, "CRS" = 0.05)) %>% tidy()
#> # A tibble: 2 × 2
#>   Grade  Prob
#>   <chr> <dbl>
#> 1 DLAE   0.2 
#> 2 CRS    0.05
CohortSizeRange(intervals = c(0, 20), cohort_size = c(1, 3)) %>% tidy()
#> # A tibble: 2 × 3
#>     min   max cohort_size
#>   <dbl> <dbl>       <int>
#> 1     0    20           1
#> 2    20   Inf           3
.DefaultCohortSizeDLT() %>% tidy()
#> # A tibble: 2 × 3
#>     min   max cohort_size
#>   <dbl> <dbl>       <int>
#> 1     0     1           1
#> 2     1   Inf           3
.DefaultCohortSizeMin() %>% tidy()
#> [[1]]
#> # A tibble: 2 × 3
#>     min   max cohort_size
#>   <dbl> <dbl>       <int>
#> 1     0    10           1
#> 2    10   Inf           3
#> 
#> [[2]]
#> # A tibble: 2 × 3
#>     min   max cohort_size
#>   <dbl> <dbl>       <int>
#> 1     0     1           1
#> 2     1   Inf           3
#> 
#> attr(,"class")
#> [1] "tbl_CohortSizeMin" "tbl_CohortSizeMin" "list"             
.DefaultCohortSizeMax() %>% tidy()
#> [[1]]
#> # A tibble: 2 × 3
#>     min   max cohort_size
#>   <dbl> <dbl>       <int>
#> 1     0    10           1
#> 2    10   Inf           3
#> 
#> [[2]]
#> # A tibble: 2 × 3
#>     min   max cohort_size
#>   <dbl> <dbl>       <int>
#> 1     0     1           1
#> 2     1   Inf           3
#> 
#> attr(,"class")
#> [1] "tbl_CohortSizeMax" "tbl_CohortSizeMax" "list"             
.DefaultCohortSizeRange() %>% tidy()
#> # A tibble: 2 × 3
#>     min   max cohort_size
#>   <dbl> <dbl>       <int>
#> 1     0    30           1
#> 2    30   Inf           3
CohortSizeParts(cohort_sizes = c(1, 3)) %>% tidy()
#> # A tibble: 2 × 2
#>    part cohort_size
#>   <int>       <int>
#> 1     1           1
#> 2     2           3
.DefaultIncrementsMin() %>% tidy()
#> [[1]]
#> # A tibble: 3 × 3
#>     min   max increment
#>   <dbl> <dbl>     <dbl>
#> 1     0     1      1   
#> 2     1     3      0.33
#> 3     3   Inf      0.2 
#> 
#> [[2]]
#> # A tibble: 2 × 3
#>     min   max increment
#>   <dbl> <dbl>     <dbl>
#> 1     0    20      1   
#> 2    20   Inf      0.33
#> 
#> attr(,"class")
#> [1] "tbl_IncrementsMin" "tbl_IncrementsMin" "list"             
CohortSizeRange(intervals = c(0, 20), cohort_size = c(1, 3)) %>% tidy()
#> # A tibble: 2 × 3
#>     min   max cohort_size
#>   <dbl> <dbl>       <int>
#> 1     0    20           1
#> 2    20   Inf           3
x <- .DefaultIncrementsRelativeDLT()
x %>% tidy()
#> # A tibble: 3 × 3
#>     min   max increment
#>   <dbl> <dbl>     <dbl>
#> 1     0     1      1   
#> 2     1     3      0.33
#> 3     3   Inf      0.2 
.DefaultIncrementsRelativeParts() %>% tidy()
#> $dlt_start
#> # A tibble: 1 × 1
#>   dlt_start
#>       <int>
#> 1         0
#> 
#> $clean_start
#> # A tibble: 1 × 1
#>   clean_start
#>         <int>
#> 1           1
#> 
#> $intervals
#> # A tibble: 2 × 1
#>   intervals
#>       <dbl>
#> 1         0
#> 2         2
#> 
#> $increments
#> # A tibble: 2 × 1
#>   increments
#>        <dbl>
#> 1          2
#> 2          1
#> 
#> attr(,"class")
#> [1] "tbl_IncrementsRelativeParts" "list"                       
NextBestNCRM(
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
) %>%
  tidy()
#> # A tibble: 3 × 4
#>   Range       min   max max_prob
#>   <chr>     <dbl> <dbl>    <dbl>
#> 1 Underdose  0     0.2     NA   
#> 2 Target     0.2   0.35    NA   
#> 3 Overdose   0.35  1        0.25
.DefaultNextBestNCRMLoss() %>% tidy()
#> # A tibble: 4 × 5
#>   Range        Lower Upper LossCoefficient MaxOverdoseProb
#>   <chr>        <dbl> <dbl>           <dbl>           <dbl>
#> 1 Underdose     0     0.2                1            0.25
#> 2 Target        0.2   0.35               0            0.25
#> 3 Overdose      0.35  0.6                1            0.25
#> 4 Unacceptable  0.6   1                  2            0.25
.DefaultDualDesign() %>% tidy()
#> $model
#> $sigma2betaW
#> # A tibble: 1 × 1
#>   sigma2betaW
#>         <dbl>
#> 1        0.01
#> 
#> $rw1
#> # A tibble: 1 × 1
#>   rw1  
#>   <lgl>
#> 1 TRUE 
#> 
#> $betaZ_params
#> # A tibble: 2 × 3
#>    mean cov[,1]  [,2] prec[,1]  [,2]
#>   <dbl>   <dbl> <dbl>    <dbl> <dbl>
#> 1     0       1     0        1     0
#> 2     1       0     1        0     1
#> 
#> $ref_dose
#> # A tibble: 1 × 1
#>   ref_dose  
#>   <pstv_nmb>
#> 1 1         
#> 
#> $use_log_dose
#> # A tibble: 1 × 1
#>   use_log_dose
#>   <lgl>       
#> 1 FALSE       
#> 
#> $sigma2W
#> # A tibble: 2 × 1
#>   sigma2W
#>     <dbl>
#> 1     0.1
#> 2     0.1
#> 
#> $rho
#> # A tibble: 2 × 1
#>     rho
#>   <dbl>
#> 1     1
#> 2     1
#> 
#> $use_fixed
#> # A tibble: 3 × 1
#>   use_fixed
#>   <lgl>    
#> 1 FALSE    
#> 2 FALSE    
#> 3 TRUE     
#> 
#> $datanames
#> # A tibble: 5 × 1
#>   datanames
#>   <chr>    
#> 1 nObs     
#> 2 w        
#> 3 x        
#> 4 xLevel   
#> 5 y        
#> 
#> $datanames_prior
#> # A tibble: 2 × 1
#>   datanames_prior
#>   <chr>          
#> 1 nGrid          
#> 2 doseGrid       
#> 
#> $sample
#> # A tibble: 5 × 1
#>   sample
#>   <chr> 
#> 1 betaZ 
#> 2 precW 
#> 3 rho   
#> 4 betaW 
#> 5 delta 
#> 
#> attr(,"class")
#> [1] "tbl_DualEndpointRW" "list"              
#> 
#> $data
#> # A tibble: 0 × 10
#> # ℹ 10 variables: ID <int>, Cohort <int>, Dose <dbl>, XLevel <int>, Tox <lgl>,
#> #   Placebo <lgl>, NObs <int>, NGrid <int>, DoseGrid <list>, W <dbl>
#> 
#> $stopping
#> $stop_list
#> $stop_list[[1]]
#> $target
#> # A tibble: 2 × 1
#>   target
#>    <dbl>
#> 1    0.9
#> 2    1  
#> 
#> $is_relative
#> # A tibble: 1 × 1
#>   is_relative
#>   <lgl>      
#> 1 TRUE       
#> 
#> $prob
#> # A tibble: 1 × 1
#>    prob
#>   <dbl>
#> 1   0.5
#> 
#> $report_label
#> # A tibble: 1 × 1
#>   report_label                           
#>   <chr>                                  
#> 1 P(0.9 ≤ Biomarker ≤ 1) ≥ 0.5 (relative)
#> 
#> attr(,"class")
#> [1] "tbl_StoppingTargetBiomarker" "list"                       
#> 
#> $stop_list[[2]]
#> # A tibble: 1 × 2
#>   nPatients report_label       
#>       <int> <chr>              
#> 1        40 ≥ 40 patients dosed
#> 
#> 
#> $report_label
#> # A tibble: 1 × 1
#>   report_label
#>   <chr>       
#> 1 NA          
#> 
#> attr(,"class")
#> [1] "tbl_StoppingAny" "list"           
#> 
#> $increments
#> # A tibble: 2 × 3
#>     min   max increment
#>   <dbl> <dbl>     <dbl>
#> 1     0    20      1   
#> 2    20   Inf      0.33
#> 
#> $pl_cohort_size
#> # A tibble: 1 × 1
#>    size
#>   <int>
#> 1     0
#> 
#> $nextBest
#> $target
#> # A tibble: 2 × 1
#>   target
#>    <dbl>
#> 1    0.9
#> 2    1  
#> 
#> $overdose
#> # A tibble: 2 × 1
#>   overdose
#>      <dbl>
#> 1     0.35
#> 2     1   
#> 
#> $max_overdose_prob
#> # A tibble: 1 × 1
#>   max_overdose_prob
#>               <dbl>
#> 1              0.25
#> 
#> $target_relative
#> # A tibble: 1 × 1
#>   target_relative
#>   <lgl>          
#> 1 TRUE           
#> 
#> $target_thresh
#> # A tibble: 1 × 1
#>   target_thresh
#>           <dbl>
#> 1          0.01
#> 
#> attr(,"class")
#> [1] "tbl_NextBestDualEndpoint" "list"                    
#> 
#> $cohort_size
#> [[1]]
#> # A tibble: 2 × 3
#>     min   max cohort_size
#>   <dbl> <dbl>       <int>
#> 1     0    30           1
#> 2    30   Inf           3
#> 
#> [[2]]
#> # A tibble: 2 × 3
#>     min   max cohort_size
#>   <dbl> <dbl>       <int>
#> 1     0     1           1
#> 2     1   Inf           3
#> 
#> attr(,"class")
#> [1] "tbl_CohortSizeMax" "tbl_CohortSizeMax" "list"             
#> 
#> $startingDose
#> # A tibble: 1 × 1
#>   startingDose
#>          <dbl>
#> 1            3
#> 
#> attr(,"class")
#> [1] "tbl_DualDesign" "list"          
options <- McmcOptions(
  burnin = 100,
  step = 1,
  samples = 2000
)

emptydata <- Data(doseGrid = c(1, 3, 5, 10, 15, 20, 25, 40, 50, 80, 100))

model <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 56
)

samples <- mcmc(emptydata, model, options)
samples %>% tidy()
#> $data
#> # A tibble: 2,000 × 10
#>    Iteration Chain alpha0 alpha1 nChains nParameters nIterations nBurnin nThin
#>        <int> <int>  <dbl>  <dbl>   <int>       <int>       <int>   <int> <int>
#>  1         1     1 -1.12   2.91        1           1        2100     100     1
#>  2         2     1 -1.19   8.37        1           1        2100     100     1
#>  3         3     1 -0.167  8.82        1           1        2100     100     1
#>  4         4     1 -0.922  0.886       1           1        2100     100     1
#>  5         5     1 -1.46   4.82        1           1        2100     100     1
#>  6         6     1  1.19   1.98        1           1        2100     100     1
#>  7         7     1 -0.914  6.60        1           1        2100     100     1
#>  8         8     1 -2.15   3.22        1           1        2100     100     1
#>  9         9     1  0.229  0.472       1           1        2100     100     1
#> 10        10     1 -0.752  1.19        1           1        2100     100     1
#> # ℹ 1,990 more rows
#> # ℹ 1 more variable: parallel <lgl>
#> 
#> $options
#> # A tibble: 1 × 5
#>   iterations burnin  step rng_kind rng_seed
#>        <int>  <int> <int> <chr>       <int>
#> 1       2100    100     1 NA             NA
#> 
#> attr(,"class")
#> [1] "tbl_Samples" "list"       
```
