# The method combining two atomic stopping rules

The method combining two atomic stopping rules

## Usage

``` r
# S4 method for class 'Stopping,Stopping'
e1 & e2
```

## Arguments

- e1:

  First
  [`Stopping`](https://openpharma.github.io/crmPack/reference/Stopping-class.md)
  object

- e2:

  Second
  [`Stopping`](https://openpharma.github.io/crmPack/reference/Stopping-class.md)
  object

## Value

The
[`StoppingAll`](https://openpharma.github.io/crmPack/reference/StoppingAll-class.md)
object

## Examples

``` r
## Example of combining two atomic stopping rules with an AND ('&') operator

myStopping1 <- StoppingMinCohorts(nCohorts = 3)
myStopping2 <- StoppingTargetProb(target = c(0.2, 0.35), prob = 0.5)

myStopping <- myStopping1 & myStopping2
```
