# The method combining an atomic and a stopping list

The method combining an atomic and a stopping list

## Usage

``` r
# S4 method for class 'Stopping,StoppingAll'
e1 & e2
```

## Arguments

- e1:

  [`Stopping`](https://openpharma.github.io/crmPack/reference/Stopping-class.md)
  object

- e2:

  [`StoppingAll`](https://openpharma.github.io/crmPack/reference/StoppingAll-class.md)
  object

## Value

The modified
[`StoppingAll`](https://openpharma.github.io/crmPack/reference/StoppingAll-class.md)
object

## Examples

``` r
## Example of combining an atomic stopping rule with a list of stopping rules
## with an AND ('&') operator

myStopping1 <- StoppingMinCohorts(nCohorts = 3)
myStopping2 <- StoppingTargetProb(target = c(0.2, 0.35), prob = 0.5)

myStopping3 <- StoppingMinPatients(nPatients = 20)

myStopping <- myStopping3 & (myStopping1 | myStopping2)
```
