# Combine Two Stopping Rules with AND

**\[stable\]**

The method combining two atomic stopping rules.

## Usage

``` r
# S4 method for class 'Stopping,Stopping'
e1 & e2
```

## Arguments

- e1:

  (`Stopping`)  
  first stopping rule object.

- e2:

  (`Stopping`)  
  second stopping rule object.

## Value

The
[`StoppingAll`](https://openpharma.github.io/crmPack/reference/StoppingAll-class.md)
object.

## Examples

``` r
## Example of combining two atomic stopping rules with an AND ('&') operator

myStopping1 <- StoppingMinCohorts(nCohorts = 3)
myStopping2 <- StoppingTargetProb(target = c(0.2, 0.35), prob = 0.5)

myStopping <- myStopping1 & myStopping2
```
