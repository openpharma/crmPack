# Combine a Stopping List and an Atomic Stopping Rule with OR

**\[stable\]**

The method combining a stopping list and an atomic stopping rule.

## Usage

``` r
# S4 method for class 'StoppingAny,Stopping'
e1 | e2
```

## Arguments

- e1:

  (`StoppingAny`)  
  stopping list object.

- e2:

  (`Stopping`)  
  stopping rule object.

## Value

The modified
[`StoppingAny`](https://openpharma.github.io/crmPack/reference/StoppingAny-class.md)
object.

## Examples

``` r
## Example of combining a list of stopping rules with an atomic stopping rule
## with an OR ('|') operator

myStopping1 <- StoppingMinCohorts(nCohorts = 3)
myStopping2 <- StoppingTargetProb(target = c(0.2, 0.35), prob = 0.5)

myStopping3 <- StoppingMinPatients(nPatients = 20)

myStopping <- (myStopping1 & myStopping2) | myStopping3
```
