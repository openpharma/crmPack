# `StoppingAll`

**\[stable\]**

`StoppingAll` is the class for testing a stopping rule that consists of
many single stopping rules that are in turn the objects of class
`Stopping`. All single stopping rules must be satisfied in order the
result of this rule to be `TRUE`.

## Usage

``` r
StoppingAll(stop_list, report_label = NA_character_)

.DefaultStoppingAll()
```

## Arguments

- stop_list:

  (`list`)  
  see slot definition.

- report_label:

  (`string`)  
  see slot definition.

## Slots

- `stop_list`:

  (`list`)  
  list of stopping rules.

- `report_label`:

  label for reporting

## Note

Typically, end users will not use the `.DefaultStoppingAll()` function.

## Examples

``` r
# Define some stopping rules.
my_stopping1 <- StoppingMinCohorts(nCohorts = 3)
my_stopping2 <- StoppingTargetProb(target = c(0.2, 0.35), prob = 0.5)
my_stopping3 <- StoppingMinPatients(nPatients = 20)

# Create a list of stopping rules (of class `StoppingAll`) which would then be
# summarized by the `all` function, meaning that the study would be stopped only
# if all of the single stopping rules are `TRUE`.
my_stopping <- StoppingAll(
  stop_list = c(my_stopping1, my_stopping2, my_stopping3)
)
```
