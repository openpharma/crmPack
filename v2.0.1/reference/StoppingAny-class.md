# `StoppingAny`

**\[stable\]**

`StoppingAny` is the class for testing a stopping rule that consists of
many single stopping rules that are in turn the objects of class
`Stopping`. At least one single stopping rule must be satisfied in order
the result of this rule to be `TRUE`.

## Usage

``` r
StoppingAny(stop_list, report_label = NA_character_)

.DefaultStoppingAny()
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

Typically, end users will not use the `.DefaultStoppingAny()` function.

## Examples

``` r
# Define some stopping rules.
my_stopping1 <- StoppingMinCohorts(nCohorts = 3)
my_stopping2 <- StoppingTargetProb(target = c(0.2, 0.35), prob = 0.5)
my_stopping3 <- StoppingMinPatients(nPatients = 20)

# Create a list of stopping rules (of class `StoppingAny`) which would then be
# summarized by the `any` function, meaning that the study would be stopped if
# any of the single stopping rules is `TRUE`.
my_stopping <- StoppingAny(
  stop_list = c(my_stopping1, my_stopping2, my_stopping3)
)
```
