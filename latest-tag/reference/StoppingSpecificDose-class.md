# `StoppingSpecificDose`

**\[experimental\]**

`StoppingSpecificDose` is the class for testing a stopping rule at
specific dose of the dose grid and not at the next best dose.

## Usage

``` r
StoppingSpecificDose(
  rule = StoppingTargetProb(target = c(0, 0.3), prob = 0.8),
  dose = 80,
  report_label = NA_character_
)

.DefaultStoppingSpecificDose()
```

## Arguments

- rule:

  (`Stopping`)  
  see slot definition.

- dose:

  (`number`)  
  see slot definition.

- report_label:

  (`string` or `NA`)  
  see slot definition.

## Slots

- `rule`:

  (`Stopping`)  
  a stopping rule available in this package.

- `dose`:

  (`positive_number`)  
  a dose that is defined as part of the dose grid of the data.

## Note

Typically, end users will not use the `.DefaultStoppingSpecificDose()`
function.

## Examples

``` r
# Stop if highest dose 80 is safe.
highest_dose_safe <- StoppingSpecificDose(
  rule = StoppingTargetProb(target = c(0, 0.3), prob = 0.8),
  dose = 80
)

# Stop if lowest dose 10 is toxic.
lowest_dose_toxic <- StoppingSpecificDose(
  rule = StoppingTargetProb(target = c(0.3, 1), prob = 0.8),
  dose = 10
)
```
