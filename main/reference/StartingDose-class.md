# `StartingDose`

**\[experimental\]**

`StartingDose` is a simple wrapper class for the `startingDose` slot of
all design classes. It is used internally by `knit_print` methods

## Usage

``` r
StartingDose(starting_dose)

.DefaultStartingDose()
```

## Arguments

- starting_dose:

  (`positive_number`)  
  see slot definition.

## Slots

- `starting_dose`:

  (`numeric`)  
  the starting dose

## Note

Typically, end users will not use the `.DefaultStartingDose()` function.
