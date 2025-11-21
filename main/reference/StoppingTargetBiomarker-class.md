# `StoppingTargetBiomarker`

**\[stable\]**

`StoppingTargetBiomarker` is a class for stopping based on probability
of target biomarker.

## Usage

``` r
StoppingTargetBiomarker(
  target = c(0.9, 1),
  prob = 0.3,
  is_relative = TRUE,
  report_label = NA_character_
)

.DefaultStoppingTargetBiomarker()
```

## Arguments

- target:

  (`numeric`)  
  see slot definition.

- prob:

  (`proportion`)  
  see slot definition.

- is_relative:

  (`flag`)  
  see slot definition.

- report_label:

  (`string` or `NA`)  
  see slot definition.

## Slots

- `target`:

  (`numeric`)  
  the biomarker target range that needs to be reached. For example,
  `target = c(0.8, 1.0)` with `is_relative = TRUE` means that we target
  a dose with at least 80% of maximum biomarker level.

- `is_relative`:

  (`flag`)  
  is target relative? If it so (default), then the `target` is
  interpreted relative to the maximum, so it must be a probability
  range. Otherwise, the `target` is interpreted as absolute biomarker
  range.

- `prob`:

  (`proportion`)  
  required target probability (except 0 or 1) for reaching sufficient
  precision.

## Note

Typically, end users will not use the
`.DefaultStoppingTargetBiomarker()` function.

## Examples

``` r
# Stopping the study if there is at least 0.5 probability that the biomarker
# (efficacy) is within the biomarker target range of [0.9, 1.0] (relative to the
# maximum for the biomarker).

my_stopping <- StoppingTargetBiomarker(target = c(0.9, 1), prob = 0.5)
```
