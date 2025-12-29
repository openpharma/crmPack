# `NextBestDualEndpoint`

**\[experimental\]**

`NextBestDualEndpoint` is the class for next best dose that is based on
the dual endpoint model.

## Usage

``` r
NextBestDualEndpoint(
  target,
  overdose,
  max_overdose_prob,
  target_relative = TRUE,
  target_thresh = 0.01
)

.DefaultNextBestDualEndpoint()
```

## Arguments

- target:

  (`numeric`)  
  see slot definition.

- overdose:

  (`numeric`)  
  see slot definition.

- max_overdose_prob:

  (`proportion`)  
  see slot definition.

- target_relative:

  (`flag`)  
  see slot definition.

- target_thresh:

  (`proportion`)  
  see slot definition.

## Details

Under this rule, at first admissible doses are found, which are those
with toxicity probability to fall in `overdose` category and being below
`max_overdose_prob`. Next, it picks (from the remaining admissible
doses) the one that maximizes the probability to be in the `target`
biomarker range. By default (`target_relative = TRUE`) the target is
specified as relative to the maximum biomarker level across the dose
grid or relative to the `Emax` parameter in case a parametric model was
selected (i.e.
[`DualEndpointBeta`](https://openpharma.github.io/crmPack/reference/DualEndpointBeta-class.md),
[`DualEndpointEmax`](https://openpharma.github.io/crmPack/reference/DualEndpointEmax-class.md)).
However, if `target_relative = FALSE`, then the absolute biomarker range
can be used as a target.

## Slots

- `target`:

  (`numeric`)  
  the biomarker target range that needs to be reached. For example, the
  target range \\(0.8, 1.0)\\ and `target_relative = TRUE` means that we
  target a dose with at least \\80\\\\ of maximum biomarker level. As an
  other example, \\(0.5, 0.8)\\ would mean that we target a dose between
  \\50\\\\ and \\80\\\\ of the maximum biomarker level.

- `overdose`:

  (`numeric`)  
  the overdose toxicity interval (lower limit excluded, upper limit
  included).

- `max_overdose_prob`:

  (`proportion`)  
  maximum overdose probability that is allowed.

- `target_relative`:

  (`flag`)  
  is `target` specified as relative? If `TRUE`, then the `target` is
  interpreted relative to the maximum, so it must be a probability
  range. Otherwise, the `target` is interpreted as absolute biomarker
  range.

- `target_thresh`:

  (`proportion`)  
  a target probability threshold that needs to be fulfilled before the
  target probability will be used for deriving the next best dose
  (default to 0.01).

## Note

Typically, end users will not use the `.DefaultNextBestDualEndpoint()`
function.

## Examples

``` r
# Target a dose achieving at least 0.9 of maximum biomarker level (efficacy)
# and with a probability below 0.25 that prob(DLT) > 0.35 (safety).
my_next_best <- NextBestDualEndpoint(
  target = c(0.9, 1),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)

# Now, using absolute target on the natural biomarker scale.
my_next_best_absolute <- NextBestDualEndpoint(
  target = c(200, 300),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25,
  target_relative = FALSE
)
```
