# `OpeningMinResponses`

**\[experimental\]**

`OpeningMinResponses` opens backfill cohorts when a minimum number of
responses has been observed in the trial. The responses can be counted
at the cohort's dose level only, or also at lower dose levels if
`include_lower_doses` is set to `TRUE`.

## Usage

``` r
OpeningMinResponses(min_responses = 1L, include_lower_doses = FALSE)

.DefaultOpeningMinResponses()
```

## Arguments

- min_responses:

  (`count`)  
  see slot definition.

- include_lower_doses:

  (`logical`)  
  see slot definition.

## Slots

- `min_responses`:

  (`count`)  
  the minimum number of responses required before backfill cohorts can
  be opened (at least 1).

- `include_lower_doses`:

  (`logical`)  
  if `TRUE`, responses at all doses less than or equal to the cohort's
  dose are counted. If `FALSE`, only responses at the cohort's dose are
  counted.

## Note

Typically, end users will not use the `.DefaultOpeningMinResponses()`
function.

## See also

[`Opening`](https://openpharma.github.io/crmPack/reference/Opening-class.md)
and the other subclasses listed in there.

## Examples

``` r
# Create an OpeningMinResponses object that requires 2 responses
opening <- OpeningMinResponses(min_responses = 2, include_lower_doses = FALSE)

# Display the object
print(opening)
#> An object of class "OpeningMinResponses"
#> Slot "min_responses":
#> [1] 2
#> 
#> Slot "include_lower_doses":
#> [1] FALSE
#> 

# Create a variant that includes lower doses
opening_inclusive <- OpeningMinResponses(
  min_responses = 2,
  include_lower_doses = TRUE
)
print(opening_inclusive)
#> An object of class "OpeningMinResponses"
#> Slot "min_responses":
#> [1] 2
#> 
#> Slot "include_lower_doses":
#> [1] TRUE
#> 
```
