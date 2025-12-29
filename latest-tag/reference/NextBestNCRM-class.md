# `NextBestNCRM`

**\[stable\]**

`NextBestNCRM` is the class for next best dose that finds the next dose
with high posterior probability to be in the target toxicity interval.

## Usage

``` r
NextBestNCRM(target, overdose, max_overdose_prob)

.DefaultNextBestNCRM()
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

## Details

To avoid numerical problems, the dose selection algorithm has been
implemented as follows: First admissible doses are found, which are
those with probability to fall in `overdose` category being below
`max_overdose_prob`. Next, within the admissible doses, the maximum
probability to fall in the `target` category is calculated. If that is
above 5% (i.e. it is not just numerical error), then the corresponding
dose is the next recommended dose. Otherwise, the highest admissible
dose is the next recommended dose.

## Slots

- `target`:

  (`numeric`)  
  the target toxicity interval (limits included).

- `overdose`:

  (`numeric`)  
  the overdose toxicity interval (lower limit excluded, upper limit
  included). It is used to filter probability samples.

- `max_overdose_prob`:

  (`proportion`)  
  maximum overdose posterior probability that is allowed, except 0 or 1.

## Note

Typically, end users will not use the `.DefaultNextBestNCRM()` function.

## Examples

``` r
# In the example below, the target toxicity interval [0.2, 0.35] while the
# overdose interval is (0.35,1]. Finally we would like to constrain the
# probability of overdosing below 25%.
my_next_best <- NextBestNCRM(
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)
```
