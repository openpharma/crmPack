# `NextBestEWOC`

**\[stable\]**

`NextBestEWOC` is the class implementing Escalation With Overdose
Control (EWOC). It recommends the highest possible dose subject to a
probabilistic constraint that the posterior probability of overdosing
does not exceed `max_overdose_prob`. Overdosing is defined as the
model-based toxicity probability lying inside the interval given by
`overdose`.

## Usage

``` r
NextBestEWOC(target, overdose, max_overdose_prob)

.DefaultNextBestEWOC()
```

## Arguments

- target:

  (`proportion`)  
  see slot definition.

- overdose:

  (`numeric`)  
  see slot definition.

- max_overdose_prob:

  (`proportion`)  
  see slot definition.

## Slots

- `target`:

  (`proportion`)  
  target toxicity probability to be achieved, below `overdose[1]`; only
  used for simulation reporting purposes.

- `overdose`:

  (`numeric`)  
  the (exclusive) lower and (inclusive) upper boundaries of the toxicity
  probability interval considered an overdose region. The prototype uses
  `c(0.35, 1)` meaning probabilities \> 0.35 are treated as overly
  toxic.

- `max_overdose_prob`:

  (`proportion`)  
  maximum acceptable posterior probability that the next recommended
  dose is in the overdose interval.

## Note

Typically, end users will not use the `.DefaultNextBestEWOC()` function.

## See also

[`NextBest`](https://openpharma.github.io/crmPack/reference/NextBest-class.md),
other next-best classes listed in its documentation.

## Examples

``` r
# Example: Define EWOC next best dose rule.
# Target toxicity probability is 0.30. Overdose region is any probability > 0.35.
# We restrict posterior probability of recommending an overdosing dose to <= 0.25.
next_best_ewoc <- NextBestEWOC(
  target = 0.30,
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)
```
