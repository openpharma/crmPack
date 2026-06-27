# Helper for Evaluating the True Toxicity Probability at a Dose Combination

**\[experimental\]**

Evaluates the true toxicity probability at a given dose combination
using the provided `truth` function. The `truth` function can be
flexible in how it accepts the dose combination as input, and this
helper tries different ways to call it in order to extract the toxicity
probability.

## Usage

``` r
h_eval_combo_truth(truth, dose_pair, ...)
```

## Arguments

- truth:

  (`function`)\
  optional function mapping a dose combination to a toxicity
  probability. It can accept one argument (length-2 numeric vector or
  one-row matrix) or two numeric arguments (`drug1`, `drug2`).

- dose_pair:

  (`numeric`)\
  a length-2 numeric vector representing the dose combination for drug1
  and drug2.

- ...:

  additional arguments passed to `truth`.

## Value

A numeric value representing the true toxicity probability at the given
dose combination, or `NA_real_` if it cannot be evaluated.
