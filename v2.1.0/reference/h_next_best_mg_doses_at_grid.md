# Get Closest Grid Doses for a Given Target Doses for `nextBest-NextBestMaxGain` Method.

**\[experimental\]**

Helper function that for a given target doses finds the dose in grid
that is closest and below the target. There are four different targets
in the context of
[`nextBest-NextBestMaxGain()`](https://openpharma.github.io/crmPack/reference/nextBest.md)
method: \\min(\`dose_mg\`, \`dose_target_drt\`)\\, `dose_mg`,
`dose_target_drt` or `dose_target_eot`.

## Usage

``` r
h_next_best_mg_doses_at_grid(
  dose_target_drt,
  dose_target_eot,
  dose_mg,
  dose_grid,
  doselimit,
  placebo
)
```

## Arguments

- dose_target_drt:

  (`number`)  
  target dose estimate during the trial.

- dose_target_eot:

  (`number`)  
  target dose estimate at the end of the trial.

- dose_mg:

  (`number`)  
  the dose corresponding to the maximum gain.

- dose_grid:

  (`numeric`)  
  all possible doses.

- doselimit:

  (`number`)  
  the maximum allowed next dose.

- placebo:

  (`flag`)  
  if `TRUE` the first dose level in the `dose_grid` is considered as
  placebo.
