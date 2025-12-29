# Building the Plot for `nextBest-NextBestNCRMLoss` Method.

**\[experimental\]**

Helper function which creates the plot for
[`nextBest-NextBestNCRMLoss()`](https://openpharma.github.io/crmPack/reference/nextBest.md)
method.

## Usage

``` r
h_next_best_ncrm_loss_plot(
  prob_mat,
  posterior_loss,
  max_overdose_prob,
  dose_grid,
  max_eligible_dose_level,
  doselimit,
  next_dose,
  is_unacceptable_specified
)
```

## Arguments

- prob_mat:

  (`numeric`)  
  matrix with probabilities of a grid doses to be in a given interval.
  If `is_unacceptable_specified` is `TRUE`, there must be 4 intervals
  (columns) in `prob_mat`: `underdosing`, `target`, `excessive`,
  `unacceptable`. Otherwise, there must be 3 intervals (columns):
  `underdosing`, `target`, `overdose`. Number of rows must be equal to
  number of doses in a grid.

- posterior_loss:

  (`numeric`)  
  posterior losses.

- max_overdose_prob:

  (`number`)  
  maximum overdose posterior probability that is allowed.

- dose_grid:

  (`numeric`)  
  dose grid.

- max_eligible_dose_level:

  (`number`)  
  maximum eligible dose level in the `dose_grid`.

- doselimit:

  (`number`)  
  the maximum allowed next dose.

- next_dose:

  (`number`)  
  next best dose.

- is_unacceptable_specified:

  (`flag`)  
  is unacceptable interval specified?
