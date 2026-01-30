# Helper Function to Update Backfill Queue

This function manages the backfill cohort queue by determining which
previous cohorts are open or closed for backfill enrollment.

## Usage

``` r
h_update_backfill_queue(backfill_cohorts, data, dose, backfill)
```

## Arguments

- backfill_cohorts:

  (`list`)  
  current queue of backfill cohorts.

- data:

  (`Data`)  
  current trial data.

- dose:

  (`number`)  
  current dose being evaluated.

- backfill:

  (`Backfill`)  
  with opening rules and cohort size.

## Value

Updated `backfill_cohorts` list.
