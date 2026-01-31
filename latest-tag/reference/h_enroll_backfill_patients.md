# Helper Function to Enroll Backfill Patients

This function executes backfill enrollment according to the priority
rule, updating the data object, backfill cohort queue, and patient
counter.

## Usage

``` r
h_enroll_backfill_patients(
  backfill_cohorts,
  data,
  backfill,
  cohort_size,
  backfill_patients,
  current_args,
  truth,
  truthResponse
)
```

## Arguments

- backfill_cohorts:

  (`list`)  
  current queue of backfill cohorts.

- data:

  (`Data`)  
  current trial data.

- backfill:

  (`Backfill`)  
  with priority and recruitment rules.

- cohort_size:

  (`count`)  
  size of the active cohort.

- backfill_patients:

  (`count`)  
  number of enrolled backfill patients.

- current_args:

  (`data.frame`)  
  arguments for the truth function.

- truth:

  (`function`)  
  defining true DLT probability.

- truthResponse:

  (`function`)  
  defining true response probability.

## Value

List with updated `data`, `backfill_cohorts`, and `backfill_patients`.
