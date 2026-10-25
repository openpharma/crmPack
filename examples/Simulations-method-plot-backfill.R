## Illustrative trials with backfill patients; no new simulation is needed.
backfill_sims <- GeneralSimulations(
  data = list(
    Data(
      x = c(1, 1, 2, 2, 1, 1, 4, 4, 2, 2),
      y = rep(0, 10), ID = 1:10, cohort = rep(1:5, each = 2),
      backfilled = rep(c(FALSE, FALSE, TRUE, FALSE, TRUE), each = 2),
      doseGrid = c(1, 2, 4)
    ),
    Data(
      x = c(1, 1, 2, 2, 1, 1),
      y = rep(0, 6), ID = 1:6, cohort = rep(1:3, each = 2),
      backfilled = rep(c(FALSE, FALSE, TRUE), each = 2),
      doseGrid = c(1, 2, 4)
    ),
    Data(
      x = c(1, 1, 2, 2),
      y = rep(0, 4), ID = 1:4, cohort = rep(1:2, each = 2),
      doseGrid = c(1, 2, 4)
    )
  ),
  doses = c(4, 2, 2), seed = 299L
)

## Pool all patients (the default), or exclude backfill patients.
print(plot(backfill_sims, include_backfill = "pooled"))
print(plot(backfill_sims, include_backfill = "no"))

## Preserve original IDs: backfill starts at ID 5, rather than patient 1.
## Show every observed position, instead of only cohort endpoints.
print(plot(backfill_sims, include_backfill = "separate", patient_scale = 1))

## Alternatively, count patients from 1 within each group. Backfill curves
## are dashed so overlapping blue and orange curves remain distinguishable.
print(plot(backfill_sims, include_backfill = "separate-number", patient_scale = 1))

## Separate survival curves condition on enrollment in each group. Here the
## backfill denominator is two trials, because the third has no backfill.
## With original IDs, the orange curve starts at ID 5 and 100%, dropping to
## 50% at ID 7. With group numbering, it starts at 1 and drops to 50% at 3.
print(plot(backfill_sims, type = "trialsurvival", include_backfill = "separate"))
