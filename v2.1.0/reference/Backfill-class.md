# `Backfill` class

**\[experimental\]**

Class representing a backfilling rule in a clinical trial design.

## Usage

``` r
Backfill(
  cohort_size = CohortSizeConst(size = 3),
  opening = OpeningMinDose(),
  recruitment = RecruitmentUnlimited(),
  max_size = 1000000L,
  priority = c("highest", "lowest", "random")
)

.DefaultBackfill()
```

## Arguments

- cohort_size:

  (`CohortSize`)  
  see the slot definition.

- opening:

  (`Opening`)  
  see the slot definition.

- recruitment:

  (`Recruitment`)  
  see the slot definition.

- max_size:

  (`count`)  
  see the slot definition.

- priority:

  (`character`)  
  see the slot definition.

## Value

An object of class `Backfill`.

## Slots

- `cohort_size`:

  (`CohortSize`)  
  the size of cohorts to be backfilled.

- `opening`:

  (`Opening`)  
  the opening criteria for backfilling.

- `recruitment`:

  (`Recruitment`)  
  recruitment criteria for backfilling.

- `max_size`:

  (`count`)  
  the maximum number of patients to be backfilled.

- `priority`:

  (`character`)  
  the priority rule for backfilling, one of "highest", "lowest", or
  "random".

## Note

Typically, end users will not use the `.DefaultBackfill()` function.
