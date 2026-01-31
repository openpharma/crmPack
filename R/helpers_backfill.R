# Get the cohort index and dose of the previously added cohort.
h_previous_cohort <- function(data) {
  last_cohort_index <- length(data@cohort)
  last_cohort <- data@cohort[[last_cohort_index]]
  previous_cohort <- last_cohort - 1L
  previous_dose <- if (previous_cohort > 0) {
    data@x[data@cohort == previous_cohort][1]
  } else {
    NA_real_
  }
  list(
    cohort = previous_cohort,
    dose = previous_dose
  )
}

# Get the dose assigned to a given cohort.
h_get_dose_for_cohort <- function(data, cohort) {
  if (cohort > 0 && cohort <= length(data@cohort)) {
    data@x[data@cohort == cohort][1]
  } else {
    NA_real_
  }
}
