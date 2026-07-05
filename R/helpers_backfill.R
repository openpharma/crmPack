# Get the cohort index and dose of the previously added cohort.
h_previous_cohort <- function(data) {
  last_cohort_index <- length(data@cohort)
  last_cohort <- if (last_cohort_index > 0) {
    data@cohort[[last_cohort_index]]
  } else {
    NA_integer_
  }
  previous_cohort <- if (!is.na(last_cohort) && last_cohort > 1) {
    last_cohort - 1L
  } else {
    NA_integer_
  }
  previous_dose <- if (!is.na(previous_cohort)) {
    if (is.matrix(data@x)) {
      data@x[data@cohort == previous_cohort, , drop = FALSE][1, ]
    } else {
      data@x[data@cohort == previous_cohort][1]
    }
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
    if (is.matrix(data@x)) {
      data@x[data@cohort == cohort, , drop = FALSE][1, ]
    } else {
      data@x[data@cohort == cohort][1]
    }
  } else {
    NA_real_
  }
}
