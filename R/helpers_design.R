h_add_dlts <- function(data,
                       dose,
                       truth,
                       cohort_size,
                       first_separate) {
  assert_class(data, "Data")
  assert_number(dose)
  assert_function(truth)
  assert_count(cohort_size, positive = TRUE)
  assert_flag(first_separate)

  prob <- truth(dose)
  size <- size(object@mono@cohort_size, dose = dose, data = data)
  dlts <- if (first_separate && size > 1) {
    first_dlts <- rbinom(n = 1, size = 1, prob = prob)
    if (first_dlts == 0) {
      c(first_dlts, rbinom(n = size - 1, size = 1, prob = prob))
    } else {
      first_dlts
    }
  } else {
    rbinom(n = size, size = 1, prob = prob)
  }
  update(object = data, x = dose, y = dlts)
}
