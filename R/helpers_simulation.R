## so this truth is...
h_this_truth <- function(dose, thisArgs, truth) {
  do.call(
    truth,
    ## First argument: the dose
    c(
      dose,
      ## Following arguments
      thisArgs
    )
  )
}

h_simulations_ouptput_format <- function(resultList) {
  ## put everything in the Simulations format:

  ## setup the list for the simulated data objects
  dataList <- lapply(resultList, "[[", "data")

  ## the vector of the final dose recommendations
  recommendedDoses <- as.numeric(sapply(resultList, "[[", "dose"))

  ## setup the list for the final fits
  fitList <- lapply(resultList, "[[", "fit")

  ## the reasons for stopping
  stopReasons <- lapply(resultList, "[[", "stop")

  # individual stopping rule results as matrix, labels as column names
  stopResults <- lapply(resultList, "[[", "report_results")
  stop_matrix <- as.matrix(do.call(rbind, stopResults))

  return(list(
    dataList = dataList,
    recommendedDoses = recommendedDoses,
    fitList = fitList,
    stopReasons = stopReasons,
    stopResults = stopResults,
    stop_matrix = stop_matrix
  ))
}

h_simulate_dlts(data,
               dose_grid = object@data@doseGrid[1])

h_simulate_dlts <- function(data,
                       dose,
                       prob,
                       prob_placebo,
                       cohort_size,
                       cohort_size_placebo,
                       dose_grid,
                       first_separate) {
  assert_class(data, "Data")
  assert_number(dose)
  assert_function(truth)
  assert_class(cohort_size, "CohortSize")
  assert_flag(first_separate)

  #prob <- truth(dose)
  #size <- size(cohort_size, dose = dose, data = data)
  dlts <- if (first_separate && cohort_size > 1) {
    first_dlts <- rbinom(n = 1, size = 1, prob = prob)
    if ((data@placebo) && cohort_size_placebo > 0) {
      first_dlts_placebo <- rbinom(n = 1, size = 1, prob = prob_placebo)
    }
    if (first_dlts == 0) {
      dlts <- c(first_dlts, rbinom(n = size - 1, size = 1, prob = prob))
      if ((data@placebo) && cohort_size_placebo > 0) {
        dlts_placebo <- c(first_dlts_placebo, rbimon(n = cohort_size_placebo, #cohort_size_placebo - 1?
                                     size = 1,
                                     prob = prob_placebo))
      }
    }
    # } else {
    #   first_dlts
    # }
  } else {
    dlts <- rbinom(n = size, size = 1, prob = prob)
    if ((data@placebo) && cohort_size_placebo > 0) {
      dlts_placebo <- rbinom(n = cohort_size, size = 1, prob = prob_placebo)
    }
  }
  #update(data, x = dose, y = dlts)
  if ((data@placebo) && cohort_size_placebo > 0) {
    this_data <- update(
      object = data,
      x = dose_grid,
      y = dlts_placebo,
      check = FALSE
    )

    ## update the data with active dose
    this_data <- update(
      object = data,
      x = dose,
      y = dlts,
      new_cohort = FALSE
    )
  } else {
    ## update the data with this cohort
    this_data <- update(
      object = data,
      x = dose,
      y = dlts
    )
  }
  return(this_data)
}
