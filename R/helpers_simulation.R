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

h_determine_dlts <- function(data,
                       dose,
                       prob,
                       prob_placebo,
                       cohort_size,
                       cohort_size_placebo,
                       dose_grid,
                       first_separate) {
  assert_class(data, "Data")
  assert_number(dose)
  assert_number(prob)
  assert_number(cohort_size)
  assert_flag(first_separate)

  if (first_separate && cohort_size > 1) {
    dlts <- rbinom(n = 1, size = 1, prob = prob)
    if ((data@placebo) && cohort_size_placebo > 0) {
      dlts_placebo <- rbinom(n = 1, size = 1, prob = prob_placebo)
    }
    if (dlts == 0) {
      dlts <- c(dlts, rbinom(n = cohort_size, size = 1, prob = prob))
      if ((data@placebo) && cohort_size_placebo > 0) {
        dlts_placebo <- c(dlts_placebo, rbimon(n = cohort_size_placebo, #cohort_size_placebo - 1?
                                     size = 1,
                                     prob = prob_placebo))
      }
    }
  } else {
    dlts <- rbinom(n = cohort_size, size = 1, prob = prob)
    if ((data@placebo) && cohort_size_placebo > 0) {
      dlts_placebo <- rbinom(n = cohort_size_placebo, size = 1, prob = prob_placebo)
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
