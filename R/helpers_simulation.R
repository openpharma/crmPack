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
