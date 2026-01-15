#' Helper Function to Set and Save the RNG Seed
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This code is basically copied from `stats:::simulate.lm`.
#'
#' @param seed an object specifying if and how the random number generator
#' should be initialized ("seeded"). Either `NULL` (default) or an
#' integer that will be used in a call to [set.seed()] before
#' simulating the response vectors. If set, the value is saved as the
#' `seed` slot of the returned object. The default, `NULL` will
#' not change the random generator state.
#' @return The integer vector containing the random number generate state will
#' be returned, in order to call this function with this input to reproduce
#' the obtained simulation results.
#'
#' @export
set_seed <- function(seed = NULL) {
  assert_number(seed, null.ok = TRUE)

  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    runif(1)
  }

  if (is.null(seed)) {
    get(".Random.seed", envir = .GlobalEnv)
  } else {
    seed <- as.integer(seed)
    r_seed <- get(".Random.seed", envir = .GlobalEnv)
    # Make sure r_seed exists in parent frame.
    assign(".r_seed", r_seed, envir = parent.frame())
    set.seed(seed)
    # Here we need the r_seed in the parent.frame!
    do.call(
      "on.exit",
      list(quote(assign(".Random.seed", .r_seed, envir = .GlobalEnv))),
      envir = parent.frame()
    )
    structure(seed, kind = as.list(RNGkind()))
  }
}

#' Helper Function to Obtain Simulation Results List
#'
#' The function `fun` can use variables that are visible to itself.
#' The names of these variables have to be given in the vector `vars`.
#'
#' @param fun (`function`)\cr the simulation function for a single iteration, which takes as
#' single parameter the iteration index.
#' @param nsim number of simulations to be conducted.
#' @param vars names of the variables.
#' @param parallel should the simulation runs be parallelized across the
#' clusters of the computer?
#' @param n_cores how many cores should be used for parallel computing?
#' @return The list with all simulation results (one iteration corresponds
#' to one list element).
#'
#' @importFrom parallel makeCluster
#' @importFrom parallelly availableCores
#' @keywords internal programming
get_result_list <- function(
  fun,
  nsim,
  vars,
  parallel,
  n_cores
) {
  assert_flag(parallel)
  assert_integerish(n_cores, lower = 1)

  if (!parallel) {
    lapply(
      X = seq_len(nsim),
      FUN = fun
    )
  } else {
    # Process all simulations.
    cores <- min(
      as.integer(n_cores),
      parallelly::availableCores()
    )

    # Start the cluster.
    cl <- parallel::makeCluster(cores)

    # Load the required R package.
    parallel::clusterEvalQ(cl, {
      library(crmPack)
      NULL
    })

    # Export local variables from the caller environment.
    # Note: parent.frame() is different from parent.env() which returns
    # the environment where this function has been defined!
    parallel::clusterExport(
      cl = cl,
      varlist = vars,
      envir = parent.frame()
    )

    # Export all global variables.
    parallel::clusterExport(
      cl = cl,
      varlist = ls(.GlobalEnv)
    )

    # Load user extensions from global options.
    crmpack_extensions <- getOption("crmpack_extensions")
    if (is.null(crmpack_extensions) != TRUE) {
      tryCatch(
        {
          parallel::clusterCall(cl, crmpack_extensions)
        },
        error = function(e) {
          stop("Failed to export crmpack_extensions: ", e$message)
        }
      )
    }

    # Do the computations in parallel.
    res <- parallel::parLapply(
      cl = cl,
      X = seq_len(nsim),
      fun = fun
    )

    # Stop the cluster.
    parallel::stopCluster(cl)

    res
  }
}


#' Helper Function to call truth calculation
#'
#' @param dose (`number`)\cr current dose.
#' @param truth (`function`)\cr defines the true probability for a DLT at a dose.
#' @param this_args (`data.frame`)\cr list of arguments for the truth.
#' @return The updated `this_truth`.
#'
#' @keywords internal
h_this_truth <- function(dose, this_args, truth) {
  do.call(
    truth,
    ## First argument: the dose
    c(
      dose,
      ## Following arguments
      this_args
    )
  )
}


#' Helper Function to create return list for Simulations output
#'
#' @param resultList (`list`)\cr raw iteration output.
#'
#' @return aggregated output for simulation object `list`.
#'
#' @keywords internal
h_simulations_output_format <- function(resultList) {
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

  # Result list of additional statistical summary.
  additional_stats <- lapply(resultList, "[[", "additional_stats")

  list(
    dataList = dataList,
    recommendedDoses = recommendedDoses,
    fitList = fitList,
    stopReasons = stopReasons,
    stopResults = stopResults,
    additional_stats = additional_stats,
    stop_matrix = stop_matrix
  )
}


#' Helper function to recursively unpack stopping rules and return lists with
#' logical value and label given
#'
#' @param stopit_tree object from simulate method
#' @return named list

h_unpack_stopit <- function(stopit_tree) {
  label <- attr(stopit_tree, "report_label")
  value <- stopit_tree[1]
  names(value) <- label
  value
  if (is.null(attr(stopit_tree, "individual"))) {
    value
  } else {
    unlist(c(
      value,
      lapply(attr(stopit_tree, "individual"), h_unpack_stopit)
    ))
  }
}


#' Helper function to determine the dlts including first separate and placebo
#' condition
#'
#' @param data (`Data`)\cr what data to start from.
#' @param dose (`number`)\cr current dose.
#' @param prob (`number`)\cr defines the true probability for a DLT at the dose.
#' @param prob_placebo (`number`)\cr defines the true probability for a DLT at a placebo condition.
#' @param prob_response (`number`)\cr defines the true probability for a response at the dose.
#' @param prob_response_placebo (`number`)\cr defines the true probability for a response at a placebo condition.
#' @param cohort_size (`number`)\cr the cohort size to use.
#' @param cohort_size_placebo (`number`)\cr the cohort size to use for placebo condition.
#' @param dose_grid (`numeric`)\cr the dose_grid as specified by the user.
#' @param first_separate (`flag`)\cr whether the first patient is enrolled separately.
#' @return updated data object
#' @keywords internal

h_determine_dlts <- function(
  data,
  dose,
  prob,
  prob_placebo = 0,
  prob_response = 0,
  prob_response_placebo = 0,
  cohort_size,
  cohort_size_placebo = 0,
  dose_grid,
  first_separate
) {
  assert_class(data, "Data")
  assert_number(dose)
  assert_number(prob, lower = 0, upper = 1)
  assert_number(prob_placebo, lower = 0, upper = 1, null.ok = TRUE)
  assert_number(prob_response, lower = 0, upper = 1)
  assert_number(prob_response_placebo, lower = 0, upper = 1, null.ok = TRUE)
  assert_number(cohort_size)
  assert_number(cohort_size_placebo, null.ok = TRUE)
  assert_flag(first_separate)

  if (first_separate && cohort_size > 1) {
    dlts <- rbinom(n = 1, size = 1, prob = prob)
    response <- rbinom(n = 1, size = 1, prob = prob_response)
    if ((data@placebo) && cohort_size_placebo > 0) {
      dlts_placebo <- rbinom(n = 1, size = 1, prob = prob_placebo)
      response_placebo <- rbinom(
        n = 1,
        size = 1,
        prob = prob_response_placebo
      )
    }
    if (dlts == 0) {
      dlts <- c(dlts, rbinom(n = cohort_size - 1L, size = 1, prob = prob))
      response <- c(
        response,
        rbinom(n = cohort_size - 1L, size = 1, prob = prob_response)
      )
      if ((data@placebo) && cohort_size_placebo > 0) {
        dlts_placebo <- c(
          dlts_placebo,
          rbinom(
            n = cohort_size_placebo - 1L,
            size = 1,
            prob = prob_placebo
          )
        )
        response_placebo <- c(
          response_placebo,
          rbinom(
            n = cohort_size_placebo - 1L,
            size = 1,
            prob = prob_response_placebo
          )
        )
      }
    }
  } else {
    dlts <- rbinom(n = cohort_size, size = 1, prob = prob)
    response <- rbinom(n = cohort_size, size = 1, prob = prob_response)
    if ((data@placebo) && cohort_size_placebo > 0) {
      dlts_placebo <- rbinom(
        n = cohort_size_placebo,
        size = 1,
        prob = prob_placebo
      )
      response_placebo <- rbinom(
        n = cohort_size_placebo,
        size = 1,
        prob = prob_response_placebo
      )
    }
  }

  if ((data@placebo) && cohort_size_placebo > 0) {
    data <- update(
      object = data,
      x = data@doseGrid[1],
      y = dlts_placebo,
      response = response_placebo,
      check = FALSE
    )

    ## update the data with active dose
    data <- update(
      object = data,
      x = dose,
      y = dlts,
      response = response,
      new_cohort = FALSE
    )
  } else {
    ## update the data with this cohort
    data <- update(
      object = data,
      x = dose,
      y = dlts,
      response = response
    )
  }
  data
}

#' Helper Function to Update Backfill Queue
#'
#' This function manages the backfill cohort queue by determining which
#' previous cohorts are open or closed for backfill enrollment.
#'
#' @param backfill_cohorts (`list`)\cr current queue of backfill cohorts.
#' @param data (`Data`)\cr current trial data.
#' @param dose (`number`)\cr current dose being evaluated.
#' @param backfill (`Backfill`)\cr with opening rules and cohort size.
#'
#' @return Updated `backfill_cohorts` list.
#'
#' @keywords internal
h_update_backfill_queue <- function(backfill_cohorts, data, dose, backfill) {
  # Loop over all previous cohorts and check which are open for backfill.
  for (cohort in seq_len(max(data@cohort) - 1)) {
    open_for_backfill <- openCohort(
      opening = backfill@opening,
      cohort = cohort,
      data = data,
      dose = dose
    )

    # Note: we index the cohorts in the queue by their cohort number
    # coerced as a string.
    cohort_string <- as.character(cohort)
    is_in_queue <- !is.null(backfill_cohorts[[cohort_string]])

    if (is_in_queue) {
      # Make sure to not reopen full backfill cohorts.
      is_full <- backfill_cohorts[[cohort_string]]$current_size ==
        backfill_cohorts[[cohort_string]]$max_size
      # Mark this cohort accordingly in the backfill queue.
      backfill_cohorts[[cohort_string]]$open <- !is_full &&
        open_for_backfill
    } else if (open_for_backfill) {
      # Add this new cohort to the backfill queue.
      backfill_dose <- h_get_dose_for_cohort(data, cohort)
      backfill_cohort_size <- size(
        object = backfill@cohort_size,
        dose = backfill_dose,
        cohort = cohort,
        data = data
      )
      backfill_cohorts[[as.character(cohort)]] <- list(
        dose = backfill_dose,
        cohort = cohort,
        current_size = 0L,
        max_size = backfill_cohort_size,
        open = TRUE # Mark as open.
      )
    }
  }

  backfill_cohorts
}

#' Helper Function to Enroll Backfill Patients
#'
#' This function executes backfill enrollment according to the priority rule,
#' updating the data object, backfill cohort queue, and patient counter.
#'
#' @param backfill_cohorts (`list`)\cr current queue of backfill cohorts.
#' @param data (`Data`)\cr current trial data.
#' @param backfill (`Backfill`)\cr with priority and recruitment rules.
#' @param cohort_size (`count`)\cr size of the active cohort.
#' @param backfill_patients (`count`)\cr number of enrolled backfill patients.
#' @param current_args (`data.frame`)\cr arguments for the truth function.
#' @param truth (`function`)\cr defining true DLT probability.
#' @param trueResponse (`function`)\cr defining true response probability.
#'
#' @return List with updated `data`, `backfill_cohorts`, and
#'   `backfill_patients`.
#'
#' @keywords internal
h_enroll_backfill_patients <- function(
  backfill_cohorts,
  data,
  backfill,
  cohort_size,
  backfill_patients,
  current_args,
  truth,
  trueResponse
) {
  # Number of backfill patients we can enroll in this cycle.
  max_recruits <- maxRecruits(
    backfill@recruitment,
    active_cohort_size = cohort_size
  )
  backfill_left <- backfill@total_size - backfill_patients
  max_recruits <- min(max_recruits, backfill_left)

  # Enroll backfill cohorts.
  if (
    length(backfill_cohorts) > 0 &&
      max_recruits > 0
  ) {
    backfill_doses <- sapply(
      backfill_cohorts,
      "[[",
      "dose"
    )
    # Order backfill cohorts according to priority rule.
    order_indices <- switch(
      backfill@priority,
      highest = order(-backfill_doses),
      lowest = order(backfill_doses),
      random = sample.int(
        n = length(backfill_cohorts),
        size = length(backfill_cohorts)
      )
    )
    for (i_bc in order_indices) {
      bc <- backfill_cohorts[[i_bc]]
      if (!bc$open) {
        next
      }
      enroll_size <- min(bc$max_size - bc$current_size, max_recruits)
      if (enroll_size == 0) {
        next
      }
      bc_dlts <- rbinom(
        n = enroll_size,
        size = 1,
        prob = h_this_truth(bc$dose, current_args, truth)
      )
      bc_response <- rbinom(
        n = enroll_size,
        size = 1,
        prob = trueResponse(bc$dose)
      )

      data <- update(
        object = data,
        x = bc$dose,
        y = bc_dlts,
        cohort = bc$cohort,
        response = bc_response,
        backfill = TRUE
      )

      # Record number of patients in the backfill cohort and overall.
      backfill_cohorts[[i_bc]]$current_size <-
        backfill_cohorts[[i_bc]]$current_size + enroll_size
      max_recruits <- max_recruits - enroll_size
      backfill_patients <- backfill_patients + enroll_size

      # Close this cohort if it's fully enrolled.
      if (
        backfill_cohorts[[i_bc]]$current_size ==
          backfill_cohorts[[i_bc]]$max_size
      ) {
        backfill_cohorts[[i_bc]]$open <- FALSE
      }

      # Stop enrolling backfill in this cycle if we reached the limit.
      if (max_recruits == 0) {
        break
      }
    }
  }

  list(
    data = data,
    backfill_cohorts = backfill_cohorts,
    backfill_patients = backfill_patients
  )
}
