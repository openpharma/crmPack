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
    n_cores) {
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
    return(value)
  } else {
    return(unlist(c(value, lapply(attr(stopit_tree, "individual"), h_unpack_stopit))))
  }
}



#' Helper function to determine the dlts including first separate and placebo
#' condition
#'
#' @param data (`Data`)\cr what data to start from.
#' @param dose (`number`)\cr current dose.
#' @param prob (`function`)\cr defines the true probability for a DLT at a dose.
#' @param prob_placebo (`function`)\cr defines the true probability for a DLT at a placebo condition.
#' @param cohort_size (`number`)\cr the cohort size to use.
#' @param cohort_size_placebo (`number`)\cr the cohort size to use for placebo condition.
#' @param dose_grid (`numeric`)\cr the dose_grid as specified by the user.
#' @param first_separate (`flag`)\cr whether the first patient is enrolled separately.
#' @return updated data object
#' @keywords internal programming


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
      dlts <- c(dlts, rbinom(n = cohort_size - 1L, size = 1, prob = prob))
      if ((data@placebo) && cohort_size_placebo > 0) {
        dlts_placebo <- c(dlts_placebo, rbinom(
          n = cohort_size_placebo, # cohort_size_placebo - 1?
          size = 1,
          prob = prob_placebo
        ))
      }
    }
  } else {
    dlts <- rbinom(n = cohort_size, size = 1, prob = prob)
    if ((data@placebo) && cohort_size_placebo > 0) {
      dlts_placebo <- rbinom(n = cohort_size_placebo, size = 1, prob = prob_placebo)
    }
  }


  if ((data@placebo) && cohort_size_placebo > 0) {
    this_data <- update(
      object = data,
      x = dose_grid,
      y = dlts_placebo,
      check = FALSE
    )

    ## update the data with active dose
    this_data <- update(
      object = this_data,
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
