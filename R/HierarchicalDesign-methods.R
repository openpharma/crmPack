#' @include Design-methods.R
#' @include HierarchicalDesign-class.R
#' @include helpers.R
#' @include mcmc.R
NULL

## show-HierarchicalDesign ----

#' Show `HierarchicalDesign` Objects
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Display a brief representation of the [`HierarchicalDesign`] object.
#'
#' @param object (`HierarchicalDesign`)\cr the object we want to print.
#'
#' @return Invisibly returns the object itself.
#'
#' @aliases show-HierarchicalDesign
#' @export
setMethod(
  f = "show",
  signature = signature(object = "HierarchicalDesign"),
  def = function(object) {
    arm_names <- names(object@arms)
    active <- vapply(object@arms, function(arm) arm@active, logical(1L))
    pool_names <- names(object@model@parameter_pools)

    cat(
      "An object of class 'HierarchicalDesign'\n",
      "Arms (",
      length(arm_names),
      "): ",
      h_show_hierarchical_names(arm_names),
      "\n",
      sep = ""
    )
    cat(
      "Active arms: ",
      h_show_hierarchical_names(arm_names[active]),
      "\n",
      "Inactive arms: ",
      h_show_hierarchical_names(arm_names[!active]),
      "\n",
      sep = ""
    )
    cat(
      "Exchangeable parameter pools (",
      length(pool_names),
      "): ",
      h_show_hierarchical_names(pool_names),
      "\n",
      sep = ""
    )

    invisible(object)
  }
)


## HierarchicalDesign ----

#' Helper function to get the samples used for decision rules in a hierarchical design
#'
#' In a hierarchical design, the samples used for decision rules in each arm may either be the overall
#' samples from the hierarchical model (if borrowing is allowed)
#' or the arm-specific samples (if no borrowing).
#'
#' @param samples the overall samples from the hierarchical model.
#' @param arm_name the name of the arm for which we want to get the samples for decision rules.
#' @param arm the `DesignArm` object for this arm.
#' @param arm_data the data for this arm.
#' @param mcmcOptions the MCMC options to use if we need to fit an arm-specific model.
#' @return the samples to be used for decision rules for this arm.
#'
#' @keywords internal
h_hierarchical_get_decision_samples <- function(
  samples,
  arm_name,
  arm,
  arm_data,
  mcmcOptions
) {
  if (arm@borrow) {
    armSamples(samples, arm_name)
  } else {
    mcmc(
      data = arm_data,
      model = arm@design@model,
      options = mcmcOptions
    )
  }
}

#' Simulate outcomes from a hierarchical CRM design
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param object the [`HierarchicalDesign`] object we want to simulate data from.
#' @param nsim (`count`)\cr the number of simulations.
#' @param seed see [set_seed()].
#' @param truth (`function` or named `list` of `function`)\cr true DLT
#'   probability function(s). If a list is supplied, names must match the
#'   hierarchical arms.
#' @param truthResponse (`function` or named `list` of `function`)\cr true
#'   response probability function(s).
#' @param args (`data.frame`)\cr arguments for the truth functions.
#' @inheritParams simulate,Design-method
#'
#' @return an object of class [`HierarchicalSimulations`].
#'
#' @export
setMethod(
  f = "simulate",
  signature = signature(
    object = "HierarchicalDesign",
    nsim = "ANY",
    seed = "ANY"
  ),
  definition = function(
    object,
    nsim = 1L,
    seed = NULL,
    truth,
    truthResponse = plogis,
    args = NULL,
    firstSeparate = FALSE,
    mcmcOptions = McmcOptions(),
    parallel = FALSE,
    nCores = min(parallel::detectCores(), 5),
    derive = list(),
    ...
  ) {
    nsim <- as.integer(nsim)
    assert_count(nsim, positive = TRUE)
    assert_flag(firstSeparate)
    assert_flag(parallel)
    if (is.na(nCores)) {
      nCores <- 1L
    }
    assert_count(nCores, positive = TRUE)
    assert_list(derive)

    arm_names <- names(object@arms)
    active_arms <- arm_names[vapply(
      object@arms,
      function(arm) arm@active,
      logical(1L)
    )]
    assert_character(active_arms, min.len = 1L)

    if (is.function(truth)) {
      # If a single function is supplied, use it for all arms.
      truth <- stats::setNames(rep(list(truth), length(arm_names)), arm_names)
    } else {
      assert_list(truth, types = "function", any.missing = FALSE)
      assert_names(names(truth), must.include = arm_names)
      truth <- truth[arm_names]
    }

    if (is.function(truthResponse)) {
      # If a single function is supplied, use it for all arms.
      truthResponse <- stats::setNames(
        rep(list(truthResponse), length(arm_names)),
        arm_names
      )
    } else {
      assert_list(truthResponse, types = "function", any.missing = FALSE)
      assert_names(names(truthResponse), must.include = arm_names)
      truthResponse <- truthResponse[arm_names]
    }

    uses_backfill <- vapply(
      object@arms,
      function(arm) !is(arm@design@backfill@opening, "OpeningNone"),
      logical(1L)
    )

    args <- as.data.frame(args)
    n_args <- max(nrow(args), 1L)
    rng_state <- set_seed(seed)
    sim_seeds <- sample.int(n = 2147483647, size = as.integer(nsim))

    call_truth <- function(fun, dose, current_args) {
      do.call(fun, c(list(dose), as.list(current_args)))
    }

    run_sim <- function(iter_sim) {
      set.seed(sim_seeds[iter_sim])

      current_args <- args[(iter_sim - 1) %% n_args + 1, , drop = FALSE]
      data <- object@data

      # Initialize storage for simulation results.
      stopped <- stats::setNames(!arm_names %in% active_arms, arm_names)
      doses <- stats::setNames(vector("list", length(arm_names)), arm_names)
      fits <- stats::setNames(vector("list", length(arm_names)), arm_names)
      stop_reasons <- stats::setNames(
        vector("list", length(arm_names)),
        arm_names
      )
      stop_report <- stats::setNames(
        vector("list", length(arm_names)),
        arm_names
      )
      additional_stats <- stats::setNames(
        vector("list", length(arm_names)),
        arm_names
      )
      backfill_cohorts <- stats::setNames(
        vector("list", length(arm_names)),
        arm_names
      )
      backfill_patients <- stats::setNames(
        rep(0L, length(arm_names)),
        arm_names
      )

      stop_reasons[stopped] <- "Historical arm: not enrolling."
      samples <- NULL

      # As long as there are arms that have not yet stopped, keep enrolling and updating them.
      while (!all(stopped)) {
        # Get overall samples from the hierarchical model on the current data.
        samples <- mcmc(
          data = data,
          model = object@model,
          options = mcmcOptions
        )

        # Go through each enrolling arm and update it separately.
        for (arm_name in arm_names[!stopped]) {
          arm <- object@arms[[arm_name]]
          arm_design <- arm@design
          arm_data <- data@arms[[arm_name]]
          arm_samples <- h_hierarchical_get_decision_samples(
            samples = samples,
            arm_name = arm_name,
            arm = arm,
            arm_data = arm_data,
            mcmcOptions = mcmcOptions
          )

          dose_limit <- maxDose(arm_design@increments, data = arm_data)
          next_dose <- if (arm_data@nObs == 0L) {
            arm_design@startingDose
          } else {
            nextBest(
              arm_design@nextBest,
              doselimit = dose_limit,
              samples = arm_samples,
              model = arm_design@model,
              data = arm_data
            )$value
          }
          doses[[arm_name]] <- next_dose

          should_stop <- stopTrial(
            arm_design@stopping,
            dose = next_dose,
            samples = arm_samples,
            model = arm_design@model,
            data = arm_data
          )
          stop_report[[arm_name]] <- h_unpack_stopit(should_stop)
          stop_reason <- attr(should_stop, "message")

          if (anyNA(next_dose) && !isTRUE(should_stop)) {
            stop_reason <- paste(
              "Next dose is NA , i.e., no active dose is safe enough",
              "according to the NextBest rule."
            )
            should_stop <- TRUE
          }

          if (isTRUE(should_stop)) {
            stopped[[arm_name]] <- TRUE
            stop_reasons[[arm_name]] <- stop_reason
            fits[[arm_name]] <- fit(
              object = arm_samples,
              model = arm_design@model,
              data = arm_data
            )
            if (
              length(derive) > 0L &&
                !is(arm_design@model, "TwoDrugsCombo")
            ) {
              target_dose_samples <- dose(
                mean(arm_design@nextBest@target),
                model = arm_design@model,
                samples = arm_samples
              )
              additional_stats[[arm_name]] <- lapply(
                derive,
                function(f) f(target_dose_samples)
              )
            }
            next
          }

          prob <- call_truth(truth[[arm_name]], next_dose, current_args)
          prob_response <- call_truth(
            truthResponse[[arm_name]],
            next_dose,
            current_args
          )
          assert_number(prob, lower = 0, upper = 1)
          assert_number(prob_response, lower = 0, upper = 1)

          cohort_size <- size(
            arm_design@cohort_size,
            dose = next_dose,
            data = arm_data
          )

          if (is(arm_data, "Data") && arm_data@placebo) {
            placebo_dose <- arm_data@doseGrid[1L]
            prob_placebo <- call_truth(
              truth[[arm_name]],
              placebo_dose,
              current_args
            )
            prob_response_placebo <- call_truth(
              truthResponse[[arm_name]],
              placebo_dose,
              current_args
            )
            cohort_size_placebo <- size(
              arm_design@pl_cohort_size,
              dose = next_dose,
              data = arm_data
            )
          } else {
            prob_placebo <- NULL
            prob_response_placebo <- NULL
            cohort_size_placebo <- NULL
          }

          if (firstSeparate && cohort_size > 1L) {
            dlts <- rbinom(n = 1L, size = 1L, prob = prob)
            response <- rbinom(n = 1L, size = 1L, prob = prob_response)
            if (dlts == 0L) {
              dlts <- c(
                dlts,
                rbinom(n = cohort_size - 1L, size = 1L, prob = prob)
              )
              response <- c(
                response,
                rbinom(
                  n = cohort_size - 1L,
                  size = 1L,
                  prob = prob_response
                )
              )
            }
          } else {
            dlts <- rbinom(n = cohort_size, size = 1L, prob = prob)
            response <- rbinom(n = cohort_size, size = 1L, prob = prob_response)
          }

          if (
            is(arm_data, "Data") && arm_data@placebo && cohort_size_placebo > 0L
          ) {
            dlts_placebo <- rbinom(
              n = cohort_size_placebo,
              size = 1L,
              prob = prob_placebo
            )
            response_placebo <- rbinom(
              n = cohort_size_placebo,
              size = 1L,
              prob = prob_response_placebo
            )
            data <- update(
              object = data,
              arm = arm_name,
              x = placebo_dose,
              y = dlts_placebo,
              response = response_placebo,
              check = FALSE
            )
            data <- update(
              object = data,
              arm = arm_name,
              x = next_dose,
              y = dlts,
              response = response,
              new_cohort = FALSE
            )
          } else {
            data <- update(
              object = data,
              arm = arm_name,
              x = next_dose,
              y = dlts,
              response = response
            )
          }

          if (uses_backfill[[arm_name]]) {
            arm_data <- data@arms[[arm_name]]
            backfill_cohorts[[arm_name]] <- h_update_backfill_queue(
              backfill_cohorts = backfill_cohorts[[arm_name]],
              data = arm_data,
              dose = next_dose,
              backfill = arm_design@backfill
            )

            arm_truth <- truth[[arm_name]]
            arm_truth_response <- truthResponse[[arm_name]]
            enrollment_result <- h_enroll_backfill_patients(
              backfill_cohorts = backfill_cohorts[[arm_name]],
              data = arm_data,
              backfill = arm_design@backfill,
              cohort_size = cohort_size,
              backfill_patients = backfill_patients[[arm_name]],
              current_args = current_args,
              truth = function(dose, ...) {
                call_truth(arm_truth, dose, current_args)
              },
              truthResponse = function(dose) {
                call_truth(arm_truth_response, dose, current_args)
              }
            )

            data@arms[[arm_name]] <- enrollment_result$data
            validObject(data)
            backfill_cohorts[[arm_name]] <-
              enrollment_result$backfill_cohorts
            backfill_patients[[arm_name]] <-
              enrollment_result$backfill_patients
          }
        }
      }

      # Just to be sure for the case where all arms are stopped from the beginning ...
      if (is.null(samples)) {
        samples <- mcmc(
          data = data,
          model = object@model,
          options = mcmcOptions
        )
      }

      # Update arm specific fits.
      for (arm_name in arm_names) {
        if (is.null(fits[[arm_name]])) {
          arm <- object@arms[[arm_name]]
          arm_design <- arm@design
          arm_samples <- h_hierarchical_get_decision_samples(
            samples = samples,
            arm_name = arm_name,
            arm = arm,
            arm_data = data@arms[[arm_name]],
            mcmcOptions = mcmcOptions
          )
          fits[[arm_name]] <- fit(
            object = arm_samples,
            model = arm_design@model,
            data = data@arms[[arm_name]]
          )
        }
      }

      list(
        data = data,
        doses = doses,
        samples = samples,
        fit = fits,
        stop = stop_reasons,
        report_results = stop_report,
        additional_stats = additional_stats
      )
    }

    result_list <- get_result_list(
      fun = run_sim,
      nsim = nsim,
      vars = c(
        "sim_seeds",
        "args",
        "n_args",
        "firstSeparate",
        "truth",
        "truthResponse",
        "object",
        "mcmcOptions",
        "derive",
        "arm_names",
        "active_arms",
        "uses_backfill"
      ),
      parallel = parallel,
      n_cores = nCores
    )

    HierarchicalSimulations(
      data = lapply(result_list, "[[", "data"),
      doses = lapply(result_list, "[[", "doses"),
      samples = lapply(result_list, "[[", "samples"),
      fit = lapply(result_list, "[[", "fit"),
      stop_report = lapply(result_list, "[[", "report_results"),
      stop_reasons = lapply(result_list, "[[", "stop"),
      additional_stats = lapply(result_list, "[[", "additional_stats"),
      seed = rng_state
    )
  }
)
