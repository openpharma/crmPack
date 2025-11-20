#' @include Data-methods.R
#' @include Design-class.R
#' @include McmcOptions-class.R
#' @include Rules-methods.R
#' @include Simulations-class.R
#' @include helpers.R
#' @include mcmc.R
NULL

# simulate ----

## Design ----

#' Simulate outcomes from a CRM design
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param object the [`Design`] object we want to simulate data from
#' @param nsim (`count`)\cr the number of simulations (default: 1)
#' @param seed see [set_seed()]
#' @param truth (`function`)\cr a function which takes as input a dose (vector) and returns the
#'   true probability (vector) for toxicity. Additional arguments can be supplied
#'   in `args`.
#' @param args (`data.frame`)\cr data frame with arguments for the `truth` function. The
#'   column names correspond to the argument names, the rows to the values of the
#'   arguments. The rows are appropriately recycled in the `nsim`
#'   simulations. In order to produce outcomes from the posterior predictive
#'   distribution, e.g, pass an `object` that contains the data observed so
#'   far, `truth` contains the `prob` function from the model in
#'   `object`, and `args` contains posterior samples from the model.
#' @param firstSeparate (`flag`)\cr enroll the first patient separately from the rest of
#'   the cohort? (not default) If yes, the cohort will be closed if a DLT occurs
#'   in this patient.
#' @param mcmcOptions ([McmcOptions])\cr object of class [`McmcOptions`],
#'   giving the MCMC options for each evaluation in the trial. By default,
#'   the standard options are used
#' @param parallel (`flag`)\cr should the simulation runs be parallelized across the
#'   clusters of the computer? (not default)
#' @param nCores (`count`)\cr how many cores should be used for parallel computing?
#'   Defaults to the number of cores on the machine, maximum 5.
#' @param ... not used
#' @param derive (`list`)\cr a named list of functions which derives statistics, based on the
#'   vector of posterior MTD samples. Each list element must therefore accept
#'   one and only one argument, which is a numeric vector, and return a number.
#'
#' @return an object of class [`Simulations`]
#'
#' @example examples/design-method-simulate-Design.R
#' @export
#' @importFrom parallel detectCores
setMethod(
  f = "simulate",
  signature = signature(
    object = "Design",
    nsim = "ANY",
    seed = "ANY"
  ),
  definition = function(
    object,
    nsim = 1L,
    seed = NULL,
    truth,
    args = NULL,
    firstSeparate = FALSE,
    mcmcOptions = McmcOptions(),
    parallel = FALSE,
    nCores = min(parallel::detectCores(), 5),
    derive = list(),
    ...
  ) {
    nsim <- as.integer(nsim)
    assert_function(truth)
    assert_flag(firstSeparate)
    assert_count(nsim, positive = TRUE)
    assert_flag(parallel)
    assert_count(nCores, positive = TRUE)

    args <- as.data.frame(args)
    n_args <- max(nrow(args), 1L)
    rng_state <- set_seed(seed)
    sim_seeds <- sample.int(n = 2147483647, size = as.integer(nsim))

    run_sim <- function(iter_sim) {
      set.seed(sim_seeds[iter_sim])

      current_args <- args[(iter_sim - 1) %% n_args + 1, , drop = FALSE]
      data <- object@data
      prob_placebo <- NULL
      cohort_size_placebo <- NULL

      if (data@placebo) {
        prob_placebo <- h_this_truth(
          object@data@doseGrid[1],
          current_args,
          truth
        )
      }

      should_stop <- FALSE
      dose <- object@startingDose

      while (!should_stop) {
        prob <- h_this_truth(dose, current_args, truth)
        cohort_size <- size(object@cohort_size, dose = dose, data = data)

        if (data@placebo) {
          cohort_size_placebo <- size(
            object@pl_cohort_size,
            dose = dose,
            data = data
          )
        } else {
          cohort_size_placebo <- NULL
        }

        data <- h_determine_dlts(
          data = data,
          dose = dose,
          prob = prob,
          prob_placebo = prob_placebo,
          cohort_size = cohort_size,
          cohort_size_placebo = cohort_size_placebo,
          dose_grid = object@data@doseGrid[1],
          first_separate = firstSeparate
        )

        dose_limit <- maxDose(object@increments, data = data)
        samples <- mcmc(
          data = data,
          model = object@model,
          options = mcmcOptions
        )

        dose <- nextBest(
          object@nextBest,
          doselimit = dose_limit,
          samples = samples,
          model = object@model,
          data = data
        )$value

        should_stop <- stopTrial(
          object@stopping,
          dose = dose,
          samples = samples,
          model = object@model,
          data = data
        )
        stopit_results <- h_unpack_stopit(should_stop)
      }

      fit_model <- fit(object = samples, model = object@model, data = data)
      target_dose_samples <- dose(
        mean(object@nextBest@target),
        model = object@model,
        samples = samples
      )
      additional_stats <- lapply(derive, function(f) f(target_dose_samples))

      list(
        data = data,
        dose = dose,
        fit = subset(fit_model, select = c(middle, lower, upper)),
        stop = attr(should_stop, "message"),
        report_results = stopit_results,
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
        "object",
        "mcmcOptions"
      ),
      parallel = parallel,
      n_cores = nCores
    )

    simulations_output <- h_simulations_output_format(result_list)

    Simulations(
      data = simulations_output$dataList,
      doses = simulations_output$recommendedDoses,
      fit = simulations_output$fitList,
      stop_report = simulations_output$stop_matrix,
      stop_reasons = simulations_output$stopReasons,
      additional_stats = simulations_output$additional_stats,
      seed = rng_state
    )
  }
)

## RuleDesign ----

#' Simulate outcomes from a rule-based design
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param object the [`RuleDesign`] object we want to simulate data from
#' @param nsim (`count`)\cr the number of simulations (default: 1)
#' @param seed see [set_seed()]
#' @param truth (`function`)\cr a function which takes as input a dose (vector) and returns the
#'   true probability (vector) for toxicity. Additional arguments can be supplied
#'   in `args`.
#' @param args (`data.frame`)\cr data frame with arguments for the `truth` function. The
#'   column names correspond to the argument names, the rows to the values of the
#'   arguments. The rows are appropriately recycled in the `nsim`
#'   simulations.
#' @param parallel (`flag`)\cr should the simulation runs be parallelized across the
#'   clusters of the computer? (not default)
#' @param nCores (`count`)\cr how many cores should be used for parallel computing?
#'   Defaults to the number of cores on the machine, maximum 5.
#' @param ... not used
#'
#' @return an object of class [`GeneralSimulations`]
#'
#' @example examples/design-method-simulate-RuleDesign.R
#' @export
setMethod(
  f = "simulate",
  signature = signature(
    object = "RuleDesign",
    nsim = "ANY",
    seed = "ANY"
  ),
  definition = function(
    object,
    nsim = 1L,
    seed = NULL,
    truth,
    args = NULL,
    parallel = FALSE,
    nCores = min(parallel::detectCores(), 5L),
    ...
  ) {
    nsim <- as.integer(nsim)
    assert_function(truth)
    assert_count(nsim, positive = TRUE)
    assert_flag(parallel)
    assert_count(nCores, positive = TRUE)
    assert_class(object, "RuleDesign")

    args <- as.data.frame(args)
    n_args <- max(nrow(args), 1L)
    rng_state <- set_seed(seed)
    sim_seeds <- sample(x = seq_len(1e5), size = as.integer(nsim))

    run_sim <- function(iter_sim) {
      set.seed(sim_seeds[iter_sim])

      current_args <- args[(iter_sim - 1) %% n_args + 1, , drop = FALSE]

      truth_with_args <- function(dose) {
        do.call(truth, c(dose, current_args))
      }

      data <- object@data
      should_stop <- FALSE
      dose <- object@startingDose

      while (!should_stop) {
        prob <- truth_with_args(dose)
        cohort_size <- size(object@cohort_size, dose = dose, data = data)

        dlts <- rbinom(n = cohort_size, size = 1L, prob = prob)
        data <- update(object = data, x = dose, y = dlts)

        outcome <- nextBest(object@nextBest, data = data)
        dose <- outcome$value
        should_stop <- outcome$stopHere
      }

      list(data = data, dose = dose)
    }

    result_list <- get_result_list(
      fun = run_sim,
      nsim = nsim,
      vars = c(
        "sim_seeds",
        "args",
        "n_args",
        "truth",
        "object"
      ),
      parallel = parallel,
      n_cores = nCores
    )

    data_list <- lapply(result_list, "[[", "data")
    recommended_doses <- as.numeric(sapply(result_list, "[[", "dose"))

    GeneralSimulations(
      data = data_list,
      doses = recommended_doses,
      seed = rng_state
    )
  }
)

## DualDesign ----

#' Simulate outcomes from a dual-endpoint design
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param object the [`DualDesign`] object we want to simulate data from
#' @param nsim (`count`)\cr the number of simulations (default: 1)
#' @param seed see [set_seed()]
#' @param trueTox (`function`)\cr a function which takes as input a dose (vector) and returns the
#'   true probability (vector) for toxicity. Additional arguments can be supplied
#'   in `args`.
#' @param trueBiomarker (`function`)\cr a function which takes as input a dose (vector) and
#'   returns the true biomarker level (vector). Additional arguments can be
#'   supplied in `args`.
#' @param args (`data.frame`)\cr data frame with arguments for the `trueTox` and
#'   `trueBiomarker` function. The column names correspond to the argument
#'   names, the rows to the values of the arguments. The rows are appropriately
#'   recycled in the `nsim` simulations.
#' @param sigma2W (`number`)\cr variance for the biomarker measurements
#' @param rho (`number`)\cr correlation between toxicity and biomarker measurements (default: 0)
#' @param firstSeparate (`flag`)\cr enroll the first patient separately from the rest of
#'   the cohort? (not default) If yes, the cohort will be closed if a DLT occurs
#'   in this patient.
#' @param mcmcOptions ([McmcOptions])\cr object of class [`McmcOptions`],
#'   giving the MCMC options for each evaluation in the trial. By default,
#'   the standard options are used
#' @param parallel (`flag`)\cr should the simulation runs be parallelized across the
#'   clusters of the computer? (not default)
#' @param nCores (`count`)\cr how many cores should be used for parallel computing?
#'   Defaults to the number of cores on the machine, maximum 5.
#' @param ... not used
#' @param derive (`list`)\cr a named list of functions which derives statistics, based on the
#'   vector of posterior MTD samples. Each list element must therefore accept
#'   one and only one argument, which is a numeric vector, and return a number.
#'
#' @return an object of class [`DualSimulations`]
#'
#' @example examples/design-method-simulate-DualDesign.R
#' @importFrom mvtnorm rmvnorm
#' @export
setMethod(
  f = "simulate",
  signature = signature(object = "DualDesign"),
  definition = function(
    object,
    nsim = 1L,
    seed = NULL,
    trueTox,
    trueBiomarker,
    args = NULL,
    sigma2W,
    rho = 0,
    firstSeparate = FALSE,
    mcmcOptions = McmcOptions(),
    parallel = FALSE,
    nCores = min(parallel::detectCores(), 5),
    derive = list(),
    ...
  ) {
    nsim <- as.integer(nsim)
    assert_function(trueTox)
    assert_function(trueBiomarker)
    assert_number(sigma2W, lower = 0)
    assert_number(rho, lower = -1, upper = 1)
    assert_flag(firstSeparate)
    assert_count(nsim, positive = TRUE)
    assert_flag(parallel)
    assert_count(nCores, positive = TRUE)
    assert_class(object, "DualDesign")
    assert_list(derive)

    args <- as.data.frame(args)
    n_args <- max(nrow(args), 1L)

    tox_arg_names <- names(formals(trueTox))[-1]
    biomarker_arg_names <- names(formals(trueBiomarker))[-1]

    covariance_matrix <- matrix(
      c(
        sigma2W,
        sqrt(sigma2W) * rho,
        sqrt(sigma2W) * rho,
        1
      ),
      nrow = 2,
      byrow = TRUE
    )

    rng_state <- set_seed(seed)
    sim_seeds <- sample(x = seq_len(1e5), size = as.integer(nsim))

    run_sim <- function(iter_sim) {
      set.seed(sim_seeds[iter_sim])

      current_args <- args[(iter_sim - 1) %% n_args + 1, , drop = FALSE]

      tox_with_args <- function(dose) {
        do.call(
          trueTox,
          c(dose, as.list(current_args)[tox_arg_names])
        )
      }

      biomarker_with_args <- function(dose) {
        do.call(
          trueBiomarker,
          c(dose, as.list(current_args)[biomarker_arg_names])
        )
      }

      data <- object@data
      should_stop <- FALSE
      dose <- object@startingDose

      prob_placebo <- NULL
      mean_z_placebo <- NULL
      mean_biomarker_placebo <- NULL

      if (data@placebo) {
        prob_placebo <- tox_with_args(object@data@doseGrid[1])
        mean_z_placebo <- qlogis(prob_placebo)
        mean_biomarker_placebo <- biomarker_with_args(object@data@doseGrid[1])
      }

      while (!should_stop) {
        prob <- tox_with_args(dose)
        mean_z <- qlogis(prob)
        mean_biomarker <- biomarker_with_args(dose)
        cohort_size <- size(object@cohort_size, dose = dose, data = data)

        cohort_size_placebo <- NULL
        if (data@placebo) {
          cohort_size_placebo <- size(
            object@pl_cohort_size,
            dose = dose,
            data = data
          )
        }

        response_data <- if (firstSeparate && (cohort_size > 1L)) {
          first_patient <- mvtnorm::rmvnorm(
            n = 1,
            mean = c(mean_biomarker, mean_z),
            sigma = covariance_matrix
          )

          first_patient_placebo <- NULL
          if (data@placebo && (cohort_size_placebo > 0L)) {
            first_patient_placebo <- mvtnorm::rmvnorm(
              n = 1,
              mean = c(mean_biomarker_placebo, mean_z_placebo),
              sigma = covariance_matrix
            )
          }

          if (first_patient[, 2] < 0) {
            remaining_patients <- mvtnorm::rmvnorm(
              n = cohort_size - 1,
              mean = c(mean_biomarker, mean_z),
              sigma = covariance_matrix
            )
            first_patient <- rbind(first_patient, remaining_patients)

            if (data@placebo && (cohort_size_placebo > 0L)) {
              remaining_patients_placebo <- mvtnorm::rmvnorm(
                n = cohort_size_placebo,
                mean = c(mean_biomarker_placebo, mean_z_placebo),
                sigma = covariance_matrix
              )
              first_patient_placebo <- rbind(
                first_patient_placebo,
                remaining_patients_placebo
              )
            }
          }

          if (data@placebo && (cohort_size_placebo > 0L)) {
            list(active = first_patient, placebo = first_patient_placebo)
          } else {
            list(active = first_patient)
          }
        } else {
          active_responses <- mvtnorm::rmvnorm(
            n = cohort_size,
            mean = c(mean_biomarker, mean_z),
            sigma = covariance_matrix
          )

          placebo_responses <- NULL
          if (data@placebo && (cohort_size_placebo > 0L)) {
            placebo_responses <- mvtnorm::rmvnorm(
              n = cohort_size_placebo,
              mean = c(mean_biomarker_placebo, mean_z_placebo),
              sigma = covariance_matrix
            )
          }

          if (data@placebo && (cohort_size_placebo > 0L)) {
            list(active = active_responses, placebo = placebo_responses)
          } else {
            list(active = active_responses)
          }
        }

        biomarkers <- response_data$active[, 1]
        dlts <- as.integer(response_data$active[, 2] > 0)

        if (data@placebo && (cohort_size_placebo > 0L)) {
          biomarkers_placebo <- response_data$placebo[, 1]
          dlts_placebo <- as.integer(response_data$placebo[, 2] > 0)

          data <- update(
            object = data,
            x = object@data@doseGrid[1],
            y = dlts_placebo,
            w = biomarkers_placebo,
            check = FALSE
          )

          data <- update(
            object = data,
            x = dose,
            y = dlts,
            w = biomarkers,
            new_cohort = FALSE
          )
        } else {
          data <- update(
            object = data,
            x = dose,
            y = dlts,
            w = biomarkers
          )
        }

        dose_limit <- maxDose(object@increments, data = data)
        samples <- mcmc(
          data = data,
          model = object@model,
          options = mcmcOptions
        )

        dose <- nextBest(
          object@nextBest,
          doselimit = dose_limit,
          samples = samples,
          model = object@model,
          data = data
        )$value

        should_stop <- stopTrial(
          object@stopping,
          dose = dose,
          samples = samples,
          model = object@model,
          data = data
        )
        stopit_results <- h_unpack_stopit(should_stop)
      }

      fit_model <- fit(object = samples, model = object@model, data = data)

      target_dose_samples <- dose(
        mean(object@nextBest@target),
        model = object@model,
        samples = samples
      )

      additional_stats <- lapply(derive, function(f) f(target_dose_samples))

      list(
        data = data,
        dose = dose,
        fitTox = subset(fit_model, select = c(middle, lower, upper)),
        fit_biomarker = subset(
          fit_model,
          select = c(middleBiomarker, lowerBiomarker, upperBiomarker)
        ),
        rho_est = median(samples@data$rho),
        sigma2w_est = median(1 / samples@data$precW),
        stop = attr(should_stop, "message"),
        additional_stats = additional_stats,
        report_results = stopit_results
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
        "trueTox",
        "trueBiomarker",
        "covariance_matrix",
        "object",
        "mcmcOptions"
      ),
      parallel = parallel,
      n_cores = nCores
    )

    data_list <- lapply(result_list, "[[", "data")
    recommended_doses <- as.numeric(sapply(result_list, "[[", "dose"))
    rho_estimates <- as.numeric(sapply(result_list, "[[", "rho_est"))
    sigma2w_estimates <- as.numeric(sapply(result_list, "[[", "sigma2w_est"))
    fit_tox_list <- lapply(result_list, "[[", "fitTox")
    fit_biomarker_list <- lapply(result_list, "[[", "fit_biomarker")
    stop_reasons <- lapply(result_list, "[[", "stop")
    stop_results <- lapply(result_list, "[[", "report_results")
    stop_report <- as.matrix(do.call(rbind, stop_results))
    additional_stats <- lapply(result_list, "[[", "additional_stats")

    DualSimulations(
      data = data_list,
      doses = recommended_doses,
      rho_est = rho_estimates,
      sigma2w_est = sigma2w_estimates,
      fit = fit_tox_list,
      fit_biomarker = fit_biomarker_list,
      stop_report = stop_report,
      stop_reasons = stop_reasons,
      additional_stats = additional_stats,
      seed = rng_state
    )
  }
)

## TDsamplesDesign ----

#' Simulate dose escalation procedure using DLE responses only with DLE samples
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This is a method to simulate dose escalation procedure only using the DLE responses.
#' This is a method based on the [`TDsamplesDesign`] where model used are of
#' [`ModelTox`] class object DLE samples are also used.
#'
#' @param object the [`TDsamplesDesign`] object we want to simulate the data from
#' @param nsim (`count`)\cr the number of simulations (default: 1)
#' @param seed see [set_seed()]
#' @param truth (`function`)\cr a function which takes as input a dose (vector) and returns the true probability
#'   (vector) of the occurrence of a DLE. Additional arguments can be supplied in `args`.
#' @param args (`data.frame`)\cr data frame with arguments for the `truth` function. The
#'   column names correspond to the argument names, the rows to the values of the
#'   arguments. The rows are appropriately recycled in the `nsim`
#'   simulations. In order to produce outcomes from the posterior predictive
#'   distribution, e.g, pass an `object` that contains the data observed so
#'   far, `truth` contains the `prob` function from the model in
#'   `object`, and `args` contains posterior samples from the model.
#' @param firstSeparate (`flag`)\cr enroll the first patient separately from the rest of
#'   the cohort? (not default) If yes, the cohort will be closed if a DLT occurs
#'   in this patient.
#' @param mcmcOptions ([McmcOptions])\cr object of class [`McmcOptions`],
#'   giving the MCMC options for each evaluation in the trial. By default,
#'   the standard options are used
#' @param parallel (`flag`)\cr should the simulation runs be parallelized across the
#'   clusters of the computer? (not default)
#' @param nCores (`count`)\cr how many cores should be used for parallel computing?
#'   Defaults to the number of cores on the machine, maximum 5.
#' @param ... not used
#'
#' @return an object of class [`PseudoSimulations`]
#'
#' @example examples/design-method-simulateTDsamplesDesign.R
#' @export
setMethod(
  f = "simulate",
  signature = signature(
    object = "TDsamplesDesign",
    nsim = "ANY",
    seed = "ANY"
  ),
  definition = function(
    object,
    nsim = 1L,
    seed = NULL,
    truth,
    args = NULL,
    firstSeparate = FALSE,
    mcmcOptions = McmcOptions(),
    parallel = FALSE,
    nCores = min(parallel::detectCores(), 5L),
    ...
  ) {
    nsim <- as.integer(nsim)
    assert_function(truth)
    assert_flag(firstSeparate)
    assert_count(nsim, positive = TRUE)
    assert_flag(parallel)
    assert_count(nCores, positive = TRUE)
    assert_class(object, "TDsamplesDesign")

    args <- as.data.frame(args)
    n_args <- max(nrow(args), 1L)
    rng_state <- set_seed(seed)

    # Keep original seed generation for snapshot test compatibility
    sim_seeds <- sample(x = seq_len(1e5), size = nsim)

    run_sim <- function(iter_sim) {
      set.seed(sim_seeds[iter_sim])

      current_args <- args[(iter_sim - 1) %% n_args + 1, , drop = FALSE]

      truth_with_args <- function(dose) {
        do.call(truth, c(dose, current_args))
      }

      data <- object@data
      prob_placebo <- NULL

      if (data@placebo) {
        prob_placebo <- truth_with_args(object@data@doseGrid[1])
      }

      should_stop <- FALSE
      dose <- object@startingDose

      while (!should_stop) {
        prob <- truth_with_args(dose)
        cohort_size <- size(object@cohort_size, dose = dose, data = data)

        cohort_size_placebo <- NULL
        if (data@placebo) {
          cohort_size_placebo <- size(
            object@pl_cohort_size,
            dose = dose,
            data = data
          )
        }

        dlts <- if (firstSeparate && (cohort_size > 1L)) {
          first_dlt <- rbinom(n = 1L, size = 1L, prob = prob)

          dlts_placebo_first <- NULL
          if (data@placebo && (cohort_size_placebo > 0L)) {
            dlts_placebo_first <- rbinom(n = 1L, size = 1L, prob = prob_placebo)
          }

          if (first_dlt == 0) {
            remaining_dlts <- rbinom(
              n = cohort_size - 1L,
              size = 1L,
              prob = prob
            )
            c(first_dlt, remaining_dlts)
          } else {
            first_dlt
          }
        } else {
          rbinom(n = cohort_size, size = 1L, prob = prob)
        }

        dlts_placebo <- NULL
        if (data@placebo && (cohort_size_placebo > 0L)) {
          if (firstSeparate && (cohort_size > 1L) && length(dlts) == 1) {
            dlts_placebo <- dlts_placebo_first
          } else {
            dlts_placebo <- if (firstSeparate && (cohort_size > 1L)) {
              c(
                dlts_placebo_first,
                rbinom(
                  n = cohort_size_placebo,
                  size = 1L,
                  prob = prob_placebo
                )
              )
            } else {
              rbinom(n = cohort_size_placebo, size = 1L, prob = prob_placebo)
            }
          }
        }

        if (data@placebo && (cohort_size_placebo > 0L)) {
          data <- update(object = data, x = dose, y = dlts)
          data <- update(
            object = data,
            x = object@data@doseGrid[1],
            y = dlts_placebo,
            new_cohort = FALSE
          )
        } else {
          data <- update(object = data, x = dose, y = dlts)
        }

        model <- update(object@model, data = data)
        dose_limit <- maxDose(object@increments, data = data)
        samples <- mcmc(data = data, model = model, options = mcmcOptions)

        next_best_result <- nextBest(
          object@nextBest,
          doselimit = dose_limit,
          samples = samples,
          model = model,
          data = data,
          in_sim = TRUE
        )

        dose <- next_best_result$next_dose_drt
        td_target_during_trial <- next_best_result$dose_target_drt
        td_target_end_of_trial <- next_best_result$dose_target_eot
        td_target_end_of_trial_at_dose_grid <- next_best_result$next_dose_eot
        ci_tdeot <- list(
          lower = next_best_result$ci_dose_target_eot[1],
          upper = next_best_result$ci_dose_target_eot[2]
        )
        ratio_tdeot <- next_best_result$ci_ratio_dose_target_eot

        should_stop <- stopTrial(
          object@stopping,
          dose = dose,
          samples = samples,
          model = model,
          data = data
        )
        stopit_results <- h_unpack_stopit(should_stop)
      }

      fit_model <- fit(object = samples, model = model, data = data)

      list(
        data = data,
        dose = dose,
        TDtargetDuringTrial = td_target_during_trial,
        TDtargetEndOfTrial = td_target_end_of_trial,
        TDtargetEndOfTrialatdoseGrid = td_target_end_of_trial_at_dose_grid,
        TDtargetDuringTrialatdoseGrid = dose,
        CITDEOT = ci_tdeot,
        ratioTDEOT = ratio_tdeot,
        fit = subset(fit_model, select = c(middle, lower, upper)),
        stop = attr(should_stop, "message"),
        report_results = stopit_results
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
        "object",
        "mcmcOptions"
      ),
      parallel = parallel,
      n_cores = nCores
    )

    data_list <- lapply(result_list, "[[", "data")

    td_target_during_trial_list <- as.numeric(sapply(
      result_list,
      "[[",
      "TDtargetDuringTrial"
    ))

    td_target_end_of_trial_list <- as.numeric(sapply(
      result_list,
      "[[",
      "TDtargetEndOfTrial"
    ))

    td_target_during_trial_dose_grid_list <- as.numeric(sapply(
      result_list,
      "[[",
      "TDtargetDuringTrialatdoseGrid"
    ))

    td_target_end_of_trial_dose_grid_list <- as.numeric(sapply(
      result_list,
      "[[",
      "TDtargetEndOfTrialatdoseGrid"
    ))

    recommended_doses <- as.numeric(sapply(
      result_list,
      "[[",
      "TDtargetEndOfTrialatdoseGrid"
    ))

    ci_list <- lapply(result_list, "[[", "CITDEOT")
    ratio_list <- as.numeric(sapply(result_list, "[[", "ratioTDEOT"))
    ci_tdeot_list <- lapply(result_list, "[[", "CITDEOT")
    ratio_tdeot_list <- as.numeric(sapply(result_list, "[[", "ratioTDEOT"))
    fit_list <- lapply(result_list, "[[", "fit")
    stop_reasons <- lapply(result_list, "[[", "stop")

    stop_results <- lapply(result_list, "[[", "report_results")
    stop_report <- as.matrix(do.call(rbind, stop_results))

    PseudoSimulations(
      data = data_list,
      doses = recommended_doses,
      fit = fit_list,
      final_td_target_during_trial_estimates = td_target_during_trial_list,
      final_td_target_end_of_trial_estimates = td_target_end_of_trial_list,
      final_td_target_during_trial_at_dose_grid = td_target_during_trial_dose_grid_list,
      final_td_target_end_of_trial_at_dose_grid = td_target_end_of_trial_dose_grid_list,
      final_cis = ci_list,
      final_ratios = ratio_list,
      final_tdeot_cis = ci_tdeot_list,
      final_tdeot_ratios = ratio_tdeot_list,
      stop_reasons = stop_reasons,
      stop_report = stop_report,
      seed = rng_state
    )
  }
)

## TDDesign ----

#' Simulate dose escalation procedure using DLE responses only without samples
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This is a method to simulate dose escalation procedure only using the DLE responses.
#' This is a method based on the [`TDDesign`] where model used are of
#' [`ModelTox`] class object and no samples are involved.
#'
#' @param object the [`TDDesign`] object we want to simulate the data from
#' @param nsim (`count`)\cr the number of simulations (default: 1)
#' @param seed see [set_seed()]
#' @param truth (`function`)\cr a function which takes as input a dose (vector) and returns the true probability
#'   (vector) of the occurrence of a DLE. Additional arguments can be supplied in `args`.
#' @param args (`data.frame`)\cr data frame with arguments for the `truth` function. The
#'   column names correspond to the argument names, the rows to the values of the
#'   arguments. The rows are appropriately recycled in the `nsim`
#'   simulations. In order to produce outcomes from the posterior predictive
#'   distribution, e.g, pass an `object` that contains the data observed so
#'   far, `truth` contains the `prob` function from the model in
#'   `object`, and `args` contains posterior samples from the model.
#' @param firstSeparate (`flag`)\cr enroll the first patient separately from the rest of
#'   the cohort? (not default) If yes, the cohort will be closed if a DLT occurs
#'   in this patient.
#' @param parallel (`flag`)\cr should the simulation runs be parallelized across the
#'   clusters of the computer? (not default)
#' @param nCores (`count`)\cr how many cores should be used for parallel computing?
#'   Defaults to the number of cores on the machine, maximum 5.
#' @param ... not used
#'
#' @return an object of class [`PseudoSimulations`]
#'
#' @example examples/design-method-simulateTDDesign.R
#' @export
setMethod(
  f = "simulate",
  signature = signature(
    object = "TDDesign",
    nsim = "ANY",
    seed = "ANY"
  ),
  definition = function(
    object,
    nsim = 1L,
    seed = NULL,
    truth,
    args = NULL,
    firstSeparate = FALSE,
    parallel = FALSE,
    nCores = min(parallel::detectCores(), 5L),
    ...
  ) {
    nsim <- as.integer(nsim)
    assert_function(truth)
    assert_flag(firstSeparate)
    assert_count(nsim, positive = TRUE)
    assert_flag(parallel)
    assert_count(nCores, positive = TRUE)
    assert_class(object, "TDDesign")

    args <- as.data.frame(args)
    n_args <- max(nrow(args), 1L)
    rng_state <- set_seed(seed)

    # Keep original seed generation for snapshot test compatibility
    sim_seeds <- sample(x = seq_len(1e5), size = nsim)

    run_sim <- function(iter_sim) {
      set.seed(sim_seeds[iter_sim])

      current_args <- args[(iter_sim - 1) %% n_args + 1, , drop = FALSE]

      truth_with_args <- function(dose) {
        do.call(truth, c(dose, current_args))
      }

      data <- object@data
      prob_placebo <- NULL

      if (data@placebo) {
        prob_placebo <- truth_with_args(object@data@doseGrid[1])
      }

      should_stop <- FALSE
      dose <- object@startingDose

      while (!should_stop) {
        prob <- truth_with_args(dose)
        cohort_size <- size(object@cohort_size, dose = dose, data = data)

        cohort_size_placebo <- NULL
        if (data@placebo) {
          cohort_size_placebo <- size(
            object@pl_cohort_size,
            dose = dose,
            data = data
          )
        }

        dlts <- if (firstSeparate && (cohort_size > 1L)) {
          first_dlt <- rbinom(n = 1L, size = 1L, prob = prob)

          dlts_placebo_first <- NULL
          if (data@placebo && (cohort_size_placebo > 0L)) {
            dlts_placebo_first <- rbinom(n = 1L, size = 1L, prob = prob_placebo)
          }

          if (first_dlt == 0) {
            remaining_dlts <- rbinom(
              n = cohort_size - 1L,
              size = 1L,
              prob = prob
            )
            c(first_dlt, remaining_dlts)
          } else {
            first_dlt
          }
        } else {
          rbinom(n = cohort_size, size = 1L, prob = prob)
        }

        dlts_placebo <- NULL
        if (data@placebo && (cohort_size_placebo > 0L)) {
          if (firstSeparate && (cohort_size > 1L) && length(dlts) == 1) {
            dlts_placebo <- dlts_placebo_first
          } else {
            dlts_placebo <- rbinom(
              n = cohort_size_placebo,
              size = 1L,
              prob = prob_placebo
            )
          }
        }

        if (data@placebo && (cohort_size_placebo > 0L)) {
          data <- update(
            object = data,
            x = object@data@doseGrid[1],
            y = dlts_placebo
          )
          data <- update(
            object = data,
            x = dose,
            y = dlts,
            new_cohort = FALSE
          )
        } else {
          data <- update(object = data, x = dose, y = dlts)
        }

        model <- update(object@model, data = data)
        dose_limit <- maxDose(object@increments, data = data)

        next_best_result <- nextBest(
          object@nextBest,
          doselimit = dose_limit,
          model = model,
          data = data,
          in_sim = TRUE
        )

        dose <- next_best_result$next_dose_drt
        td_target_during_trial <- next_best_result$dose_target_drt
        td_target_end_of_trial <- next_best_result$dose_target_eot
        td_target_end_of_trial_at_dose_grid <- next_best_result$next_dose_eot
        ci_tdeot <- list(
          lower = next_best_result$ci_dose_target_eot[1],
          upper = next_best_result$ci_dose_target_eot[2]
        )
        ratio_tdeot <- next_best_result$ci_ratio_dose_target_eot

        should_stop <- stopTrial(
          object@stopping,
          dose = dose,
          model = model,
          data = data
        )
        stopit_results <- h_unpack_stopit(should_stop)
      }

      prob_fun <- probFunction(
        model,
        phi1 = model@phi1,
        phi2 = model@phi2
      )
      fit_model <- list(
        phi1 = model@phi1,
        phi2 = model@phi2,
        probDLE = prob_fun(object@data@doseGrid)
      )

      list(
        data = data,
        dose = dose,
        TDtargetDuringTrial = td_target_during_trial,
        TDtargetEndOfTrial = td_target_end_of_trial,
        TDtargetEndOfTrialatdoseGrid = td_target_end_of_trial_at_dose_grid,
        TDtargetDuringTrialatdoseGrid = dose,
        CITDEOT = ci_tdeot,
        ratioTDEOT = ratio_tdeot,
        fit = fit_model,
        stop = attr(should_stop, "message"),
        report_results = stopit_results
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
        "object"
      ),
      parallel = parallel,
      n_cores = nCores
    )

    data_list <- lapply(result_list, "[[", "data")

    td_target_during_trial_list <- as.numeric(sapply(
      result_list,
      "[[",
      "TDtargetDuringTrial"
    ))

    td_target_end_of_trial_list <- as.numeric(sapply(
      result_list,
      "[[",
      "TDtargetEndOfTrial"
    ))

    td_target_during_trial_dose_grid_list <- as.numeric(sapply(
      result_list,
      "[[",
      "TDtargetDuringTrialatdoseGrid"
    ))

    td_target_end_of_trial_dose_grid_list <- as.numeric(sapply(
      result_list,
      "[[",
      "TDtargetEndOfTrialatdoseGrid"
    ))

    recommended_doses <- as.numeric(sapply(
      result_list,
      "[[",
      "TDtargetEndOfTrialatdoseGrid"
    ))

    ci_list <- lapply(result_list, "[[", "CITDEOT")
    ratio_list <- as.numeric(sapply(result_list, "[[", "ratioTDEOT"))
    ci_tdeot_list <- lapply(result_list, "[[", "CITDEOT")
    ratio_tdeot_list <- as.numeric(sapply(result_list, "[[", "ratioTDEOT"))
    fit_list <- lapply(result_list, "[[", "fit")
    stop_reasons <- lapply(result_list, "[[", "stop")

    stop_results <- lapply(result_list, "[[", "report_results")
    stop_report <- as.matrix(do.call(rbind, stop_results))

    PseudoSimulations(
      data = data_list,
      doses = recommended_doses,
      fit = fit_list,
      final_td_target_during_trial_estimates = td_target_during_trial_list,
      final_td_target_end_of_trial_estimates = td_target_end_of_trial_list,
      final_td_target_during_trial_at_dose_grid = td_target_during_trial_dose_grid_list,
      final_td_target_end_of_trial_at_dose_grid = td_target_end_of_trial_dose_grid_list,
      final_cis = ci_list,
      final_ratios = ratio_list,
      final_tdeot_cis = ci_tdeot_list,
      final_tdeot_ratios = ratio_tdeot_list,
      stop_reasons = stop_reasons,
      stop_report = stop_report,
      seed = rng_state
    )
  }
)

## DualResponsesDesign ----

#' Simulate dose escalation procedure using both DLE and efficacy responses without samples
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This is a method to simulate dose escalation procedure using both DLE and efficacy responses.
#' This is a method based on the [`DualResponsesDesign`] where DLE model used are of
#' [`ModelTox`] class object and efficacy model used are of [`ModelEff`]
#' class object. In addition, no DLE and efficacy samples are involved or generated in the simulation
#' process.
#'
#' @param object the [`DualResponsesDesign`] object we want to simulate the data from
#' @param nsim (`count`)\cr the number of simulations (default: 1)
#' @param seed see [set_seed()]
#' @param trueDLE (`function`)\cr a function which takes as input a dose (vector) and returns the true probability
#'   (vector) of the occurrence of a DLE. Additional arguments can be supplied in `args`.
#' @param trueEff (`function`)\cr a function which takes as input a dose (vector) and returns the expected efficacy
#'   responses (vector). Additional arguments can be supplied in `args`.
#' @param trueNu (`number`)\cr the precision, the inverse of the variance of the efficacy responses
#' @param args (`data.frame`)\cr data frame with arguments for the `trueDLE` and
#'   `trueEff` function. The column names correspond to the argument
#'   names, the rows to the values of the arguments. The rows are appropriately
#'   recycled in the `nsim` simulations.
#' @param firstSeparate (`flag`)\cr enroll the first patient separately from the rest of
#'   the cohort? (not default) If yes, the cohort will be closed if a DLT occurs
#'   in this patient.
#' @param parallel (`flag`)\cr should the simulation runs be parallelized across the
#'   clusters of the computer? (not default)
#' @param nCores (`count`)\cr how many cores should be used for parallel computing?
#'   Defaults to the number of cores on the machine, maximum 5.
#' @param ... not used
#'
#' @return an object of class [`PseudoDualSimulations`]
#'
#' @example examples/design-method-simulateDualResponsesDesign.R
#' @export
setMethod(
  f = "simulate",
  signature = signature(
    object = "DualResponsesDesign",
    nsim = "ANY",
    seed = "ANY"
  ),
  definition = function(
    object,
    nsim = 1L,
    seed = NULL,
    trueDLE,
    trueEff,
    trueNu,
    args = NULL,
    firstSeparate = FALSE,
    parallel = FALSE,
    nCores = min(parallel::detectCores(), 5L),
    ...
  ) {
    nsim <- as.integer(nsim)
    assert_function(trueDLE)
    assert_function(trueEff)
    assert_true(trueNu > 0)
    assert_flag(firstSeparate)
    assert_count(nsim, positive = TRUE)
    assert_flag(parallel)
    assert_count(nCores, positive = TRUE)
    assert_class(object, "DualResponsesDesign")

    args <- as.data.frame(args)
    n_args <- max(nrow(args), 1L)

    dle_arg_names <- names(formals(trueDLE))[-1]
    eff_arg_names <- names(formals(trueEff))[-1]

    rng_state <- set_seed(seed)

    # Keep original seed generation for snapshot test compatibility
    sim_seeds <- sample(x = seq_len(1e5), size = nsim)

    run_sim <- function(iter_sim) {
      set.seed(sim_seeds[iter_sim])

      current_args <- args[(iter_sim - 1) %% n_args + 1, , drop = FALSE]

      dle_with_args <- function(dose) {
        do.call(
          trueDLE,
          c(dose, as.list(current_args)[dle_arg_names])
        )
      }

      eff_with_args <- function(dose) {
        do.call(
          trueEff,
          c(dose, as.list(current_args)[eff_arg_names])
        )
      }

      data <- object@data
      sigma2 <- 1 / trueNu
      prob_placebo <- NULL
      mean_eff_placebo <- NULL

      if (data@placebo) {
        prob_placebo <- dle_with_args(object@data@doseGrid[1])
        mean_eff_placebo <- eff_with_args(object@data@doseGrid[1])
      }

      should_stop <- FALSE
      dose <- object@startingDose

      while (!should_stop) {
        prob <- dle_with_args(dose)
        mean_eff <- eff_with_args(dose)
        cohort_size <- size(object@cohort_size, dose = dose, data = data)

        cohort_size_placebo <- NULL
        if (data@placebo) {
          cohort_size_placebo <- size(
            object@pl_cohort_size,
            dose = dose,
            data = data
          )
        }

        if (firstSeparate && (cohort_size > 1L)) {
          dlts <- rbinom(n = 1L, size = 1L, prob = prob)
          eff_responses <- rnorm(n = 1L, mean = mean_eff, sd = sqrt(sigma2))

          dlts_placebo <- NULL
          eff_responses_placebo <- NULL
          if (data@placebo && (cohort_size_placebo > 0L)) {
            dlts_placebo <- rbinom(n = 1L, size = 1L, prob = prob_placebo)
            eff_responses_placebo <- rnorm(
              n = 1L,
              mean = mean_eff_placebo,
              sd = sqrt(sigma2)
            )
          }

          if (dlts == 0) {
            remaining_dlts <- rbinom(
              n = cohort_size - 1L,
              size = 1L,
              prob = prob
            )
            remaining_eff <- rnorm(
              n = cohort_size - 1L,
              mean = mean_eff,
              sd = sqrt(sigma2)
            )
            dlts <- c(dlts, remaining_dlts)
            eff_responses <- c(eff_responses, remaining_eff)

            if (data@placebo && (cohort_size_placebo > 0L)) {
              remaining_dlts_placebo <- rbinom(
                n = cohort_size_placebo,
                size = 1L,
                prob = prob_placebo
              )
              remaining_eff_placebo <- rnorm(
                n = cohort_size_placebo,
                mean = mean_eff_placebo,
                sd = sqrt(sigma2)
              )
              dlts_placebo <- c(dlts_placebo, remaining_dlts_placebo)
              eff_responses_placebo <- c(
                eff_responses_placebo,
                remaining_eff_placebo
              )
            }
          }
        } else {
          dlts <- rbinom(n = cohort_size, size = 1L, prob = prob)
          eff_responses <- rnorm(
            n = cohort_size,
            mean = mean_eff,
            sd = sqrt(sigma2)
          )

          dlts_placebo <- NULL
          eff_responses_placebo <- NULL
          if (data@placebo && (cohort_size_placebo > 0L)) {
            dlts_placebo <- rbinom(
              n = cohort_size_placebo,
              size = 1L,
              prob = prob_placebo
            )
            eff_responses_placebo <- rnorm(
              n = cohort_size_placebo,
              mean = mean_eff_placebo,
              sd = sqrt(sigma2)
            )
          }
        }

        if (data@placebo && (cohort_size_placebo > 0L)) {
          data <- update(
            object = data,
            x = object@data@doseGrid[1],
            y = dlts_placebo,
            w = eff_responses_placebo,
            check = FALSE
          )
          data <- update(
            object = data,
            x = dose,
            y = dlts,
            w = eff_responses,
            new_cohort = FALSE
          )
        } else {
          data <- update(object = data, x = dose, y = dlts, w = eff_responses)
        }

        dle_model <- update(object = object@model, data = data)
        eff_model <- update(object = object@eff_model, data = data)

        eff_nu <- eff_model@nu
        eff_sigma2 <- if (eff_model@use_fixed) {
          1 / eff_nu
        } else {
          1 / (as.numeric(eff_nu["a"] / eff_nu["b"]))
        }

        dose_limit <- maxDose(object@increments, data = data)

        next_best_result <- nextBest(
          object@nextBest,
          doselimit = dose_limit,
          model = dle_model,
          data = data,
          model_eff = eff_model,
          in_sim = TRUE
        )

        dose <- next_best_result$next_dose
        td_target_during_trial <- next_best_result$dose_target_drt
        td_target_during_trial_at_dose_grid <- next_best_result$next_dose_drt
        td_target_end_of_trial <- next_best_result$dose_target_eot
        td_target_end_of_trial_at_dose_grid <- next_best_result$next_dose_eot
        gstar <- next_best_result$dose_max_gain
        gstar_at_dose_grid <- next_best_result$next_dose_max_gain

        recommend <- min(
          td_target_end_of_trial_at_dose_grid,
          gstar_at_dose_grid
        )

        ci_tdeot <- list(
          lower = next_best_result$ci_dose_target_eot[1],
          upper = next_best_result$ci_dose_target_eot[2]
        )
        ratio_tdeot <- next_best_result$ci_ratio_dose_target_eot

        ci_gstar <- list(
          lower = next_best_result$ci_dose_max_gain[1],
          upper = next_best_result$ci_dose_max_gain[2]
        )
        ratio_gstar <- next_best_result$ci_ratio_dose_max_gain

        optimal_dose <- min(gstar, td_target_end_of_trial)

        if (optimal_dose == gstar) {
          ratio <- ratio_gstar
          ci <- ci_gstar
        } else {
          ratio <- ratio_tdeot
          ci <- ci_tdeot
        }

        should_stop <- stopTrial(
          object@stopping,
          dose = dose,
          model = dle_model,
          data = data,
          Effmodel = eff_model
        )
        stopit_results <- h_unpack_stopit(should_stop)
      }

      prob_fun <- probFunction(
        dle_model,
        phi1 = dle_model@phi1,
        phi2 = dle_model@phi2
      )
      dle_fit <- list(
        phi1 = dle_model@phi1,
        phi2 = dle_model@phi2,
        probDLE = prob_fun(object@data@doseGrid)
      )

      eff_fun <- efficacyFunction(
        eff_model,
        theta1 = eff_model@theta1,
        theta2 = eff_model@theta2
      )
      eff_fit <- list(
        theta1 = eff_model@theta1,
        theta2 = eff_model@theta2,
        ExpEff = eff_fun(object@data@doseGrid)
      )

      list(
        data = data,
        dose = dose,
        TDtargetDuringTrial = td_target_during_trial,
        TDtargetDuringTrialAtDoseGrid = td_target_during_trial_at_dose_grid,
        TDtargetEndOfTrial = td_target_end_of_trial,
        TDtargetEndOfTrialAtDoseGrid = td_target_end_of_trial_at_dose_grid,
        Gstar = gstar,
        GstarAtDoseGrid = gstar_at_dose_grid,
        Recommend = recommend,
        OptimalDose = optimal_dose,
        OptimalDoseAtDoseGrid = recommend,
        ratio = ratio,
        CI = ci,
        ratioGstar = ratio_gstar,
        CIGstar = ci_gstar,
        ratioTDEOT = ratio_tdeot,
        CITDEOT = ci_tdeot,
        fitDLE = dle_fit,
        fitEff = eff_fit,
        sigma2est = eff_sigma2,
        stop = attr(should_stop, "message"),
        report_results = stopit_results
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
        "trueDLE",
        "trueEff",
        "trueNu",
        "object"
      ),
      parallel = parallel,
      n_cores = nCores
    )

    data_list <- lapply(result_list, "[[", "data")
    recommended_doses <- as.numeric(sapply(result_list, "[[", "Recommend"))

    td_target_during_trial_list <- as.numeric(sapply(
      result_list,
      "[[",
      "TDtargetDuringTrial"
    ))

    td_target_end_of_trial_list <- as.numeric(sapply(
      result_list,
      "[[",
      "TDtargetEndOfTrial"
    ))

    td_target_during_trial_dose_grid_list <- as.numeric(sapply(
      result_list,
      "[[",
      "TDtargetDuringTrialAtDoseGrid"
    ))

    td_target_end_of_trial_dose_grid_list <- as.numeric(sapply(
      result_list,
      "[[",
      "TDtargetEndOfTrialAtDoseGrid"
    ))

    gstar_list <- as.numeric(sapply(result_list, "[[", "Gstar"))
    gstar_at_dose_grid_list <- as.numeric(sapply(
      result_list,
      "[[",
      "GstarAtDoseGrid"
    ))

    optimal_dose_list <- as.numeric(sapply(result_list, "[[", "OptimalDose"))
    optimal_dose_at_dose_grid_list <- as.numeric(sapply(
      result_list,
      "[[",
      "Recommend"
    ))

    ci_list <- lapply(result_list, "[[", "CI")
    ratio_list <- as.numeric(sapply(result_list, "[[", "ratio"))
    ci_tdeot_list <- lapply(result_list, "[[", "CITDEOT")
    ratio_tdeot_list <- as.numeric(sapply(result_list, "[[", "ratioTDEOT"))
    ci_gstar_list <- lapply(result_list, "[[", "CIGstar")
    ratio_gstar_list <- as.numeric(sapply(result_list, "[[", "ratioGstar"))

    fit_dle_list <- lapply(result_list, "[[", "fitDLE")
    fit_eff_list <- lapply(result_list, "[[", "fitEff")
    sigma2_estimates <- as.numeric(sapply(result_list, "[[", "sigma2est"))
    stop_reasons <- lapply(result_list, "[[", "stop")

    stop_results <- lapply(result_list, "[[", "report_results")
    stop_report <- as.matrix(do.call(rbind, stop_results))

    PseudoDualSimulations(
      data = data_list,
      doses = recommended_doses,
      final_td_target_during_trial_estimates = td_target_during_trial_list,
      final_td_target_end_of_trial_estimates = td_target_end_of_trial_list,
      final_td_target_during_trial_at_dose_grid = td_target_during_trial_dose_grid_list,
      final_td_target_end_of_trial_at_dose_grid = td_target_end_of_trial_dose_grid_list,
      final_cis = ci_list,
      final_ratios = ratio_list,
      final_gstar_estimates = gstar_list,
      final_gstar_at_dose_grid = gstar_at_dose_grid_list,
      final_gstar_cis = ci_gstar_list,
      final_gstar_ratios = ratio_gstar_list,
      final_tdeot_cis = ci_tdeot_list,
      final_tdeot_ratios = ratio_tdeot_list,
      final_optimal_dose = optimal_dose_list,
      final_optimal_dose_at_dose_grid = optimal_dose_at_dose_grid_list,
      fit = fit_dle_list,
      fit_eff = fit_eff_list,
      sigma2_est = sigma2_estimates,
      stop_reasons = stop_reasons,
      stop_report = stop_report,
      seed = rng_state
    )
  }
)

## DualResponsesSamplesDesign ----

### h_simulate_flexi ----

h_simulate_flexi <- function(
  object,
  nsim = 1L,
  seed = NULL,
  trueDLE,
  trueEff,
  trueNu = NULL,
  trueSigma2 = NULL,
  trueSigma2betaW = NULL,
  args = NULL,
  firstSeparate = FALSE,
  mcmcOptions = McmcOptions(),
  parallel = FALSE,
  nCores = min(parallel::detectCores(), 5L),
  ...
) {
  stopifnot(
    trueSigma2 > 0,
    trueSigma2betaW > 0,
    is.numeric(trueEff),
    length(trueEff) == length(object@data@doseGrid)
  )

  args <- as.data.frame(args)
  n_args <- max(nrow(args), 1L)

  # Get argument names (excluding the first one which is the dose)
  dle_arg_names <- names(formals(trueDLE))[-1]

  # Seed handling
  rng_state <- set_seed(seed)

  # Generate individual seeds for simulation runs
  sim_seeds <- sample(x = seq_len(1e5), size = as.integer(nsim))

  # Function to run a single simulation with index "iter_sim"
  run_sim <- function(iter_sim) {
    # Set the seed for this run
    set.seed(sim_seeds[iter_sim])

    # Get current arguments (appropriately recycled)
    current_args <- args[(iter_sim - 1) %% n_args + 1, , drop = FALSE]

    # DLE truth function with current arguments
    dle_with_args <- function(dose) {
      do.call(
        trueDLE,
        # First argument: the dose
        c(
          dose,
          # Following arguments
          current_args
        )
      )
    }

    # Efficacy truth function (fixed for EffFlexi)
    eff_truth <- trueEff

    # Start with the provided data
    data <- object@data

    # Trial control variables
    should_stop <- FALSE
    dose <- object@startingDose
    dose_pl <- object@data@doseGrid[1]

    # Start with specified variance parameters
    sigma2 <- trueSigma2
    sigma2_beta_w <- trueSigma2betaW

    # Main simulation loop
    while (!should_stop) {
      # Calculate probabilities and outcomes at current dose
      dle_prob <- dle_with_args(dose)
      dle_prob_pl <- dle_with_args(dose_pl)

      dose_index <- which(dose == data@doseGrid)
      mean_eff <- eff_truth[dose_index]
      mean_eff_pl <- eff_truth[1]

      # Determine cohort size
      cohort_size <- size(object@cohort_size, dose = dose, data = data)

      if (data@placebo) {
        placebo_size <- size(
          object@pl_cohort_size,
          dose = dose,
          data = data
        )
      }

      ## simulate DLTs: depends on whether we
      ## separate the first patient or not.
      if (firstSeparate && (cohort_size > 1L)) {
        ## dose the first patient
        dlts <- rbinom(
          n = 1L,
          size = 1L,
          prob = dle_prob
        )

        if (data@placebo && (placebo_size > 0L)) {
          dlts_pl <- rbinom(
            n = 1L,
            size = 1L,
            prob = dle_prob_pl
          )
        }

        eff_responses <- rnorm(
          n = 1L,
          mean = mean_eff,
          sd = sqrt(trueSigma2)
        )

        if (data@placebo && (placebo_size > 0L)) {
          eff_responses_pl <- rnorm(
            n = 1L,
            mean = mean_eff_pl,
            sd = sqrt(trueSigma2)
          )
        }

        # If no DLT in first patient, enroll remaining patients
        if (dlts == 0) {
          dlts <- c(
            dlts,
            rbinom(
              n = cohort_size - 1L,
              size = 1L,
              prob = dle_prob
            )
          )
          eff_responses <- c(
            eff_responses,
            rnorm(
              n = cohort_size - 1L,
              mean = mean_eff,
              sd = sqrt(trueSigma2)
            )
          )
          if (data@placebo && (placebo_size > 0L)) {
            dlts_pl <- c(
              dlts_pl,
              rbinom(
                n = placebo_size,
                size = 1L,
                prob = dle_prob_pl
              )
            )
            eff_responses_pl <- c(
              eff_responses_pl,
              rnorm(
                n = placebo_size,
                mean = mean_eff_pl,
                sd = sqrt(trueSigma2)
              )
            )
          }
        }
      } else {
        # Dose all patients directly
        dlts <- rbinom(
          n = cohort_size,
          size = 1L,
          prob = dle_prob
        )

        eff_responses <- rnorm(
          n = cohort_size,
          mean = mean_eff,
          sd = sqrt(trueSigma2)
        )
        if (data@placebo && (placebo_size > 0L)) {
          dlts_pl <- rbinom(
            n = placebo_size,
            size = 1L,
            prob = dle_prob_pl
          )
          eff_responses_pl <- rnorm(
            n = placebo_size,
            mean = mean_eff_pl,
            sd = sqrt(trueSigma2)
          )
        }
      }

      ## update the data with this placebo (if any) cohort and then with active dose
      if (data@placebo && (placebo_size > 0L)) {
        data <- update(
          object = data,
          x = object@data@doseGrid[1],
          y = dlts_pl,
          w = eff_responses_pl,
          check = FALSE
        )

        ## update the data with active dose
        data <- update(
          object = data,
          x = dose,
          y = dlts,
          w = eff_responses,
          new_cohort = FALSE
        )
      } else {
        ## update the data with this cohort
        data <- update(
          object = data,
          x = dose,
          y = dlts,
          w = eff_responses
        )
      }

      # Update models with new data
      dle_model <- update(
        object = object@model,
        data = data
      )

      eff_model <- update(
        object = object@eff_model,
        data = data
      )

      # Calculate dose limit
      dose_limit <- maxDose(object@increments, data = data)

      # Generate MCMC samples from both models
      dle_samples <- mcmc(
        data = data,
        model = dle_model,
        options = mcmcOptions
      )

      eff_samples <- mcmc(
        data = data,
        model = eff_model,
        options = mcmcOptions
      )

      # Update variance estimates from MCMC samples
      sigma2 <- mean(eff_samples@data$sigma2W)
      sigma2_beta_w <- mean(eff_samples@data$sigma2betaW)

      # Calculate next best dose
      next_bd <- nextBest(
        object@nextBest,
        doselimit = dose_limit,
        samples = dle_samples,
        model = dle_model,
        model_eff = eff_model,
        samples_eff = eff_samples,
        data = data,
        in_sim = TRUE
      )

      # Extract dose recommendations
      dose <- next_bd$next_dose
      td_target_during_trial <- next_bd$dose_target_drt
      td_target_during_trial_at_dose_grid <- next_bd$next_dose_drt
      td_target_end_of_trial <- next_bd$dose_target_eot
      td_target_end_of_trial_at_dose_grid <- next_bd$next_dose_eot
      gstar <- next_bd$dose_max_gain
      gstar_at_dose_grid <- next_bd$next_dose_max_gain

      recommend <- min(
        td_target_end_of_trial_at_dose_grid,
        gstar_at_dose_grid
      )

      # Calculate 95% confidence intervals and ratios
      ci_tdeot <- list(
        lower = next_bd$ci_dose_target_eot[1],
        upper = next_bd$ci_dose_target_eot[2]
      )
      ratio_tdeot <- next_bd$ci_ratio_dose_target_eot

      ci_gstar <- list(
        lower = next_bd$ci_dose_max_gain[1],
        upper = next_bd$ci_dose_max_gain[2]
      )
      ratio_gstar <- next_bd$ci_ratio_dose_max_gain

      # Find the optimal dose
      optimal_dose <- min(gstar, td_target_end_of_trial)

      if (optimal_dose == gstar) {
        ratio <- ratio_gstar
        ci <- ci_gstar
      } else {
        ratio <- ratio_tdeot
        ci <- ci_tdeot
      }

      # Evaluate stopping rules
      should_stop <- stopTrial(
        object@stopping,
        dose = dose,
        samples = dle_samples,
        model = dle_model,
        data = data,
        TDderive = object@nextBest@derive,
        Effmodel = eff_model,
        Effsamples = eff_samples,
        Gstarderive = object@nextBest@mg_derive
      )
      stop_results <- h_unpack_stopit(should_stop)
    }

    # Calculate final model fits
    dle_fit <- fit(
      object = dle_samples,
      model = dle_model,
      data = data
    )

    eff_fit <- fit(
      object = eff_samples,
      model = eff_model,
      data = data
    )

    # Return simulation results
    list(
      data = data,
      dose = dose,
      TDtargetDuringTrial = td_target_during_trial,
      TDtargetDuringTrialAtDoseGrid = td_target_during_trial_at_dose_grid,
      TDtargetEndOfTrial = td_target_end_of_trial,
      TDtargetEndOfTrialAtDoseGrid = td_target_end_of_trial_at_dose_grid,
      Gstar = gstar,
      GstarAtDoseGrid = gstar_at_dose_grid,
      Recommend = recommend,
      OptimalDose = optimal_dose,
      OptimalDoseAtDoseGrid = recommend,
      ratio = ratio,
      CI = ci,
      ratioGstar = ratio_gstar,
      CIGstar = ci_gstar,
      ratioTDEOT = ratio_tdeot,
      CITDEOT = ci_tdeot,
      fitDLE = subset(dle_fit, select = c(middle, lower, upper)),
      fitEff = subset(eff_fit, select = c(middle, lower, upper)),
      sigma2est = sigma2,
      sigma2betaWest = sigma2_beta_w,
      stop = attr(should_stop, "message"),
      report_results = stop_results
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
      "trueDLE",
      "trueEff",
      "trueSigma2",
      "trueSigma2betaW",
      "object",
      "mcmcOptions"
    ),
    parallel = parallel,
    n_cores = nCores
  )

  # Process simulation results
  data_list <- lapply(result_list, "[[", "data")
  recommended_doses <- as.numeric(sapply(result_list, "[[", "Recommend"))

  # Extract model fits and variance estimates
  fit_dle_list <- lapply(result_list, "[[", "fitDLE")
  fit_eff_list <- lapply(result_list, "[[", "fitEff")
  sigma2_estimates <- as.numeric(sapply(result_list, "[[", "sigma2est"))
  sigma2_beta_w_estimates <- as.numeric(sapply(
    result_list,
    "[[",
    "sigma2betaWest"
  ))

  # Extract TD target estimates
  td_target_during_trial_list <- as.numeric(sapply(
    result_list,
    "[[",
    "TDtargetDuringTrial"
  ))

  td_target_end_of_trial_list <- as.numeric(sapply(
    result_list,
    "[[",
    "TDtargetEndOfTrial"
  ))

  td_target_during_trial_dose_grid_list <- as.numeric(sapply(
    result_list,
    "[[",
    "TDtargetDuringTrialAtDoseGrid"
  ))

  td_target_end_of_trial_dose_grid_list <- as.numeric(sapply(
    result_list,
    "[[",
    "TDtargetEndOfTrialAtDoseGrid"
  ))

  # Extract Gstar and optimal dose estimates
  gstar_list <- as.numeric(sapply(result_list, "[[", "Gstar"))

  gstar_at_dose_grid_list <- as.numeric(sapply(
    result_list,
    "[[",
    "GstarAtDoseGrid"
  ))

  optimal_dose_list <- as.numeric(sapply(result_list, "[[", "OptimalDose"))

  optimal_dose_at_dose_grid_list <- as.numeric(sapply(
    result_list,
    "[[",
    "Recommend"
  ))

  # Extract confidence intervals and ratios
  ci_list <- lapply(result_list, "[[", "CI")

  ratio_list <- as.numeric(sapply(result_list, "[[", "ratio"))

  ci_tdeot_list <- lapply(result_list, "[[", "CITDEOT")

  ratio_tdeot_list <- as.numeric(sapply(result_list, "[[", "ratioTDEOT"))

  ci_gstar_list <- lapply(result_list, "[[", "CIGstar")

  ratio_gstar_list <- as.numeric(sapply(result_list, "[[", "ratioGstar"))

  # Extract stopping information
  stop_reasons <- lapply(result_list, "[[", "stop")

  stop_results <- lapply(result_list, "[[", "report_results")
  stop_report <- as.matrix(do.call(rbind, stop_results))

  # Return simulation results
  PseudoDualFlexiSimulations(
    data = data_list,
    doses = recommended_doses,
    final_td_target_during_trial_estimates = td_target_during_trial_list,
    final_td_target_end_of_trial_estimates = td_target_end_of_trial_list,
    final_td_target_during_trial_at_dose_grid = td_target_during_trial_dose_grid_list,
    final_td_target_end_of_trial_at_dose_grid = td_target_end_of_trial_dose_grid_list,
    final_cis = ci_list,
    final_ratios = ratio_list,
    final_gstar_estimates = gstar_list,
    final_gstar_at_dose_grid = gstar_at_dose_grid_list,
    final_gstar_cis = ci_gstar_list,
    final_gstar_ratios = ratio_gstar_list,
    final_tdeot_cis = ci_tdeot_list,
    final_tdeot_ratios = ratio_tdeot_list,
    final_optimal_dose = optimal_dose_list,
    final_optimal_dose_at_dose_grid = optimal_dose_at_dose_grid_list,
    fit = fit_dle_list,
    fit_eff = fit_eff_list,
    sigma2_est = sigma2_estimates,
    sigma2_beta_w_est = sigma2_beta_w_estimates,
    stop_reasons = stop_reasons,
    stop_report = stop_report,
    seed = rng_state
  )
}

### h_simulate_nonflexi ----

h_simulate_nonflexi <- function(
  object,
  nsim = 1L,
  seed = NULL,
  trueDLE,
  trueEff,
  trueNu = NULL,
  trueSigma2 = NULL,
  trueSigma2betaW = NULL,
  args = NULL,
  firstSeparate = FALSE,
  mcmcOptions = McmcOptions(),
  parallel = FALSE,
  nCores = min(parallel::detectCores(), 5L),
  ...
) {
  stopifnot(
    trueNu > 0,
    is.function(trueEff)
  )

  args <- as.data.frame(args)
  n_args <- max(nrow(args), 1L)

  # Get argument names (excluding the first one which is the dose)
  dle_arg_names <- names(formals(trueDLE))[-1]
  eff_arg_names <- names(formals(trueEff))[-1]

  # Seed handling
  rng_state <- set_seed(seed)

  # Generate individual seeds for simulation runs
  sim_seeds <- sample(x = seq_len(1e5), size = nsim)

  # Function to run a single simulation with index "iter_sim"
  run_sim <- function(iter_sim) {
    # Set the seed for this run
    set.seed(sim_seeds[iter_sim])

    # Get current arguments (appropriately recycled)
    current_args <- args[(iter_sim - 1) %% n_args + 1, , drop = FALSE]

    # DLE truth function with current arguments
    dle_with_args <- function(dose) {
      do.call(
        trueDLE,
        # First argument: the dose
        c(
          dose,
          # Following arguments: take only those that
          # are required by the DLE function
          as.list(current_args)[dle_arg_names]
        )
      )
    }

    # Efficacy truth function with current arguments
    eff_with_args <- function(dose) {
      do.call(
        trueEff,
        # First argument: the dose
        c(
          dose,
          # Following arguments: take only those that
          # are required by the Eff function
          as.list(current_args)[eff_arg_names]
        )
      )
    }

    # Find true sigma2 to generate responses
    true_sigma2 <- 1 / trueNu

    # Start with the provided data
    data <- object@data

    # Trial control variables
    should_stop <- FALSE
    dose <- object@startingDose

    # Main simulation loop
    while (!should_stop) {
      # Calculate probabilities and outcomes at current dose
      dle_prob <- dle_with_args(dose)
      mean_eff <- eff_with_args(dose)

      # Determine cohort size
      cohort_size <- size(object@cohort_size, dose = dose, data = data)

      if (data@placebo) {
        placebo_size <- size(
          object@pl_cohort_size,
          dose = dose,
          data = data
        )
        dose_pl <- data@doseGrid[1]
        dle_prob_pl <- dle_with_args(dose_pl)
        mean_eff_pl <- eff_with_args(dose_pl)
      }

      ## simulate DLTs: depends on whether we
      ## separate the first patient or not.
      if (firstSeparate && (cohort_size > 1L)) {
        # Dose the first patient
        dlts <- rbinom(
          n = 1L,
          size = 1L,
          prob = dle_prob
        )

        if (data@placebo && (placebo_size > 0L)) {
          dlts_pl <- rbinom(
            n = 1L,
            size = 1L,
            prob = dle_prob_pl
          )
        }

        eff_responses <- rnorm(
          n = 1L,
          mean = mean_eff,
          sd = sqrt(true_sigma2)
        )

        if (data@placebo && (placebo_size > 0L)) {
          eff_responses_pl <- rnorm(
            n = 1L,
            mean = mean_eff_pl,
            sd = sqrt(true_sigma2)
          )
        }

        # If there is no DLT, enroll the remaining patients
        if (dlts == 0) {
          dlts <- c(
            dlts,
            rbinom(
              n = cohort_size - 1L,
              size = 1L,
              prob = dle_prob
            )
          )
          eff_responses <- c(
            eff_responses,
            rnorm(
              n = cohort_size - 1L,
              mean = mean_eff,
              sd = sqrt(true_sigma2)
            )
          )

          if (data@placebo && (placebo_size > 0L)) {
            dlts_pl <- c(
              dlts_pl,
              rbinom(
                n = placebo_size,
                size = 1L,
                prob = dle_prob_pl
              )
            )
            eff_responses_pl <- c(
              mean_eff_pl,
              rnorm(
                n = placebo_size,
                mean = mean_eff_pl,
                sd = sqrt(true_sigma2)
              )
            )
          }
        }
      } else {
        # Directly dose all patients
        dlts <- rbinom(
          n = cohort_size,
          size = 1L,
          prob = dle_prob
        )
        eff_responses <- rnorm(
          n = cohort_size,
          mean = mean_eff,
          sd = sqrt(true_sigma2)
        )

        if (data@placebo && (placebo_size > 0L)) {
          dlts_pl <- rbinom(
            n = placebo_size,
            size = 1L,
            prob = dle_prob_pl
          )
          eff_responses_pl <- rnorm(
            n = placebo_size,
            mean = mean_eff_pl,
            sd = sqrt(true_sigma2)
          )
        }
      }

      ## update the data with this placebo (if any) cohort and then with active dose
      if (data@placebo && (placebo_size > 0L)) {
        data <- update(
          object = data,
          x = object@data@doseGrid[1],
          y = dlts_pl,
          w = eff_responses_pl,
          check = FALSE
        )

        # Update the data with active dose
        data <- update(
          object = data,
          x = dose,
          y = dlts,
          w = eff_responses,
          new_cohort = FALSE
        )
      } else {
        # Update the data with this cohort
        data <- update(
          object = data,
          x = dose,
          y = dlts,
          w = eff_responses
        )
      }

      # Update models with new data
      dle_model <- update(
        object = object@model,
        data = data
      )

      eff_model <- update(
        object = object@eff_model,
        data = data
      )

      nu <- eff_model@nu

      dle_samples <- mcmc(
        data = data,
        model = dle_model,
        options = mcmcOptions
      )

      eff_samples <- mcmc(
        data = data,
        model = eff_model,
        options = mcmcOptions
      )

      sigma2 <- if (eff_model@use_fixed) {
        1 / nu
      } else {
        1 / (as.numeric(nu["a"] / nu["b"]))
      }

      # Calculate dose limit
      dose_limit <- maxDose(object@increments, data = data)

      # Calculate next best dose
      next_bd <- nextBest(
        object@nextBest,
        doselimit = dose_limit,
        samples = dle_samples,
        model = dle_model,
        data = data,
        model_eff = eff_model,
        samples_eff = eff_samples,
        in_sim = TRUE
      )

      # Extract dose recommendations
      dose <- next_bd$next_dose
      td_target_during_trial <- next_bd$dose_target_drt
      td_target_during_trial_at_dose_grid <- next_bd$next_dose_drt
      td_target_end_of_trial <- next_bd$dose_target_eot
      td_target_end_of_trial_at_dose_grid <- next_bd$next_dose_eot
      gstar <- next_bd$dose_max_gain
      gstar_at_dose_grid <- next_bd$next_dose_max_gain

      recommend <- min(
        td_target_end_of_trial_at_dose_grid,
        gstar_at_dose_grid
      )

      # Calculate 95% confidence intervals and ratios
      ci_tdeot <- list(
        lower = next_bd$ci_dose_target_eot[1],
        upper = next_bd$ci_dose_target_eot[2]
      )
      ratio_tdeot <- next_bd$ci_ratio_dose_target_eot

      ci_gstar <- list(
        lower = next_bd$ci_dose_max_gain[1],
        upper = next_bd$ci_dose_max_gain[2]
      )
      ratio_gstar <- next_bd$ci_ratio_dose_max_gain

      # Find the optimal dose
      optimal_dose <- min(gstar, td_target_end_of_trial)

      if (optimal_dose == gstar) {
        ratio <- ratio_gstar
        ci <- ci_gstar
      } else {
        ratio <- ratio_tdeot
        ci <- ci_tdeot
      }

      # Evaluate stopping rules
      should_stop <- stopTrial(
        object@stopping,
        dose = dose,
        samples = dle_samples,
        model = dle_model,
        data = data,
        TDderive = object@nextBest@derive,
        Effmodel = eff_model,
        Effsamples = eff_samples,
        Gstarderive = object@nextBest@mg_derive
      )
      stop_results <- h_unpack_stopit(should_stop)
    }
    # Calculate final model fits
    dle_fit <- fit(
      object = dle_samples,
      model = dle_model,
      data = data
    )

    eff_fit <- fit(
      object = eff_samples,
      model = eff_model,
      data = data
    )

    # Return simulation results
    list(
      data = data,
      dose = dose,
      TDtargetDuringTrial = td_target_during_trial,
      TDtargetDuringTrialAtDoseGrid = td_target_during_trial_at_dose_grid,
      TDtargetEndOfTrial = td_target_end_of_trial,
      TDtargetEndOfTrialAtDoseGrid = td_target_end_of_trial_at_dose_grid,
      Gstar = gstar,
      GstarAtDoseGrid = gstar_at_dose_grid,
      Recommend = recommend,
      OptimalDose = optimal_dose,
      OptimalDoseAtDoseGrid = recommend,
      ratio = ratio,
      CI = ci,
      ratioGstar = ratio_gstar,
      CIGstar = ci_gstar,
      ratioTDEOT = ratio_tdeot,
      CITDEOT = ci_tdeot,
      fitDLE = subset(dle_fit, select = c(middle, lower, upper)),
      fitEff = subset(eff_fit, select = c(middle, lower, upper)),
      sigma2est = sigma2,
      stop = attr(
        should_stop,
        "message"
      ),
      report_results = stop_results
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
      "trueDLE",
      "trueEff",
      "trueNu",
      "object"
    ),
    parallel = parallel,
    n_cores = nCores
  )

  # Process simulation results
  data_list <- lapply(result_list, "[[", "data")
  recommended_doses <- as.numeric(sapply(result_list, "[[", "Recommend"))

  # Extract TD target estimates
  td_target_during_trial_list <- as.numeric(sapply(
    result_list,
    "[[",
    "TDtargetDuringTrial"
  ))

  td_target_end_of_trial_list <- as.numeric(sapply(
    result_list,
    "[[",
    "TDtargetEndOfTrial"
  ))

  td_target_during_trial_dose_grid_list <- as.numeric(sapply(
    result_list,
    "[[",
    "TDtargetDuringTrialAtDoseGrid"
  ))

  td_target_end_of_trial_dose_grid_list <- as.numeric(sapply(
    result_list,
    "[[",
    "TDtargetEndOfTrialAtDoseGrid"
  ))

  # Extract Gstar and optimal dose estimates
  gstar_list <- as.numeric(sapply(result_list, "[[", "Gstar"))

  gstar_at_dose_grid_list <- as.numeric(sapply(
    result_list,
    "[[",
    "GstarAtDoseGrid"
  ))

  optimal_dose_list <- as.numeric(sapply(result_list, "[[", "OptimalDose"))

  optimal_dose_at_dose_grid_list <- as.numeric(sapply(
    result_list,
    "[[",
    "Recommend"
  ))

  # Extract confidence intervals and ratios
  ci_list <- lapply(result_list, "[[", "CI")
  ratio_list <- as.numeric(sapply(result_list, "[[", "ratio"))

  ci_tdeot_list <- lapply(result_list, "[[", "CITDEOT")
  ratio_tdeot_list <- as.numeric(sapply(result_list, "[[", "ratioTDEOT"))

  ci_gstar_list <- lapply(result_list, "[[", "CIGstar")
  ratio_gstar_list <- as.numeric(sapply(result_list, "[[", "ratioGstar"))

  # Extract model fits and variance estimates
  fit_dle_list <- lapply(result_list, "[[", "fitDLE")
  fit_eff_list <- lapply(result_list, "[[", "fitEff")
  sigma2_estimates <- as.numeric(sapply(result_list, "[[", "sigma2est"))

  # Extract stopping information
  stop_reasons <- lapply(result_list, "[[", "stop")

  stop_results <- lapply(result_list, "[[", "report_results")
  stop_report <- as.matrix(do.call(rbind, stop_results))

  # Return simulation results
  PseudoDualSimulations(
    data = data_list,
    doses = recommended_doses,
    final_td_target_during_trial_estimates = td_target_during_trial_list,
    final_td_target_end_of_trial_estimates = td_target_end_of_trial_list,
    final_td_target_during_trial_at_dose_grid = td_target_during_trial_dose_grid_list,
    final_td_target_end_of_trial_at_dose_grid = td_target_end_of_trial_dose_grid_list,
    final_cis = ci_list,
    final_ratios = ratio_list,
    final_gstar_estimates = gstar_list,
    final_gstar_at_dose_grid = gstar_at_dose_grid_list,
    final_gstar_cis = ci_gstar_list,
    final_gstar_ratios = ratio_gstar_list,
    final_tdeot_cis = ci_tdeot_list,
    final_tdeot_ratios = ratio_tdeot_list,
    final_optimal_dose = optimal_dose_list,
    final_optimal_dose_at_dose_grid = optimal_dose_at_dose_grid_list,
    fit = fit_dle_list,
    fit_eff = fit_eff_list,
    sigma2_est = sigma2_estimates,
    stop_reasons = stop_reasons,
    stop_report = stop_report,
    seed = rng_state
  )
}

### method definition ----

#' Simulate dose escalation procedure using DLE and efficacy responses with samples
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This is a method to simulate dose escalation procedure using both DLE and efficacy responses.
#' This is a method based on the [`DualResponsesSamplesDesign`] where DLE model used are of
#' [`ModelTox`] class object and efficacy model used are of [`ModelEff`]
#' class object (special case is [`EffFlexi`] class model object).
#' In addition, DLE and efficacy samples are involved or generated in the simulation
#' process.
#'
#' @param object the [`DualResponsesSamplesDesign`] object we want to
#'   simulate the data from
#' @param nsim (`count`)\cr the number of simulations (default: 1)
#' @param seed see [set_seed()]
#' @param trueDLE (`function`)\cr a function which takes as input a dose (vector) and returns the true probability
#'   (vector) of the occurrence of a DLE. Additional arguments can be supplied in `args`.
#' @param trueEff (`function`)\cr a function which takes as input a dose (vector) and returns the expected
#'   efficacy responses (vector). Additional arguments can be supplied in `args`.
#' @param trueNu (`number`)\cr (not with [`EffFlexi`]) the precision, the inverse of the
#'   variance of the efficacy responses
#' @param trueSigma2 (`number`)\cr (only with [`EffFlexi`]) the true variance of the efficacy
#'   responses which must be a single positive scalar.
#' @param trueSigma2betaW (`number`)\cr (only with [`EffFlexi`]) the true variance for the
#'   random walk model used for smoothing. This must be a single positive scalar.
#' @param args (`data.frame`)\cr data frame with arguments for the `trueDLE` and
#'   `trueEff` function. The column names correspond to the argument
#'   names, the rows to the values of the arguments. The rows are appropriately
#'   recycled in the `nsim` simulations.
#' @param firstSeparate (`flag`)\cr enroll the first patient separately from the rest of
#'   the cohort? (not default) If yes, the cohort will be closed if a DLT occurs
#'   in this patient.
#' @param mcmcOptions ([McmcOptions])\cr object of class [`McmcOptions`],
#'   giving the MCMC options for each evaluation in the trial. By default,
#'   the standard options are used
#' @param parallel (`flag`)\cr should the simulation runs be parallelized across the
#'   clusters of the computer? (not default)
#' @param nCores (`count`)\cr how many cores should be used for parallel computing?
#'   Defaults to the number of cores on the machine, maximum 5.
#' @param ... not used
#'
#' @return an object of class [`PseudoDualSimulations`] or
#'   [`PseudoDualFlexiSimulations`]
#'
#' @example examples/design-method-simulateDualResponsesSamplesDesign.R
#' @export
setMethod(
  f = "simulate",
  signature = signature(
    object = "DualResponsesSamplesDesign",
    nsim = "ANY",
    seed = "ANY"
  ),
  definition = function(
    object,
    nsim = 1L,
    seed = NULL,
    trueDLE,
    trueEff,
    trueNu = NULL,
    trueSigma2 = NULL,
    trueSigma2betaW = NULL,
    args = NULL,
    firstSeparate = FALSE,
    mcmcOptions = McmcOptions(),
    parallel = FALSE,
    nCores = min(parallel::detectCores(), 5L),
    ...
  ) {
    # Common checks and validations
    assert_function(trueDLE)
    assert_flag(firstSeparate)
    assert_count(nsim, positive = TRUE)
    assert_flag(parallel)
    assert_count(nCores, positive = TRUE)

    # Check if special case applies
    is_flexi <- is(object@eff_model, "EffFlexi")

    # Dispatch to appropriate helper based on model type
    if (is_flexi) {
      h_simulate_flexi(
        object = object,
        nsim = nsim,
        seed = seed,
        trueDLE = trueDLE,
        trueEff = trueEff,
        trueSigma2 = trueSigma2,
        trueSigma2betaW = trueSigma2betaW,
        args = args,
        firstSeparate = firstSeparate,
        mcmcOptions = mcmcOptions,
        parallel = parallel,
        nCores = nCores
      )
    } else {
      h_simulate_nonflexi(
        object = object,
        nsim = nsim,
        seed = seed,
        trueDLE = trueDLE,
        trueEff = trueEff,
        trueNu = trueNu,
        args = args,
        firstSeparate = firstSeparate,
        mcmcOptions = mcmcOptions,
        parallel = parallel,
        nCores = nCores
      )
    }
  }
)

## DADesign ----

#' Simulate outcomes from a time-to-DLT augmented CRM design
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This method simulates dose escalation trials using time-to-DLT data,
#' where the timing of dose-limiting toxicities is explicitly modeled.
#'
#' @param object the [`DADesign`] object we want to simulate data from
#' @param nsim (`count`)\cr the number of simulations (default: 1)
#' @param seed see [set_seed()]
#' @param truthTox (`function`)\cr a function which takes as input a dose (vector) and returns the
#'   true probability (vector) for toxicity and the time DLT occurs. Additional
#'   arguments can be supplied in `args`.
#' @param truthSurv (`function`)\cr a CDF which takes as input a time (vector) and returns
#'   the true cumulative probability (vector) that the DLT would occur conditioning on the patient
#'   has DLTs.
#' @param trueTmax (`number` or `NULL`)\cr the true maximum time at which DLTs can occur.
#'   Note that this must be larger than `Tmax` from the `object`'s base data, which is
#'   the length of the DLT window, i.e. until which time DLTs are officially declared
#'   as such and used in the trial.
#' @param args (`data.frame`)\cr data frame with arguments for the `truthTox` function. The
#'   column names correspond to the argument names, the rows to the values of the
#'   arguments. The rows are appropriately recycled in the `nsim`
#'   simulations. In order to produce outcomes from the posterior predictive
#'   distribution, e.g, pass an `object` that contains the data observed so
#'   far, `truthTox` contains the `prob` function from the model in
#'   `object`, and `args` contains posterior samples from the model.
#' @param firstSeparate (`flag`)\cr enroll the first patient separately from the rest of
#'   the cohort? (not default) If yes, the cohort will be closed if a DLT occurs
#'   in this patient.
#' @param deescalate (`flag`)\cr allow deescalation when a DLT occurs in cohorts with lower dose
#'   level? (default: TRUE)
#' @param mcmcOptions ([McmcOptions])\cr object of class [`McmcOptions`],
#'   giving the MCMC options for each evaluation in the trial. By default,
#'   the standard options are used.
#' @param DA (`flag`)\cr use dose-adaptation rules? (default: TRUE)
#' @param parallel (`flag`)\cr should the simulation runs be parallelized across the
#'   clusters of the computer? (not default)
#' @param nCores (`count`)\cr how many cores should be used for parallel computing?
#'   Defaults to the number of cores on the machine, maximum 5.
#' @param derive (`list`)\cr a named list of functions which derives statistics, based on the
#'   vector of posterior MTD samples. Each list element must therefore accept
#'   one and only one argument, which is a numeric vector, and return a number.
#' @param ... not used
#'
#' @return an object of class [`Simulations`]
#'
#' @example examples/design-method-simulate-DADesign.R
#' @export
setMethod(
  f = "simulate",
  signature = signature(
    object = "DADesign",
    nsim = "ANY",
    seed = "ANY"
  ),
  definition = function(
    object,
    nsim = 1L,
    seed = NULL,
    truthTox,
    truthSurv,
    trueTmax = NULL,
    args = NULL,
    firstSeparate = FALSE,
    deescalate = TRUE,
    mcmcOptions = McmcOptions(),
    DA = TRUE,
    parallel = FALSE,
    nCores = min(parallel::detectCores(), 5),
    derive = list(),
    ...
  ) {
    # Validate inputs
    assert_function(truthTox)
    assert_function(truthSurv)
    assert_flag(firstSeparate)
    assert_count(nsim, positive = TRUE)
    assert_flag(parallel)
    assert_count(nCores, positive = TRUE)

    args <- as.data.frame(args)
    n_args <- max(nrow(args), 1L)

    # Seed handling
    rng_state <- set_seed(seed)

    # Generate individual seeds for simulation runs
    sim_seeds <- sample(x = seq_len(1e5), size = as.integer(nsim))

    # Define inverse function for DLT survival generation.
    inverse <- function(f, lower = -100, upper = 100) {
      function(y) {
        uniroot((function(x) f(x) - y), lower = lower, upper = upper)[1]$root
      }
    }

    # Get DLT window length.
    data <- object@data
    t_max <- data@Tmax

    if (is.null(trueTmax)) {
      trueTmax <- t_max
    } else if (trueTmax < t_max) {
      warning("trueTmax < Tmax! trueTmax is set to Tmax")
      trueTmax <- t_max
    }

    # Calculate the inverse function of survival to DLT CDF.
    inverse_truth_surv <- inverse(truthSurv, 0, trueTmax)

    # Generate random survival times for DLT data.
    # Returns t_max when no DLT occurs.
    generate_surv_times <- function(
      dlt,
      t_max,
      inverse_surv = inverse_truth_surv
    ) {
      surv_times <- rep(-100, length(dlt))

      if (sum(dlt == 0) > 0) {
        surv_times[dlt == 0] <- t_max
      }

      if (sum(dlt == 1) > 0) {
        surv_times[dlt == 1] <- unlist(lapply(
          runif(sum(dlt == 1), 0, 1),
          inverse_surv
        ))
      }

      # Ensure results are always positive.
      surv_times[surv_times <= 0] <- 0.05
      surv_times
    }

    # Check if follow-up requirements are fulfilled for opening next cohort.
    ready_to_open <- function(day, window, surv_times) {
      cohort_size <- length(surv_times)
      # Calculate when patients start.
      start_time <- apply(
        rbind(surv_times[-cohort_size], window$patientGap[-1]),
        2,
        min
      )
      # Calculate relative time for each patient on the specified day.
      individual_check <- day - cumsum(c(0, start_time))
      # Ensure minimum is 0.
      individual_check[individual_check < 0] <- 0
      follow_up <- apply(rbind(surv_times, individual_check), 2, min)

      all(
        (follow_up -
          apply(rbind(window$patientFollow, surv_times), 2, min)) >=
          0
      ) &
        (max(follow_up) >= min(window$patientFollowMin, max(surv_times)))
    }

    # Determine when to open the next cohort.
    # Assumes sufficient patients are available for immediate enrollment.
    next_open <- function(window, surv_times) {
      cohort_size <- length(surv_times)

      window$patientGap <- window$patientGap[1:cohort_size]
      # If DLT happens before end of DLT window, next cohort opens earlier.
      start_time <- apply(
        rbind(surv_times[-cohort_size], window$patientGap[-1]),
        2,
        min
      )
      # Duration until all DLT windows finished.
      max_time <- max(surv_times + cumsum(c(0, start_time)))

      requirements_met <- sapply(1:max_time, function(i) {
        ready_to_open(i, window, surv_times)
      })
      if (sum(requirements_met) > 0) {
        # Earliest time that requirements are met.
        time <- min(c(1:max_time)[requirements_met])
      } else {
        time <- max_time
      }
      time
    }

    # Function to run a single simulation.
    run_sim <- function(iter_sim) {
      # Set the seed for this run.
      set.seed(sim_seeds[iter_sim])

      # Get current arguments (appropriately recycled).
      current_args <- args[(iter_sim - 1) %% n_args + 1, , drop = FALSE]

      # Truth function with current arguments.
      truth_with_args <- function(dose) {
        do.call(
          truthTox,
          c(
            dose,
            current_args
          )
        )
      }

      # Start with the provided data.
      data <- object@data

      # Handle placebo if present.
      if (data@placebo) {
        prob_pl <- truth_with_args(object@data@doseGrid[1])
      }

      # Trial control variables.
      should_stop <- FALSE
      trial_time <- 0

      # Initialize observed DLT data.
      observed_dlts <- data@y
      observed_surv <- data@u
      observed_t0 <- data@t0

      # Initialize with starting dose.
      dose <- object@startingDose

      # Main simulation loop.
      while (!should_stop) {
        # Calculate toxicity probability at current dose.
        prob <- truth_with_args(dose)

        # Determine cohort size.
        cohort_size <- size(object@cohort_size, dose = dose, data = data)

        if (data@placebo) {
          placebo_size <- size(
            object@pl_cohort_size,
            dose = dose,
            data = data
          )
        }

        total_size <- if (data@placebo) {
          cohort_size + placebo_size
        } else {
          cohort_size
        }

        safety_window <- windowLength(object@safetyWindow, total_size)

        # Simulate DLTs for cohort.
        # If any patient has DLT before first patient finishes staggered window,
        # further enrollment will be stopped.
        h_generate_dlt_and_surv <- function(n, prob, start = NULL) {
          dlts <- rbinom(
            n = n,
            size = 1L,
            prob = prob
          )
          surv_times <- ceiling(generate_surv_times(
            dlts,
            trueTmax,
            inverse_surv = inverse_truth_surv
          ))

          if (!is.null(start)) {
            dlts <- c(start$dlts, dlts)
            surv_times <- c(start$surv, surv_times)
          }

          if (t_max < trueTmax) {
            dlts[dlts == 1 & surv_times > t_max] <- 0

            surv_times <- apply(
              rbind(surv_times, rep(t_max, length(surv_times))),
              2,
              min
            )
          }

          list(dlts = dlts, surv = surv_times)
        }

        # Update data with active and placebo cohorts.
        h_update_data_da <- function(active, placebo, time) {
          result <- update(
            object = data,
            y = c(observed_dlts, active$dlts),
            u = c(observed_surv, active$surv),
            t0 = c(observed_t0, cohort_t0),
            x = dose,
            trialtime = time
          )

          if (data@placebo) {
            result <- update(
              object = result,
              y = c(observed_dlts, active$dlts, placebo$dlts),
              u = c(observed_surv, active$surv, placebo$surv),
              t0 = c(
                observed_t0,
                cohort_t0,
                rep(cohort_t0[1], length(placebo$dlts))
              ),
              x = object@data@doseGrid[1],
              trialtime = time
            )
          }

          result
        }

        if (firstSeparate && (cohort_size > 1L)) {
          # Dose the first patient.
          active_dlt_surv <- h_generate_dlt_and_surv(1L, prob)
          placebo_dlt_surv <- if (data@placebo && (placebo_size > 0L)) {
            # If placebo, also dose one placebo patient.
            h_generate_dlt_and_surv(1L, prob_pl)
          } else {
            list()
          }

          cohort_t0 <- trial_time

          # Check if there are DLTs during safety window.
          temp_data <- h_update_data_da(
            active_dlt_surv,
            placebo_dlt_surv,
            trial_time + safety_window$patientGap[2]
          )
          temp_time <- (temp_data@u + temp_data@t0)[
            temp_data@y == 1 & temp_data@x <= dose
          ]

          # If no DLTs occur during safety window, enroll remaining patients.
          if (sum(temp_time > trial_time) == 0) {
            # Enroll the remaining patients.
            active_dlt_surv <- h_generate_dlt_and_surv(
              cohort_size - 1L,
              prob,
              start = active_dlt_surv
            )
            placebo_dlt_surv <- if (data@placebo && (placebo_size > 1L)) {
              h_generate_dlt_and_surv(
                placebo_size - 1L,
                prob_pl,
                start = placebo_dlt_surv
              )
            } else {
              list()
            }

            # Adjust for DLTs happening before end of safety window.
            real_window <- apply(
              rbind(
                c(active_dlt_surv$surv, placebo_dlt_surv$surv)[-cohort_size],
                safety_window$patientGap[-1]
              ),
              2,
              min
            )

            cohort_t0 <- trial_time + c(0, cumsum(real_window))
          }

          rm(temp_data)
          rm(temp_time)
        } else {
          # Directly dose all patients.
          active_dlt_surv <- h_generate_dlt_and_surv(
            cohort_size,
            prob
          )
          placebo_dlt_surv <- if (data@placebo) {
            h_generate_dlt_and_surv(
              placebo_size,
              prob_pl
            )
          } else {
            list()
          }

          # Adjust for DLTs happening before end of safety window.
          real_window <- apply(
            rbind(
              c(active_dlt_surv$surv, placebo_dlt_surv$surv)[-cohort_size],
              safety_window$patientGap[-1]
            ),
            2,
            min
          )

          cohort_t0 <- trial_time + c(0, cumsum(real_window))
        }

        # Update observed data with new cohort.
        old_dlts <- observed_dlts

        observed_dlts <- c(
          observed_dlts,
          placebo_dlt_surv$dlts,
          active_dlt_surv$dlts
        )

        observed_surv <- c(
          observed_surv,
          placebo_dlt_surv$surv,
          active_dlt_surv$surv
        )

        observed_t0 <- c(
          observed_t0,
          rep(cohort_t0[1], length(placebo_dlt_surv$dlts)),
          rep(cohort_t0, length.out = length(active_dlt_surv$dlts))
        )

        time_to_next <- next_open(
          window = safety_window,
          surv_times = c(placebo_dlt_surv$surv, active_dlt_surv$surv)
        )

        # Handle deescalation if DLTs occur in previous cohorts.
        if (deescalate == TRUE) {
          are_dlts_after_trial_start <- (observed_surv + observed_t0) >
            trial_time
          are_dlts_before_open_next_cohort <- (observed_surv +
            observed_t0 -
            trial_time) <=
            time_to_next
          are_dlts_happening <- observed_dlts == 1
          is_new_dlt <- (are_dlts_after_trial_start &
            are_dlts_before_open_next_cohort &
            are_dlts_happening)

          new_dlt_ids <- seq_along(observed_dlts)[is_new_dlt]
          last_id_previous_cohort <- length(old_dlts)
          is_new_dlt_in_previous_cohort <- new_dlt_ids <=
            last_id_previous_cohort

          new_dlt_ids <- new_dlt_ids[is_new_dlt_in_previous_cohort]

          if (length(new_dlt_ids) > 0) {
            for (this_new_dlt_id in new_dlt_ids) {
              this_new_dlt_time <- (observed_surv + observed_t0)[
                this_new_dlt_id
              ]

              # Identify patients at higher doses who are impacted.
              later_ids <- c(this_new_dlt_id:length(observed_dlts))
              all_doses <- c(data@x, rep(dose, length(active_dlt_surv$dlts)))
              this_new_dlt_dose <- all_doses[this_new_dlt_id]
              is_dose_higher_than_this_new_dlt_dose <- all_doses[later_ids] >
                this_new_dlt_dose
              ids_to_deescalate <- later_ids[
                is_dose_higher_than_this_new_dlt_dose
              ]

              if (length(ids_to_deescalate) > 0) {
                # DLT will be observed once follow-up time >= time to DLT.
                this_new_dlt_time_after_followup <- this_new_dlt_time >=
                  (observed_t0[ids_to_deescalate] +
                    observed_surv[ids_to_deescalate])
                observed_dlts[ids_to_deescalate] <- as.integer(
                  observed_dlts[ids_to_deescalate] *
                    this_new_dlt_time_after_followup
                )

                # Some patients in later cohorts may not be enrolled yet when new DLT occurs.
                # Remove those patients from the cohort.
                ids_not_enrolled <- ids_to_deescalate[
                  (observed_t0[ids_to_deescalate] >= this_new_dlt_time)
                ]

                ids_enrolled <- setdiff(
                  ids_to_deescalate,
                  ids_not_enrolled
                )

                # Update DLT-free survival time for already enrolled patients.
                if (length(ids_enrolled) > 0) {
                  surv_time <- pmin(
                    observed_surv[ids_enrolled],
                    this_new_dlt_time - observed_t0[ids_enrolled]
                  )
                  assert_true(all(surv_time >= 0))

                  observed_surv[ids_enrolled] <- surv_time
                }

                # Remove patients not yet enrolled.
                if (length(ids_not_enrolled) > 0) {
                  observed_surv <- observed_surv[-ids_not_enrolled]
                  observed_t0 <- observed_t0[-ids_not_enrolled]
                  observed_dlts <- observed_dlts[-ids_not_enrolled]
                }
              }
            }

            time_to_next <- min(
              time_to_next,
              max((observed_surv + observed_t0)[
                (length(old_dlts) + 1):length(observed_dlts)
              ]) -
                trial_time
            )
          }
        }

        # Update trial time.
        trial_time <- trial_time + time_to_next

        # Update data object with observations available when next cohort opens.
        if (data@placebo) {
          # First patients are from placebo.
          data <- update(
            object = data,
            y = head(observed_dlts, -length(active_dlt_surv$dlts)),
            u = head(observed_surv, -length(active_dlt_surv$surv)),
            t0 = head(observed_t0, -length(active_dlt_surv$surv)),
            x = object@data@doseGrid[1],
            trialtime = trial_time
          )
        }
        data <- update(
          object = data,
          y = observed_dlts,
          u = observed_surv,
          t0 = observed_t0,
          x = dose,
          trialtime = trial_time
        )

        try(
          if (
            length(data@x) != length(data@u) ||
              length(data@u) != length(data@y)
          ) {
            stop("x,y,u dimension error")
          }
        )

        # Calculate dose limit.
        dose_limit <- maxDose(object@increments, data = data)

        # Generate MCMC samples from model.
        if (DA == TRUE) {
          samples <- mcmc(
            data = data,
            model = object@model,
            options = mcmcOptions
          )
        } else if (DA == FALSE) {
          temp_model <- LogisticLogNormal(
            mean = object@model@params@mean,
            cov = object@model@params@cov,
            ref_dose = object@model@refDose
          )

          truncated_data <- Data(
            x = data@x,
            y = data@y,
            doseGrid = data@doseGrid,
            cohort = data@cohort,
            ID = data@ID
          )

          samples <- mcmc(
            data = truncated_data,
            model = temp_model,
            options = mcmcOptions
          )
        }

        # Calculate next best dose.
        dose <- nextBest(
          object@nextBest,
          doselimit = dose_limit,
          samples = samples,
          model = object@model,
          data = data
        )$value

        # Evaluate stopping rules.
        should_stop <- stopTrial(
          object@stopping,
          dose = dose,
          samples = samples,
          model = object@model,
          data = data
        )
        stop_results <- h_unpack_stopit(should_stop)
      }

      # Calculate final model fit.
      fit_result <- fit(
        object = samples,
        model = object@model,
        data = data
      )

      # Get MTD estimate from samples.
      target_dose_samples <- dose(
        mean(object@nextBest@target),
        model = object@model,
        samples = samples
      )

      # Calculate additional statistics.
      additional_stats <- lapply(derive, function(f) f(target_dose_samples))

      # Return simulation results.
      list(
        data = data,
        dose = dose,
        duration = trial_time,
        fit = subset(fit_result, select = c(middle, lower, upper)),
        stop = attr(
          should_stop,
          "message"
        ),
        report_results = stop_results,
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
        "truthTox",
        "truthSurv",
        "object",
        "mcmcOptions",
        "next_open",
        "ready_to_open"
      ),
      parallel = parallel,
      n_cores = nCores
    )

    # Process simulation results.
    data_list <- lapply(result_list, "[[", "data")
    recommended_doses <- as.numeric(sapply(result_list, "[[", "dose"))
    trial_duration <- as.numeric(sapply(result_list, "[[", "duration"))
    fit_list <- lapply(result_list, "[[", "fit")

    stop_reasons <- lapply(result_list, "[[", "stop")

    stop_results <- lapply(result_list, "[[", "report_results")
    stop_report <- as.matrix(do.call(rbind, stop_results))

    additional_stats <- lapply(result_list, "[[", "additional_stats")

    # Return simulation results.
    DASimulations(
      data = data_list,
      doses = recommended_doses,
      fit = fit_list,
      trial_duration = trial_duration,
      stop_report = stop_report,
      stop_reasons = stop_reasons,
      additional_stats = additional_stats,
      seed = rng_state
    )
  }
)

## DesignGrouped ----

#' Simulate Method for the [`DesignGrouped`] Class
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A simulate method for [`DesignGrouped`] designs.
#'
#' @param object (`DesignGrouped`)\cr the design we want to simulate trials from.
#' @param nsim (`number`)\cr how many trials should be simulated.
#' @param seed (`RNGstate`)\cr generated with [set_seed()].
#' @param truth (`function`)\cr a function which takes as input a dose (vector) and
#'   returns the true probability (vector) for toxicity for the mono arm.
#'   Additional arguments can be supplied in `args`.
#' @param combo_truth (`function`)\cr same as `truth` but for the combo arm.
#' @param args (`data.frame`)\cr optional `data.frame` with arguments that work
#'   for both the `truth` and `combo_truth` functions. The column names correspond to
#'   the argument names, the rows to the values of the arguments. The rows are
#'   appropriately recycled in the `nsim` simulations.
#' @param firstSeparate (`flag`)\cr whether to enroll the first patient separately
#'   from the rest of the cohort and close the cohort in case a DLT occurs in this
#'   first patient.
#' @param mcmcOptions (`McmcOptions`)\cr MCMC options for each evaluation in the trial.
#' @param parallel (`flag`)\cr whether the simulation runs are parallelized across the
#'   cores of the computer.
#' @param nCores (`number`)\cr how many cores should be used for parallel computing.
#' @param ... not used.
#'
#' @return A list of `mono` and `combo` simulation results as [`Simulations`] objects.
#'
#' @aliases simulate-DesignGrouped
#' @export
#' @example examples/Design-method-simulate-DesignGrouped.R
#'
setMethod(
  "simulate",
  signature = signature(
    object = "DesignGrouped",
    nsim = "ANY",
    seed = "ANY"
  ),
  def = function(
    object,
    nsim = 1L,
    seed = NULL,
    truth,
    combo_truth,
    args = data.frame(),
    firstSeparate = FALSE,
    mcmcOptions = McmcOptions(),
    parallel = FALSE,
    nCores = min(parallelly::availableCores(), 5),
    ...
  ) {
    nsim <- as.integer(nsim)
    assert_function(truth)
    assert_function(combo_truth)
    assert_data_frame(args)
    assert_count(nsim, positive = TRUE)
    assert_flag(firstSeparate)
    assert_flag(parallel)
    assert_count(nCores, positive = TRUE)

    n_args <- max(nrow(args), 1L)
    rng_state <- set_seed(seed)
    sim_seeds <- sample.int(n = 2147483647, size = nsim)

    run_sim <- function(iter_sim) {
      set.seed(sim_seeds[iter_sim])
      current <- list(mono = list(), combo = list())
      # Define true toxicity functions.
      current$args <- args[(iter_sim - 1) %% n_args + 1, , drop = FALSE]
      current$mono$truth <- function(dose) do.call(truth, c(dose, current$args))
      current$combo$truth <- function(dose) {
        do.call(combo_truth, c(dose, current$args))
      }
      # Start the simulated data with the provided one.
      current$mono$data <- object@mono@data
      current$combo$data <- object@combo@data
      # We are in the first cohort and continue for mono and combo.
      current$first <- TRUE
      current$mono$stop <- current$combo$stop <- FALSE

      # What are the next doses to be used? Initialize with starting doses.
      if (
        object@same_dose_for_all ||
          (!object@first_cohort_mono_only && object@same_dose_for_start)
      ) {
        current$mono$dose <- current$combo$dose <- min(
          object@mono@startingDose,
          object@combo@startingDose
        )
      } else {
        current$mono$dose <- object@mono@startingDose
        current$combo$dose <- object@combo@startingDose
      }

      # Inside this loop we simulate the whole trial, until stopping.
      while (!(current$mono$stop && current$combo$stop)) {
        if (!current$mono$stop) {
          cohort_size_mono <- size(
            object@mono@cohort_size,
            dose = current$mono$dose,
            data = current$mono$data
          )
          this_prob_mono <- current$mono$truth(current$mono$dose)
          current$mono$data <- current$mono$data %>%
            h_determine_dlts(
              dose = current$mono$dose,
              prob = this_prob_mono,
              cohort_size = cohort_size_mono,
              first_separate = firstSeparate
            )
        }
        if (
          !current$combo$stop &&
            (!current$first || !object@first_cohort_mono_only)
        ) {
          cohort_size_combo <- size(
            object@combo@cohort_size,
            dose = current$combo$dose,
            data = current$combo$data
          )
          this_prob_combo <- current$combo$truth(current$combo$dose)
          current$combo$data <- current$combo$data %>%
            h_determine_dlts(
              dose = current$combo$dose,
              prob = this_prob_combo,
              cohort_size = cohort_size_combo,
              first_separate = firstSeparate
            )
        }

        current$grouped <- h_group_data(current$mono$data, current$combo$data)
        current$samples <- mcmc(current$grouped, object@model, mcmcOptions)
        if (!current$mono$stop) {
          current$mono$limit <- maxDose(
            object@mono@increments,
            data = current$mono$data
          )
          current$mono$dose <- object@mono@nextBest %>%
            nextBest(
              current$mono$limit,
              current$samples,
              object@model,
              current$grouped,
              group = "mono"
            )
          current$mono$dose <- current$mono$dose$value
        }
        if (
          !current$combo$stop &&
            (!current$first || !object@first_cohort_mono_only)
        ) {
          current$combo$limit <- if (is.na(current$mono$dose)) {
            0
          } else {
            maxDose(object@combo@increments, current$combo$data) %>%
              min(current$mono$dose, na.rm = TRUE)
          }
          current$combo$dose <- object@combo@nextBest %>%
            nextBest(
              current$combo$limit,
              current$samples,
              object@model,
              current$grouped,
              group = "combo"
            )
          current$combo$dose <- current$combo$dose$value
          current$combo$stop <- object@combo@stopping %>%
            stopTrial(
              current$combo$dose,
              current$samples,
              object@model,
              current$combo$data,
              group = "combo"
            )
          current$combo$results <- h_unpack_stopit(current$combo$stop)
        }
        if (!current$mono$stop) {
          current$mono$stop <- object@mono@stopping %>%
            stopTrial(
              current$mono$dose,
              current$samples,
              object@model,
              current$mono$data,
              group = "mono",
              external = current$combo$stop
            )
          current$mono$results <- h_unpack_stopit(current$mono$stop)
        }
        if (
          object@same_dose_for_all && !current$mono$stop && !current$combo$stop
        ) {
          current$mono$dose <- current$combo$dose <- min(
            current$mono$dose,
            current$combo$dose
          )
        }
        if (current$first) {
          current$first <- FALSE
          if (object@first_cohort_mono_only && object@same_dose_for_start) {
            current$mono$dose <- current$combo$dose <- min(
              current$mono$dose,
              current$combo$dose
            )
          }
        }
      }
      current$mono$fit <- fit(
        current$samples,
        object@model,
        current$grouped,
        group = "mono"
      )
      current$combo$fit <- fit(
        current$samples,
        object@model,
        current$grouped,
        group = "combo"
      )
      lapply(
        X = current[c("mono", "combo")],
        FUN = with,
        list(
          data = data,
          dose = dose,
          fit = subset(fit, select = -dose),
          stop = attr(stop, "message"),
          results = results
        )
      )
    }
    vars_needed <- c(
      "simSeeds",
      "args",
      "nArgs",
      "truth",
      "combo_truth",
      "firstSeparate",
      "object",
      "mcmcOptions"
    )

    result_list <- get_result_list(run_sim, nsim, vars_needed, parallel, nCores)
    # Now we have a list with each element containing mono and combo. Reorder this a bit:
    result_list <- list(
      mono = lapply(result_list, "[[", "mono"),
      combo = lapply(result_list, "[[", "combo")
    )
    # Put everything in a list with both mono and combo Simulations:
    lapply(result_list, function(this_list) {
      data_list <- lapply(this_list, "[[", "data")
      recommended_doses <- as.numeric(sapply(this_list, "[[", "dose"))
      fit_list <- lapply(this_list, "[[", "fit")
      stop_reasons <- lapply(this_list, "[[", "stop")
      report_results <- lapply(this_list, "[[", "results")
      stop_report <- as.matrix(do.call(rbind, report_results))
      additional_stats <- lapply(this_list, "[[", "additional_stats")

      Simulations(
        data = data_list,
        doses = recommended_doses,
        fit = fit_list,
        stop_reasons = stop_reasons,
        stop_report = stop_report,
        additional_stats = additional_stats,
        seed = rng_state
      )
    })
  }
)

# examine ----

#' Obtain Hypothetical Trial Course Table for a Design
#'
#' This generic function takes a design and generates a `data.frame`
#' showing the beginning of several hypothetical trial courses under
#' the design. This means, from the generated `data.frame` one can read off:
#'
#' - how many cohorts are required in the optimal case (no DLTs observed) in
#'   order to reach the highest dose of the specified dose grid (or until
#'   the stopping rule is fulfilled)
#' - assuming no DLTs are observed until a certain dose level, what the next
#'   recommended dose is for all possible number of DLTs observed
#' - the actual relative increments that will be used in these cases
#' - whether the trial would stop at a certain cohort
#'
#' Examining the "single trial" behavior of a dose escalation design is
#' the first important step in evaluating a design, and cannot be replaced by
#' studying solely the operating characteristics in "many trials". The cohort
#' sizes are also taken from the design, assuming no DLTs occur until the dose
#' listed.
#'
#' @param object ([`Design`] or [`RuleDesign`])\cr the design we want to examine
#' @param ... additional arguments (see methods)
#' @param maxNoIncrement maximum number of contiguous next doses at 0
#'   DLTs that are the same as before, i.e. no increment (default to 100)
#'
#' @return The data frame
#'
#' @export
#' @keywords methods regression
setGeneric(
  "examine",
  def = function(object, ..., maxNoIncrement = 100L) {
    assert_count(maxNoIncrement, positive = TRUE)
    standardGeneric("examine")
  },
  valueClass = "data.frame"
)

## Design ----

#' @describeIn examine Examine a model-based CRM.
#'
#' @param mcmcOptions ([`McmcOptions`])\cr giving the MCMC options
#'   for each evaluation in the trial. By default, the standard options are used.
#'
#' @example examples/design-method-examine-Design.R
setMethod(
  "examine",
  signature = signature(object = "Design"),
  def = function(object, mcmcOptions = McmcOptions(), ..., maxNoIncrement) {
    ret <- data.frame(
      dose = numeric(),
      DLTs = integer(),
      nextDose = numeric(),
      stop = logical(),
      increment = integer()
    )
    base_data <- object@data

    should_stop <- FALSE

    # Counter how many contiguous doses at 0 DLTs with no increment.
    no_increment_counter <- 0L

    # Initialize with starting dose.
    dose <- object@startingDose

    while (!should_stop) {
      # What is the cohort size at this dose?
      cohort_size <- size(object@cohort_size, dose = dose, data = base_data)

      if (base_data@placebo) {
        cohort_size_pl <- size(
          object@pl_cohort_size,
          dose = dose,
          data = base_data
        )
      }

      # For all possible number of DLTs:
      for (num_dlts in 0:cohort_size) {
        # Update data with corresponding DLT vector.
        if (base_data@placebo && (cohort_size_pl > 0L)) {
          data_updated <- update(
            object = base_data,
            x = base_data@doseGrid[1],
            y = rep(0, cohort_size_pl),
            check = FALSE
          )

          data_updated <- update(
            object = data_updated,
            x = dose,
            y = rep(
              x = c(0, 1),
              times = c(
                cohort_size - num_dlts,
                num_dlts
              )
            ),
            new_cohort = FALSE
          )
        } else {
          data_updated <- update(
            object = base_data,
            x = dose,
            y = rep(
              x = c(0, 1),
              times = c(
                cohort_size - num_dlts,
                num_dlts
              )
            )
          )
        }

        # Calculate dose limit.
        dose_limit <- maxDose(object@increments, data = data_updated)

        # Generate samples from the model.
        samples <- mcmc(
          data = data_updated,
          model = object@model,
          options = mcmcOptions
        )

        # Calculate next best dose.
        next_dose <- nextBest(
          object@nextBest,
          doselimit = dose_limit,
          samples = samples,
          model = object@model,
          data = data_updated
        )$value

        # Compute relative increment in percent.
        increment <- round((next_dose - dose) / dose * 100)

        # Evaluate stopping rules.
        stop_this_trial <- stopTrial(
          object@stopping,
          dose = next_dose,
          samples = samples,
          model = object@model,
          data = data_updated
        )

        # Append information to the data frame.
        ret <- rbind(
          ret,
          list(
            dose = dose,
            DLTs = num_dlts,
            nextDose = next_dose,
            stop = stop_this_trial,
            increment = as.integer(increment)
          )
        )
      }

      # Update base data.
      if (base_data@placebo && (cohort_size_pl > 0L)) {
        base_data <- update(
          object = base_data,
          x = base_data@doseGrid[1],
          y = rep(0, cohort_size_pl),
          check = FALSE
        )

        base_data <- update(
          object = base_data,
          x = dose,
          y = rep(0, cohort_size),
          new_cohort = FALSE
        )
      } else {
        base_data <- update(
          object = base_data,
          x = dose,
          y = rep(0, cohort_size)
        )
      }

      # Extract results if 0 DLTs.
      results_no_dlts <- subset(
        tail(ret, cohort_size + 1),
        dose == dose & DLTs == 0
      )

      # Determine new dose.
      new_dose <- as.numeric(results_no_dlts$nextDose)

      # Calculate difference to previous dose.
      dose_diff <- new_dose - dose

      # Update the counter for no increments of the dose.
      if (dose_diff == 0) {
        no_increment_counter <- no_increment_counter + 1L
      } else {
        no_increment_counter <- 0L
      }

      # Check if stopping rule would be fulfilled.
      stop_already <- results_no_dlts$stop

      # Update dose.
      dose <- new_dose

      # Check if too many times no increment.
      stop_no_increment <- (no_increment_counter >= maxNoIncrement)
      if (stop_no_increment) {
        warning(paste(
          "Stopping because",
          no_increment_counter,
          "times no increment vs. previous dose"
        ))
      }

      # Check if we can stop:
      # Either when we have reached the highest dose in the next cohort,
      # or when the stopping rule is already fulfilled,
      # or when too many times no increment.
      should_stop <- (dose >= max(object@data@doseGrid)) ||
        stop_already ||
        stop_no_increment
    }
    ret
  }
)

## RuleDesign ----

#' @describeIn examine Examine a rule-based design.
#' @example examples/design-method-examine-RuleDesign.R
setMethod(
  "examine",
  signature = signature(object = "RuleDesign"),
  def = function(object, ..., maxNoIncrement) {
    # Start with the empty table.
    ret <- data.frame(
      dose = numeric(),
      DLTs = integer(),
      nextDose = numeric(),
      stop = logical(),
      increment = integer()
    )

    # Start the base data with the provided one.
    base_data <- object@data

    # Are we finished and can stop?
    should_stop <- FALSE

    # Counter: contiguous doses at 0 DLTs with no increment.
    no_increment_counter <- 0L

    # Initialize with starting dose.
    dose <- object@startingDose

    # Continue filling up the table until stopping.
    while (!should_stop) {
      # Cohort size at this dose.
      cohort_size <- size(object@cohort_size, dose = dose, data = base_data)

      # For all possible number of DLTs.
      for (num_dlts in 0:cohort_size) {
        # Update data with corresponding DLT vector.
        data_updated <- update(
          object = base_data,
          x = dose,
          y = rep(
            x = c(0, 1),
            times = c(
              cohort_size - num_dlts,
              num_dlts
            )
          )
        )

        # Evaluate the rule.
        outcome <- nextBest(object@nextBest, data = data_updated)

        # Next dose and whether to stop here.
        next_dose <- outcome$value
        stop_this_trial <- outcome$stopHere

        # Compute relative increment in percent.
        increment <- round((next_dose - dose) / dose * 100)

        # Append information to the data frame.
        ret <- rbind(
          ret,
          list(
            dose = dose,
            DLTs = num_dlts,
            nextDose = next_dose,
            stop = stop_this_trial,
            increment = as.integer(increment)
          )
        )
      }

      # Change base data.
      base_data <- update(
        object = base_data,
        x = dose,
        y = rep(0, cohort_size)
      )

      # Results if 0 DLTs.
      results_no_dlts <- subset(
        tail(ret, cohort_size + 1),
        dose == dose & DLTs == 0
      )

      # New dose and difference to previous dose.
      new_dose <- as.numeric(results_no_dlts$nextDose)
      dose_diff <- new_dose - dose

      # Update the counter for no increments of the dose.
      if (dose_diff == 0) {
        no_increment_counter <- no_increment_counter + 1L
      } else {
        no_increment_counter <- 0L
      }

      # Would stopping rule be fulfilled already?
      stop_already <- results_no_dlts$stop

      # Update dose.
      dose <- new_dose

      # Too many times no increment?
      stop_no_increment <- (no_increment_counter >= maxNoIncrement)
      if (stop_no_increment) {
        warning(paste(
          "Stopping because",
          no_increment_counter,
          "times no increment vs. previous dose"
        ))
      }

      # Check if we can stop:
      # highest dose reached next cohort, stopping rule fulfilled, or too many no-increment.
      should_stop <- (dose >= max(object@data@doseGrid)) ||
        stop_already ||
        stop_no_increment
    }

    ret
  }
)

## DADesign ----

#' @describeIn examine Examine a model-based CRM.
#'
#' @param mcmcOptions ([`McmcOptions`])\cr
#'   giving the MCMC options for each evaluation in the trial. By default,
#'   the standard options are used
#'
#' @example examples/design-method-examine-DADesign.R
setMethod(
  "examine",
  signature = signature(object = "DADesign"),
  def = function(object, mcmcOptions = McmcOptions(), ..., maxNoIncrement) {
    # Check follow-up sufficiency (TRUE/FALSE);
    ready_to_open <- function(day, window, this_surv) {
      size <- length(this_surv)
      start_time <- apply(
        rbind(this_surv[-size], window$patientGap[-1]),
        2,
        min
      )
      individual_check <- day - cumsum(c(0, start_time))
      individual_check[individual_check < 0] <- 0
      follow_up <- apply(rbind(this_surv, individual_check), 2, min)
      all(
        (follow_up - apply(rbind(window$patientFollow, this_surv), 2, min)) >= 0
      ) &&
        (max(follow_up) >= min(window$patientFollowMin, max(this_surv)))
    }

    # Determine when to open the next cohort; applies to all trials.
    next_open <- function(window, this_surv) {
      size <- length(this_surv)
      window$patientGap <- window$patientGap[1:size]
      start_time <- apply(
        rbind(this_surv[-size], window$patientGap[-1]),
        2,
        min
      )
      max_t <- max(this_surv + cumsum(c(0, start_time)))

      met <- sapply(1:max_t, function(i) ready_to_open(i, window, this_surv))
      if (sum(met) > 0) min(c(1:max_t)[met]) else max_t
    }

    # Initialize result table.
    ret <- data.frame(
      DLTsearly_1 = integer(),
      dose = numeric(),
      DLTs = integer(),
      nextDose = numeric(),
      stop = logical(),
      increment = integer()
    )

    # Base data and trial state.
    base_data <- object@data
    should_stop <- FALSE
    dose <- object@startingDose

    # Observed facts trackers (cumulative across cohorts).
    observed_dlts <- base_data@y
    observed_surv <- base_data@u
    observed_t0 <- base_data@t0

    # Global trial clock and previous cohort timing.
    trial_time <- 0
    prev_time <- 0

    # DLT window length.
    t_max <- base_data@Tmax

    # Number of patients with unfinished DLT window (initially none).
    prev_size <- 0

    # Iterate cohorts until stopping.
    while (!should_stop) {
      cohort_size <- size(object@cohort_size, dose = dose, data = base_data)
      safety_window <- windowLength(object@safetyWindow, cohort_size)

      # When cohort patients start relative to trial clock.

      cohort_t0 <- trial_time + cumsum(safety_window$patientGap)

      # Append placeholders for the incoming cohort (no DLTs yet, censored at t_max).
      observed_dlts <- c(observed_dlts, rep(0, cohort_size))
      observed_surv <- c(observed_surv, rep(t_max, cohort_size))
      observed_t0 <- c(observed_t0, cohort_t0)

      # Advance time until next cohort may open (all follow-up constraints satisfied).
      trial_time <- trial_time +
        next_open(window = safety_window, this_surv = rep(t_max, cohort_size))

      # Count patients still within DLT window (for nFollow loop).
      n_follow <- cohort_size + prev_size

      # Identify censored patients indices.
      npt <- length(base_data@x)
      censored_indices <- c(
        which((trial_time - base_data@t0) < base_data@Tmax & base_data@y == 0),
        (npt + 1):(npt + cohort_size)
      )

      # For all possible number of DLTs (0..n_follow):
      for (num_dlts in 0:n_follow) {
        if (num_dlts == 0) {
          # Update base_data for zero DLTs scenario.
          base_data <- update(
            object = base_data,
            y = observed_dlts,
            u = observed_surv,
            t0 = observed_t0,
            x = dose,
            trialtime = trial_time
          )

          dose_limit <- maxDose(object@increments, data = base_data)
          samples <- mcmc(
            data = base_data,
            model = object@model,
            options = mcmcOptions
          )
          next_dose <- nextBest(
            object@nextBest,
            doselimit = dose_limit,
            samples = samples,
            model = object@model,
            data = base_data
          )$value

          increment <- round((next_dose - dose) / dose * 100)
          stop_this_trial <- stopTrial(
            object@stopping,
            dose = next_dose,
            samples = samples,
            model = object@model,
            data = base_data
          )

          ret <- rbind(
            ret,
            list(
              DLTsearly_1 = 0,
              dose = dose,
              DLTs = num_dlts,
              nextDose = next_dose,
              stop = stop_this_trial,
              increment = as.integer(increment)
            )
          )
        } else {
          # Consider two extremes: DLTs at longest vs shortest follow-ups.
          for (dlt_early in 1:num_dlts) {
            curr_dlts <- observed_dlts
            curr_surv <- observed_surv

            if (dlt_early == 1) {
              # Longest follow-up patients have DLTs.
              curr_dlts[censored_indices][1:num_dlts] <- 1
              curr_surv[censored_indices][1:num_dlts] <- apply(
                rbind(
                  rep(t_max, num_dlts),
                  trial_time - observed_t0[censored_indices][1:num_dlts]
                ),
                2,
                min
              )

              data_current <- update(
                object = base_data,
                y = curr_dlts,
                u = curr_surv,
                t0 = observed_t0,
                x = dose,
                trialtime = trial_time
              )
            } else {
              # Shortest follow-up patients have DLTs.
              curr_dlts[rev(censored_indices)][1:num_dlts] <- 1
              curr_surv[rev(censored_indices)][1:num_dlts] <- apply(
                rbind(
                  rep(1, num_dlts),
                  prev_time + 1 - observed_t0[rev(censored_indices)][1:num_dlts]
                ),
                2,
                max
              )

              temp_time <- if (num_dlts >= cohort_size) {
                1 + max(cohort_t0)
              } else {
                trial_time
              }

              data_current <- update(
                object = base_data,
                y = curr_dlts,
                u = curr_surv,
                t0 = observed_t0,
                x = dose,
                trialtime = temp_time
              )
            }

            dose_limit <- maxDose(object@increments, data = data_current)
            samples <- mcmc(
              data = data_current,
              model = object@model,
              options = mcmcOptions
            )
            next_dose <- nextBest(
              object@nextBest,
              doselimit = dose_limit,
              samples = samples,
              model = object@model,
              data = data_current
            )$value

            increment <- round((next_dose - dose) / dose * 100)
            stop_this_trial <- stopTrial(
              object@stopping,
              dose = next_dose,
              samples = samples,
              model = object@model,
              data = data_current
            )

            ret <- rbind(
              ret,
              list(
                DLTsearly_1 = dlt_early,
                dose = dose,
                DLTs = num_dlts,
                nextDose = next_dose,
                stop = stop_this_trial,
                increment = as.integer(increment)
              )
            )
          }
        }
      }

      # Update previous time and compute next state.
      prev_time <- trial_time

      # Filter results at this dose with 0 DLTs and derive new dose.
      results_no_dlts <- subset(ret, dose == dose & DLTs == 0)
      new_dose <- as.numeric(results_no_dlts$nextDose)
      dose_diff <- new_dose - dose
      stop_already <- any(results_no_dlts$stop)

      # Update dose to the maximum recommended among ties.
      dose <- max(new_dose)

      # Patients still within DLT window.
      prev_size <- sum(base_data@u[base_data@y == 0] < base_data@Tmax)

      # No-increment counter and stopping due to no increment.
      no_increment_counter <- if (all(dose_diff == 0)) {
        no_increment_counter + 1L
      } else {
        0L
      }
      stop_no_increment <- (no_increment_counter >= maxNoIncrement)
      if (stop_no_increment) {
        warning(paste(
          "Stopping because",
          no_increment_counter,
          "times no increment vs. previous dose"
        ))
      }

      # Overall stop condition.
      should_stop <- (dose >= max(object@data@doseGrid)) ||
        stop_already ||
        stop_no_increment
    }

    ret
  }
)

# tidy ----

## tidy-DualDesign ----

#' @rdname tidy
#' @aliases tidy-DualDesign
#' @example examples/Design-method-tidyDualDesign.R
#'
#' @export
setMethod(
  f = "tidy",
  signature = signature(x = "DualDesign"),
  definition = function(x, ...) {
    # Some Design objects have complex attributes whose structure is not supported.
    rv <- h_tidy_all_slots(x, attributes = FALSE) %>% h_tidy_class(x)
    if (length(rv) == 1) {
      rv[[names(rv)[1]]] %>% h_tidy_class(x)
    } else {
      rv
    }
  }
)
