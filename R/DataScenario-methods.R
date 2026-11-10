#' @include DataScenario-class.R CrmPackClass-methods.R
NULL

#' Format a Dose Vector
#' @param dose Named numeric dose.
#' @param digits Dose decimal places.
#' @param drug_names Optional display names in component order.
#' @param units Optional units in component order.
#' @param separator Component separator.
#' @return A character dose label.
#' @keywords internal
#' @noRd
h_scenario_dose_label <- function(dose, digits, drug_names, units, separator) {
  if (!length(dose) || anyNA(dose)) {
    return(NA_character_)
  }
  parts <- format(round(dose, digits), trim = TRUE, scientific = FALSE)
  if (length(drug_names)) {
    parts <- paste(rep_len(drug_names, length(parts)), parts)
  }
  if (length(units)) {
    parts <- paste(parts, rep_len(units, length(parts)))
  }
  paste(parts, collapse = separator)
}

#' Match a Dose Against a Probability Table
#' @param dose Named dose vector.
#' @param probs Probability data frame.
#' @param dose_cols Dose column names.
#' @param column Probability column.
#' @return Probability or NA when unavailable.
#' @keywords internal
#' @noRd
h_scenario_probability <- function(dose, probs, dose_cols, column) {
  if (!length(dose) || anyNA(dose) || !column %in% names(probs)) {
    return(NA_real_)
  }
  if (length(dose_cols) != length(dose)) {
    stop(
      "Probability table dose columns do not match the arm's dose dimensions."
    )
  }
  matched <- rep(TRUE, nrow(probs))
  for (j in seq_along(dose)) {
    matched <- matched &
      !is.na(match_within_tolerance(probs[[dose_cols[j]]], dose[j]))
  }
  index <- which(matched)
  if (length(index) > 1L) {
    stop("Probability table contains duplicate matching doses.")
  }
  if (!length(index)) NA_real_ else as.numeric(probs[[column]][index])
}

#' Tidy an Evaluated Data Scenario
#'
#' Counts use observed data; dose list-columns retain numeric components.
#' No model is refitted. Probabilities are taken only from the selected rule.
#' @param x A DataScenario.
#' @param probabilities Probability names, or NULL for the intersection available
#'   in all evaluated arms. Known target_prob/overdose_prob spellings normalize
#'   to target/overdose. Underdosing is never inferred.
#' @param dose_digits Decimal places in dose labels.
#' @param drug_names,units Optional component display names and units.
#' @param separator Separator between combination components.
#' @param ... Unused.
#' @return A tbl_DataScenario tibble with one row per arm and observed dose;
#'   empty arms have one row with zero counts and unavailable doses.
#' @export
setMethod(
  "tidy",
  "DataScenario",
  function(
    x,
    probabilities = NULL,
    dose_digits = 3L,
    drug_names = NULL,
    units = NULL,
    separator = " + ",
    ...
  ) {
    validObject(x)
    assert_count(dose_digits)
    arms <- if (is(x@data, "HierarchicalData")) {
      x@data@arms
    } else {
      list(overall = x@data)
    }
    tables <- lapply(names(arms), function(arm) {
      p <- x@next_best[[arm]]$probs
      if (is.null(p)) {
        return(NULL)
      }
      p <- as.data.frame(p)
      names(p) <- sub("^(target|overdose|underdosing)_prob$", "\\1", names(p))
      p
    })
    names(tables) <- names(arms)
    dose_columns <- lapply(arms, function(data) {
      if (is(data, "DataCombo")) data@drugNames else "dose"
    })
    available <- lapply(names(arms), function(arm) {
      setdiff(names(tables[[arm]]), c(dose_columns[[arm]], "not_eligible"))
    })
    evaluated <- !vapply(x@next_best, is.null, logical(1))
    if (is.null(probabilities)) {
      probabilities <- if (any(evaluated)) {
        Reduce(intersect, available[evaluated])
      } else {
        character()
      }
    }
    assert_character(probabilities, any.missing = FALSE, unique = TRUE)
    for (i in which(evaluated)) {
      missing <- setdiff(probabilities, available[[i]])
      if (length(missing)) {
        stop(sprintf(
          "Arm '%s': unavailable probabilities: %s. Available: %s.",
          names(arms)[i],
          paste(missing, collapse = ", "),
          paste(available[[i]], collapse = ", ")
        ))
      }
    }
    rows <- lapply(names(arms), function(arm) {
      data <- arms[[arm]]
      if (is(data, "DataOrdinal")) {
        stop("DataScenario reporting requires binary toxicity outcomes.")
      }
      d <- tidy(data)
      combo <- is(data, "DataCombo")
      components <- if (combo) data@drugNames else "dose"
      cols <- if (combo) paste0("Dose_", components) else "Dose"
      doses <- lapply(seq_len(nrow(d)), function(i) {
        stats::setNames(
          as.numeric(unlist(d[i, cols, drop = FALSE])),
          components
        )
      })
      keys <- vapply(doses, function(z) paste(z, collapse = ":"), character(1))
      distinct <- unique(keys)
      observed <- doses[match(distinct, keys)]
      if (!length(observed)) {
        observed <- list(stats::setNames(
          rep(NA_real_, length(components)),
          components
        ))
      }
      current <- stats::setNames(rep(NA_real_, length(components)), components)
      if (nrow(d)) {
        last <- if (combo) nrow(d) else which(d$Cohort == max(d$Cohort))
        if (!combo && data@placebo) {
          last <- last[d$Dose[last] != data@doseGrid[1L]]
        }
        latest <- unique(keys[last])
        if (length(latest) > 1L) {
          stop("Ambiguous non-placebo doses in the latest cohort.")
        }
        if (length(latest)) current <- doses[[match(latest, keys)]]
      }
      proposed <- x@next_dose[[arm]]
      if (!length(proposed)) {
        proposed <- rep(NA_real_, length(components))
      }
      proposed <- stats::setNames(as.numeric(proposed), components)
      n <- length(observed)
      label <- function(z) {
        h_scenario_dose_label(z, dose_digits, drug_names, units, separator)
      }
      p <- tables[[arm]]
      current_prob <- h_scenario_probability(
        current,
        p,
        dose_columns[[arm]],
        "overdose"
      )
      cohort <- x@cohort_size[[arm]]
      stopped <- x@stop[[arm]]
      reason <- x@stop_reason[[arm]]
      result <- tibble::tibble(
        arm = arm,
        observed_dose = observed,
        observed_dose_display = vapply(observed, label, character(1)),
        n_patients = if (length(distinct)) {
          as.integer(tabulate(match(keys, distinct)))
        } else {
          0L
        },
        n_dlts = if (length(distinct)) {
          vapply(
            distinct,
            function(k) sum(d$Tox[keys == k]),
            integer(1),
            USE.NAMES = FALSE
          )
        } else {
          0L
        },
        current_dose = rep(list(current), n),
        current_dose_display = label(current),
        current_overdose_prob = current_prob,
        proposed_next_dose = rep(list(proposed), n),
        proposed_next_dose_display = label(proposed),
        cohort_size = if (length(cohort)) cohort else NA_integer_,
        stop = if (length(stopped)) as.logical(stopped) else NA,
        stop_reason = if (length(reason)) {
          paste(unlist(reason), collapse = "; ")
        } else {
          NA_character_
        }
      )
      for (column in probabilities) {
        result[[column]] <- h_scenario_probability(
          proposed,
          p,
          dose_columns[[arm]],
          column
        )
      }
      result
    })
    result <- dplyr::bind_rows(rows)
    attr(result, "probabilities") <- probabilities
    attr(result, "hierarchical") <- is(x@data, "HierarchicalData")
    h_tidy_class(result, x)
  }
)

#' Show an Evaluated Scenario
#' @param object A DataScenario.
#' @return The object invisibly.
#' @export
setMethod("show", "DataScenario", function(object) {
  for (arm in names(object@stop)) {
    cat(
      arm,
      ": ",
      if (isTRUE(object@stop[[arm]])) {
        "Stop"
      } else if (is.na(object@stop[[arm]])) {
        "Unavailable"
      } else {
        "Continue"
      },
      "; proposed dose: ",
      paste(object@next_dose[[arm]], collapse = " + "),
      "\n",
      sep = ""
    )
  }
  invisible(object)
})
