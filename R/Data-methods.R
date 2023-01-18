# plot ----

## Data ----

#' Plot Method for the [`Data`] Class
#'
#' @description `r lifecycle::badge("stable")`
#'
#' A method that creates a plot for [`Data`] object.
#'
#' @param x (`Data`)\cr object we want to plot.
#' @param y (`missing`)\cr missing object, for compatibility with the generic
#'   function.
#' @param blind (`flag`)\cr indicates whether to blind the data.
#'   If `TRUE`, then placebo subjects are reported at the same level
#'   as the active dose level in the corresponding cohort,
#'   and DLTs are always assigned to the first subjects in a cohort.
#' @param legend (`flag`)\cr whether the legend should be added.
#' @param ... not used.
#'
#' @return The [`ggplot2`] object.
#'
#' @aliases plot-Data
#' @export
#' @example examples/Data-method-plot.R
#'
setMethod(
  f = "plot",
  signature = signature(x = "Data", y = "missing"),
  definition = function(x, y, blind = FALSE, legend = TRUE, ...) {
    assert_flag(blind)
    assert_flag(legend)

    if (x@nObs == 0L) {
      return()
    }

    df <- h_plot_data_df(x, blind)

    p <- ggplot(df, aes(x = patient, y = dose)) +
      geom_point(aes(shape = toxicity, colour = toxicity), size = 3) +
      scale_colour_manual(
        name = "Toxicity", values = c(Yes = "red", No = "black")
      ) +
      scale_shape_manual(name = "Toxicity", values = c(Yes = 17, No = 16)) +
      scale_x_continuous(breaks = df$patient, minor_breaks = NULL) +
      scale_y_continuous(
        breaks = sort(unique(c(0, df$dose))),
        minor_breaks = NULL,
        limits = c(0, max(df$dose) * 1.1)
      ) +
      xlab("Patient") +
      ylab("Dose Level")

    p <- p + h_plot_data_cohort_lines(df$cohort, placebo = x@placebo)

    if (!blind) {
      p <- p +
        geom_text(
          aes(label = ID, size = 2),
          data = df,
          hjust = 0,
          vjust = 0.5,
          angle = 90,
          colour = "black",
          show.legend = FALSE
        )
    }

    if (!legend) {
      p <- p + theme(legend.position = "none")
    }

    p
  }
)

## DataDual ----

#' Plot Method for the [`DataDual`] Class
#'
#' @description `r lifecycle::badge("stable")`
#'
#' A method that creates a plot for [`DataDual`] object.
#'
#' @param x (`DataDual`)\cr object we want to plot.
#' @param y (`missing`)\cr missing object, for compatibility with the generic
#'   function.
#' @param blind (`flag`)\cr indicates whether to blind the data.
#'   If `TRUE`, then placebo subjects are reported at the same level
#'   as the active dose level in the corresponding cohort,
#'   and DLTs are always assigned to the first subjects in a cohort.
#' @param ... passed to the first inherited method `plot` after this current
#'   method.
#'
#' @return The [`ggplot2`] object.
#'
#' @aliases plot-DataDual
#' @export
#' @example examples/Data-method-plot-DataDual.R
#'
setMethod(
  f = "plot",
  signature = signature(x = "DataDual", y = "missing"),
  definition = function(x, y, blind = FALSE, ...) {
    assert_flag(blind)

    # Call the superclass method, to get the first plot.
    plot1 <- callNextMethod(x, blind = blind, legend = FALSE, ...)

    # Create the second, biomarker plot.
    df <- h_plot_data_df(x, blind, biomarker = x@w)

    plot2 <- ggplot(df, aes(x = dose, y = biomarker)) +
      geom_point(aes(shape = toxicity, colour = toxicity), size = 3) +
      scale_colour_manual(
        name = "Toxicity", values = c(Yes = "red", No = "black")
      ) +
      scale_shape_manual(name = "Toxicity", values = c(Yes = 17, No = 16)) +
      xlab("Dose Level") +
      ylab("Biomarker")

    if (!blind) {
      plot2 <- plot2 +
        geom_text(
          aes(
            y = biomarker + 0.02 * diff(range(biomarker)),
            label = patient, size = 2
          ),
          data = df,
          hjust = 0,
          vjust = 0.5,
          angle = 90,
          colour = "black",
          show.legend = FALSE
        )
    }

    # Arrange both plots side by side.
    gridExtra::arrangeGrob(plot1, plot2, ncol = 2)
  }
)

## DataDA ----

#' Plot Method for the [`DataDA`] Class
#'
#' @description `r lifecycle::badge("stable")`
#'
#' A method that creates a plot for [`DataDA`] object.
#'
#' @param x (`DataDA`)\cr object we want to plot.
#' @param y (`missing`)\cr missing object, for compatibility with the generic
#'   function.
#' @param blind (`flag`)\cr indicates whether to blind the data.
#'   If `TRUE`, then placebo subjects are reported at the same level
#'   as the active dose level in the corresponding cohort,
#'   and DLTs are always assigned to the first subjects in a cohort.
#' @param ... passed to the first inherited method `plot` after this current
#'   method.
#'
#' @return The [`ggplot2`] object.
#'
#' @aliases plot-DataDA
#' @export
#' @example examples/Data-method-plot-DataDA.R
#'
setMethod(
  f = "plot",
  signature = signature(x = "DataDA", y = "missing"),
  definition = function(x, y, blind = FALSE, ...) {
    assert_flag(blind)

    # Call the superclass method, to get the first plot.
    plot1 <- callNextMethod(x, blind = blind, legend = FALSE, ...)

    # Prepare data set for the second, time plot.
    df <- h_plot_data_df(x, blind, u = x@u, t0 = x@t0)
    df$censored <- ifelse(df$u < x@Tmax & df$toxicity == 0, 1, 0)
    df$tend <- df$t0 + df$u # `tend` stands for `time end`
    df$t0_case <- "Start"
    df$tend_case <- ifelse(
      df$toxicity == "Yes",
      "Yes",
      ifelse(df$censored, "Censored", "No")
    )

    # Build plot object.
    plot2 <- ggplot(df, aes(x = t0, y = patient)) +
      geom_segment(aes(xend = tend, yend = patient)) +
      geom_point(aes(shape = t0_case, colour = t0_case), size = 3) +
      geom_point(
        aes(x = tend, shape = tend_case, colour = tend_case),
        size = 3
      ) +
      scale_colour_manual(
        name = "Toxicity",
        values = c(
          Yes = "red", No = "black", Start = "black", Censored = "black"
        )
      ) +
      scale_shape_manual(
        name = "Toxicity",
        values = c(Yes = 17, No = 16, Start = 1, Censored = 4)
      ) +
      scale_y_continuous(breaks = df$patient, minor_breaks = NULL) +
      xlab("Time") +
      ylab("Patient")

    plot2 <- plot2 +
      h_plot_data_cohort_lines(df$cohort, placebo = x@placebo, vertical = FALSE)

    if (!blind) {
      plot2 <- plot2 +
        geom_text(
          aes(label = ID, size = 2),
          size = 3,
          hjust = 1.5,
          vjust = 0,
          angle = 0,
          colour = "black",
          show.legend = FALSE
        )
    }

    # Arrange both plots side by side.
    gridExtra::arrangeGrob(plot1, plot2, ncol = 1)
  }
)

# update ----

## Data ----

#' Updating `Data` Objects
#'
#' @description `r lifecycle::badge("stable")`
#'
#' A method that updates existing [`Data`] object with new data.
#'
#' @param object (`Data`)\cr object you want to update.
#' @param x (`number`)\cr the dose level (one level only!).
#' @param y (`integer`)\cr the DLT vector (0/1 vector) for all patients in this
#'   cohort. You can also supply `numeric` vectors, but these will then be
#'   converted to `integer` internally.
#' @param ID (`integer`)\cr the patient IDs.
#'   You can also supply `numeric` vectors, but these will then be converted to
#'   `integer` internally.
#' @param new_cohort (`flag`)\cr if `TRUE` (default) the new data are assigned
#'   to a new cohort.
#' @param check (`flag`)\cr whether the validation of the updated object should
#'   be conducted. Current implementation of this `update` method allows for
#'   updating the `Data` class object by adding a single dose level `x` only.
#'   However, there might be some use cases where the new cohort to be added
#'   contains a placebo and active dose. Hence, such update would need to be
#'   performed iteratively by calling the `update` method twice. For example,
#'   in the first call a user can add a placebo, and then in the second call,
#'   an active dose. Since having a cohort with placebo only is not allowed,
#'   the `update` method would normally throw the error when attempting to add
#'   a placebo in the first call. To allow for such updates, the `check`
#'   parameter should be then set to `FALSE` for that first call.
#' @param ... not used.
#'
#' @return The new, updated [`Data`] object.
#'
#' @aliases update-Data
#' @export
#' @example examples/Data-method-update.R
#'
setMethod(
  f = "update",
  signature = signature(object = "Data"),
  definition = function(object,
                        x,
                        y,
                        ID = length(object@ID) + seq_along(y),
                        new_cohort = TRUE,
                        check = TRUE,
                        ...) {
    assert_numeric(x, min.len = 0, max.len = 1)
    assert_numeric(y, lower = 0, upper = 1)
    assert_numeric(ID, len = length(y))
    assert_disjunct(object@ID, ID)
    assert_flag(new_cohort)
    assert_flag(check)

    # How many additional patients, ie. the length of the update.
    n <- length(y)

    # Which grid level is the dose?
    gridLevel <- matchTolerance(x, object@doseGrid)
    object@xLevel <- c(object@xLevel, rep(gridLevel, n))

    # Add dose.
    object@x <- c(object@x, rep(as.numeric(x), n))

    # Add DLT data.
    object@y <- c(object@y, safeInteger(y))

    # Add ID.
    object@ID <- c(object@ID, safeInteger(ID))

    # Add cohort number.
    new_cohort_id <- if (object@nObs == 0) {
      1L
    } else {
      tail(object@cohort, 1L) + ifelse(new_cohort, 1L, 0L)
    }
    object@cohort <- c(object@cohort, rep(new_cohort_id, n))

    # Increment sample size.
    object@nObs <- object@nObs + n

    if (check) {
      validObject(object)
    }

    object
  }
)

## DataParts ----

#' Updating `DataParts` Objects
#'
#' @description `r lifecycle::badge("stable")`
#'
#' A method that updates existing [`DataParts`] object with new data.
#'
#' @param object (`DataParts`)\cr object you want to update.
#' @inheritParams update,Data-method
#' @param ... further arguments passed to `Data` update method [`update-Data`].
#' @param check (`flag`)\cr whether the validation of the updated object
#'   should be conducted. See help for [`update-Data`] for more details
#'   on the use case of this parameter.
#'
#' @return The new, updated [`DataParts`] object.
#'
#' @aliases update-DataParts
#' @export
#' @example examples/Data-method-update-DataParts.R
#'
setMethod(
  f = "update",
  signature = signature(object = "DataParts"),
  definition = function(object, x, y, ..., check = TRUE) {
    assert_numeric(y)
    assert_flag(check)

    # Update slots corresponding to `Data` class.
    object <- callNextMethod(object = object, x = x, y = y, ..., check = FALSE)

    # Update the part information.

    object@part <- c(object@part, rep(object@nextPart, length(y)))

    # Decide which part the next cohort will belong to:
    # only if the `nextPart` was 1, it can potentially be required
    # to change it to 2 (once it is 2, it stays).
    if (object@nextPart == 1L) {
      # If there was a DLT in one of the cohorts,
      # or if the current dose was the highest from part 1.
      if (any(object@y == 1L) || x == max(object@part1Ladder)) {
        # Then this closes part 1 and the next cohort will be from part 2.
        object@nextPart <- 2L
      }
    }

    if (check) {
      validObject(object)
    }

    object
  }
)

## DataDual ----

#' Updating `DataDual` Objects
#'
#' @description `r lifecycle::badge("stable")`
#'
#' A method that updates existing [`DataDual`] object with new data.
#'
#' @param object (`DataDual`)\cr object you want to update.
#' @param w (`numeric`)\cr the continuous vector of biomarker values
#'   for all the patients in this update.
#' @param ... further arguments passed to `Data` update method [`update-Data`].
#' @param check (`flag`)\cr whether the validation of the updated object
#'   should be conducted. See help for [`update-Data`] for more details
#'   on the use case of this parameter.
#'
#' @return The new, updated [`DataDual`] object.
#'
#' @aliases update-DataDual
#' @export
#' @example examples/Data-method-update-DataDual.R
#'
setMethod(
  f = "update",
  signature = signature(object = "DataDual"),
  definition = function(object, w, ..., check = TRUE) {
    assert_numeric(w)
    assert_flag(check)

    # Update slots corresponding to `Data` class.
    object <- callNextMethod(object = object, ..., check = FALSE)

    # Update the biomarker information.
    object@w <- c(object@w, w)

    if (check) {
      validObject(object)
    }

    object
  }
)

## DataDA ----

#' Updating `DataDA` Objects
#'
#' @description `r lifecycle::badge("stable")`
#'
#' A method that updates existing [`DataDA`] object with new data.
#'
#' @note This function is capable of not only adding new patients but also
#'   updates existing ones with respect to `y`, `t0`, `u` slots.
#'
#' @param object (`DataDA`)\cr object you want to update.
#' @param u (`numeric`)\cr the new DLT free survival times for all patients,
#'   i.e. for existing patients in the `object` as well as for new patients.
#' @param t0 (`numeric`)\cr the time that each patient starts DLT observation
#'   window. This parameter covers all patients, i.e. existing patients in the
#'   `object` as well as for new patients.
#' @param trialtime (`number`)\cr current time in the trial, i.e. a followup
#'   time.
#' @param y (`numeric`)\cr the new DLTs for all patients, i.e. for existing
#'   patients in the `object` as well as for new patients.
#' @param ... further arguments passed to `Data` update method [`update-Data`].
#'   These are used when there are new patients to be added to the cohort.
#' @param check (`flag`)\cr whether the validation of the updated object
#'   should be conducted. See help for [`update-Data`] for more details
#'   on the use case of this parameter.
#'
#' @return The new, updated [`DataDA`] object.
#'
#' @aliases update-DataDA
#' @export
#' @example examples/Data-method-update-DataDA.R
#'
setMethod(
  f = "update",
  signature = signature(object = "DataDA"),
  definition = function(object,
                        u,
                        t0,
                        trialtime,
                        y,
                        ...,
                        check = TRUE) {
    assert_flag(check)
    assert_numeric(y, lower = 0, upper = 1)
    assert_true(length(y) == 0 || length(y) >= object@nObs)
    assert_numeric(u, lower = 0, len = length(y))
    assert_numeric(t0, lower = 0, len = length(y))
    if (length(y) > 0) {
      assert_number(trialtime, lower = max(c(object@t0, t0)))
    }

    # How many additional patients.
    n <- max(length(y) - object@nObs, 0L)

    # Update slots corresponding to `Data` class.
    object <- callNextMethod(
      object = object,
      y = y[object@nObs + seq_len(n)], # Empty vector when n = 0.
      ...,
      check = FALSE
    )

    # DLT will be observed once the followup time >= the time to DLT
    # and y = 1 at the same time.
    object@y <- safeInteger(y * (trialtime >= t0 + u))

    # Update DLT free survival time.
    object@u <- apply(rbind(u, trialtime - t0), 2, min)

    # Update t0.
    object@t0 <- t0

    if (check) {
      validObject(object)
    }

    object
  }
)

# getEff ----

## generic ----

#' Extracting Efficacy Responses for Subjects Categorized by the DLT
#'
#' @description `r lifecycle::badge("stable")`
#'
#' A method that extracts efficacy responses for subjects and categorizes it
#' with respect to DLT, i.e. DLT or no DLT. The efficacy responses
#' are reported together with their corresponding dose levels.
#'
#' @param object (`DataDual`)\cr object from which the responses and dose levels
#'   are extracted.
#' @param ... further arguments passed to class-specific methods.
#' @return `list` with efficacy responses categorized by the DLT value.
#' @export
#'
setGeneric(
  name = "getEff",
  def = function(object, ...) {
    standardGeneric("getEff")
  },
  valueClass = "list"
)

## DataDual ----

#' @rdname getEff
#'
#' @param no_dlt (`flag`)\cr should only no DLT responses be returned? Otherwise,
#'   all responses are returned.
#'
#' @aliases getEff-DataDual
#' @example examples/Data-method-getEff.R
#'
setMethod(
  f = "getEff",
  signature = signature(object = "DataDual"),
  definition = function(object, no_dlt = FALSE) {
    assert_flag(no_dlt)

    is_dlt <- object@y == 1L
    is_no_dlt <- !is_dlt

    eff <- if (any(is_no_dlt)) {
      list(x_no_dlt = object@x[is_no_dlt], w_no_dlt = object@w[is_no_dlt])
    } else {
      list(x_no_dlt = NULL, w_no_dlt = NULL)
    }

    if (!no_dlt) {
      eff_dlt <- if (any(is_dlt)) {
        list(x_dlt = object@x[is_dlt], w_dlt = object@w[is_dlt])
      } else {
        list(x_dlt = NULL, w_dlt = NULL)
      }
      eff <- c(eff, eff_dlt)
    }
    eff
  }
)

# ngrid ----

## generic ----

#' Number of Doses in Grid
#'
#' @description `r lifecycle::badge("stable")`
#'
#' A function that gets the number of doses in grid. User can choose whether
#' the placebo dose (if any) should be counted or not.
#'
#' @param object (`Data`)\cr object with dose grid.
#' @param ... further arguments passed to class-specific methods.
#' @return `integer` the number of doses in grid.
#' @export
#'
setGeneric(
  name = "ngrid",
  def = function(object, ...) {
    standardGeneric("ngrid")
  },
  valueClass = "integer"
)

## Data ----

#' @rdname ngrid
#'
#' @param ignore_placebo (`flag`)\cr should placebo dose (if any) not be counted?
#'
#' @aliases ngrid-Data
#' @example examples/Data-method-ngrid.R
#'
setMethod(
  f = "ngrid",
  signature = signature(object = "Data"),
  definition = function(object, ignore_placebo = TRUE) {
    assert_flag(ignore_placebo)

    if (ignore_placebo && object@placebo) {
      max(object@nGrid - 1L, 0L)
    } else {
      object@nGrid
    }
  }
)
