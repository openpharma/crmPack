#' Helper Function to Blind Plot Data
#'
#' @param df (`GeneralData`)\cr The data to be blinded
#' @param blind (`flag`)\cr Should the data be blinded?
#' @param has_placebo (`flag`)\cr Does the data contain a placebo dose?
#' @param pbo_dose (`positive_number`)\cr The dose to be taken as placebo.
#' Ignored if `has_placebo` is `FALSE`
#' @returns The blinded data
h_blind_plot_data <- function(df, blind, has_placebo, pbo_dose) {
  if (blind) {
    # This is to blind the data.
    # For each cohort, all DLTs are assigned to the first subjects in the cohort.
    # In addition, the placebo (if any) is set to the active dose level for that
    # cohort.
    # Notice: dapply reorders records of df according to the lexicographic order
    # of cohort.
    df <- dapply(df, f = ~cohort, FUN = function(coh) {
      coh$toxicity <- sort(coh$toxicity, decreasing = TRUE)
      coh$dose <- max(coh$dose)
      coh
    })
  } else if (has_placebo) {
    # Placebo will be plotted at y = 0 level.
    df$dose[df$dose == pbo_dose] <- 0
  }
  df
}

# h_plot_data_df ----

## generic ----

#' Helper Function for the Plot Method of subclasses of [`GeneralData`]
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A method that transforms [`GeneralData`]  objects into a `tibble` suitable for
#' plotting with `ggplot2` methods
#'
#' @param data (`GeneralData`)\cr object from which data is extracted and converted
#' into a data frame.
#' @param ... further arguments passed to class-specific methods.
#' @return `data.frame` containing columns for patient, cohort, dose and toxicity grade
#' @aliases h_plot_data_df
#'
setGeneric(
  name = "h_plot_data_df",
  def = function(data, ...) standardGeneric("h_plot_data_df"),
  valueClass = "data.frame"
)

# Data ----

#' Helper Function for the Plot Method of [`Data`]
#'
#' @param data (`Data`)\cr object from which data is extracted and converted
#'   into a data frame.
#' @param blind (`flag`)\cr should data be blinded?
#'   If `TRUE`, then for each cohort, all DLTs are assigned to the first
#'   subjects in the cohort. In addition, the placebo (if any) is set to the
#'   active dose level for that cohort.
#' @param legend (`flag`)\cr Display the legend for the toxicity categories
#' @param ... further arguments passed to `data.frame` constructor.
#'   It can be e.g. an extra `column_name = value` pair based on a slot
#'   from `x` (which in this case might be a subclass of `Data`)
#'   which does not appear in `Data`.
#' @return A `data.frame` object with columns patient, ID, cohort, dose and
#' toxicity.
#' @describeIn h_plot_data_df method for [`Data`].
setMethod(
  f = "h_plot_data_df",
  signature = signature(data = "Data"),
  definition = function(data, blind = FALSE, legend = TRUE, ...) {
    df <- data.frame(
      patient = seq_along(data@x),
      ID = paste(" ", data@ID),
      cohort = data@cohort,
      dose = data@x,
      toxicity = ifelse(data@y == 1, "Yes", "No"),
      ...
    )
    df <- h_blind_plot_data(df, blind, data@placebo, data@doseGrid[1])
    df
  }
)

# DataOrdinal ----

#' Helper Function for the Plot Method of [`DataOrdinal`]
#'
#' @describeIn h_plot_data_df Class specific method for [`DataOrdinal`]
setMethod(
  f = "h_plot_data_df",
  signature = signature(data = "DataOrdinal"),
  definition = function(data, blind = FALSE, legend = TRUE, ...) {
    df <- data.frame(
      patient = seq_along(data@x),
      ID = paste(" ", data@ID),
      cohort = data@cohort,
      dose = data@x,
      toxicity = names(data@yCategories)[1 + data@y],
      ...
    )
    df <- h_blind_plot_data(df, blind, data@placebo, data@doseGrid[1])
    df
  }
)


# h_plot_data_dataordinal

## Data ----

#' Helper Function for the Plot Method of the Data and DataOrdinal Classes
#'
#' @description `r lifecycle::badge("stable")`
#'
#' A method that creates a plot for [`Data`]  and [`DataOrdinal`] objects.
#'
#' @note The default values of `tox_shapes` and `tox_labels` result in DLTs
#' being displayed as red triangles and other responses as black circles.
#' @return The [`ggplot2`] object.
#'
#' @rdname plot-Data
h_plot_data_dataordinal <- function(
  x,
  blind = FALSE,
  legend = TRUE,
  tox_labels = c(Yes = "red", No = "black"),
  tox_shapes = c(Yes = 17L, No = 16L),
  mark_backfill = FALSE,
  ...
) {
  assert_flag(blind)
  assert_flag(legend)
  assert_flag(mark_backfill)
  assert_character(tox_labels, any.missing = FALSE, unique = TRUE)
  assert_integer(tox_shapes, any.missing = FALSE, unique = TRUE)
  assert_true(length(tox_shapes) == length(tox_labels))
  assert_subset(x@y, as.integer(0:(length(tox_shapes) - 1)))
  if (x@nObs == 0L) {
    return()
  }
  df <- h_plot_data_df(x, blind, ...)

  # Determine shape key per-point, using open shapes for backfill patients.
  if (mark_backfill) {
    if (!("backfilled" %in% names(df))) {
      # Try to obtain backfilled information from the object if available.
      df$backfilled <- if (
        methods::is(x, "Data") && length(x@backfilled) == nrow(df)
      ) {
        x@backfilled
      } else {
        rep(FALSE, nrow(df))
      }
    }
    df$shape_key <- ifelse(
      df$backfilled,
      paste0(df$toxicity, "_bf"),
      df$toxicity
    )
  } else {
    df$shape_key <- df$toxicity
  }

  # Build shape scale values, adding open-shape variants for backfill when needed.
  shape_values <- tox_shapes
  if (mark_backfill) {
    convert_to_open <- function(s) {
      # Map common filled shapes to their open counterparts.
      switch(
        as.character(s),
        "16" = 1L, # filled circle -> open circle
        "17" = 2L, # filled triangle -> open triangle
        "15" = 0L, # filled square -> open square
        "18" = 5L, # filled diamond -> open diamond
        "19" = 1L, # large filled circle -> open circle
        # If already open or unknown, keep as-is
        as.integer(s)
      )
    }
    bf_shapes <- vapply(unname(tox_shapes), convert_to_open, integer(1))
    names(bf_shapes) <- paste0(names(tox_shapes), "_bf")
    shape_values <- c(shape_values, bf_shapes)
  }

  p <- ggplot(df, aes(x = .data$patient, y = .data$dose)) +
    geom_point(
      aes(shape = .data$shape_key, colour = .data$toxicity),
      size = 3
    ) +
    scale_colour_manual(
      name = "Toxicity",
      values = tox_labels,
      breaks = names(tox_labels),
      guide = guide_legend(
        reverse = TRUE,
        override.aes = list(
          shape = unname(rev(tox_shapes[names(tox_labels)]))
        )
      )
    ) +
    scale_shape_manual(
      values = shape_values,
      guide = "none"
    ) +
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
        aes(label = .data$ID, size = 2),
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

#' Helper Function Containing Common Functionality
#'
#' Used by `dose_grid_range-Data` and `dose_grid_range-DataOrdinal`
#' @param object (`Data` or `DataOrdinal`)\cr the object for which the dose grid
#' range is required
#' @param ignore_placebo (`flag`)\cr should placebo dose (if any) not be counted?
#'
h_obtain_dose_grid_range <- function(object, ignore_placebo) {
  assert_flag(ignore_placebo)

  dose_grid <- if (ignore_placebo && object@placebo && object@nGrid >= 1) {
    object@doseGrid[-1]
  } else {
    object@doseGrid
  }

  if (length(dose_grid) == 0L) {
    c(-Inf, Inf)
  } else {
    range(dose_grid)
  }
}

#' Convert a Ordinal Data to the Equivalent Binary Data for a Specific
#' Grade
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A simple helper function that takes a [`DataOrdinal`] object and an
#' integer grade and converts them to the equivalent `Data` object.
#'
#' @param data_ord (`DataOrdinal`)\cr the `DataOrdinal` object to covert
#' @param grade (`integer`)\cr the toxicity grade for which the equivalent data
#' is required.
#' @return A [`Data`] object.
#'
#' @export
h_convert_ordinal_data <- function(data_ord, grade) {
  # Validate
  assert_integer(grade, len = 1, lower = 1)
  assert_class(data_ord, "DataOrdinal")
  # Execute
  Data(
    ID = data_ord@ID,
    cohort = data_ord@cohort,
    x = data_ord@x,
    y = as.integer(data_ord@y >= grade),
    doseGrid = data_ord@doseGrid,
    nGrid = data_ord@nGrid,
    xLevel = data_ord@xLevel,
    placebo = data_ord@placebo
  )
}
