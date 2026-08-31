#' Convenience function to make barplots of percentages
#'
#' @param x vector of samples
#' @param description xlab string
#' @param xaxisround rounding for xaxis labels (default: 0, i.e. integers will
#' be used)
#' @param x_is_discrete whether the values on the x-axis should be treated as
#'   discrete categories
#'
#' @return the ggplot2 object
#'
#' @keywords internal
#' @importFrom ggplot2 ggplot geom_histogram aes xlab ylab xlim
h_barplot_percentages <- function(
  x,
  description,
  xaxisround = 0,
  x_is_discrete = FALSE
) {
  assert_number(xaxisround, lower = 0)
  assert_character(description, len = 1, any.missing = FALSE)
  assert_numeric(x)
  assert_flag(x_is_discrete)

  tabx <- table(x) / length(x)
  dat <- data.frame(
    x = if (x_is_discrete) {
      factor(names(tabx), levels = names(tabx))
    } else {
      as.numeric(names(tabx))
    },
    perc = as.numeric(tabx) * 100
  )
  bar_width <- if (x_is_discrete) {
    0.8
  } else if (nrow(dat) > 1L) {
    min(diff(dat$x)) / 2
  } else {
    1
  }

  plot <- ggplot() +
    geom_bar(
      aes(x = x, y = perc),
      data = dat,
      stat = "identity",
      position = "identity",
      width = bar_width
    ) +
    xlab(description) +
    ylab("Percent")

  if (x_is_discrete) {
    plot + scale_x_discrete(drop = FALSE)
  } else {
    plot +
      scale_x_continuous(
        breaks = round(dat$x, xaxisround)
      )
  }
}


#' Convenience Function to Make Histograms of Percentages
#'
#' Creates a histogram where the height of each bin is the percentage of all
#' observations in that bin.
#'
#' @param x (`numeric`)
#'   vector of samples.
#' @param description (`string`)
#'   x-axis label.
#' @param bins (`count`)
#'   number of histogram bins.
#'
#' @return A `ggplot2` object.
#'
#' @keywords internal
h_histogram_percentages <- function(x, description, bins = 30L) {
  assert_numeric(x, any.missing = FALSE)
  assert_character(description, len = 1L, any.missing = FALSE)
  assert_count(bins, positive = TRUE)

  ggplot(data.frame(x = x), aes(x = x)) +
    geom_histogram(
      aes(y = after_stat(.data$count / sum(.data$count) * 100)),
      bins = bins,
      boundary = 0
    ) +
    xlab(description) +
    ylab("Percent")
}


#' Helper function to calculate percentage of true stopping rules for
#' report label output
#' calculates true column means and converts output into percentages
#' before combining the output with the report label; output is passed
#' to [`show()`] and output with cat to console
#'
#' @param stop_report object from summary method
#' @return named list with label and percentage of rule activation

h_calc_report_label_percentage <- function(stop_report) {
  stop_pct <- colMeans(stop_report) * 100
  stop_pct_to_print <- stop_pct[!is.na(names(stop_pct))]
  stop_pct_to_print
}


#' Helper function to calculate average across iterations for each additional
#' reporting parameter
#' extracts parameter names as specified by user and averaged the values
#' for each specified parameter to [`show()`] and output with cat to console
#'
#' @param stats_list object from simulation with nested parameter values
#' (sublist for each parameter)
#' @return list of parameter names and averaged values for console output

h_summarize_add_stats <- function(stats_list) {
  # Extract the parameter names
  param_names <- names(stats_list[[1]])

  # Calculate the average for each parameter
  averages <- lapply(param_names, function(param) {
    values <- sapply(stats_list, function(x) x[[param]])
    mean(values)
  })

  list(param_names, averages)
}
