#' Convenience function to make barplots of percentages
#'
#' @param x vector of samples
#' @param description xlab string
#' @param xaxisround rounding for xaxis labels (default: 0, i.e. integers will
#' be used)
#'
#' @return the ggplot2 object
#'
#' @keywords internal
#' @importFrom ggplot2 ggplot geom_histogram aes xlab ylab xlim
h_barplot_percentages <- function(x, description, xaxisround = 0) {
  assert_number(xaxisround, lower = 0)
  assert_character(description, len = 1, any.missing = FALSE)
  assert_numeric(x)

  tabx <- table(x) / length(x)
  dat <- data.frame(x = as.numeric(names(tabx)), perc = as.numeric(tabx) * 100)
  ggplot() +
    geom_bar(aes(x = x, y = perc),
      data = dat,
      stat = "identity",
      position = "identity",
      width = ifelse(nrow(dat) > 1, min(diff(dat$x)) / 2, 1)
    ) +
    xlab(description) +
    ylab("Percent") +
    scale_x_continuous(
      breaks =
        round(dat$x, xaxisround)
    )
}



#' calculate percentage of true stopping rules for report label output
#'
#' @description
#'
#' calculates true column means and converts output into percentages
#' before combining the output with the report label; output is passed
#' to [`show()`] and output with cat to console
#'
#' @param stop_report object from summary method


h_calc_report_label_percentage <- function(stop_report) {
  stop_pct <- colMeans(stop_report) * 100
  stop_pct_to_print <- stop_pct[!is.na(names(stop_pct))]
  return(stop_pct_to_print)
}



#' calculate average across iterations for each additional reporting parameter
#'
#' @description
#'
#' extracts parameter names as specified by user and averaged the values
#' for each specified parameter to [`show()`] and output with cat to console
#'
#' @param stats_list object from simulation with nested parameter values
#' (sublist for each parameter)


h_summarize_add_stats <- function(stats_list) {
  # Extract the parameter names
  param_names <- names(stats_list[[1]])

  # Calculate the average for each parameter
  averages <- lapply(param_names, function(param) {
    values <- sapply(stats_list, function(x) x[[param]])
    mean(values)
  })

  return(list(param_names, averages))
}
