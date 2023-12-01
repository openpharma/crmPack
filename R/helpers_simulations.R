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


h_summarize_add_stats <- function(stats_list) {
  # stats_list <- object@additional_stats

  # Extract the parameter names
  param_names <- names(stats_list[[1]])

  # Calculate the average for each parameter
  averages <- lapply(param_names, function(param) {
    values <- sapply(stats_list, function(x) x[[param]])
    mean(values)
  })

  return(list(param_names, averages))
}
