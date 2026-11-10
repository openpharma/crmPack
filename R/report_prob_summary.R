#' Evaluate Dose-toxicity Probability Samples
#'
#' @param samples A `Samples` object.
#' @param model A `GeneralModel` object with a scalar toxicity probability.
#' @param data A single-agent `Data` object.
#' @param points Numeric doses at which to evaluate the curves.
#' @return A matrix with samples in rows and doses in columns.
#' @keywords internal
h_prob_samples <- function(samples, model, data, points = data@doseGrid) {
  assert_class(samples, "Samples")
  assert_class(model, "GeneralModel")
  assert_class(data, "Data")
  assert_numeric(
    points,
    lower = 0,
    finite = TRUE,
    any.missing = FALSE,
    min.len = 1
  )
  if (size(samples) < 1L) {
    stop("At least one probability sample is required.")
  }
  values <- lapply(points, function(dose) {
    p <- prob(dose, model, samples)
    if (
      !is.numeric(p) ||
        !is.null(dim(p)) ||
        length(p) != size(samples) ||
        anyNA(p) ||
        any(!is.finite(p)) ||
        any(p < 0 | p > 1)
    ) {
      stop(
        "The model must return one toxicity probability per sample and dose."
      )
    }
    p
  })
  matrix(unlist(values), nrow = size(samples), ncol = length(points))
}

#' Summarize Prior or Posterior Toxicity Probabilities
#'
#' Evaluate existing samples without rerunning MCMC. Intervals are left-closed
#' and right-open, except the last interval includes its upper endpoint.
#'
#' @param samples A `Samples` object, from prior or posterior sampling.
#' @param model A single-agent `GeneralModel` with scalar toxicity probabilities.
#' @param data A `Data` object supplying the dose grid.
#' @param quantiles Unique quantile probabilities in `[0, 1]`; use `numeric()`
#'   to omit quantiles.
#' @param mean,sd Logical flags selecting the mean and standard deviation.
#' @param intervals Optional strictly increasing interval boundaries in `[0, 1]`.
#' @return A `tbl_ProbSummary` tibble with one row per dose and unrounded
#'   mean, SD, quantile and interval-probability columns as requested.
#' @export
report_prob_summary <- function(
  samples,
  model,
  data,
  quantiles = c(0.025, 0.5, 0.975),
  mean = TRUE,
  sd = TRUE,
  intervals = NULL
) {
  assert_flag(mean)
  assert_flag(sd)
  assert_numeric(
    quantiles,
    lower = 0,
    upper = 1,
    any.missing = FALSE,
    unique = TRUE
  )
  if (!is.null(intervals)) {
    assert_numeric(
      intervals,
      lower = 0,
      upper = 1,
      any.missing = FALSE,
      min.len = 2
    )
    if (any(diff(intervals) <= 0)) {
      stop("Interval boundaries must be strictly increasing.")
    }
  }
  p <- h_prob_samples(samples, model, data)
  out <- tibble::tibble(dose = data@doseGrid)
  if (mean) {
    out$mean <- colMeans(p)
  }
  if (sd) {
    out$sd <- apply(p, 2L, stats::sd)
  }
  for (q in quantiles) {
    name <- paste0("q", format(q * 100, trim = TRUE, scientific = FALSE))
    if (name %in% names(out)) {
      stop("Quantile column names must be unique.")
    }
    out[[name]] <- apply(p, 2L, stats::quantile, probs = q, names = FALSE)
  }
  for (i in seq_len(max(0L, length(intervals) - 1L))) {
    last <- i == length(intervals) - 1L
    name <- paste0(
      "p_[",
      intervals[i],
      ",",
      intervals[i + 1L],
      if (last) "]" else ")"
    )
    out[[name]] <- colMeans(
      p >= intervals[i] &
        if (last) p <= intervals[i + 1L] else p < intervals[i + 1L]
    )
  }
  class(out) <- c("tbl_ProbSummary", class(out))
  out
}

#' Plot Random Prior or Posterior Dose-toxicity Curves
#'
#' Each curve uses the same sampled parameter vector at every dose. Sampling
#' is without replacement on a fine grid spanning the supplied dose grid.
#'
#' @inheritParams report_prob_summary
#' @param n Number of curves, no greater than the number of available samples.
#' @param seed Optional random seed; when supplied the caller's RNG state is preserved.
#' @return A `ggplot` object; probabilities are displayed on a percentage scale.
#' @export
plot_prior_draws <- function(samples, model, data, n = 100, seed = NULL) {
  assert_class(data, "Data")
  assert_count(n, positive = TRUE)
  if (!is.null(seed)) {
    assert_count(seed)
    had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    if (had_seed) {
      old_seed <- get(".Random.seed", envir = .GlobalEnv)
    }
    on.exit(
      {
        if (had_seed) {
          assign(".Random.seed", old_seed, envir = .GlobalEnv)
        } else if (
          exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
        ) {
          rm(".Random.seed", envir = .GlobalEnv)
        }
      },
      add = TRUE
    )
    set.seed(seed)
  }
  points <- sort(unique(c(
    data@doseGrid,
    seq(min(data@doseGrid), max(data@doseGrid), length.out = 101)
  )))
  p <- h_prob_samples(samples, model, data, points)
  if (n > nrow(p)) {
    stop("'n' must not exceed the number of available samples.")
  }
  draws <- sample.int(nrow(p), n)
  curves <- data.frame(
    dose = rep(points, each = n),
    probability = as.vector(p[draws, , drop = FALSE]) * 100,
    draw = rep(seq_len(n), length(points))
  )
  ggplot(
    curves,
    aes(x = .data$dose, y = .data$probability, group = .data$draw)
  ) +
    geom_line(alpha = 0.2) +
    coord_cartesian(ylim = c(0, 100)) +
    labs(x = "Dose level", y = "Probability of DLT [%]")
}

#' Render a Probability Summary Table
#'
#' @inheritParams report_table
#' @param digits Number of decimal places for display.
#' @return A `knitr_kable` or `gt_tbl` object.
#' @export
report_table.tbl_ProbSummary <- function(
  x,
  format = c("kable", "gt"),
  ...,
  digits = 3L
) {
  format <- match.arg(format)
  assert_count(digits)
  if (format == "kable") {
    return(knitr::kable(
      as.data.frame(x),
      format = "pipe",
      digits = digits,
      row.names = FALSE,
      ...
    ))
  }
  if (!requireNamespace("gt", quietly = TRUE)) {
    stop(
      "Package 'gt' is required for format = 'gt'. Please install it.",
      call. = FALSE
    )
  }
  table <- gt::gt(as.data.frame(x), ...)
  table <- gt::fmt_number(
    table,
    columns = setdiff(names(x), "dose"),
    decimals = digits
  )
  for (prefix in c("q", "p_")) {
    columns <- names(x)[startsWith(names(x), prefix)]
    if (length(columns)) {
      table <- gt::tab_spanner(
        table,
        label = if (prefix == "q") {
          "Quantiles"
        } else {
          "Probability of true DLT rate in ..."
        },
        columns = columns
      )
    }
  }
  table
}

#' Render a Probability Summary in Knitr
#'
#' @param x A `tbl_ProbSummary` tibble.
#' @param ... Arguments passed to [report_table()].
#' @param options Knitr chunk options (unused).
#' @return Markdown wrapped in `knitr::asis_output()`.
#' @export
knit_print.tbl_ProbSummary <- function(x, ..., options = NULL) {
  h_knit_report_table(x, ...)
}
