#' A reporter that outputs slow tests to a CSV file.
CsvSlowReporter <- R6::R6Class(
  "CsvSlowReporter",
  inherit = testthat::SlowReporter,

  public = list(
    file = NULL,

    initialize = function(file = "slow-tests.csv", min_time = 0.5, ...) {
      super$initialize(min_time = min_time, ...)
      self$file <- file
    },

    end_reporter = function() {
      # Keep the normal console output from SlowReporter
      super$end_reporter()

      timings <- self$test_timings

      if (length(timings) == 0) {
        out <- data.frame(
          file = character(),
          test = character(),
          time = numeric(),
          stringsAsFactors = FALSE
        )
      } else {
        out <- do.call(
          rbind,
          lapply(timings, function(x) {
            data.frame(
              file = x$file,
              test = x$test,
              time = x$time,
              stringsAsFactors = FALSE
            )
          })
        )

        out <- out[out$time >= self$min_time, , drop = FALSE]
        out <- out[order(out$time, decreasing = TRUE), , drop = FALSE]
      }

      dir.create(dirname(self$file), recursive = TRUE, showWarnings = FALSE)
      utils::write.csv(out, self$file, row.names = FALSE)
    }
  )
)

# Run like this to identify slow tests that are not yet skipped on CRAN:

if (FALSE) {
  Sys.setenv(NOT_CRAN = "false")
  devtools::test(
    reporter = CsvSlowReporter$new(
      file = "slow-tests-cran.csv",
      min_time = 0.5
    )
  )
  Sys.unsetenv("NOT_CRAN")
}
