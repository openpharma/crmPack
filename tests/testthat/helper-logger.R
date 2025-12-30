log_trace_output_type <- if (packageVersion("futile.logger") >= "1.4.9") {
  "message"
} else {
  "output"
}

h_test_logging_enabled <- function() {
  log_msg <- capture.output(
    log_trace("test_that message"),
    type = log_trace_output_type
  )
  verb <- is_logging_enabled()
  log1 <- grepl("TRACE", log_msg)
  log2 <- grepl("test_that message", log_msg)

  verb && log1 && log2
}

h_test_logging_disabled <- function() {
  log_msg <- capture.output(
    log_trace("test_that message"),
    type = log_trace_output_type
  )
  verb <- is_logging_enabled()
  log1 <- length(log_msg) == 0L

  !verb && log1
}
