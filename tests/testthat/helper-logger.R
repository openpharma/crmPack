h_test_verbose <- function() {
  log_msg <- capture.output(log_trace("test_that message"))
  verb <- is_logging_verbose()
  log1 <- grepl("TRACE", log_msg)
  log2 <- grepl("test_that message", log_msg)

  verb && log1 && log2
}

h_test_no_verbose <- function() {
  log_msg <- capture.output(log_trace("test_that message"))
  verb <- is_logging_verbose()
  log1 <- length(log_msg) == 0L

  !verb && log1
}
