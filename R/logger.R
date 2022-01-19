#' Verbose Logging
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A family of wrappers of selected [`futile.logger`] functions that control
#' the logging mechanism in `crmPack`. The `crmPack` uses [`futile.logger`]
#' package for the logging purposes. All the messages logged in `crmPack` are
#' logged into `crmPack` logger at the [`futile.logger::TRACE`] level. Hence,
#' enabling verbose logging means that the logging threshold will be set to
#' [`futile.logger::TRACE`] for the `crmPack` logger, and disabling verbose
#' logging means that it will be set to [`futile.logger::FATAL`].
#'
#' @describeIn enable_logging A simple wrapper of
#'   [futile.logger::flog.threshold()] that enables `crmPack` verbose logging by
#'   setting logging threshold to [`futile.logger::TRACE`] for `crmPack` logger.
#'
#' @export
#'
enable_logging <- function() {
  invisible(
    futile.logger::flog.threshold(futile.logger::TRACE, name = "crmPack")
  )
}

#' @describeIn enable_logging A simple wrapper of
#'   [futile.logger::flog.threshold()] that disables `crmPack` verbose logging
#'   by setting logging threshold to [`futile.logger::FATAL`] for `crmPack`
#'   logger.
#'
#' @export
#'
disable_logging <- function() {
  invisible(
    futile.logger::flog.threshold(futile.logger::FATAL, name = "crmPack")
  )
}

#' @describeIn enable_logging A simple wrapper of
#'   [futile.logger::flog.logger()] that checks whether current threshold level
#'   for `crmPack` logger is verbose, which is [`futile.logger::TRACE`].
#'   It returns `TRUE` if the current logging level is verbose, `FALSE`
#'   otherwise.
#'
#' @export
#'
is_logging_verbose <- function() {
  logger <- futile.logger::flog.logger(name = "crmPack")
  unname(logger$threshold == futile.logger::TRACE)
}

#' @describeIn enable_logging A simple wrapper of
#'   [futile.logger::flog.trace()] that prints a log message in the `crmPack`
#'   logger.
#'
#' @inheritParams futile.logger::flog.trace
#'
#' @export
#'
log_trace <- function(msg, ..., capture = FALSE) {
  assert_string(msg)
  assert_flag(capture)

  futile.logger::flog.trace(msg = msg, ..., name = "crmPack", capture = capture)
}
