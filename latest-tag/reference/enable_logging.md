# Verbose Logging

**\[experimental\]**

A family of wrappers of selected
[`futile.logger::futile.logger`](https://rdrr.io/pkg/futile.logger/man/futile.logger-package.html)
functions that control the logging mechanism in `crmPack`. The `crmPack`
uses
[`futile.logger::futile.logger`](https://rdrr.io/pkg/futile.logger/man/futile.logger-package.html)
package for the logging purposes. All the messages logged in `crmPack`
are logged into `crmPack` logger at the
[`futile.logger::TRACE`](https://rdrr.io/pkg/futile.logger/man/logger.options.html)
level. Hence, enabling verbose logging means that the logging threshold
will be set to
[`futile.logger::TRACE`](https://rdrr.io/pkg/futile.logger/man/logger.options.html)
for the `crmPack` logger, and disabling verbose logging means that it
will be set to
[`futile.logger::FATAL`](https://rdrr.io/pkg/futile.logger/man/logger.options.html).

## Usage

``` r
enable_logging()

disable_logging()

is_logging_enabled()

log_trace(msg, ..., capture = FALSE)
```

## Arguments

- msg:

  The message to log

- ...:

  Optional arguments to populate the format string

- capture:

  Capture print output of variables instead of interpolate

## Functions

- `enable_logging()`: A simple wrapper of
  [`futile.logger::flog.threshold()`](https://rdrr.io/pkg/futile.logger/man/flog.threshold.html)
  that enables `crmPack` verbose logging by setting logging threshold to
  [`futile.logger::TRACE`](https://rdrr.io/pkg/futile.logger/man/logger.options.html)
  for `crmPack` logger.

- `disable_logging()`: A simple wrapper of
  [`futile.logger::flog.threshold()`](https://rdrr.io/pkg/futile.logger/man/flog.threshold.html)
  that disables `crmPack` verbose logging by setting logging threshold
  to
  [`futile.logger::FATAL`](https://rdrr.io/pkg/futile.logger/man/logger.options.html)
  for `crmPack` logger.

- `is_logging_enabled()`: A simple wrapper of
  [`futile.logger::flog.logger()`](https://rdrr.io/pkg/futile.logger/man/flog.logger.html)
  that checks whether current threshold level for `crmPack` logger is
  verbose, which is
  [`futile.logger::TRACE`](https://rdrr.io/pkg/futile.logger/man/logger.options.html).
  It returns `TRUE` if the current logging level is verbose, `FALSE`
  otherwise.

- `log_trace()`: A simple wrapper of
  [`futile.logger::flog.trace()`](https://rdrr.io/pkg/futile.logger/man/flog.logger.html)
  that prints a log message in the `crmPack` logger.
