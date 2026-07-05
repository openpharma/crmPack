# Before each CRAN release, we need to rebuild the precomputed vignettes,
# by executing this script.
#
# Important: The webshot2 package and a Chrome/Chromium browser need to be
# installed to capture htmlwidget figures in example.Rmd successfully.

rm(list = ls())

if (!requireNamespace("webshot2", quietly = TRUE)) {
  stop(
    "The webshot2 package is required to rebuild the precomputed vignettes. ",
    "Please install it before running this script.",
    call. = FALSE
  )
}

setwd("vignettes")
devtools::load_all("..")

source_files <- commandArgs(trailingOnly = TRUE)
if (length(source_files) == 0L) {
  source_files <- sort(list.files(pattern = "[.]Rmd[.]orig$"))
}

for (source_file in source_files) {
  out <- sub("[.]orig$", "", source_file)

  cli::cli_alert("Precomputing {.file {source_file}}")
  knitr::knit(input = source_file, output = out, quiet = TRUE)
  cli::cli_alert("Resulting vignette in {.file {out}}")
}
