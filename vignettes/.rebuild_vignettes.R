# Before each CRAN release, we need to rebuild the precomputed vignettes,
# by executing this script.

source_files <- c(
  "example.Rmd.orig",
  "crmPack-jss-paper.Rmd.orig",
  "knit_print.Rmd.orig",
  "rolling-crm.Rmd.orig",
  "trial_sanity_checks.Rmd.orig"
)
setwd("vignettes")
devtools::load_all("..")

for (source_file in source_files) {
  out <- gsub(".orig", "", source_file)
  usethis::use_build_ignore(file.path("vignettes", source_file))

  cli::cli_alert("Precomputing {.file {source_file}}")
  knitr::knit(input = source_file, output = out, quiet = TRUE)
  cli::cli_alert("Resulting vignette in {.file {out}}")
}
