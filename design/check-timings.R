# Run R CMD check (also devtools::check will include the --timings flag),
# then read the timings file and show the top 10 longest running examples:

timings <- read.table("../crmPack.Rcheck/crmPack-Ex.timings", header = TRUE)

library(dplyr)

timings |>
  arrange(-elapsed) |>
  head(10)
