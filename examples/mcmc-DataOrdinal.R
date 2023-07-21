data <- DataOrdinal(
  doseGrid = seq(10, 100, 10),
  x = c(10, 20, 30, 40, 50, 50, 50),
  y = c(0L, 0L, 0L, 0L, 0L, 1L, 2L),
  ID = 1L:7L,
  cohort = as.integer(c(1:4, 5, 5, 5)),
  yCategories = c("No Tox" = 0L, "Sub tox AE" = 1L, "DLT" = 2L)
)
model <- .DefaultLogisticLogNormalOrdinal()
options <- McmcOptions(rng_kind = "Mersenne-Twister", rng_seed = 1234567)
samples <- mcmc(data, model, options)
