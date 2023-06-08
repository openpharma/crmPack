data <- list(
  new("Data", x = 1:3, y = 0:2, doseGrid = 1:3, ID = 1L:3L, cohort = 1L:3L),
  new("Data", x = 4:6, y = 0:2, doseGrid = 4:6, ID = 1L:3L, cohort = 1L:3L)
)

doses <- c(1, 2)

seed <- 123

simulations <- GeneralSimulations(data, doses, seed)
