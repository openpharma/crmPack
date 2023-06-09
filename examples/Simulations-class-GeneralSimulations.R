data <- list(
  Data(x = 1:3, y = c(0,1,0), doseGrid = 1:3, ID = 1L:3L, cohort = 1L:3L),
  Data(x = 4:6, y = c(0,1,0), doseGrid = 4:6, ID = 1L:3L, cohort = 1L:3L)
)

doses <- c(1, 2)

seed <- 123L

simulations <- GeneralSimulations(data, doses, seed)
