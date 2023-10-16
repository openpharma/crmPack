my_data <- DataGrouped(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = c(0, 0, 1, 1, 0, 0, 1, 0),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2)),
  group = c("mono", "mono", "mono", "mono", "mono", "mono", "combo", "combo")
)

# Set up an empty data set.
empty_data <- DataGrouped(
  doseGrid = c(0.1, 0.5, 1, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
)
empty_data
