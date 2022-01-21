# Create an object of class 'DataParts'.
my_data <- DataParts(
  x = c(0.1, 0.5, 1.5),
  y = c(0, 0, 0),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2)),
  part = c(1L, 1L, 1L),
  nextPart = 1L,
  part1Ladder = c(0.1, 0.5, 1.5, 3, 6, 10)
)

# Update the data with a new cohort.
# Note that since we reached the last level from 'part1Ladder'
# then the 'nextPart' is switched from '1' to '2'.
my_data1 <- update(my_data, x = 10, y = 0L)
my_data1
