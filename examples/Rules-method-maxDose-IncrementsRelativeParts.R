# Example of usage for `IncrementsRelativeParts` maxDose class.

# Create an object of class `DataParts`.
my_data <- DataParts(
  x = c(0.1, 0.5, 1.5),
  y = c(0, 0, 0),
  ID = 1:3,
  cohort = 1:3,
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, 10),
  part = c(1L, 1L, 1L),
  nextPart = 1L,
  part1Ladder = c(0.1, 0.5, 1.5, 3, 6, 10)
)

my_increments <- IncrementsRelativeParts(
  dlt_start = 0,
  clean_start = 1
)

max_dose <- maxDose(my_increments, data = my_data)
