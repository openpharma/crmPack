
# create an object of class 'DataParts'
my_Data <- DataParts(
  x = c(0.1, 0.5, 1.5),
  y = c(0, 0, 0),
  doseGrid = c(
    0.1, 0.5, 1.5, 3, 6,
    seq(from = 10, to = 80, by = 2)
  ),
  part = c(1L, 1L, 1L),
  nextPart = 1L,
  part1Ladder = c(0.1, 0.5, 1.5, 3, 6, 10)
)


my_Increments <- IncrementsRelativeParts(
  dltStart = 0,
  cleanStart = 1
)

nex_tMaxDose <- maxDose(my_Increments,
  data = my_Data
)
