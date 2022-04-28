# Obtain prior modal estimates given the pseudo data.
# First we used an empty data set such that only the dose levels under
# investigations are given. In total, 12 dose levels are under investigation
# ranging from 25 to 300 mg with increments of 25 (i.e 25, 50, 75, ..., 300).
emptydata <- Data(doseGrid = seq(25, 300, 25))

# Fix two dose levels 25 and 300 mg (DLEdose).
# Total number of subjects treated in each of these levels is 3, (DLEweights).
# The number of subjects observed with a DLE is 1.05 at dose 25 mg and 1.8 at dose 300 mg (binDLE).
my_model1 <- LogisticIndepBeta(
  binDLE = c(1.05, 1.8),
  DLEdose = c(25, 300),
  DLEweights = c(3, 3),
  data = emptydata
)

# Use observed DLE responses to obtain posterior modal estimates.
my_data <- Data(
  x = c(25, 50, 50, 75, 100, 100, 225, 300),
  y = c(0, 0, 0, 0, 1, 1, 1, 1),
  doseGrid = emptydata@doseGrid
)

my_model2 <- LogisticIndepBeta(
  binDLE = c(1.05, 1.8),
  DLEdose = c(25, 300),
  DLEweights = c(3, 3),
  data = my_data
)
