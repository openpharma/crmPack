# Create a sample Data object
sample_data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = c(0, 0, 0, 0, 0, 0, 1, 0),
  cohort = c(1, 2, 3, 4, 5, 6, 6, 6),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2)),
  response = c(0, 0, 0, 0, 0, 1, NA, NA),
  backfilled = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)
)

# Tidy the Data object
tidied_data <- tidy(sample_data)

# Print the tidied data
print(tidied_data)
