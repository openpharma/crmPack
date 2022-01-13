# Create some data of class 'Data'
my_data <- Data(
  x = 0.1,
  y = 0,
  doseGrid = c(0.1, 0.5)
)

my_data_2 <- Data(
  x = c(0.1, 0.5),
  y = c(0, 1),
  doseGrid = c(0.1, 0.5)
)

# Append dummy to `x` and `y`.
add_dummy(my_data, where = c("x", "y"))

# Append dummy to `x` and `y`. No effect as `my_data_2@nObs != 1`.
add_dummy(my_data_2, where = c("x", "y"))
