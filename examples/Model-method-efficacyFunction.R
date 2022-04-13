my_data <- DataDual(
  doseGrid = c(0.001, seq(25, 300, 25)),
  placebo = TRUE
)

my_model <- Effloglog(
  Eff = c(1.223, 2.513),
  Effdose = c(25, 300),
  nu = c(a = 1, b = 0.025),
  data = my_data,
  c = 2
)

eff_fun <- efficacyFunction(my_model, theta1 = -4.8, theta2 = 3.7)
eff_fun(30)
