# Some model function
my_model <- function() {
  alpha0 <- mean(1:10)
  alpha1 <- 600000
}

write_model(my_model, file = "my_model.jags", digits = 5)
