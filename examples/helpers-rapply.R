# Some model function.
my_model <- function() {
  alpha0 <- mean(1:10)
  alpha1 <- 600000
}

# Replace format of numbers using `formatC` function.
h_rapply(
  x = body(my_model),
  fun = formatC,
  classes = c("integer", "numeric"),
  digits = 3,
  format = "E"
)
