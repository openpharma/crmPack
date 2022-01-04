# Some model function.
my_model <- function() {
  alpha0 <- mean(1:10)
  alpha1 <- 600000
}

# Replace format of numbers using `h_format_number` function.
my_model_sci_replaced <- h_rapply(
  x = body(my_model),
  fun = h_format_number,
  classes = c("integer", "numeric")
)
