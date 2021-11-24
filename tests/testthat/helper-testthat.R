expect_valid <- function(object, class) {
  expect_s4_class(object, class)
  expect_true(validObject(object))
}
