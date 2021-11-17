# Validate ----

test_that("Validate returns NULL for valid object, or message for invalid object", {
  
  msg_A <- "some_msg_A"
  msg_B <- "some_msg_B"
  
  o <- Validate()
  
  expect_identical(o$msg, character(0))
  expect_null(o$check(TRUE, msg_A))
  expect_identical(o$msg, character(0))
  
  expect_identical(o$check(FALSE, msg_A), msg_A)
  expect_identical(o$msg, msg_A)
  
  expect_identical(o$check(FALSE, msg_B), c(msg_A, msg_B))
  expect_null(o$check(TRUE, msg_B))
})
