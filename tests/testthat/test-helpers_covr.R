test_that("h_covr_active infers covr running through environment variable", {
  withr::with_envvar(c(R_COVR = ""), {
    expect_false(h_covr_active())
  })

  withr::with_envvar(c(R_COVR = "true"), {
    expect_true(h_covr_active())
  })
})

test_that("h_is_covr_trace tests whether an expression appears to be a covr counter", {
  expect_true(h_is_covr_trace(quote(if (TRUE) {
    covr:::count("file.R:1:2:3:4:5:6:7:8")
    1 + 2
  })))

  expect_false(h_is_covr_trace(quote(if (TRUE) {
    covr:::another_function("file.R:1:2:3:4:5:6:7:8")
    1 + 2
  })))

  expect_false(h_is_covr_trace(quote(if (TRUE) {
    1 + 2
  })))
})

test_that("h_covr_detrace_call extracts un-modified code from covr trace", {
  expr <- quote(if (TRUE) {
    covr:::count("file.R:1:2:3:4:5:6:7:8")
    1 + 2
  })

  expect_equal(h_covr_detrace_call(expr), quote(1 + 2))

  expr <- quote(if (TRUE) {
    covr:::another_function("file.R:1:2:3:4:5:6:7:8")
    1 + 2
  })

  expect_equal(h_covr_detrace_call(expr), expr)
})

test_that("h_covr_detrace removes all covr traces", {
  expr <- quote(if (TRUE) {
    covr:::count("file.R:1:2:3:4:5:6:7:8")
    1 + 2 + if (TRUE) {
      covr:::count("file.R:11:12:13:14:15:16:17:18")
      three()
    }
  })

  expect_equal(
    withr::with_envvar(c(R_COVR = "true"), h_covr_detrace(expr)),
    quote(1 + 2 + three())
  )

  expect_equal(
    withr::with_envvar(c(R_COVR = ""), h_covr_detrace(expr)),
    expr
  )

  expr <- quote(if (TRUE) {
    covr:::another_function("file.R:1:2:3:4:5:6:7:8")
    1 + 2
  })

  expect_equal(
    withr::with_envvar(c(R_COVR = "true"), h_covr_detrace(expr)),
    expr
  )
})
