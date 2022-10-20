test_method_exists_for_known_subclasses <- function(cls, method) {
  sub_class_names <- names(getClass(cls)@subclasses)
  lapply(
    sub_class_names,
    function(name) {
      cat(name)
      expect_true(is.function(selectMethod(f = !! method, signature = !! name, optional=TRUE)), info=)
    }
  )
}

test_all_methods_exist_for_known_subclasses <- function(cls) {
  test_method_exists_for_known_subclasses(cls, "tidy")
  # test_method_exists_for_known_subclasses(cls, "augment")
  # test_method_exists_for_known_subclasses(cls, "glimpse")
}

test_that("broom methods exist", {
  test_all_methods_exist_for_known_subclasses("CohortSize")
  test_all_methods_exist_for_known_subclasses("Data")
  test_all_methods_exist_for_known_subclasses("Design")
  test_all_methods_exist_for_known_subclasses("Increments")
  test_all_methods_exist_for_known_subclasses("GeneralModel")
  test_all_methods_exist_for_known_subclasses("NextBest")
  test_all_methods_exist_for_known_subclasses("Stopping")
})

test_that("Data::tidy() works with no observed data", {
  expect_true(Data(doseGrid=1:10) %>% tidy() %>% nrow() > 0)
})

test_tidy_return_value <- function(obj, col_names=c()) {
  rv <- obj %>% tidy()
  if (is.data.frame(rv)) {
    if (length(col_names) == 0) {
      col_names <- slotNames(obj)
    }
    # It's a tibble'
    expect_true(tibble::is_tibble(rv))
    # The correcty class has been prepended to the class name vector
    expect_true(paste0("tbl_", class(obj)[1]) %in% class(rv))
    # The column names are as expected
    expect_true(length(setdiff(names(rv), col_names)) == 0)
  } else {
    expect_true(all(lapply(rv, is.data.frame)))
    expect_true(
      all(
        lapply(
          names(rv),
          function(x) {
            if (length(col_names[[x]]) > 0) {
              cols <- col_names[[x]]
            } else {
              cols <- slotNames(rv[[x]])
            }
            # identical(names(rv[[x]]), cols)
            length(setdiff(names(rv[[x]]), cols) == 0)
          }
        )
      )
    )
  }
}

test_that("tidy() works for subclasses of CohortSize", {
  test_tidy_return_value(CohortSizeRange(intervals=c(30, 100, 1000), cohortSize=c(3, 2, 1)))
  test_tidy_return_value(CohortSizeDLT(DLTintervals=0:2, cohortSize=1:3))
  test_tidy_return_value(CohortSizeConst(size = 3))
  test_tidy_return_value(CohortSizeParts(sizes=c(2, 3)), c("part", "sizes"))
  test_tidy_return_value(
    CohortSizeMax(
      cohortSizeList=c(
        CohortSizeRange(intervals=c(30, 100, 1000), cohortSize=c(3, 2, 1)),
        CohortSizeDLT(DLTintervals=0:2, cohortSize=1:3)
      )
    ),
    col_names=c("Index", "intervals", "cohortSize", "DLTintervals")
  )
  test_tidy_return_value(
    CohortSizeMin(
      cohortSizeList=c(
        CohortSizeRange(intervals=c(30, 100, 1000), cohortSize=c(3, 2, 1)),
        CohortSizeDLT(DLTintervals=0:2, cohortSize=1:3)
      )
    ),
    col_names=c("Index", "intervals", "cohortSize", "DLTintervals")
  )
})

test_that("tidy() works for subclasses of Data", {
  test_tidy_return_value(
    Data(
      x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
      y = c(0, 0, 0, 0, 0, 0, 1, 0),
      doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
    )
  )
  test_tidy_return_value(
    DataDual(
      w = rnorm(8),
      x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
      y = c(0, 0, 0, 0, 0, 0, 1, 0),
      doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
    )
  )

  test_tidy_return_value(
     DataParts(
       x = c(0.1, 0.5, 1.5),
       y = c(0, 0, 0),
       ID = 1:3,
       cohort = 1:3,
       doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2)),
       part = c(1L, 1L, 1L),
       nextPart = 1L,
       part1Ladder = c(0.1, 0.5, 1.5, 3, 6, 10)
    )
  )

  test_tidy_return_value(
    DataMixture(
      xshare = c(12, 14, 16, 18.0),
      yshare = c(0L, 1L, 1L, 1L),
      nObsshare = 4L,
      x = c(0.1, 0.5, 1.5),
      y = c(0, 0, 0),
      ID = 1:3,
      cohort = 1:3,
      doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
    ),
    col_names=c("cohort", "doseGrid", "ID", "nGrid", "nObs", "placebo", "share", "x", "xLevel", "y")
  )

  test_tidy_return_value(
    DataDA(
      u = c(42, 30, 15, 5, 20, 25, 30, 60),
      t0 = c(0, 15, 30, 40, 55, 70, 75, 85),
      Tmax = 60,
      x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
      y = c(0, 0, 1, 1, 0, 0, 1, 0),
      doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
    )
  )
})

