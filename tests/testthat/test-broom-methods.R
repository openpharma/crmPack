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

is_valid_type <- function(x) {
  if (tibble::is_tibble(x)) {
    return(TRUE)
  } else {
    return(all(sapply(x, is_valid_type)))
  }
}

check_column_names <- function(obj, tbl, col_names=c()) {
  if (is_tibble(tbl)) {
    if (length(col_names) == 0) {
      col_names <- slotNames(obj)
      return(length(setdiff(names(tbl), col_names)) == 0)
    }
    return(setdiff())
  } else {
    return(
      all(
        sapply(
          names(tbl),
          function(x) {
            check_column_names(slot(obj, x), tbl[[x]])
          }
        )
      )
    )
  }
}

test_that("Data::tidy() works with no observed data", {
  expect_true(Data(doseGrid=1:10) %>% tidy() %>% nrow() > 0)
})

test_tidy_return_value <- function(obj, col_names=c()) {
  rv <- obj %>% tidy()
  if (is.data.frame(rv)) {
    if (length(col_names) == 0) {
      col_names <- slotNames(obj)
    }
    # It's a tibble
    expect_true(tibble::is_tibble(rv))
    # The correctly class has been prepended to the class name vector
    expect_true(paste0("tbl_", class(obj)[1]) %in% class(rv))
    # The column names are as expected
    expect_true(length(setdiff(names(rv), col_names)) == 0)
  } else {
    # It's a list of tibbles or a list of lists of tibbles
    expect_true(is_valid_type(rv))
  }
  check_column_names(obj, rv)
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
      ID = 1:8,
      cohort = 1:8,
      doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
    )
  )
  test_tidy_return_value(
    DataDual(
      w = rnorm(8),
      x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
      y = c(0, 0, 0, 0, 0, 0, 1, 0),
      ID = 1:8,
      cohort = 1:8,
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
      ID = 1:8,
      cohort = 1:8,
      doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
    )
  )
})

test_that("tidy() works for Design", {
  emptydata <- Data(doseGrid = c(1, 3, 5, 10, 15, 20, 25, 40, 50, 80, 100))

  model <- LogisticLogNormal(
             mean=c(-0.85, 1),
             cov=matrix(c(1, -0.5, -0.5, 1), nrow=2),
             ref_dose=56
           )

myNextBest <- NextBestNCRM(
                target=c(0.2, 0.35),
                overdose=c(0.35, 1),
                max_overdose_prob=0.25
              )

mySize1 <- CohortSizeRange(intervals=c(0, 30), cohortSize=c(1, 3))
mySize2 <- CohortSizeDLT(DLTintervals=c(0, 1), cohortSize=c(1, 3))
mySize <- maxSize(mySize1, mySize2)

myStopping1 <- StoppingMinCohorts(nCohorts=3)
myStopping2 <- StoppingTargetProb(target=c(0.2, 0.35), prob=0.5)
myStopping3 <- StoppingMinPatients(nPatients=20)
myStopping <- (myStopping1 & myStopping2) | myStopping3

myIncrements <- IncrementsRelative(intervals=c(0, 20), increments=c(1, 0.33))

design <- Design(
            model=model,
            nextBest=myNextBest,
            stopping=myStopping,
            increments=myIncrements,
            cohortSize=mySize,
            data=emptydata,
            startingDose=3
          )

test_tidy_return_value(design)
})
