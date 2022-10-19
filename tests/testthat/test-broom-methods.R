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
  # test_all_methods_exist_for_known_subclasses("Model")
  # test_all_methods_exist_for_known_subclasses("NextBest")
  test_all_methods_exist_for_known_subclasses("Stopping")
})

test_that("Data::tidy() works with no observed data", {
  expect_true(Data(doseGrid=1:10) %>% tidy() %>% nrow() > 0)
})
