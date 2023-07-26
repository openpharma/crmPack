# nolint start

test_that("CrmPackClass correctly identifies crmPack classes", {
  crmPack_class_list <- getClasses(asNamespace("crmPack"))
  exclusions <- c("DualEndpoint")
  crmPack_class_list <- setdiff(crmPack_class_list, exclusions)

  for (cls in crmPack_class_list) {
    if (!isClassUnion(cls)) {
      constructor_name <- paste0(".Default", cls)
      if (exists(constructor_name, mode = "function")) {
        expect_true(is(do.call(paste0(".Default", !!cls), list()), "CrmPackClass"))
      } else {
        # TODO:
        #  1: Create missing default constructors
        #  2: Convert this message to an error
        message(paste0("No default constructor for ", cls))
        expect_true(TRUE)
      }
    }
  }
})


test_that("CrmPackClass does not identify random non-crmPack classes", {
  non_crmPack_object_list <- list(
    tibble::tibble(),
    glm(mpg ~ wt + gear, data = mtcars)
  )

  for (obj in non_crmPack_object_list) {
    expect_false(is(obj, "CrmPackClass"))
  }
})

# nolint end
