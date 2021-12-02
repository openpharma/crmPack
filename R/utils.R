#' Apply a Function to Subsets of Data Frame.
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' `dapply` splits the data `df` into the subsets defined by `f`,
#' and applies function `FUN` to each of the subset.
#' All the results are row-binded and returned as `data.frame` object.
#'
#' @param df (`data frame`)\cr data set to be divided into groups.
#' @param f (`factor` or `formula` or `list`)\cr a factor in the sense that
#'   `as.factor(f)` defines the grouping, or a `list` of such factors in which case
#'   their interaction is used for the grouping. `f` can also be a formula of the form
#'   `~ g1 + ... + gk` to split by the interaction of the variables `g1, ..., gk`.
#'   This parameter is passed directly into [base::split()] function.
#' @param FUN (`function`)\cr the function to be applied to each subset of `df` defined by `f`.
#' @param ... parameters passed to [lapply()], which is used when applying a function `FUN`
#'   over groups defined by `f`.
#'
#' @return The [`data frame`] object with results from `FUN`.
#'
#' @export
#'
# dapply <- function(df, f, FUN, ...) {
#   assert_data_frame(df)
# 
#   list_df <- split(df, f = f)
#   list_df <- lapply(list_df, FUN, ...)
#   do.call(rbind, c(list_df, make.row.names = FALSE))
# }

#' Wrapper of tapply, it assumess that the `FUN` returns an output of length
#' equal to 1 or equal to length of each cell of X. The groupped output of `FUN` is 
#' not sorted with respect to unique values of `INDEX` but it preserve its ordering
#' as originally given in `INDEX`. The length of output object is always equal to 
#' the length of `X`
#' 
#' x <- c(0.1, 5, 5, 5, 0.1, 20, 20)
#' i <- c("B", "B", "B", "B", "A", "A", "A")
#' 
#' tapply(x, i, max)
#' h_tapply(x, i, max)
#' 
#' tapply(x, i, `+`, 100)
#' h_tapply(x, i, `+`, 100)
#' 
#' @export
h_tapply <- function(X, INDEX, FUN, ...) {
  list_splitted <-  tapply(X, INDEX, FUN, ..., simplify = FALSE)
  for(i in names(list_splitted)) {
    X[i == INDEX] <- list_splitted[[i]]
  }
  X
}

#' This is to blind the [`Data`].
#' For each cohort, the placebo is set to the active dose level for that cohort.
#' In addition, all DLTs are assigned to the first subjects in the cohort.
#'
#' @export
h_data_blind <- function(x) {
  assert_class(x, "Data")
  
  if (x@placebo) {
    x@x <- h_tapply(x@x, x@cohort, max)
    x@y <- h_tapply(x@y, x@cohort, sort, decreasing = TRUE)
  }
  x
}
