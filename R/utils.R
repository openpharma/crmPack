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
#' @return The `data.frame` object with results from `FUN`.
#'
#' @export
#' @example examples/utils-dapply.R
#'
dapply <- function(df, f, FUN, ...) {
  assert_data_frame(df)

  list_df <- split(df, f = f)
  list_df <- lapply(list_df, FUN, ...)
  df2 <- do.call(rbind, list_df)
  rownames(df2) <- NULL
  df2
}
