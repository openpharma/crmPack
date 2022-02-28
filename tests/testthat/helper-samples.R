h_as_samples <- function(x) {
  dim_x <- dim(x[[1]])
  n_samples <- if (is.null(dim_x)) {
    length(x[[1]])
  } else {
    dim_x[1]
  }

  Samples(
    data = x,
    options = McmcOptions(samples = n_samples)
  )
}
