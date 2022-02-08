h_as_samples <- function(x) {
  Samples(
    data = x,
    options = McmcOptions(samples = length(x[[1]]))
  )
}
