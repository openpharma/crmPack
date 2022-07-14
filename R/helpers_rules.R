#' Calculating the Information Theoretic Distance
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Helper function which provides the value of the
#' divergence as given by equation in (7) in the reference at
#' https://doi.org/10.1002/sim.8450.
#'
#' @param prob (`numeric`)\cr vector or matrix with probabilities of a DLT occurring.
#' @param target (`number `)\cr single target probability of a DLT.
#' @param asymmetry (`number`)\cr describes the rate of penalization
#'   for overly toxic does, range 0 to 2.
#'
#' @export
#' @examples
#' h_info_theory_dist(c(0.5, 0.2), 0.4, 1.2)
h_info_theory_dist <- function(prob, target, asymmetry) {
  assert_probabilities(prob)
  assert_true(test_vector(prob) || test_matrix(prob))
  assert_number(target, finite = TRUE)
  assert_number(asymmetry, lower = 0, upper = 2)

  ((prob - target)^2) / (((prob^asymmetry) * (1 - prob)^(2 - asymmetry)))
}
