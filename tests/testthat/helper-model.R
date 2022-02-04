h_get_general_model <- function() {
  .GeneralModel(
    datamodel = function(x) {
      x
    },
    priormodel = function(x) {
      x
    },
    modelspecs = function(x) {
      x
    },
    init = function(x) {
      x
    },
    sample = "param1",
    datanames = "x"
  )
}

h_get_model <- function() {
  .Model(
    dose = function(prob, param1) {
      prob
    },
    prob = function(dose, param1) {
      dose
    },
    datamodel = function(x) {
      x
    },
    priormodel = function(x) {
      x
    },
    modelspecs = function(x) {
      x
    },
    init = function(x) {
      x
    },
    sample = "param1",
    datanames = "x"
  )
}

h_get_model_log_normal <- function() {
  ModelLogNormal(
    mean = c(-0.85, 1),
    cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
    ref_dose = 50
  )
}

h_get_logistic_normal <- function() {
  LogisticNormal(
    mean = c(-0.85, 1),
    cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
    ref_dose = 50
  )
}

h_get_logistic_log_normal <- function() {
  LogisticLogNormal(
    mean = c(-0.85, 1),
    cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
    ref_dose = 50
  )
}

h_get_logistic_log_normal_sub <- function() {
  LogisticLogNormalSub(
    mean = c(1, 5),
    cov = diag(4, ncol = 2, nrow = 2),
    ref_dose = 2
  )
}

h_get_probit_log_normal_rel <- function() {
  ProbitLogNormalRel(
    mean = c(-0.85, 1),
    cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
    ref_dose = 2
  )
}

h_get_probit_log_normal <- function() {
  ProbitLogNormal(
    mean = c(-0.85, 1),
    cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
    ref_dose = 7.2
  )
}
