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

h_get_logistic_indep_beta <- function() {
  LogisticIndepBeta(
    binDLE = c(1.05, 1.8),
    DLEweights = c(3, 3),
    DLEdose = c(25, 300),
    data = h_get_data_dual()
  )
}

h_get_logistic_kadane <- function() {
  LogisticKadane(
    theta = 0.33,
    xmin = 1,
    xmax = 200
  )
}

h_get_logistic_kadane_beta_gam <- function() {
  LogisticKadaneBetaGamma(
    theta = 0.3,
    xmin = 0,
    xmax = 7,
    alpha = 1,
    beta = 19,
    shape = 0.5625,
    rate = 0.125
  )
}

h_get_logistic_normal_mix <- function() {
  LogisticNormalMixture(
    comp1 = ModelParamsNormal(mean = c(0, 3), cov = diag(2)),
    comp2 = ModelParamsNormal(mean = c(-1, 6), cov = c(2, 4) * diag(2)),
    weightpar = c(a = 1, b = 5),
    ref_dose = 2
  )
}

h_get_logistic_normal_fixed_mix <- function() { # nolint
  LogisticNormalFixedMixture(
    components = list(
      comp1 = ModelParamsNormal(
        mean = c(-0.85, 1),
        cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2)
      ),
      comp2 = ModelParamsNormal(
        mean = c(1, 1.5),
        cov = matrix(c(1.2, -0.45, -0.45, 0.6), nrow = 2)
      )
    ),
    weights = c(0.3, 0.7),
    ref_dose = 50
  )
}

h_get_logistic_log_normal_mix <- function() {
  LogisticLogNormalMixture(
    mean = c(0, 1),
    cov = diag(2),
    share_weight = 0.1,
    ref_dose = 1.5
  )
}
