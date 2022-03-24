h_get_model_4comp <- function() {
  list(
    priormodel = function() {
      y ~ x + 1
    },
    modelspecs = list(x = 2),
    init = list(y = 2),
    sample = c("a", "b")
  )
}

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
    shape = 5,
    rate = 1
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

h_get_dual_endpoint <- function(use_log_dose = FALSE, fixed = TRUE) {
  if (fixed) {
    sigma2W <- 1 # nolint
    rho <- 0
  } else {
    sigma2W <- c(a = 1, b = 2) # nolint
    rho <- c(a = 1.5, b = 2.5)
  }

  DualEndpoint(
    mean = c(0, 1),
    cov = diag(2),
    ref_dose = 2,
    use_log_dose = use_log_dose,
    sigma2W = sigma2W,
    rho = rho
  )
}

h_get_dual_endpoint_rw <- function(use_log_dose = FALSE, rw1 = TRUE, fixed = TRUE) {
  de <- h_get_dual_endpoint(use_log_dose = use_log_dose, fixed = fixed)
  sigma2betaW <- if (fixed) { # nolint
    0.01
  } else {
    c(a = 1, b = 2)
  }

  DualEndpointRW(
    mean = de@betaZ_params@mean,
    cov = de@betaZ_params@cov,
    ref_dose = de@ref_dose,
    use_log_dose = de@use_log_dose,
    sigma2W = de@sigma2W,
    rho = de@rho,
    sigma2betaW = sigma2betaW,
    rw1 = rw1
  )
}

h_get_dual_endpoint_beta <- function(use_log_dose = FALSE, fixed = TRUE) {
  de <- h_get_dual_endpoint(use_log_dose = use_log_dose, fixed = fixed)
  if (fixed) {
    E0 <- 10 # nolint
    Emax <- 50 # nolint
    delta1 <- 3
    mode <- 5
  } else {
    E0 <- c(1, 6) # nolint
    Emax <- c(2, 9) # nolint
    delta1 <- 3 # c(1, 5), waiting for fixing issue no 162 # nolint
    mode <- 5 # c(3, 10), waiting for fixing issue no 162 # nolint
  }

  DualEndpointBeta(
    mean = de@betaZ_params@mean,
    cov = de@betaZ_params@cov,
    ref_dose = de@ref_dose,
    use_log_dose = de@use_log_dose,
    sigma2W = de@sigma2W,
    rho = de@rho,
    E0 = E0,
    Emax = Emax,
    delta1 = delta1,
    mode = mode,
    ref_dose_beta = 10
  )
}
