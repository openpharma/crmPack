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
    dose = function(x, param1) {
      x
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

h_get_logistic_normal_fixed_mix <- function(log_normal = FALSE) { # nolint
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
    ref_dose = 50,
    log_normal = log_normal
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
    delta1 <- 3
    mode <- 5
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
    ref_dose_beta = 400 # When used for mcmc, it must be greater than data@doseGrid[nGrid].
  )
}

h_get_dual_endpoint_emax <- function(use_log_dose = FALSE, fixed = TRUE) {
  de <- h_get_dual_endpoint(use_log_dose = use_log_dose, fixed = fixed)
  if (fixed) {
    E0 <- 10 # nolint
    Emax <- 50 # nolint
    ED50 <- 20 # nolint
  } else {
    E0 <- c(0, 100) # nolint
    Emax <- c(0, 500) # nolint
    ED50 <- c(0, 500) # nolint
  }

  DualEndpointEmax(
    mean = de@betaZ_params@mean,
    cov = de@betaZ_params@cov,
    ref_dose = de@ref_dose,
    use_log_dose = de@use_log_dose,
    sigma2W = de@sigma2W,
    rho = de@rho,
    E0 = E0,
    Emax = Emax,
    ED50 = ED50,
    ref_dose_emax = 10
  )
}

h_get_logistic_indep_beta <- function(emptydata = FALSE) {
  dose_grid <- seq(25, 300, 25)
  data <- if (emptydata) {
    Data(doseGrid = dose_grid)
  } else {
    Data(
      x = c(25, 50, 50, 75, 100, 100, 225, 300),
      y = c(0, 0, 0, 0, 1, 1, 1, 1),
      ID = 1:8,
      cohort = c(1L, 2L, 2L, 3L, 4L, 4L, 5L, 6L),
      doseGrid = dose_grid
    )
  }

  LogisticIndepBeta(
    binDLE = c(1.05, 1.8),
    DLEdose = c(25, 300),
    DLEweights = c(3, 3),
    data = data
  )
}

h_get_eff_log_log <- function(emptydata = FALSE, dlt_observed_only = FALSE) {
  dose_grid <- seq(25, 300, 25)

  data <- if (emptydata) {
    DataDual(
      doseGrid = dose_grid,
      placebo = FALSE
    )
  } else {
    # Observed data.
    y <- if (dlt_observed_only) {
      rep(1L, 8)
    } else {
      c(0, 0, 0, 0, 1, 1, 1, 1)
    }
    DataDual(
      x = c(25, 50, 50, 75, 100, 100, 225, 300),
      y = y,
      ID = 1:8,
      cohort = c(1L, 2L, 2L, 3L, 4L, 4L, 5L, 6L),
      w = c(0.31, 0.42, 0.59, 0.45, 0.6, 0.7, 0.6, 0.52),
      doseGrid = dose_grid
    )
  }

  Effloglog(
    eff = c(1.223, 2.513),
    eff_dose = c(25, 300),
    nu = c(a = 1, b = 0.025),
    data = data,
    const = 2
  )
}

h_get_eff_flexi <- function(emptydata = FALSE, rw1 = TRUE, dlt_observed_only = FALSE) {
  dose_grid <- seq(25, 300, 25)

  data <- if (emptydata) {
    DataDual(
      doseGrid = dose_grid,
      placebo = FALSE
    )
  } else {
    # Observed data.
    y <- if (dlt_observed_only) {
      rep(1L, 8)
    } else {
      c(0, 0, 0, 0, 1, 1, 1, 1)
    }
    DataDual(
      x = c(25, 50, 50, 75, 100, 100, 225, 300),
      y = y,
      ID = 1:8,
      cohort = c(1L, 2L, 2L, 3L, 4L, 4L, 5L, 6L),
      w = c(0.31, 0.42, 0.59, 0.45, 0.6, 0.7, 0.6, 0.52),
      doseGrid = dose_grid
    )
  }

  EffFlexi(
    eff = c(1.223, 2.513),
    eff_dose = c(25, 300),
    sigma2W = c(a = 0.1, b = 0.1),
    sigma2betaW = c(a = 20, b = 50),
    rw1 = rw1,
    data = data
  )
}

h_get_da_logistic_log_normal <- function() {
  DALogisticLogNormal(
    mean = c(0, 1),
    cov = diag(2),
    ref_dose = 1,
    npiece = 3,
    l = c(0.5, 0.5, 0.5),
    c_par = 2
  )
}
