library(crmPack)

crmpack_extensions <- function() {
  .LogisticNormalTruncPrior <- setClass(
    Class = "LogisticNormalTruncPrior",
    contains = "GeneralModel",
    slots = c(
      mean1 = "numeric",
      mean2 = "numeric",
      var1 = "numeric",
      var2 = "numeric"
    )
  )

  LogisticNormalTruncPrior <<- function(mean1, mean2, var1, var2) {
    .LogisticNormalTruncPrior(
      mean1 = mean1,
      mean2 = mean2,
      var1 = var1,
      var2 = var2,
      datamodel = function() {
        for (i in 1:nObs) {
          y[i] ~ dbern(mean[i])
          logit(mean[i]) <- alpha0 + alpha1 * x[i]
        }
      },
      priormodel = function() {
        alpha0 ~ dnorm(mean1, var1)
        alpha1 ~ dnorm(mean2, var2) %_% I(0, )
      },
      datanames = c("nObs", "y", "x"),
      modelspecs = function() {
        list(
          mean1 = mean1,
          mean2 = mean2,
          var1 = var1,
          var2 = var2
        )
      },
      init = function() {
        list(alpha0 = mean1, alpha1 = mean2)
      },
      sample = c("alpha0", "alpha1")
    )
  }

  setMethod(
    f = "dose",
    signature = signature(
      x = "numeric",
      model = "LogisticNormalTruncPrior",
      samples = "Samples"
    ),
    definition = function(x, model, samples) {
      alpha0 <- samples@data$alpha0
      alpha1 <- samples@data$alpha1
      (logit(x) - alpha0) / alpha1
    }
  )

  setMethod(
    f = "prob",
    signature = signature(
      dose = "numeric",
      model = "LogisticNormalTruncPrior",
      samples = "Samples"
    ),
    definition = function(dose, model, samples) {
      alpha0 <- samples@data$alpha0
      alpha1 <- samples@data$alpha1
      1 / (1 + exp(-alpha0 - alpha1 * dose))
    }
  )
}

# Execute the user written extensions.
crmpack_extensions()

# Create the dose grid.
emptydata <- Data(
  doseGrid = c(
    10, 15, 20, 30, 40, 60, 80, 120, 160, 240, 320,
    480, 640, 960, 1280, 1920, 2400, 3000, 4000
  ),
  placebo = F
)

# Create data for basic testing of the setup.
my_data <- Data(
  x = c(10, 20, 40, 80, 80, 160, 160),
  y = c(0, 0, 0, 0, 0, 1, 1),
  cohort = c(1, 2, 3, 4, 4, 5, 5),
  ID = 1:7,
  doseGrid = emptydata@doseGrid
)

# Setup the model.
my_model <- LogisticNormalTruncPrior(
  mean1 = -3,
  mean2 = 0.00075,
  var1 = 1,
  var2 = 0.000009
)

# Options used for simulations.
my_options <- McmcOptions(
  burnin = 100,
  step = 2,
  samples = 100,
  rng_kind = "Mersenne-Twister",
  rng_seed = 94
)

# Create mcmc samples.
my_samples <- mcmc(my_data, my_model, my_options)

# Plot the dose toxicity curve.
plot(my_samples, my_model, my_data)

# Specify increments.
my_increments <- IncrementsRelativeDLT(
  dlt_intervals = c(0, 1),
  increments = c(1, 0.5)
)

# Maximum dose.
this_max_dose <- maxDose(my_increments, my_data)

# Next best dose.
my_next_best <- NextBestMinDist(target = 0.3)
this_next_dose <- nextBest(my_next_best, this_max_dose, my_samples, my_model, my_data)$value

# Stopping rule.
my_stopping <- StoppingPatientsNearDose(nPatients = 9, percentage = 0)

# Stop trial based on criteria and observed data.
stopTrial(my_stopping, this_next_dose, my_samples, my_model, my_data)

# Cohorts size.
my_size <- CohortSizeDLT(
  dlt_intervals = c(0, 1),
  cohort_size = c(1, 3)
)

# Design.
my_design <- Design(
  model = my_model,
  nextBest = my_next_best,
  stopping = my_stopping,
  increments = my_increments,
  cohortSize = my_size,
  data = emptydata,
  startingDose = 10
)

# Examine the design.
examine(my_design, my_options)

# Set up simulation scenarios.
safe_scenario  <- probFunction(my_model, alpha0 = logit(0.05), alpha1 = (logit(0.3)-logit(0.05))/20000)
late_scenario  <- probFunction(my_model, alpha0 = logit(0.05), alpha1 = (logit(0.3)-logit(0.05))/2000)
early_scenario <- probFunction(my_model, alpha0 = logit(0.05), alpha1 = (logit(0.3)-logit(0.05))/700)
toxic_scenario <- probFunction(my_model, alpha0 = logit(0.6), alpha1 = (logit(0.3)-logit(0.6))/-300)
peak_scenario  <- function(dose,
                           scenario=cbind(emptydata@doseGrid, c(rep(0.05,11), rep(0.80,8))))
{scenario[match(dose, scenario[, 1]), 2]}


# Helper function that outputs the used time.
report_time <- function(report_text){
  cat(
    format(Sys.time(), usetz = TRUE),
    report_text,
    "done - used time:",
    round(difftime(Sys.time(), start_time, units = "mins"), digits = 1),
    "\n"
  )
}

# Helper function that simulates a specific truth.
get_oc <- function(truth) {
  simulate(
    my_design,
    args = NULL,
    truth = truth,
    nsim = my_nsim,
    mcmcOptions = my_options,
    parallel = do_parallel,
    nCores = parallelly::availableCores()
  )
}

# get operation characteristics without utilizing parallel computing for one truth.
time_no_parallel <- system.time({
  start_time <- Sys.time()
  cat(format(Sys.time(), usetz = TRUE), "start", "\n")

  my_nsim = 10
  do_parallel = FALSE

  safe <- get_oc(safe_scenario)

  report_time("safe")
})

# Get full operation characteristics utilizing parallel computing.
time_parallel <- system.time({
  start_time <- Sys.time()
  cat(format(Sys.time(), usetz = TRUE), "start", "\n")

  my_nsim = 10
  do_parallel = TRUE

  safe <- get_oc(safe_scenario)

  report_time("safe")

  late <- get_oc(late_scenario)

  report_time("late")

  early <- get_oc(early_scenario)

  report_time("late")

  toxic <- get_oc(toxic_scenario)

  report_time("late")

  peak <- get_oc(peak_scenario)

  report_time("peak")
})
