library(crmPack)

source("LogisticNormalTruncPrior.R")
crmpack_extensions()

emptydata <- Data(
  doseGrid = c(
    10, 15, 20, 30, 40, 60, 80, 120, 160, 240, 320,
    480, 640, 960, 1280, 1920, 2400, 3000, 4000
  ),
  placebo = F
)

#observed study data
my_data <- Data(
  x = c(10, 20, 40, 80, 80, 160, 160),
  y = c(0, 0, 0, 0, 0, 1, 1),
  cohort = c(1, 2, 3, 4, 4, 5, 5),
  ID = 1:7,
  doseGrid = emptydata@doseGrid
)

#model
my_model <- LogisticNormalTruncPrior(
  mean1 = -3,
  mean2 = 0.00075,
  var1 = 1,
  var2 = 111111.11111
)

#options for simulations
options <- McmcOptions(
  burnin = 10000,
  step = 2,
  samples = 10000,
  rng_kind = "Mersenne-Twister",
  rng_seed = 94
)

# increments
my_increments <- IncrementsRelativeDLT(
  dlt_intervals = c(0, 1),
  increments = c(1, 0.5)
)

# maximum dose
this_max_dose <- maxDose(my_increments, my_data)

# Next best dose
my_next_best <- NextBestMinDist(target = 0.3)
this_next_dose <- nextBest(my_next_best, this_max_dose, my_samples, my_model, my_data)$value

# stopping rule
stopping_1 <- StoppingPatientsNearDose(nPatients = 9, percentage = 0)

#stop trial based on criteria and observed data
stopTrial(stopping_1, this_next_dose, my_samples, my_model, my_data)

# Cohorts size
size <- CohortSizeDLT(
  dlt_intervals = c(0, 1),
  cohort_size = c(1, 3)
)

my_design <- Design(
  model = my_model,
  nextBest = my_next_best,
  stopping = stopping_1,
  increments = my_increments,
  cohortSize = size,
  data = emptydata,
  startingDose = 10
)

examine(my_design, options)

# scenarios
safe_scenario  <- probFunction(my_model, alpha0 = logit(0.05), alpha1 = (logit(0.3)-logit(0.05))/20000)
late_scenario  <- probFunction(my_model, alpha0 = logit(0.05), alpha1 = (logit(0.3)-logit(0.05))/2000)
early_scenario <- probFunction(my_model, alpha0 = logit(0.05), alpha1 = (logit(0.3)-logit(0.05))/700)
toxic_scenario <- probFunction(my_model, alpha0 = logit(0.6), alpha1 = (logit(0.3)-logit(0.6))/-300)
peak_scenario  <- function(dose,
                  scenario=cbind(emptydata@doseGrid, c(rep(0.05,11), rep(0.80,8))))
  {scenario[match(dose, scenario[, 1]), 2]}


nsim = 10
parallel = TRUE

report_time <- function(report_text){
  cat(
    format(Sys.time(), usetz = TRUE),
    report_text,
    "done - used time:",
    round(difftime(Sys.time(), start_time, units = "mins"), digits = 1),
    "\n"
  )
}

get_oc <- function(truth) {
  simulate(
    my_design,
    args = NULL,
    truth = truth,
    nsim = nsim,
    #seed = use_seed,
    mcmcOptions = options,
    parallel = parallel,
    nCores = parallelly::availableCores()
  )
}

time <- system.time({
  start_time <- Sys.time()
  cat(format(Sys.time(), usetz = TRUE), "start", "\n")

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

