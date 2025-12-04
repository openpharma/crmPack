# Create some data.
my_data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = c(0, 0, 0, 0, 0, 0, 1, 0),
  ID = 1:8,
  cohort = c(0, 1, 2, 3, 4, 5, 5, 5),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
)

# Initialize the CRM model used to model the data.
my_model <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 50
)

# Set-up some MCMC parameters and generate samples from the posterior.
my_options <- McmcOptions(burnin = 10, step = 2, samples = 40)
my_samples <- mcmc(my_data, my_model, my_options)

# Define the rule which will be used to select the next best dose
# based on the class 'NextBestNCRM'.
my_nb_ncrm <- NextBestNCRM(
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)

# Calculate the next best dose.
my_dose_recommendation <- nextBest(
  nextBest = my_nb_ncrm,
  doselimit = 100,
  samples = my_samples,
  model = my_model,
  data = my_data
)

# Define the stopping rules.
highest_dose_safe <- StoppingSpecificDose(
  rule = StoppingTargetProb(target = c(0, 0.3), prob = 0.8),
  dose = 80
)
max_patients <- StoppingMinPatients(nPatients = 20)
patients_near_dose <- StoppingPatientsNearDose(nPatients = 3, percentage = 0)

# Create a list of stopping rules (of class 'StoppingList') which will then be
# summarized (in this specific example) with the 'any' function, meaning that
# the study would be stopped if 'any' of the single stopping rules is TRUE.
my_stopping <- highest_dose_safe | max_patients | patients_near_dose

# Evaluate if to stop the Trial
stopTrial(
  stopping = my_stopping,
  dose = my_dose_recommendation$value,
  samples = my_samples,
  model = my_model,
  data = my_data
)
