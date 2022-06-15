

# Create the data
data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = c(0, 0, 0, 0, 0, 0, 1, 0),
  cohort = c(0, 1, 2, 3, 4, 5, 5, 5),
  doseGrid =
    c(
      0.1, 0.5, 1.5, 3, 6,
      seq(from = 10, to = 80, by = 2)
    )
)

# Initialize the CRM model used to model the data
model <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov =
    matrix(c(1, -0.5, -0.5, 1),
      nrow = 2
    ),
  ref_dose = 56
)

# Set-up some MCMC parameters and generate samples from the posterior
options <- McmcOptions(
  burnin = 100,
  step = 2,
  samples = 2000
)
set.seed(94)
samples <- mcmc(data, model, options)

# Define the rule for dose increments and calculate the maximum dose allowed
my_increments <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)
next_max_dose <- maxDose(my_increments,
  data = data
)

# Define the rule which will be used to select the next best dose
# based on the class 'NextBestNCRMLoss'
my_next_best <- NextBestNCRMLoss(
  target_int = c(0.2, 0.35),
  overdose_int = c(0.35, 0.6),
  unacceptable_int = c(0.6, 1),
  max_overdose_prob = 0.25,
  losses = c(1, 0, 1, 2)
)

# Calculate the next best dose
dose_recommendation <- nextBest(myNextBest,
  doselimit = nextMaxDose,
  samples = samples,
  model = model,
  data = data
)

# Look at the probabilities
doseRecommendation$`Interval probabilities`
