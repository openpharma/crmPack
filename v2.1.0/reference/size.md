# Size of an Object

**\[stable\]**

A method that computes the size of a given object. This can be for
instance a size of a MCMC sample, or the size of a cohort. See the help
of a specific method for more details.

## Usage

``` r
size(object, ...)

# S4 method for class 'McmcOptions'
size(object, ...)

# S4 method for class 'CohortSizeRange'
size(object, dose, data)

# S4 method for class 'CohortSizeDLT'
size(object, dose, data)

# S4 method for class 'CohortSizeMax'
size(object, dose, data)

# S4 method for class 'CohortSizeMin'
size(object, dose, data)

# S4 method for class 'CohortSizeConst'
size(object, dose, ...)

# S4 method for class 'CohortSizeRandom'
size(object, dose, ...)

# S4 method for class 'CohortSizeParts'
size(object, dose, data)

# S4 method for class 'CohortSizeOrdinal'
size(object, dose, data, ...)

# S4 method for class 'Samples'
size(object, ...)
```

## Arguments

- object:

  (`McmcOptions` or `Samples` or `CohortSize`)  
  an object for which the size is computed.

- ...:

  not used.

- dose:

  (`numeric`) the next dose.

- data:

  the data input, an object of class
  [`DataOrdinal`](https://openpharma.github.io/crmPack/reference/DataOrdinal-class.md).

## Value

A size of a given object.

## Functions

- `size(McmcOptions)`: compute the number of MCMC samples based on
  `McmcOptions` object.

- `size(CohortSizeRange)`: Determines the size of the next cohort based
  on the range into which the next dose falls into.

- `size(CohortSizeDLT)`: Determines the size of the next cohort based on
  the number of DLTs so far.

- `size(CohortSizeMax)`: Determines the size of the next cohort based on
  maximum of multiple cohort size rules.

- `size(CohortSizeMin)`: Determines the size of the next cohort based on
  minimum of multiple cohort size rules.

- `size(CohortSizeConst)`: Constant cohort size.

- `size(CohortSizeRandom)`: Random cohort size drawn uniformly between
  min and max size.

- `size(CohortSizeParts)`: Determines the size of the next cohort based
  on the parts.

- `size(CohortSizeOrdinal)`: Determines the size of the next cohort in a
  ordinal CRM trial.

- `size(Samples)`: get the number of MCMC samples from `Samples` object.

## Examples

``` r
# Set up the MCMC option in order to have a burn-in of 10000 iterations and
# then take every other iteration up to a collection of 10000 samples.
my_options <- McmcOptions(burnin = 10000, step = 2, samples = 10000)

size(my_options)
#> [1] 10000
# nolint start

# Create the data
data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = c(0, 0, 0, 0, 0, 0, 1, 0),
  cohort = c(0, 1, 2, 3, 4, 5, 5, 5),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
)
#> Used default patient IDs!

# Initialize the CRM model used to model the data
model <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 56
)

# Set-up some MCMC parameters and generate samples from the posterior
options <- McmcOptions(burnin = 100, step = 2, samples = 2000)
set.seed(94)
samples <- mcmc(data, model, options)

# Define the rule for dose increments and calculate the maximum dose allowed
myIncrements <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)
nextMaxDose <- maxDose(myIncrements, data = data)

# Define the rule which will be used to select the next best dose
# based on the class 'NextBestNCRM'
myNextBest <- NextBestNCRM(
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)

# Calculate the next best dose
doseRecommendation <- nextBest(
  myNextBest,
  doselimit = nextMaxDose,
  samples = samples,
  model = model,
  data = data
)

# Rule for the cohort size:
#   - having cohort of size 1 for doses <10
#   - and having cohort of size 3 for doses >=10
mySize <- CohortSizeRange(intervals = c(0, 10), cohort_size = c(1, 3))

# Determine the cohort size for the next cohort
size(mySize, dose = doseRecommendation$value, data = data)
#> [1] 3

# nolint end
# nolint start

# Create the data
data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = c(0, 0, 0, 0, 0, 0, 1, 0),
  cohort = c(0, 1, 2, 3, 4, 5, 5, 5),
  doseGrid = c(
    0.1,
    0.5,
    1.5,
    3,
    6,
    seq(from = 10, to = 80, by = 2)
  )
)
#> Used default patient IDs!

# Initialize the CRM model used to model the data
model <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
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
myIncrements <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)
nextMaxDose <- maxDose(myIncrements, data = data)

# Define the rule which will be used to select the next best dose
# based on the class 'NextBestNCRM'
myNextBest <- NextBestNCRM(
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)

# Calculate the next best dose
doseRecommendation <- nextBest(
  myNextBest,
  doselimit = nextMaxDose,
  samples = samples,
  model = model,
  data = data
)

# Rule for the cohort size:
#   - having cohort of size 1 if no DLTs were yet observed
#   - and having cohort of size 3 if at least 1 DLT was already observed
mySize <- CohortSizeDLT(
  intervals = c(0, 1),
  cohort_size = c(1, 3)
)

# Determine the cohort size for the next cohort
size(mySize, dose = doseRecommendation$value, data = data)
#> [1] 3

# nolint end
# nolint start

# Create the data
data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = c(0, 0, 0, 0, 0, 0, 1, 0),
  cohort = c(0, 1, 2, 3, 4, 5, 5, 5),
  doseGrid = c(
    0.1,
    0.5,
    1.5,
    3,
    6,
    seq(from = 10, to = 80, by = 2)
  )
)
#> Used default patient IDs!

# Initialize the CRM model used to model the data
model <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
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
myIncrements <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)
nextMaxDose <- maxDose(myIncrements, data = data)

# Define the rule which will be used to select the next best dose
# based on the class 'NextBestNCRM'
myNextBest <- NextBestNCRM(
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)

# Calculate the next best dose
doseRecommendation <- nextBest(
  myNextBest,
  doselimit = nextMaxDose,
  samples = samples,
  model = model,
  data = data
)

# Rule for having cohort of size 1 for doses <30
#      and having cohort of size 3 for doses >=30
mySize1 <- CohortSizeRange(
  intervals = c(0, 10),
  cohort_size = c(1, 3)
)

# Rule for having cohort of size 1 until no DLT were observed
#      and having cohort of size 3 as soon as 1 DLT is observed
mySize2 <- CohortSizeDLT(
  intervals = c(0, 1),
  cohort_size = c(1, 3)
)

# Combining the two rules for cohort size by taking the maximum of the sample sizes
# of the single rules
mySize <- maxSize(mySize1, mySize2)

# Determine the cohort size for the next cohort
size(mySize, dose = doseRecommendation$value, data = data)
#> [1] 3

# nolint end
# nolint start

# Create the data
data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = c(0, 0, 0, 0, 0, 0, 1, 0),
  cohort = c(0, 1, 2, 3, 4, 5, 5, 5),
  doseGrid = c(
    0.1,
    0.5,
    1.5,
    3,
    6,
    seq(from = 10, to = 80, by = 2)
  )
)
#> Used default patient IDs!

# Initialize the CRM model used to model the data
model <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
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
myIncrements <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)
nextMaxDose <- maxDose(myIncrements, data = data)

# Define the rule which will be used to select the next best dose
# based on the class 'NextBestNCRM'
myNextBest <- NextBestNCRM(
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)

# Calculate the next best dose
doseRecommendation <- nextBest(
  myNextBest,
  doselimit = nextMaxDose,
  samples = samples,
  model = model,
  data = data
)

# Rule for having cohort of size 1 for doses <30
#      and having cohort of size 3 for doses >=30
mySize1 <- CohortSizeRange(
  intervals = c(0, 30),
  cohort_size = c(1, 3)
)

# Rule for having cohort of size 1 until no DLT were observed
#      and having cohort of size 3 as soon as 1 DLT is observed
mySize2 <- CohortSizeDLT(
  intervals = c(0, 1),
  cohort_size = c(1, 3)
)

# Combining the two rules for cohort size by taking the minimum of the sample sizes
# of the single rules
mySize <- minSize(mySize1, mySize2)

# Determine the cohort size for the next cohort
size(mySize, dose = doseRecommendation$value, data = data)
#> [1] 1

# nolint end
# nolint start

# Create the data
data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = c(0, 0, 0, 0, 0, 0, 1, 0),
  cohort = c(0, 1, 2, 3, 4, 5, 5, 5),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
)
#> Used default patient IDs!

# Initialize the CRM model used to model the data
model <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 56
)

# Set-up some MCMC parameters and generate samples from the posterior
options <- McmcOptions(burnin = 100, step = 2, samples = 2000)
set.seed(94)
samples <- mcmc(data, model, options)

# Define the rule for dose increments and calculate the maximum dose allowed
myIncrements <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)
nextMaxDose <- maxDose(myIncrements, data = data)

# Define the rule which will be used to select the next best dose
# based on the class 'NextBestNCRM'
myNextBest <- NextBestNCRM(
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)

# Calculate the next best dose
doseRecommendation <- nextBest(
  myNextBest,
  doselimit = nextMaxDose,
  samples = samples,
  model = model,
  data = data
)

# Rule for having cohorts with constant cohort size of 3
mySize <- CohortSizeConst(size = 3)

# Determine the cohort size for the next cohort
size(mySize, dose = doseRecommendation$value)
#> [1] 3

# nolint end
# Rule for having cohorts with random cohort size between 2 and 4
mySize <- CohortSizeRandom(min_size = 2, max_size = 4)

# Determine the cohort size for the next cohort
# This will return a random integer between 2 and 4 (inclusive)
set.seed(123)
size(mySize, dose = 5)
#> [1] 4
# nolint start

# create an object of class 'DataParts'
data <- DataParts(
  x = c(0.1, 0.5, 1.5),
  y = c(0, 0, 0),
  doseGrid = c(
    0.1,
    0.5,
    1.5,
    3,
    6,
    seq(from = 10, to = 80, by = 2)
  ),
  part = c(1L, 1L, 1L),
  nextPart = 1L,
  part1Ladder = c(0.1, 0.5, 1.5, 3, 6, 10)
)
#> Used default patient IDs!
#> Used best guess cohort indices!

# Initialize the CRM model used to model the data
model <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
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

myIncrements <- IncrementsRelativeParts(
  dlt_start = 0,
  clean_start = 1
)
nextMaxDose <- maxDose(myIncrements, data = data)

# Define the rule which will be used to select the next best dose
# based on the class 'NextBestNCRM'
myNextBest <- NextBestNCRM(
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)

# Calculate the next best dose
doseRecommendation <- nextBest(
  myNextBest,
  doselimit = nextMaxDose,
  samples = samples,
  model = model,
  data = data
)

# Rule for the cohort size:
mySize <- CohortSizeParts(cohort_sizes = c(1, 3))

# Determine the cohort size for the next cohort
size(mySize, dose = doseRecommendation$value, data = data)
#> [1] 1

# nolint end
CohortSizeOrdinal(
  grade = 1L,
  rule = CohortSizeRange(intervals = c(0L, 30L), cohort_size = c(1L, 3L))
)
#> An object of class "CohortSizeOrdinal"
#> Slot "grade":
#> [1] 1
#> 
#> Slot "rule":
#> An object of class "CohortSizeRange"
#> Slot "intervals":
#> [1]  0 30
#> 
#> Slot "cohort_size":
#> [1] 1 3
#> 
#> 
# Set up the MCMC option in order to have a burn-in of 100 iterations and
# then take every other iteration up to a collection of 200 samples.
my_options <- McmcOptions(burnin = 100, step = 2, samples = 200)

my_samples <- Samples(
  data = list(alpha = rnorm(200), beta = rnorm(200)),
  options = my_options
)

size(my_samples)
#> [1] 200
```
