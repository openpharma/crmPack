# Parallel computing with extensions

## Introduction

One of the big advantages of `crmPack` over existing R implementations
is its flexible framework based on the S4 classes and methods system
(Chambers 2008). Users can extend the existing functionality easily to
the specific needs of the study (Sabanés Bové et al. 2019).

User defined extensions of classes and methods can be easily created and
used together with existing `crmPack` classes and methods when setting
up the study and performing analysis with observed data. As long as no
parallel computing is used, no special care needs to be taken utilizing
both native and user defined classes and methods. When trial simulations
need to be performed to derive the operational characteristics of a
study setup, run times may become long with single core processing. The
run time depends mainly on the number of study replications and MCMC
samples. In cases of long run times, utilizing parallel computing, i.e.,
using multiple CPU cores, can overcome these challenge and decrease run
times significantly.

Parallel computing is supported by `crmPack` by design, i.e., whenever
the simulation call is invoked with parameters `parallel = TRUE` and
`nCores =` more than one CPU, each core is initialized with the
`crmPack` package and the global environment.

Due to the nature of the S4 object system, user defined classes and
method that were defined in the global environment cannot just be made
available at each core by initializing the core with the global
environment. User defined classes and method needs to be executed at any
core that is initialized to warrant full functionality of the S4
objects.

The following paragraphs provide an example of how user defined
extensions can be used utilizing parallel computing within `crmPack`.

## High level usage

To use user written extension with parallel computing and `crmPack`, the
user written code needs to be embedded in a function. This function will
be stored in the value of a global option with the fixed name
`crmpack_extensions`. The existence of the global option is evaluated
when parallel computing is requested. In case the option
`crmpack_extensions` exists, the content is submitted to any node of the
cluster, so that the user written S4 classes and methods are available
along with the `crmPack` package.

The following code show at a high level, how the option is utilized.

``` r

options(
  crmpack_extensions = function() {
    # ..... user code .....
  }
)
```

## Important information for usage

When operation characteristics of the design needs to be derived
utilizing parallel computing, the simulation function must be executed
with the parameter `parallel = TRUE` and `nCores =` a number greater
than one. In this case, it is evaluated if a global option with the name
`crmpack_extensions` exists. If this is true, the value of the global
option `crmpack_extensions` is executed at each core at the time of
initialization, i.e., the character string that is equal to the global
option `crmpack_extensions` is executed at any core of the cluster, so
that the code is available for the use with parallel computing.

Please note that the wrapper function that holds the user extensions is
the most convenient way to hand over `class` and `method` definitions
and the user constructor function to the `crmpack_extensions` option.
However, the constructor function must be available in the environment
from which it is called. When the extensions are directly coded within
the wrapper function, the `<<-` operator must be used for any new user
defined constructor function that is defined within the wrapper
function. The `<<-` operator will force a function to become visible
within the global environment. Another possibility is to store the user
extensions in a file that is sourced within the wrapper function. In
this case user defined constructor function can be assigned with the
regular `<-` operator, as source parse the content of the file in the
environment.

To avoid errors it is important to understand that packages attached to
the current environment are not available at the workers environment.
I.e., even though the code may work without any problems when used with
single core processing, it may fail in parallel execution as functions
are not attached to the workers environment. Therefore it is necessary
to make sure that all called functions are available in the workers
environment. This can be achieved by including a `library` statement
within the wrapper function or by using the double colon operator `::`.

Any of the above described usage options are further detailed in the
example.

## Information for debugging

It is expected as a good practice, that any new written extension is
first tested and validated with one CPU core, before utilizing the
global option `crmpack_extensions`. It is important to understand that
the character string that is the value of the global option
`crmpack_extensions` is executed directly at each core and will not be
checked for validity or correctness. In case that the code run into
errors, the returned error message while using parallel computing may be
misleading and may not help to identify the root cause of problem.
Debugging of newly written code is much easier without using parallel
computing.

## Worked out example

Let us assume we want to utilize a two-parameter logistic regression
model, where the slope parameter should have only positive values, with
a normal prior that is truncated for the slope parameter, so that the
slope can have only positive values. In this case we want to set the
prior distribution of the slope parameter to be a truncated normal
distribution bounded by zero on the lower end. This model representation
is not part of the `crmPack` package but can easily be added as shown
below. Please note that for this example the extra code is directly
inserted into the option `crmpack_extensions` as a function. As
mentioned before, user should write and test their code first without
embedding it into a function which is set equal to the option
`crmpack_extensions`, to assure that no errors occur during the whole
execution and the results are as expected.

``` r

library(crmPack)

my_own_extension <- function() {
  # Attach the package checkmate with library(checkmate) here, to avoid usage of
  # the :: operator in the code below.

  # LogisticNormalTruncPrior ----

  ## class ----

  #' `LogisticNormalTruncPrior`
  #'
  #' @description `r lifecycle::badge("experimental")`
  #'
  #' [`LogisticNormalTruncPrior`] is the class for the usual logistic regression
  #'  model with bivariate normal prior on the intercept and slope.
  #'
  #' @aliases LogisticNormalTruncPrior
  #' @export
  #'
  #' @slot mean1 the mean of the intercept
  #' @slot mean2 the mean of the slope
  #' @slot var1 the variance of the intercept
  #' @slot var2 the variance of the slope
  #'
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

  ## constructor ----

  #' @rdname LogisticNormalTruncPrior-class

  #' Initialization function for the `LogisticNormalTruncPrior` class
  #'
  #' @param mean1 the mean of the intercept
  #' @param mean2 the mean of the slope
  #' @param var1 the variance of the intercept
  #' @param var2 the variance of the slope
  #' @return the \code{\linkS4class{LogisticNormalTruncPrior}} object
  #'
  #' @export
  #' @keywords methods
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
        alpha0 ~ dnorm(mean1, 1 / var1)
        alpha1 ~ dnorm(mean2, 1 / var2) %_% I(0, )
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

  ## dose ----

  #' @describeIn dose compute the dose level reaching a specific toxicity
  #'   probability.
  #'
  #' @aliases dose-LogisticNormalTruncPrior
  #' @export
  #'
  setMethod(
    f = "dose",
    signature = signature(
      x = "numeric",
      model = "LogisticNormalTruncPrior",
      samples = "Samples"
    ),
    definition = function(x, model, samples) {
      checkmate::assert_probabilities(x)
      checkmate::assert_subset(c("alpha0", "alpha1"), names(samples))
      assert_length(x, len = size(samples))

      alpha0 <- samples@data$alpha0
      alpha1 <- samples@data$alpha1
      (logit(x) - alpha0) / alpha1
    }
  )

  ## prob ----

  #' @describeIn prob compute the toxicity probability of a specific dose.
  #'
  #' @aliases prob-LogisticNormalTruncPrior
  #' @export
  #'
  setMethod(
    f = "prob",
    signature = signature(
      dose = "numeric",
      model = "LogisticNormalTruncPrior",
      samples = "Samples"
    ),
    definition = function(dose, model, samples) {
      checkmate::assert_numeric(
        dose,
        lower = 0L, any.missing = FALSE, min.len = 1
      )
      checkmate::assert_subset(c("alpha0", "alpha1"), names(samples))
      assert_length(dose, len = size(samples))

      alpha0 <- samples@data$alpha0
      alpha1 <- samples@data$alpha1
      1 / (1 + exp(-alpha0 - alpha1 * dose))
    }
  )
}
```

After the new code is defined and embedded into a function, we can store
the function as value of a global option, which we will name
`crmpack_extensions`.

Please note that `<<-`operator is used to assign the
`LogisticNormalTruncPrior` function to the global environment. The
assertion functions from the package `checkmate` are referenced by the
double colon operator in this example. In case that the package
`checkmate` is attached within the function, i.e., the statement
[`library(checkmate)`](https://mllg.github.io/checkmate/) is used in the
code above, the direct reference `checkmate::` can be removed. Either
way works with parallel computing as this makes sure that the functions
can be used in the current environment and at the workers when parallel
study simulations are performed.

``` r

# Store the function into the global option crmpack_extensions.
options(crmpack_extensions = my_own_extension)
```

Now we are able to use the newly created model
`LogisticNormalTruncPrior` to set up a study. First, we get the value
from the global option `crmpack_extensions`, which is the function that
we have defined above. Then we execute the function within the global
environment, so that the constructor function and the corresponding
classes and methods for `prob` and `dose` become available.

``` r

# Execute the user written extensions.
getOption("crmpack_extensions")()
```

Next we can set up the study with the new model and desired design
features.

``` r

# Create the dose grid.
emptydata <- Data(
  doseGrid = c(
    10, 15, 20, 30, 40, 60, 80, 120, 160, 240, 320,
    480, 640, 960, 1280, 1920, 2400, 3000, 4000
  ),
  placebo = FALSE
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
  intervals = c(0, 1),
  increments = c(1, 0.5)
)

# Maximum dose.
this_max_dose <- maxDose(my_increments, my_data)

# Next best dose.
my_next_best <- NextBestMinDist(target = 0.3)
this_next_dose <- nextBest(
  my_next_best, this_max_dose, my_samples, my_model, my_data
)$value

# Stopping rule.
my_stopping <- StoppingPatientsNearDose(nPatients = 9, percentage = 0)

# Stop trial based on criteria and observed data.
stopTrial(my_stopping, this_next_dose, my_samples, my_model, my_data)

# Cohorts size.
my_size <- CohortSizeDLT(
  intervals = c(0, 1),
  cohort_size = c(1, 3)
)

# Design.
my_design <- Design(
  model = my_model,
  nextBest = my_next_best,
  stopping = my_stopping,
  increments = my_increments,
  cohort_size = my_size,
  data = emptydata,
  startingDose = 10
)
```

After setting up the model and the design features, it is very useful to
check the model decisions in case that no DLT is observed until a
certain dose level before any study simulations are performed. This
check can also serve as additional test for the new written code.

``` r

# Examine the design.
examine(my_design, my_options)
```

When examine runs as expected, study simulations can be performed. To
demonstrate the difference between single core processing and multiple
core processing, two scenarios where processed with single core
processing, followed by 5 scenarios utilizing parallel computing.

With regard to run times in this example, please see the [note](#note)
at the end of the vignette.

``` r

# Set up scenarios
scenario_setup <- function(intercept, mtd_prob, mtd_dose) {
  probFunction(
    my_model,
    alpha0 = logit(intercept),
    alpha1 = (logit(mtd_prob) - logit(intercept)) / mtd_dose
  )
}

safe_scenario <- scenario_setup(0.05, 0.3, 20000)
late_scenario <- scenario_setup(0.05, 0.3, 2000)
early_scenario <- scenario_setup(0.05, 0.3, 700)
toxic_scenario <- scenario_setup(0.6, 0.3, -300)
peak_scenario <- function(
    dose,
    scenario = cbind(emptydata@doseGrid, c(rep(0.05, 11), rep(0.80, 8)))) {
  scenario[match(dose, scenario[, 1]), 2]
}

# Helper function that outputs the elapsed time.
report_time <- function(report_text) {
  cat(
    format(Sys.time(), usetz = TRUE),
    report_text,
    "done - elapsed time from start:",
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

# get operation characteristics without utilizing parallel computing for
# selected truth (to reduce the run time).
time_no_parallel <- system.time({
  start_time <- Sys.time()
  cat(format(Sys.time(), usetz = TRUE), "start", "\n")

  my_nsim <- 10
  do_parallel <- FALSE

  safe <- get_oc(safe_scenario)

  report_time("safe (single core processing)")

  late <- get_oc(late_scenario)

  report_time("late (single core processing)")
})
```

When simulations with single core processing are running successful, the
full operation characteristics utilizing parallel computing can be
derived.

``` r

# Get full operation characteristics utilizing parallel computing.
time <- system.time({
  start_time <- Sys.time()
  cat(format(Sys.time(), usetz = TRUE), "start", "\n")

  my_nsim <- 10
  do_parallel <- TRUE

  safe <- get_oc(safe_scenario)

  report_time("safe")

  late <- get_oc(late_scenario)

  report_time("late")

  early <- get_oc(early_scenario)

  report_time("early")

  toxic <- get_oc(toxic_scenario)

  report_time("toxic")

  peak <- get_oc(peak_scenario)

  report_time("peak")
})
```

## Alternative: read user code from external file

As an alternative it may be more convenient to store the user extensions
in an external file and source the file in the wrapper function. This
has the advantage, that the `<<-` operator is not necessary for user
constructor functions and can be replaced by the usual assignment `<-`.

``` r

if (FALSE) {
  # Store code example form above in external file and
  # remove the wrapper function structure.
  dump("my_own_extension", file = "user_extension.R")
  file_con <- file("user_extension.R")
  tmp <- readLines(file_con)[-c(1:3, 135)]
  tmp <- gsub("<<-", "<-", tmp)
  writeLines(tmp, file_con)

  # Source the stored file in the wrapper function.
  my_own_extension2 <- function() {
    source("user_extension.R")
  }

  options(crmpack_extensions = my_own_extension2)
  getOption("crmpack_extensions")()

  # Run the rest of the code from above example
}
```

## Note

The analyses presented in this vignette have used chains of a very short
length as well as a very limited number of trial simulations. This is
purely for convenience. Study simulations for real trials should use
considerably longer chains and a much higher number of trial
simulations.

In this example it is apparent that for small number of study
simulations, the overhead of initializing the cluster for parallel
computing, leads to longer run time of the simulations in comparison to
single core processing. However, for a relevant number of study
simulations, the substantial run time benefit utilizing parallel
computation can easily be seen.

## References

Chambers, John. 2008. *Software for Data Analysis*. Statistics and
Computing. Springer-Verlag.

Sabanés Bové, Daniel, Wai Yin Yeung, Giuseppe Palermo, and Thomas Jaki.
2019. “Model-Based Dose Escalation Designs in r with crmPack.” *Journal
of Statistical Software* 89. <https://doi.org/10.18637/jss.v089.i10>.
