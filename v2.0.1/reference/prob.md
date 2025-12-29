# Computing Toxicity Probabilities for a Given Dose, Model and Samples

**\[stable\]**

A function that computes the probability of the occurrence of a DLE at a
specified dose level, based on the model parameters (samples).

## Usage

``` r
prob(dose, model, samples, ...)

# S4 method for class 'numeric,LogisticNormal,Samples'
prob(dose, model, samples, ...)

# S4 method for class 'numeric,LogisticLogNormal,Samples'
prob(dose, model, samples, ...)

# S4 method for class 'numeric,LogisticLogNormalSub,Samples'
prob(dose, model, samples, ...)

# S4 method for class 'numeric,ProbitLogNormal,Samples'
prob(dose, model, samples, ...)

# S4 method for class 'numeric,ProbitLogNormalRel,Samples'
prob(dose, model, samples, ...)

# S4 method for class 'numeric,LogisticLogNormalGrouped,Samples'
prob(dose, model, samples, group, ...)

# S4 method for class 'numeric,LogisticKadane,Samples'
prob(dose, model, samples, ...)

# S4 method for class 'numeric,LogisticKadaneBetaGamma,Samples'
prob(dose, model, samples, ...)

# S4 method for class 'numeric,LogisticNormalMixture,Samples'
prob(dose, model, samples, ...)

# S4 method for class 'numeric,LogisticNormalFixedMixture,Samples'
prob(dose, model, samples, ...)

# S4 method for class 'numeric,LogisticLogNormalMixture,Samples'
prob(dose, model, samples, ...)

# S4 method for class 'numeric,DualEndpoint,Samples'
prob(dose, model, samples, ...)

# S4 method for class 'numeric,LogisticIndepBeta,Samples'
prob(dose, model, samples, ...)

# S4 method for class 'numeric,LogisticIndepBeta,missing'
prob(dose, model, samples, ...)

# S4 method for class 'numeric,OneParLogNormalPrior,Samples'
prob(dose, model, samples, ...)

# S4 method for class 'numeric,OneParExpPrior,Samples'
prob(dose, model, samples, ...)

# S4 method for class 'numeric,LogisticLogNormalOrdinal,Samples'
prob(dose, model, samples, grade, cumulative = TRUE, ...)
```

## Arguments

- dose:

  (`number` or `numeric`)  
  the dose which is targeted. The following recycling rule applies when
  `samples` is not missing: vectors of size 1 will be recycled to the
  size of the sample (i.e. `size(samples)`). Otherwise, `dose` must have
  the same size as the sample.

- model:

  (`GeneralModel` or `ModelTox`)  
  the model for single agent dose escalation or pseudo DLE
  (dose-limiting events)/toxicity model.

- samples:

  (`Samples`)  
  the samples of model's parameters that will be used to compute
  toxicity probabilities. Can also be missing for some models.

- ...:

  model specific parameters when `samples` are not used.

- group:

  (`character` or `factor`)  
  for
  [`LogisticLogNormalGrouped`](https://openpharma.github.io/crmPack/reference/LogisticLogNormalGrouped-class.md),
  indicating whether to calculate the probability for the `mono` or for
  the `combo` arm.

- grade:

  (`integer` or `integer_vector`)  
  The toxicity grade for which probabilities are required

- cumulative:

  (`flag`)  
  Should the returned probability be cumulative (the default) or
  grade-specific?

## Value

A `proportion` or `numeric` vector with the toxicity probabilities. If
non-scalar `samples` were used, then every element in the returned
vector corresponds to one element of a sample. Hence, in this case, the
output vector is of the same length as the sample vector. If scalar
`samples` were used or no `samples` were used, e.g. for pseudo
DLE/toxicity `model`, then the output is of the same length as the
length of the `dose`. In the case of `LogisticLogNormalOrdinal`, the
probabilities relate to toxicities of grade given by `grade`.

## Details

The `prob()` function computes the probability of toxicity for given
doses, using samples of the model parameter(s). If you work with
multivariate model parameters, then assume that your model specific
`prob()` method receives a samples matrix where the rows correspond to
the sampling index, i.e. the layout is then `nSamples x dimParameter`.

## Functions

- `prob(dose = numeric, model = LogisticNormal, samples = Samples)`:

- `prob(dose = numeric, model = LogisticLogNormal, samples = Samples)`:

- `prob(dose = numeric, model = LogisticLogNormalSub, samples = Samples)`:

- `prob(dose = numeric, model = ProbitLogNormal, samples = Samples)`:

- `prob(dose = numeric, model = ProbitLogNormalRel, samples = Samples)`:

- `prob(dose = numeric, model = LogisticLogNormalGrouped, samples = Samples)`:
  method for
  [`LogisticLogNormalGrouped`](https://openpharma.github.io/crmPack/reference/LogisticLogNormalGrouped-class.md)
  which needs `group` argument in addition.

- `prob(dose = numeric, model = LogisticKadane, samples = Samples)`:

- `prob(dose = numeric, model = LogisticKadaneBetaGamma, samples = Samples)`:

- `prob(dose = numeric, model = LogisticNormalMixture, samples = Samples)`:

- `prob(dose = numeric, model = LogisticNormalFixedMixture, samples = Samples)`:

- `prob(dose = numeric, model = LogisticLogNormalMixture, samples = Samples)`:

- `prob(dose = numeric, model = DualEndpoint, samples = Samples)`:

- `prob(dose = numeric, model = LogisticIndepBeta, samples = Samples)`:
  compute toxicity probabilities of the occurrence of a DLE at a
  specified dose level, based on the samples of
  [`LogisticIndepBeta`](https://openpharma.github.io/crmPack/reference/LogisticIndepBeta-class.md)
  model parameters.

- `prob(dose = numeric, model = LogisticIndepBeta, samples = missing)`:
  compute toxicity probabilities of the occurrence of a DLE at a
  specified dose level, based on the
  [`LogisticIndepBeta`](https://openpharma.github.io/crmPack/reference/LogisticIndepBeta-class.md)
  model parameters. All model parameters (except `dose`) should be
  present in the `model` object.

- `prob(dose = numeric, model = OneParLogNormalPrior, samples = Samples)`:

- `prob(dose = numeric, model = OneParExpPrior, samples = Samples)`:

- `prob(dose = numeric, model = LogisticLogNormalOrdinal, samples = Samples)`:

## Note

The `prob()` and
[`dose()`](https://openpharma.github.io/crmPack/reference/dose.md)
functions are the inverse of each other, for all
[`dose()`](https://openpharma.github.io/crmPack/reference/dose.md)
methods for which its first argument, i.e. a given independent variable
that dose depends on, represents toxicity probability.

## See also

[`probFunction()`](https://openpharma.github.io/crmPack/reference/probFunction.md),
[`dose()`](https://openpharma.github.io/crmPack/reference/dose.md),
[`efficacy()`](https://openpharma.github.io/crmPack/reference/efficacy.md).

## Examples

``` r
# Create some data.
my_data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = c(0, 0, 0, 0, 0, 0, 1, 0),
  cohort = c(0, 1, 2, 3, 4, 5, 5, 5),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
)
#> Used default patient IDs!

# Initialize a model, e.g. 'LogisticLogNormal'.
my_model <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 56
)

# Get samples from posterior.
my_options <- McmcOptions(burnin = 100, step = 2, samples = 20)
my_samples <- mcmc(data = my_data, model = my_model, options = my_options)

# Posterior for Prob(DLT | dose = 50).
prob(dose = 50, model = my_model, samples = my_samples)
#>  [1] 0.3922556 0.5412933 0.4465669 0.3337452 0.4031542 0.4031542 0.3530718
#>  [8] 0.4003008 0.4900596 0.4900596 0.4900596 0.4900596 0.4900596 0.4900596
#> [15] 0.4900596 0.4900596 0.2688184 0.2025457 0.2025457 0.3015397

# Create data from the 'DataDual' class.
data_dual <- DataDual(
  x = c(25, 50, 25, 50, 75, 300, 250, 150),
  y = c(0, 0, 0, 0, 0, 1, 1, 0),
  w = c(0.31, 0.42, 0.59, 0.45, 0.6, 0.7, 0.6, 0.52),
  doseGrid = seq(from = 25, to = 300, by = 25)
)
#> Used default patient IDs!
#> Used best guess cohort indices!

# Initialize a toxicity model using 'LogisticIndepBeta' model.
dlt_model <- LogisticIndepBeta(
  binDLE = c(1.05, 1.8),
  DLEweights = c(3, 3),
  DLEdose = c(25, 300),
  data = data_dual
)

# Get samples from posterior.
dlt_sample <- mcmc(data = data_dual, model = dlt_model, options = my_options)

# Posterior for Prob(DLT | dose = 100).
prob(dose = 100, model = dlt_model, samples = dlt_sample)
#>  [1] 0.1723670 0.1723670 0.4748224 0.4748224 0.4748224 0.2628373 0.2628373
#>  [8] 0.4670407 0.4670407 0.4670407 0.4670407 0.4670407 0.4670407 0.4670407
#> [15] 0.4670407 0.4670407 0.4670407 0.4670407 0.4670407 0.4670407
prob(dose = c(50, 150), model = dlt_model)
#> [1] 0.1981823 0.4601234
```
