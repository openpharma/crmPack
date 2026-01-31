# Fit method for the Samples class

Note this new generic function is necessary because the
[`fitted`](https://rdrr.io/r/stats/fitted.values.html) function only
allows the first argument `object` to appear in the signature. But we
need also other arguments in the signature.

## Usage

``` r
fit(object, model, data, ...)

# S4 method for class 'Samples,GeneralModel,Data'
fit(
  object,
  model,
  data,
  points = data@doseGrid,
  quantiles = c(0.025, 0.975),
  middle = mean,
  ...
)

# S4 method for class 'Samples,DualEndpoint,DataDual'
fit(object, model, data, quantiles = c(0.025, 0.975), middle = mean, ...)

# S4 method for class 'Samples,LogisticIndepBeta,Data'
fit(
  object,
  model,
  data,
  points = data@doseGrid,
  quantiles = c(0.025, 0.975),
  middle = mean,
  ...
)

# S4 method for class 'Samples,Effloglog,DataDual'
fit(
  object,
  model,
  data,
  points = data@doseGrid,
  quantiles = c(0.025, 0.975),
  middle = mean,
  ...
)

# S4 method for class 'Samples,EffFlexi,DataDual'
fit(
  object,
  model,
  data,
  points = data@doseGrid,
  quantiles = c(0.025, 0.975),
  middle = mean,
  ...
)

# S4 method for class 'Samples,LogisticLogNormalOrdinal,DataOrdinal'
fit(
  object,
  model,
  data,
  points = data@doseGrid,
  quantiles = c(0.025, 0.975),
  middle = mean,
  ...
)
```

## Arguments

- object:

  the
  [`Samples`](https://openpharma.github.io/crmPack/reference/Samples-class.md)
  object

- model:

  the
  [`GeneralModel`](https://openpharma.github.io/crmPack/reference/GeneralModel-class.md)
  object

- data:

  the
  [`Data`](https://openpharma.github.io/crmPack/reference/Data-class.md)
  object

- ...:

  passed down to the
  [`prob()`](https://openpharma.github.io/crmPack/reference/prob.md)
  method.

- points:

  at which dose levels is the fit requested? default is the dose grid

- quantiles:

  the quantiles to be calculated (default: 0.025 and 0.975)

- middle:

  the function for computing the middle point. Default:
  [`mean`](https://rdrr.io/r/base/mean.html)

## Value

the data frame with required information (see method details)

## Functions

- `fit(object = Samples, model = GeneralModel, data = Data)`: This
  method returns a data frame with dose, middle, lower and upper
  quantiles for the dose-toxicity curve

- `fit(object = Samples, model = DualEndpoint, data = DataDual)`: This
  method returns a data frame with dose, and middle, lower and upper
  quantiles, for both the dose-tox and dose-biomarker (suffix
  "Biomarker") curves, for all grid points (Note that currently only the
  grid points can be used, because the DualEndpointRW models only allow
  that)

- `fit(object = Samples, model = LogisticIndepBeta, data = Data)`: This
  method return a data frame with dose, middle lower and upper quantiles
  for the dose-DLE curve using DLE samples for “LogisticIndepBeta” model
  class

- `fit(object = Samples, model = Effloglog, data = DataDual)`: This
  method returns a data frame with dose, middle, lower, upper quantiles
  for the dose-efficacy curve using efficacy samples for “Effloglog”
  model class

- `fit(object = Samples, model = EffFlexi, data = DataDual)`: This
  method returns a data frame with dose, middle, lower and upper
  quantiles for the dose-efficacy curve using efficacy samples for
  “EffFlexi” model class

- `fit(object = Samples, model = LogisticLogNormalOrdinal, data = DataOrdinal)`:
  This method returns a data frame with dose, middle, lower and upper
  quantiles for the dose-efficacy curve using efficacy samples for the
  “LogisticLogNormalOrdinal” model class

## Examples

``` r
# nolint start

# Create some data
data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = c(0, 0, 0, 0, 0, 0, 1, 0),
  cohort = c(0, 1, 2, 3, 4, 5, 5, 5),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
)
#> Used default patient IDs!

# Initialize a model
model <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 56
)

# Get posterior for all model parameters
options <- McmcOptions(burnin = 100, step = 2, samples = 2000)
set.seed(94)
samples <- mcmc(data, model, options)

# Extract the posterior mean  (and empirical 2.5 and 97.5 percentile)
# for the prob(DLT) by doses
fitted <- fit(
  object = samples,
  model = model,
  data = data,
  quantiles = c(0.025, 0.975),
  middle = mean
)


# ----------------------------------------------
# A different example using a different model
## we need a data object with doses >= 1:
data <- Data(
  x = c(25, 50, 50, 75, 150, 200, 225, 300),
  y = c(0, 0, 0, 0, 1, 1, 1, 1),
  doseGrid = seq(from = 25, to = 300, by = 25)
)
#> Used default patient IDs!
#> Used best guess cohort indices!


model <- LogisticIndepBeta(
  binDLE = c(1.05, 1.8),
  DLEweights = c(3, 3),
  DLEdose = c(25, 300),
  data = data
)
options <- McmcOptions(burnin = 100, step = 2, samples = 200)
## samples must be from 'Samples' class (object slot in fit)
samples <- mcmc(data, model, options)

fitted <- fit(object = samples, model = model, data = data)

# nolint end
# nolint start

# Create some data
data <- DataDual(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10, 20, 20, 20, 40, 40, 40, 50, 50, 50),
  y = c(0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1),
  w = c(
    0.31,
    0.42,
    0.59,
    0.45,
    0.6,
    0.7,
    0.55,
    0.6,
    0.52,
    0.54,
    0.56,
    0.43,
    0.41,
    0.39,
    0.34,
    0.38,
    0.21
  ),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
)
#> Used default patient IDs!
#> Used best guess cohort indices!

# Initialize the Dual-Endpoint model (in this case RW1)
model <- DualEndpointRW(
  mean = c(0, 1),
  cov = matrix(c(1, 0, 0, 1), nrow = 2),
  sigma2betaW = 0.01,
  sigma2W = c(a = 0.1, b = 0.1),
  rho = c(a = 1, b = 1),
  rw1 = TRUE
)

# Set-up some MCMC parameters and generate samples from the posterior
options <- McmcOptions(burnin = 100, step = 2, samples = 500)
set.seed(94)
samples <- mcmc(data, model, options)

# Extract the posterior mean  (and empirical 2.5 and 97.5 percentile)
# for the prob(DLT) by doses and the Biomarker by doses
fitted <- fit(
  object = samples,
  model = model,
  data = data,
  quantiles = c(0.025, 0.975),
  middle = mean
)

# nolint end
##Obtain the 'fit' the middle, uppper and lower quantiles for the dose-DLE curve
## at all dose levels using a DLE sample, a DLE model and the data
## samples must be from 'Samples' class (object slot)
## we need a data object with doses >= 1:
data <- Data(
  x = c(25, 50, 50, 75, 150, 200, 225, 300),
  y = c(0, 0, 0, 0, 1, 1, 1, 1),
  doseGrid = seq(from = 25, to = 300, by = 25)
)
#> Used default patient IDs!
#> Used best guess cohort indices!
## model must be from 'Model' or 'ModelTox' class e.g using 'LogisticIbdepBeta' model class
model <- LogisticIndepBeta(
  binDLE = c(1.05, 1.8),
  DLEweights = c(3, 3),
  DLEdose = c(25, 300),
  data = data
)
##options for MCMC
options <- McmcOptions(burnin = 100, step = 2, samples = 200)
## samples must be from 'Samples' class (object slot in fit)
samples <- mcmc(data, model, options)

fit(object = samples, model = model, data = data)
#>    dose    middle      lower     upper
#> 1    25 0.2362155 0.08981218 0.4190492
#> 2    50 0.3682220 0.20111117 0.5781557
#> 3    75 0.4575540 0.27620943 0.6661194
#> 4   100 0.5223768 0.33161940 0.7239995
#> 5   125 0.5717475 0.37825701 0.7709958
#> 6   150 0.6107229 0.40917199 0.8141918
#> 7   175 0.6423542 0.42871110 0.8437866
#> 8   200 0.6685943 0.44575657 0.8651832
#> 9   225 0.6907535 0.46059389 0.8819519
#> 10  250 0.7097445 0.47392655 0.8953912
#> 11  275 0.7262234 0.48877922 0.9063659
#> 12  300 0.7406746 0.51041966 0.9148683
##Obtain the 'fit' the middle, uppper and lower quantiles for the dose-efficacy curve
## at all dose levels using an efficacy sample, a pseudo efficacy model and the data
## data must be from 'DataDual' class
data <- DataDual(
  x = c(25, 50, 25, 50, 75, 300, 250, 150),
  y = c(0, 0, 0, 0, 0, 1, 1, 0),
  w = c(0.31, 0.42, 0.59, 0.45, 0.6, 0.7, 0.6, 0.52),
  doseGrid = seq(25, 300, 25),
  placebo = FALSE
)
#> Used default patient IDs!
#> Used best guess cohort indices!
## model must be from 'ModelEff' e.g using 'Effloglog' class
Effmodel <- Effloglog(
  c(1.223, 2.513),
  c(25, 300),
  nu = c(a = 1, b = 0.025),
  data = data,
  c = 0
)
## samples must be from 'Samples' class (object slot in fit)
options <- McmcOptions(burnin = 100, step = 2, samples = 200)
Effsamples <- mcmc(data = data, model = Effmodel, options = options)
fit(object = Effsamples, model = Effmodel, data = data)
#>    dose   middle      lower    upper
#> 1    25 0.465844 -0.1165332 1.007491
#> 2    50 0.822886  0.3886473 1.180275
#> 3    75 1.003435  0.5768330 1.403713
#> 4   100 1.121531  0.6228397 1.630016
#> 5   125 1.208159  0.6601782 1.787815
#> 6   150 1.276017  0.6894266 1.902756
#> 7   175 1.331491  0.7064292 2.018253
#> 8   200 1.378223  0.6925712 2.105017
#> 9   225 1.418476  0.6806345 2.173231
#> 10  250 1.453748  0.6701746 2.233005
#> 11  275 1.485081  0.6687583 2.286102
#> 12  300 1.513225  0.6801521 2.333796
# nolint start

##Obtain the 'fit' the middle, uppper and lower quantiles for the dose-efficacy curve
## at all dose levels using an efficacy sample, the 'EffFlexi' efficacy model and the data
## data must be from 'DataDual' class
data <- DataDual(
  x = c(25, 50, 25, 50, 75, 300, 250, 150),
  y = c(0, 0, 0, 0, 0, 1, 1, 0),
  w = c(0.31, 0.42, 0.59, 0.45, 0.6, 0.7, 0.6, 0.52),
  doseGrid = seq(25, 300, 25),
  placebo = FALSE
)
#> Used default patient IDs!
#> Used best guess cohort indices!
## model must be from 'ModelEff' e.g using 'Effloglog' class
Effmodel <- EffFlexi(
  eff = c(1.223, 2.513),
  eff_dose = c(25, 300),
  sigma2W = c(a = 0.1, b = 0.1),
  sigma2betaW = c(a = 20, b = 50),
  rw1 = FALSE,
  data = data
)

## samples must be from 'Samples' class (object slot in fit)
options <- McmcOptions(burnin = 100, step = 2, samples = 200)
Effsamples <- mcmc(data = data, model = Effmodel, options = options)
fit(object = Effsamples, model = Effmodel, data = data)
#>    dose    middle      lower     upper
#> 1    25 0.7036011  0.6866660 0.7158120
#> 2    50 0.4494999  0.4100730 0.5051847
#> 3    75 0.5795760  0.5125865 0.6297975
#> 4   100 0.6321164 -1.3094061 2.4041159
#> 5   125 0.5879013 -1.3602565 2.2919075
#> 6   150 0.5246635  0.5162698 0.5419775
#> 7   175 0.6355070 -1.7183340 3.1674019
#> 8   200 0.8103666 -3.1066777 5.2791132
#> 9   225 1.0235281 -4.1707667 5.8087216
#> 10  250 1.4342934 -4.1558082 6.0408674
#> 11  275 1.9401852 -1.9698781 5.1282467
#> 12  300 2.5119573  2.5075891 2.5129950

# nolint end
model <- .DefaultLogisticLogNormalOrdinal()
ordinal_data <- .DefaultDataOrdinal()
options <- .DefaultMcmcOptions()
samples <- mcmc(ordinal_data, model, options)

grade1_fit <- fit(samples, model, ordinal_data, grade = 1L)
grade2_fit <- fit(samples, model, ordinal_data, grade = 2L)
```
