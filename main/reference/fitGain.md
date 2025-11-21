# Get the fitted values for the gain values at all dose levels based on a given pseudo DLE model, DLE sample, a pseudo efficacy model, a Efficacy sample and data. This method returns a data frame with dose, middle, lower and upper quantiles of the gain value samples

Get the fitted values for the gain values at all dose levels based on a
given pseudo DLE model, DLE sample, a pseudo efficacy model, a Efficacy
sample and data. This method returns a data frame with dose, middle,
lower and upper quantiles of the gain value samples

## Usage

``` r
fitGain(DLEmodel, DLEsamples, Effmodel, Effsamples, data, ...)

# S4 method for class 'ModelTox,Samples,ModelEff,Samples,DataDual'
fitGain(
  DLEmodel,
  DLEsamples,
  Effmodel,
  Effsamples,
  data,
  points = data@doseGrid,
  quantiles = c(0.025, 0.975),
  middle = mean,
  ...
)
```

## Arguments

- DLEmodel:

  the DLE pseudo model of
  [`ModelTox`](https://openpharma.github.io/crmPack/reference/ModelTox-class.md)
  class object

- DLEsamples:

  the DLE samples of
  [`Samples`](https://openpharma.github.io/crmPack/reference/Samples-class.md)
  class object

- Effmodel:

  the efficacy pseudo model of
  [`ModelEff`](https://openpharma.github.io/crmPack/reference/ModelEff-class.md)
  class object

- Effsamples:

  the efficacy samples of
  [`Samples`](https://openpharma.github.io/crmPack/reference/Samples-class.md)
  class object

- data:

  the data input of
  [`DataDual`](https://openpharma.github.io/crmPack/reference/DataDual-class.md)
  class object

- ...:

  additional arguments for methods

- points:

  at which dose levels is the fit requested? default is the dose grid

- quantiles:

  the quantiles to be calculated (default: 0.025 and 0.975)

- middle:

  the function for computing the middle point. Default:
  [`mean`](https://rdrr.io/r/base/mean.html)

## Functions

- `fitGain( DLEmodel = ModelTox, DLEsamples = Samples, Effmodel = ModelEff, Effsamples = Samples, data = DataDual )`:
  This method returns a data frame with dose, middle, lower, upper
  quantiles for the gain values obtained given the DLE and the efficacy
  samples

## Examples

``` r
##Obtain the 'fitGain' the middle, uppper and lower quantiles for the samples of gain values
## at all dose levels using a pseudo DLE model, a DLE sample, a pseudo Efficacy model and
## a efficacy sample
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
## DLE model must be from 'ModelTox' class e.g using 'LogisticIndepBeta' model
DLEmodel <- LogisticIndepBeta(
  binDLE = c(1.05, 1.8),
  DLEweights = c(3, 3),
  DLEdose = c(25, 300),
  data = data
)

## Efficacy model must be from 'ModelEff' class e.g using 'Effloglog' model
Effmodel <- Effloglog(
  c(1.223, 2.513),
  c(25, 300),
  nu = c(a = 1, b = 0.025),
  data = data,
  c = 0
)
## samples must be from 'Samples' class (object slot in fit)
options <- McmcOptions(burnin = 100, step = 2, samples = 200)
##set up the same data set in class 'Data' for MCMC sampling for DLE
data1 <- Data(x = data@x, y = data@y, doseGrid = data@doseGrid)
#> Used default patient IDs!
#> Used best guess cohort indices!

DLEsamples <- mcmc(data = data1, model = DLEmodel, options = options)
Effsamples <- mcmc(data = data, model = Effmodel, options = options)

fitGain(
  DLEmodel = DLEmodel,
  DLEsamples = DLEsamples,
  Effmodel = Effmodel,
  Effsamples = Effsamples,
  data = data
)
#>    dose    middle       lower     upper
#> 1    25 0.3005989 -0.08718754 0.7332158
#> 2    50 0.5185839  0.23160384 0.8886416
#> 3    75 0.6105264  0.30266627 0.9820510
#> 4   100 0.6618601  0.30095400 1.0883378
#> 5   125 0.6946738  0.28103149 1.1346464
#> 6   150 0.7174815  0.26318215 1.2011287
#> 7   175 0.7342843  0.25135980 1.2535173
#> 8   200 0.7472094  0.23857418 1.2962004
#> 9   225 0.7574892  0.22706268 1.3318520
#> 10  250 0.7658857  0.21668277 1.3754153
#> 11  275 0.7728943  0.20729352 1.4124810
#> 12  300 0.7788504  0.19876788 1.4322783
##Obtain the 'fitGain' the middle, uppper and lower quantiles for the samples of gain values
## at all dose levels using a pseudo DLE model, a DLE sample, a pseudo Efficacy model and
## a efficacy sample
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
## DLE model must be from 'ModelTox' class e.g using 'LogisticIndepBeta' model
DLEmodel <- LogisticIndepBeta(
  binDLE = c(1.05, 1.8),
  DLEweights = c(3, 3),
  DLEdose = c(25, 300),
  data = data
)

## Efficacy model must be from 'ModelEff' class e.g using 'Effloglog' model
Effmodel <- Effloglog(
  c(1.223, 2.513),
  c(25, 300),
  nu = c(a = 1, b = 0.025),
  data = data,
  c = 0
)
## samples must be from 'Samples' class (object slot in fit)
options <- McmcOptions(burnin = 100, step = 2, samples = 200)
##set up the same data set in class 'Data' for MCMC sampling for DLE
data1 <- Data(x = data@x, y = data@y, doseGrid = data@doseGrid)
#> Used default patient IDs!
#> Used best guess cohort indices!

DLEsamples <- mcmc(data = data1, model = DLEmodel, options = options)
Effsamples <- mcmc(data = data, model = Effmodel, options = options)

fitGain(
  DLEmodel = DLEmodel,
  DLEsamples = DLEsamples,
  Effmodel = Effmodel,
  Effsamples = Effsamples,
  data = data
)
#>    dose    middle      lower     upper
#> 1    25 0.3035639 -0.1419281 0.7336815
#> 2    50 0.5442238  0.2441919 0.8627864
#> 3    75 0.6400330  0.3130091 1.0534260
#> 4   100 0.6901998  0.3051387 1.1878586
#> 5   125 0.7198842  0.2679615 1.2763298
#> 6   150 0.7386820  0.2648011 1.3378176
#> 7   175 0.7510732  0.2812509 1.3772132
#> 8   200 0.7594264  0.2943053 1.4172885
#> 9   225 0.7651054  0.2841997 1.4543642
#> 10  250 0.7689465  0.2694592 1.4789627
#> 11  275 0.7714876  0.2563641 1.4993280
#> 12  300 0.7730880  0.2446324 1.5163999
```
