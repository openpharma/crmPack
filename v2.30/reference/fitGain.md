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
  [ModelTox](https://docs.crmpack.org/reference/ModelTox-class.md) class
  object

- DLEsamples:

  the DLE samples of
  [Samples](https://docs.crmpack.org/reference/Samples-class.md) class
  object

- Effmodel:

  the efficacy pseudo model of
  [ModelEff](https://docs.crmpack.org/reference/ModelEff-class.md) class
  object

- Effsamples:

  the efficacy samples of
  [Samples](https://docs.crmpack.org/reference/Samples-class.md) class
  object

- data:

  the data input of
  [DataDual](https://docs.crmpack.org/reference/DataDual-class.md) class
  object

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
#>    dose    middle      lower     upper
#> 1    25 0.3438477 -0.1699007 0.8544627
#> 2    50 0.5987149  0.2547977 0.9663746
#> 3    75 0.6767000  0.3040261 1.0822600
#> 4   100 0.7013123  0.2964078 1.1623733
#> 5   125 0.7042382  0.2695312 1.2074615
#> 6   150 0.6972572  0.2438631 1.2303285
#> 7   175 0.6855757  0.2240500 1.2498266
#> 8   200 0.6717347  0.2064641 1.2612999
#> 9   225 0.6570509  0.1851396 1.2673728
#> 10  250 0.6422299  0.1680738 1.2696973
#> 11  275 0.6276542  0.1555099 1.2693313
#> 12  300 0.6135277  0.1446233 1.2668871
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
#> 1    25 0.3442700 -0.2511183 0.9175569
#> 2    50 0.6051121  0.2599777 1.0301263
#> 3    75 0.6827104  0.3462284 1.0908986
#> 4   100 0.7050072  0.3326112 1.1992161
#> 5   125 0.7057281  0.2824561 1.2516880
#> 6   150 0.6972930  0.2543596 1.3306478
#> 7   175 0.6849992  0.2131238 1.3920869
#> 8   200 0.6712913  0.1845311 1.4412821
#> 9   225 0.6573428  0.1655045 1.4802234
#> 10  250 0.6437208  0.1457352 1.5127283
#> 11  275 0.6306888  0.1270349 1.5067852
#> 12  300 0.6183552  0.1115851 1.4874941
```
