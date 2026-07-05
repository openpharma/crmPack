# Computing Expected Efficacy for a Given Dose, Model and Samples

**\[stable\]**

A function that computes the value of expected efficacy at a specified
dose level, based on the model specific parameters. The model parameters
(samples) are obtained based on prior specified in form of pseudo data
combined with observed responses (if any).

## Usage

``` r
efficacy(dose, model, samples, ...)

# S4 method for class 'numeric,Effloglog,Samples'
efficacy(dose, model, samples)

# S4 method for class 'numeric,Effloglog,missing'
efficacy(dose, model)

# S4 method for class 'numeric,EffFlexi,Samples'
efficacy(dose, model, samples)
```

## Arguments

- dose:

  (`numeric`)\
  the dose which is targeted. The following recycling rule applies when
  `samples` is not missing: vectors of size 1 will be recycled to the
  size of the sample (i.e. `size(samples)`). Otherwise, `dose` must have
  the same size as the sample.

- model:

  (`ModelEff`)\
  the efficacy model with pseudo data prior.

- samples:

  (`Samples`)\
  samples of model's parameters that will be used to compute expected
  efficacy values. Can also be missing for some models.

- ...:

  model specific parameters when `samples` are not used.

## Value

A `numeric` vector with the values of expected efficacy. If non-scalar
`samples` were used, then every element in the returned vector
corresponds to one element of a sample. Hence, in this case, the output
vector is of the same length as the sample vector. If scalar `samples`
were used or no `samples` were used, e.g. for pseudo DLE/toxicity
`model`, then the output is of the same length as the length of the
`dose`.

## Details

The `efficacy()` function computes the expected efficacy for given
doses, using samples of the model parameter(s). If you work with
multivariate model parameters, then assume that your model specific
`efficacy()` method receives a samples matrix where the rows correspond
to the sampling index, i.e. the layout is then
`nSamples x dimParameter`.

## Functions

- `efficacy(dose = numeric, model = Effloglog, samples = Samples)`:
  compute the expected efficacy at a specified dose level, based on the
  samples of
  [`Effloglog`](https://docs.crmpack.org/reference/Effloglog-class.md)
  model parameters.

- `efficacy(dose = numeric, model = Effloglog, samples = missing)`:
  compute the expected efficacy at a specified dose level, based on the
  [`Effloglog`](https://docs.crmpack.org/reference/Effloglog-class.md)
  model parameters. All model parameters (except `dose`) should be
  present in the `model` object.

- `efficacy(dose = numeric, model = EffFlexi, samples = Samples)`:
  compute the expected efficacy at a specified dose level, based on the
  samples of
  [`EffFlexi`](https://docs.crmpack.org/reference/EffFlexi-class.md)
  model parameters. If a given dose in the `dose` vector is from outside
  of the dose grid range, the `NA_real` is returned for this dose and
  the warning is thrown.

## See also

[`dose()`](https://docs.crmpack.org/reference/dose.md),
[`prob()`](https://docs.crmpack.org/reference/prob.md).

## Examples

``` r
# Obtain the expected efficacy value for a given dose, a given pseudo efficacy
# model (in flexible form for prior) and efficacy samples.

# Empty data (i.e. no observed data), dose grid only.
my_data <- DataDual(doseGrid = seq(25, 300, 25))

my_model <- EffFlexi(
  eff = c(1.223, 2.513),
  eff_dose = c(25, 300),
  sigma2W = c(a = 0.1, b = 0.1),
  sigma2betaW = c(a = 20, b = 50),
  rw1 = FALSE,
  data = my_data
)

my_options <- McmcOptions(
  burnin = 100,
  step = 2,
  samples = 200,
  rng_kind = "Mersenne-Twister",
  rng_seed = 94
)

my_samples <- mcmc(data = my_data, model = my_model, options = my_options)

# Efficacy for dose 75.
efficacy(dose = 75, model = my_model, samples = my_samples)
#>   [1]   9.07133091   1.53424048  11.07308839   6.74968857  -2.87681905
#>   [6]  -0.52774208   0.98461243   8.07698912  -2.44894407  -5.85523700
#>  [11]  11.31738072  -5.20839309  -3.53568739   1.84098767   2.63215398
#>  [16]   1.08547205  -1.14150225  -1.17196071  -4.07450990   3.83602092
#>  [21]  -1.51766461   9.58344159  12.66103757  -4.07515232   4.55952976
#>  [26]   3.07600733  -0.45143577  12.68867587  -0.79739141  -6.60007580
#>  [31]   6.08270229  -5.09099874   1.30242100  -1.38790974   2.11417328
#>  [36]   8.37133821   1.50986899   6.84880575   5.43897499   2.79542447
#>  [41]  11.45417302   2.06748782   1.35245992  -1.23701296   7.91038987
#>  [46]   6.51728419  -5.90538823   0.50485996   4.46024145  -6.19234053
#>  [51]  -1.03422769  -4.60878828  -7.09786728   6.77762041  -7.80093619
#>  [56]  -0.35335674   0.06016899   5.09042210  -1.08931226  -5.13714299
#>  [61] -11.25227813   6.94417969  -0.18607453   6.70974746   7.07740656
#>  [66]   1.81144127  -1.46201083  -3.59961582   5.98766120  -3.06347907
#>  [71]   1.48952524  -3.87648489   9.53195596  -5.09183042   2.55946664
#>  [76]   5.36514495   3.35805663   4.04528900  -2.47968051   2.38776563
#>  [81]   3.30544598   6.08668755   8.10409159  -2.86897901  -2.56968895
#>  [86]  -2.71078157   6.57400099  -0.64078913  -0.77931969   0.44535965
#>  [91]   2.48733763   1.62006538   5.18380783  15.91917003  14.37031355
#>  [96]  -4.95225083  -0.11454594   3.37173952   4.36757735   9.93988922
#> [101]   0.14708030   0.66970048   2.93648731   6.62081659  -5.72655553
#> [106]   7.32219826   3.85369534  -5.64247557   3.26712563   1.53773741
#> [111]   5.36646375  -2.24826433   1.53796441  -1.47481547   5.66412948
#> [116]  -0.08562487   0.92285513   6.54480180   2.76592451   9.04534245
#> [121]   2.67298217   9.37451070   3.31718865  -4.39655806  -2.77475285
#> [126]   0.73845209  -1.14838485  -0.04538310  13.15372079  -8.24336360
#> [131]   1.00955294   3.84117706   7.63816224   7.15423998  -0.31603848
#> [136]  -5.15339494   4.46319473   0.61902688   1.75161458  -3.48460935
#> [141]   1.03111557   7.35215316  14.50755214   0.45564792  -0.33913496
#> [146]  -1.04517173   8.55653204  -0.33228919   7.57900504   5.90623049
#> [151]   0.95421800   9.08101740   9.43334290  -7.55120809  -2.72471203
#> [156]  -7.35230299   3.70368743   3.84477094   4.16388471  -1.68082994
#> [161]  -1.92123896   2.04731124  -2.26118158   1.72862392  -1.79117652
#> [166]   3.04772415   1.34065202   6.35926350  -1.78732266  -3.18937218
#> [171]  -5.18426934   6.64292813   0.21495831  -5.04417938  -3.64536665
#> [176]   6.97433099   4.69569241  10.84773802  10.07835651   7.98940931
#> [181]  -1.64566370  -6.87224825  -4.75312374  -1.98193506  11.94430550
#> [186]   7.12994070  11.82172904   5.74335173   4.51767170   1.09675636
#> [191]   5.90817166  -1.89174876  -0.15421814   2.06690552   1.63930492
#> [196]   3.77873641   2.64667197   9.84146185   2.29768549  -0.54599122

# Obtain the expected efficacy value for a given dose, a given pseudo efficacy
# model (linear log-log efficacy) and no samples.
my_model_ll <- Effloglog(
  eff = c(1.223, 2.513),
  eff_dose = c(25, 300),
  nu = c(a = 1, b = 0.025),
  data = my_data,
  const = 0
)

efficacy(dose = 75, model = my_model_ll)
#> [1] 1.885121
```
