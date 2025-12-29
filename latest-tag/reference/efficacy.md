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

  (`numeric`)  
  the dose which is targeted. The following recycling rule applies when
  `samples` is not missing: vectors of size 1 will be recycled to the
  size of the sample (i.e. `size(samples)`). Otherwise, `dose` must have
  the same size as the sample.

- model:

  (`ModelEff`)  
  the efficacy model with pseudo data prior.

- samples:

  (`Samples`)  
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
  [`Effloglog`](https://openpharma.github.io/crmPack/reference/Effloglog-class.md)
  model parameters.

- `efficacy(dose = numeric, model = Effloglog, samples = missing)`:
  compute the expected efficacy at a specified dose level, based on the
  [`Effloglog`](https://openpharma.github.io/crmPack/reference/Effloglog-class.md)
  model parameters. All model parameters (except `dose`) should be
  present in the `model` object.

- `efficacy(dose = numeric, model = EffFlexi, samples = Samples)`:
  compute the expected efficacy at a specified dose level, based on the
  samples of
  [`EffFlexi`](https://openpharma.github.io/crmPack/reference/EffFlexi-class.md)
  model parameters. If a given dose in the `dose` vector is from outside
  of the dose grid range, the `NA_real` is returned for this dose and
  the warning is thrown.

## See also

[`dose()`](https://openpharma.github.io/crmPack/reference/dose.md),
[`prob()`](https://openpharma.github.io/crmPack/reference/prob.md).

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
#>   [1]   1.53309381  -0.36750631   7.55527614   5.25469404   3.66693278
#>   [6]   8.70838981   4.90252140  -0.69061135   1.34203932  -7.43545911
#>  [11]  -2.94369030  -0.31605961   0.50693514  -9.26862484   1.46306208
#>  [16]  -1.14134478   7.32450512  -1.62809243  -2.49305731  -1.66356028
#>  [21]   0.14878359  -0.49947849   4.42304913   4.56649426  -3.57361192
#>  [26]  -5.63080113  -1.94459230  14.44628993   7.35954897   3.02553881
#>  [31]  11.10188815  11.22388514   6.80186903   0.36884435  -2.82813022
#>  [36]  -4.73915711   9.09265745   0.45171075   7.47281283  -3.04597000
#>  [41]  12.38787082   2.42193236   0.61608253  -0.13622731  -6.08412025
#>  [46]  -1.86297634   3.28013838  -2.48647780   1.42981578  -6.81130220
#>  [51]  -5.26399353   2.63505491  -5.57528885  -1.18080239   0.68363630
#>  [56]   0.80957042   1.66747143   1.90085264   0.31491916  -2.97512796
#>  [61]   6.73581440   0.80529855  10.99746433  -4.50426438   2.35555834
#>  [66]  -2.03180783   3.97573796   4.27632858  -9.42646554  12.55749274
#>  [71]  -0.80789290  -1.90257411  -2.49043558   5.63952769   3.45659353
#>  [76]   0.47354025   2.52357271  -1.38263265  -7.15088137  -1.94147061
#>  [81]  -1.49545417  -4.77468458  -2.72741400   9.03347724   2.29069087
#>  [86]   3.92829723   2.63532137   1.82836285   0.58927326   4.21572326
#>  [91]  -2.45214763   7.88420978   0.19709801   0.89906020   4.88309101
#>  [96]   3.04825966  -1.81374639   1.45541231  14.61364519  10.31328290
#> [101]  -7.21042554   2.98659493   8.94809729  -0.36850893   0.77558282
#> [106]   3.24200951  -9.74784292  12.41222304   1.69010933  -1.74952128
#> [111]   5.69098809   2.62087863  -4.88471879  -1.90574868  -0.46153796
#> [116]   2.26830157  -1.42972655  10.97802797  -4.39581100   0.51699864
#> [121]   3.48569230   0.79299301  -6.87113975   3.24208183   6.76958848
#> [126]  -1.49208773   3.78647974   6.56550883   4.58980593  -0.41956371
#> [131]   3.92797953  -0.25794403   3.79244917  -3.41126635  -2.31361622
#> [136]   0.68751868   1.86167000   3.36000659   0.42099810   4.07698962
#> [141]   4.50480555   6.29500656  -4.07015627   6.86309949   4.04608404
#> [146]   1.83641311  -5.48623907  -4.74101984  -5.79376521   1.15017134
#> [151]  -2.50564693  -3.48767730   3.83157946   2.09528864  -1.72330218
#> [156]   8.21151143   6.95861675   5.57982266   8.51214305  -2.05622819
#> [161]   9.80142831  -5.12209892  -6.73822609   2.30227490   1.75097164
#> [166]   1.44273838   2.96978667  10.76845222   4.07874269   7.10792088
#> [171]  -2.05910186   7.30915648   2.61942655  -3.28100602  -0.59016549
#> [176]  -0.08740698   9.11846595  -2.74656639   2.79106308 -10.22195117
#> [181]  -1.57917435  -0.75159739  -0.42006406   5.53433055  -0.76741452
#> [186]  -4.14286245   1.27944063   3.12421033  -3.08574641  10.18627697
#> [191]   9.74964035  -1.02936258   4.68939972   1.95388350   0.99957216
#> [196]  -0.74528033  -0.29508297   2.96868789  14.94223266   4.32409978

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
