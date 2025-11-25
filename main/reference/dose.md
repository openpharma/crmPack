# Computing the Doses for a given independent variable, Model and Samples

**\[stable\]**

A function that computes the dose reaching a specific target value of a
given variable that dose depends on. The meaning of this variable
depends on the type of the model. For instance, for single agent dose
escalation model or pseudo DLE (dose-limiting events)/toxicity model,
this variable represents the a probability of the occurrence of a DLE.
For efficacy models, it represents expected efficacy. The doses are
computed based on the samples of the model parameters (samples).

## Usage

``` r
dose(x, model, samples, ...)

# S4 method for class 'numeric,LogisticNormal,Samples'
dose(x, model, samples)

# S4 method for class 'numeric,LogisticLogNormal,Samples'
dose(x, model, samples)

# S4 method for class 'numeric,LogisticLogNormalOrdinal,Samples'
dose(x, model, samples, grade)

# S4 method for class 'numeric,LogisticLogNormalSub,Samples'
dose(x, model, samples)

# S4 method for class 'numeric,ProbitLogNormal,Samples'
dose(x, model, samples)

# S4 method for class 'numeric,ProbitLogNormalRel,Samples'
dose(x, model, samples)

# S4 method for class 'numeric,LogisticLogNormalGrouped,Samples'
dose(x, model, samples, group)

# S4 method for class 'numeric,LogisticKadane,Samples'
dose(x, model, samples)

# S4 method for class 'numeric,LogisticKadaneBetaGamma,Samples'
dose(x, model, samples)

# S4 method for class 'numeric,LogisticNormalMixture,Samples'
dose(x, model, samples)

# S4 method for class 'numeric,LogisticNormalFixedMixture,Samples'
dose(x, model, samples)

# S4 method for class 'numeric,LogisticLogNormalMixture,Samples'
dose(x, model, samples)

# S4 method for class 'numeric,DualEndpoint,Samples'
dose(x, model, samples)

# S4 method for class 'numeric,LogisticIndepBeta,Samples'
dose(x, model, samples)

# S4 method for class 'numeric,LogisticIndepBeta,missing'
dose(x, model)

# S4 method for class 'numeric,Effloglog,missing'
dose(x, model)

# S4 method for class 'numeric,EffFlexi,Samples'
dose(x, model, samples)

# S4 method for class 'numeric,OneParLogNormalPrior,Samples'
dose(x, model, samples)

# S4 method for class 'numeric,OneParExpPrior,Samples'
dose(x, model, samples)
```

## Arguments

- x:

  (`proportion` or `numeric`)  
  a value of an independent variable on which dose depends. The
  following recycling rule applies when `samples` is not missing:
  vectors of size 1 will be recycled to the size of the sample (i.e.
  `size(samples)`). Otherwise, `x` must have the same size as the
  sample.

- model:

  (`GeneralModel` or `ModelPseudo`)  
  the model.

- samples:

  (`Samples`)  
  the samples of model's parameters that will be used to compute the
  resulting doses. Can also be missing for some models.

- ...:

  model specific parameters when `samples` are not used.

- grade:

  (`integer`)  
  The toxicity grade for which probabilities are required

- group:

  (`character` or `factor`)  
  for
  [`LogisticLogNormalGrouped`](https://openpharma.github.io/crmPack/reference/LogisticLogNormalGrouped-class.md),
  indicating whether to calculate the dose for the `mono` or for the
  `combo` arm.

## Value

A `number` or `numeric` vector with the doses. If non-scalar `samples`
were used, then every element in the returned vector corresponds to one
element of a sample. Hence, in this case, the output vector is of the
same length as the sample vector. If scalar `samples` were used or no
`samples` were used, e.g. for pseudo DLE/toxicity `model`, then the
output is of the same length as the length of the `prob`.

## Details

The `dose()` function computes the doses corresponding to a value of a
given independent variable, using samples of the model parameter(s). If
you work with multivariate model parameters, then assume that your model
specific `dose()` method receives a samples matrix where the rows
correspond to the sampling index, i.e. the layout is then
`nSamples x dimParameter`.

## Functions

- `dose(x = numeric, model = LogisticNormal, samples = Samples)`:
  compute the dose level reaching a specific target probability of the
  occurrence of a DLE (`x`).

- `dose(x = numeric, model = LogisticLogNormal, samples = Samples)`:
  compute the dose level reaching a specific target probability of the
  occurrence of a DLE (`x`).

- `dose(x = numeric, model = LogisticLogNormalOrdinal, samples = Samples)`:
  compute the dose level reaching a specific target probability of the
  occurrence of a DLE (`x`).

  In the case of a `LogisticLogNormalOrdinal` model, `dose` returns only
  the probability of toxicity at the given grade or higher

- `dose(x = numeric, model = LogisticLogNormalSub, samples = Samples)`:
  compute the dose level reaching a specific target probability of the
  occurrence of a DLE (`x`).

- `dose(x = numeric, model = ProbitLogNormal, samples = Samples)`:
  compute the dose level reaching a specific target probability of the
  occurrence of a DLE (`x`).

- `dose(x = numeric, model = ProbitLogNormalRel, samples = Samples)`:
  compute the dose level reaching a specific target probability of the
  occurrence of a DLE (`x`).

- `dose(x = numeric, model = LogisticLogNormalGrouped, samples = Samples)`:
  method for
  [`LogisticLogNormalGrouped`](https://openpharma.github.io/crmPack/reference/LogisticLogNormalGrouped-class.md)
  which needs `group` argument in addition.

- `dose(x = numeric, model = LogisticKadane, samples = Samples)`:
  compute the dose level reaching a specific target probability of the
  occurrence of a DLE (`x`).

- `dose(x = numeric, model = LogisticKadaneBetaGamma, samples = Samples)`:
  compute the dose level reaching a specific target probability of the
  occurrence of a DLE (`x`).

- `dose(x = numeric, model = LogisticNormalMixture, samples = Samples)`:
  compute the dose level reaching a specific target probability of the
  occurrence of a DLE (`x`).

- `dose(x = numeric, model = LogisticNormalFixedMixture, samples = Samples)`:
  compute the dose level reaching a specific target probability of the
  occurrence of a DLE (`x`).

- `dose(x = numeric, model = LogisticLogNormalMixture, samples = Samples)`:
  compute the dose level reaching a specific target probability of the
  occurrence of a DLE (`x`).

- `dose(x = numeric, model = DualEndpoint, samples = Samples)`: compute
  the dose level reaching a specific target probability of the
  occurrence of a DLE (`x`).

- `dose(x = numeric, model = LogisticIndepBeta, samples = Samples)`:
  compute the dose level reaching a specific target probability of the
  occurrence of a DLE (`x`).

- `dose(x = numeric, model = LogisticIndepBeta, samples = missing)`:
  compute the dose level reaching a specific target probability of the
  occurrence of a DLE (`x`). All model parameters (except `x`) should be
  present in the `model` object.

- `dose(x = numeric, model = Effloglog, samples = missing)`: compute the
  dose level reaching a specific target probability of the occurrence of
  a DLE (`x`). All model parameters (except `x`) should be present in
  the `model` object.

- `dose(x = numeric, model = EffFlexi, samples = Samples)`: compute the
  dose level reaching a specific target probability of the occurrence of
  a DLE (`x`). For this method `x` must be a scalar.

- `dose(x = numeric, model = OneParLogNormalPrior, samples = Samples)`:
  compute the dose level reaching a specific target probability of the
  occurrence of a DLT (`x`).

- `dose(x = numeric, model = OneParExpPrior, samples = Samples)`:
  compute the dose level reaching a specific target probability of the
  occurrence of a DLT (`x`).

## Note

The `dose()` and
[`prob()`](https://openpharma.github.io/crmPack/reference/prob.md)
methods are the inverse of each other, for all `dose()` methods for
which its first argument, i.e. a given independent variable that dose
depends on, represents toxicity probability.

## See also

[`doseFunction()`](https://openpharma.github.io/crmPack/reference/doseFunction.md),
[`prob()`](https://openpharma.github.io/crmPack/reference/prob.md),
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

# Posterior for the dose achieving Prob(DLT) = 0.45.
dose(x = 0.45, model = my_model, samples = my_samples)
#>  [1]  84.10844  84.10844  84.10844  29.13947  29.13947  29.13947  29.13947
#>  [8]  29.13947  29.13947  29.13947  29.13947 179.92857 179.92857 179.92857
#> [15] 179.92857  58.65283  58.65283  58.65283  72.45148  72.45148

# Create data from the 'Data' (or 'DataDual') class.
dlt_data <- Data(
  x = c(25, 50, 25, 50, 75, 300, 250, 150),
  y = c(0, 0, 0, 0, 0, 1, 1, 0),
  doseGrid = seq(from = 25, to = 300, by = 25)
)
#> Used default patient IDs!
#> Used best guess cohort indices!

# Initialize a toxicity model using 'LogisticIndepBeta' model.
dlt_model <- LogisticIndepBeta(
  binDLE = c(1.05, 1.8),
  DLEweights = c(3, 3),
  DLEdose = c(25, 300),
  data = dlt_data
)

# Get samples from posterior.
dlt_sample <- mcmc(data = dlt_data, model = dlt_model, options = my_options)

# Posterior for the dose achieving Prob(DLT) = 0.45.
dose(x = 0.45, model = dlt_model, samples = dlt_sample)
#>  [1]  5.394717e+07 2.079593e-131 2.079593e-131 2.079593e-131 2.079593e-131
#>  [6]  2.103099e+01  2.103099e+01  1.875484e+02  3.718879e+01  3.718879e+01
#> [11]  3.718879e+01  3.718879e+01  3.718879e+01  3.718879e+01  3.718879e+01
#> [16]  3.718879e+01  3.718879e+01  3.718879e+01  3.718879e+01  6.519215e+01
dose(x = c(0.45, 0.6), model = dlt_model)
#> [1] 144.6624 247.7348
data_ordinal <- .DefaultDataOrdinal()
model <- .DefaultLogisticLogNormalOrdinal()
options <- .DefaultMcmcOptions()
samples <- mcmc(data_ordinal, model, options)

dose(0.25, model, samples, grade = 2L)
#>    [1] 6.011783e+01 6.439945e+01 7.039470e+01 6.331981e+01 6.797086e+01
#>    [6] 1.313320e+02 5.676411e+01 6.270962e+01 6.914567e+01 6.113204e+01
#>   [11] 5.467049e+01 8.591472e+01 9.138000e+01 9.939665e+01 1.250932e+02
#>   [16] 5.916321e+01 5.222363e+01 5.879380e+01 6.416255e+01 3.072507e+02
#>   [21] 1.088001e+02 8.769488e+01 6.879394e+01 6.480254e+01 1.176861e+02
#>   [26] 5.867473e+01 6.149868e+01 6.072065e+01 6.571546e+01 6.168967e+01
#>   [31] 7.606997e+01 5.889954e+01 5.986846e+01 9.202826e+01 7.139956e+01
#>   [36] 6.745303e+01 6.163422e+01 6.061905e+01 6.300469e+01 1.674024e+02
#>   [41] 1.282943e+02 6.060131e+01 6.412800e+01 5.358492e+01 6.030856e+01
#>   [46] 5.773880e+01 5.804244e+01 6.323250e+01 6.215266e+01 6.042074e+01
#>   [51] 8.036630e+01 1.246026e+02 7.449957e+01 9.535787e+01 6.473716e+01
#>   [56] 8.345373e+02 1.901264e+03 5.834061e+01 5.940316e+01 5.895084e+01
#>   [61] 7.338531e+01 6.829671e+01 7.577783e+01 1.185546e+02 6.428896e+01
#>   [66] 7.087286e+01 8.146468e+01 7.250124e+01 6.923402e+01 8.928326e+01
#>   [71] 1.566641e+02 5.854695e+01 1.233187e+02 5.710932e+01 6.581436e+01
#>   [76] 1.072521e+02 6.334700e+01 9.527305e+01 5.574732e+01 5.140134e+02
#>   [81] 6.801930e+01 1.343001e+02 6.310478e+01 5.461251e+01 6.474376e+01
#>   [86] 5.784229e+01 6.003417e+01 6.758630e+01 7.820364e+01 8.539463e+01
#>   [91] 7.721674e+01 5.738320e+01 6.111454e+01 6.969543e+01 9.345737e+01
#>   [96] 7.372954e+01 6.784486e+01 6.101428e+01 8.086609e+01 6.489925e+01
#>  [101] 6.954908e+01 1.059532e+02 6.156246e+01 6.294501e+01 1.283370e+02
#>  [106] 8.582627e+01 5.694903e+01 6.336311e+01 6.103065e+01 5.651851e+01
#>  [111] 6.867185e+01 8.978039e+01 6.697359e+01 6.084745e+01 8.140400e+01
#>  [116] 5.734513e+01 1.307549e+02 4.541100e+02 6.616049e+01 1.031508e+02
#>  [121] 1.150505e+02 3.256587e+02 6.080640e+01 1.041605e+02 6.852038e+01
#>  [126] 7.028982e+01 6.136845e+01 2.771009e+02 1.746631e+03 9.484411e+01
#>  [131] 5.766527e+01 6.175022e+01 6.019962e+01 5.761987e+01 1.781432e+02
#>  [136] 6.188680e+01 5.925638e+01 6.758131e+01 5.805720e+01 8.562777e+01
#>  [141] 6.604294e+01 5.710143e+01 7.226617e+01 5.608644e+01 1.934952e+02
#>  [146] 5.677350e+01 1.018054e+02 6.362380e+01 7.696263e+01 4.618421e+03
#>  [151] 5.487931e+01 1.040106e+02 8.476437e+01 8.567303e+01 4.975700e+01
#>  [156] 4.700943e+01 6.416590e+01 6.115852e+01 7.928450e+01 7.408981e+01
#>  [161] 5.587416e+01 5.571053e+01 4.847980e+02 1.962892e+02 7.841446e+01
#>  [166] 7.078595e+01 5.782822e+01 5.923480e+01 9.154697e+01 5.415328e+01
#>  [171] 6.857701e+01 6.149605e+01 5.723872e+01 5.369557e+01 3.736239e+02
#>  [176] 6.502861e+01 6.560087e+01 2.003214e+02 8.844617e+01 6.278519e+01
#>  [181] 7.236845e+01 6.809544e+01 1.165549e+02 6.044166e+01 5.963513e+01
#>  [186] 5.585011e+01 6.288837e+01 5.744964e+01 6.666321e+01 6.198533e+01
#>  [191] 7.641204e+01 5.811325e+01 7.224358e+01 5.872545e+01 1.050960e+02
#>  [196] 7.618161e+01 6.732704e+01 7.534661e+01 4.866538e+02 6.803825e+01
#>  [201] 8.325203e+01 7.187591e+01 7.624813e+01 5.688139e+01 7.119813e+01
#>  [206] 5.874473e+01 7.398003e+01 7.033111e+01 6.111217e+01 6.273955e+01
#>  [211] 5.760527e+01 6.974755e+01 6.441938e+01 6.954017e+01 6.033436e+01
#>  [216] 7.687283e+01 7.543316e+01 5.339325e+01 6.065783e+01 5.912086e+01
#>  [221] 8.416181e+01 6.639626e+01 6.037538e+01 5.968496e+01 6.655024e+01
#>  [226] 6.575062e+01 2.124496e+02 6.859207e+01 5.583764e+01 7.988720e+01
#>  [231] 6.305809e+01 6.763478e+01 7.617060e+01 8.004463e+01 6.422558e+01
#>  [236] 7.071950e+01 3.418584e+03 3.121191e+02 1.946323e+02 6.987016e+01
#>  [241] 9.701723e+01 5.607562e+01 8.048558e+01 8.835549e+01 8.741980e+01
#>  [246] 6.865471e+01 6.317202e+01 5.671907e+01 6.657549e+01 8.267718e+01
#>  [251] 6.290735e+01 7.743594e+01 1.091125e+02 6.916830e+01 7.771580e+01
#>  [256] 6.152442e+01 4.532632e+02 5.771138e+01 6.210287e+01 8.211990e+01
#>  [261] 6.123700e+01 6.123461e+01 5.676528e+01 6.415032e+01 4.494132e+03
#>  [266] 9.460911e+01 5.936866e+01 6.737036e+01 6.003369e+01 6.678160e+01
#>  [271] 7.380063e+01 7.829762e+01 5.227394e+01 6.985868e+01 1.103554e+02
#>  [276] 8.940908e+01 6.048084e+01 5.435228e+01 7.151476e+01 9.684857e+01
#>  [281] 5.934183e+01 8.279452e+01 6.265235e+01 6.742979e+01 6.473391e+01
#>  [286] 6.093223e+01 9.326150e+01 5.936789e+01 1.266932e+02 6.615654e+01
#>  [291] 1.824381e+02 6.341919e+01 5.934406e+01 5.464033e+01 5.373352e+01
#>  [296] 5.274918e+01 4.558168e+01 7.262947e+01 5.645482e+01 6.356576e+01
#>  [301] 6.413155e+01 6.283455e+01 6.483781e+01 7.216236e+01 6.403622e+01
#>  [306] 7.301642e+01 8.317331e+01 5.696908e+01 6.293728e+01 5.666150e+01
#>  [311] 6.595560e+01 8.820785e+01 7.035925e+01 6.521670e+01 6.669853e+01
#>  [316] 5.984218e+01 6.251045e+01 8.625887e+01 6.875183e+01 1.262027e+02
#>  [321] 6.776068e+01 5.303226e+01 6.630603e+01 5.933057e+01 5.481049e+01
#>  [326] 7.356975e+01 6.313804e+01 6.277382e+01 6.266416e+01 4.032125e+02
#>  [331] 6.905566e+01 8.152162e+01 5.452923e+01 6.991778e+01 6.194562e+01
#>  [336] 7.438824e+01 6.496229e+01 6.073993e+01 1.031051e+02 7.135535e+01
#>  [341] 2.090957e+02 5.542584e+01 6.540629e+01 6.505387e+01 8.293963e+01
#>  [346] 1.089631e+02 6.698565e+01 5.606500e+01 5.886989e+01 5.992060e+01
#>  [351] 5.939697e+01 9.208632e+01 2.351131e+02 5.066552e+01 2.807770e+02
#>  [356] 7.941913e+01 2.880395e+02 6.631975e+01 5.892945e+01 6.202155e+01
#>  [361] 5.521630e+01 6.039136e+01 5.947521e+01 6.193933e+01 7.599387e+01
#>  [366] 6.311144e+01 7.097781e+01 6.311939e+01 5.643919e+01 1.094022e+02
#>  [371] 1.111629e+02 1.228148e+02 7.421748e+01 1.190113e+02 1.351406e+02
#>  [376] 8.433071e+01 7.724345e+01 6.384432e+01 6.953584e+01 7.182360e+01
#>  [381] 6.413358e+01 7.053753e+01 5.670969e+01 5.858879e+01 6.308845e+01
#>  [386] 5.728148e+01 6.347349e+01 5.883519e+01 5.932033e+01 1.409232e+02
#>  [391] 6.506718e+01 9.447075e+01 6.209254e+01 6.003343e+01 5.842212e+01
#>  [396] 1.295941e+02 9.225930e+01 5.918415e+01 8.146541e+01 5.794056e+01
#>  [401] 7.342614e+01 6.407072e+01 5.949631e+01 6.434132e+01 6.721384e+01
#>  [406] 7.044936e+01 7.031796e+01 6.422048e+01 5.967664e+01 6.769175e+01
#>  [411] 5.987944e+01 1.862760e+02 5.681937e+01 8.775281e+01 1.077562e+02
#>  [416] 5.803726e+01 7.490584e+01 5.388642e+01 5.635090e+01 8.923720e+01
#>  [421] 6.138329e+01 6.121797e+01 7.665418e+01 7.690180e+01 6.089429e+01
#>  [426] 5.701506e+01 6.745579e+01 5.974378e+01 5.733935e+01 7.977311e+01
#>  [431] 3.069682e+02 6.540109e+01 7.137461e+01 6.312886e+01 5.272278e+01
#>  [436] 6.914994e+01 8.620077e+01 6.520392e+01 5.582676e+01 7.096563e+01
#>  [441] 3.882549e+02 1.581832e+02 6.424994e+01 6.057155e+01 1.568087e+03
#>  [446] 5.475318e+01 1.032931e+02 7.482790e+01 5.508257e+01 7.087179e+01
#>  [451] 8.585591e+01 5.989994e+01 6.670245e+01 8.375082e+01 7.023007e+01
#>  [456] 6.501693e+01 6.166888e+01 5.550594e+01 7.360215e+01 8.965796e+01
#>  [461] 5.695948e+01 7.920812e+01 5.601877e+01 6.258835e+01 1.480993e+02
#>  [466] 6.805358e+01 5.827090e+01 5.900008e+01 6.242015e+01 1.106413e+02
#>  [471] 1.638513e+02 8.489473e+01 7.166611e+01 6.823636e+01 1.022142e+02
#>  [476] 5.638464e+01 1.410276e+02 6.929187e+01 8.283288e+01 6.511899e+01
#>  [481] 6.190496e+01 6.422741e+01 7.639923e+01 5.639183e+01 7.944469e+01
#>  [486] 8.533725e+01 7.448337e+01 8.958183e+01 8.202214e+01 6.517935e+01
#>  [491] 5.324106e+01 7.488263e+01 7.637250e+01 1.394142e+02 6.424683e+01
#>  [496] 1.319316e+02 6.958361e+01 4.634331e+05 5.509757e+01 7.837137e+01
#>  [501] 6.726012e+01 6.205258e+01 7.059397e+01 6.295914e+01 6.551018e+01
#>  [506] 6.564198e+01 2.970245e+02 7.758099e+01 5.333409e+01 7.186018e+01
#>  [511] 7.890038e+01 5.955466e+01 5.919882e+01 6.470797e+01 5.089094e+01
#>  [516] 6.390554e+01 6.011460e+01 5.934055e+01 6.071214e+01 1.009582e+02
#>  [521] 6.358583e+01 7.675915e+01 6.209944e+01 5.502470e+01 1.438833e+02
#>  [526] 8.237002e+01 8.481652e+01 6.960406e+01 1.511035e+02 6.261246e+01
#>  [531] 6.204590e+01 6.048608e+01 6.980406e+01 4.149253e+02 6.644812e+01
#>  [536] 7.111205e+01 6.590030e+01 6.874072e+01 3.917328e+02 5.475650e+01
#>  [541] 6.912388e+01 5.461832e+01 6.986379e+01 9.042101e+01 7.225961e+01
#>  [546] 6.034058e+01 7.181357e+01 5.620259e+01 6.172960e+01 5.944730e+01
#>  [551] 6.269141e+01 7.915468e+01 1.105880e+02 5.785193e+01 1.086253e+02
#>  [556] 6.441981e+01 6.601012e+01 8.331107e+01 7.899126e+01 6.769326e+01
#>  [561] 5.557940e+01 5.945919e+01 8.014834e+01 7.163454e+01 5.356983e+01
#>  [566] 6.000325e+01 5.876078e+01 5.911211e+01 7.421892e+01 5.651438e+01
#>  [571] 7.274869e+01 6.395352e+01 7.097615e+01 1.280291e+02 5.882010e+01
#>  [576] 6.231064e+01 5.365001e+01 6.514184e+01 5.773541e+01 5.882337e+01
#>  [581] 6.609259e+01 6.636168e+01 6.405338e+01 6.945567e+01 1.023762e+02
#>  [586] 5.342642e+01 6.930600e+01 6.076996e+01 6.148900e+01 1.162274e+02
#>  [591] 6.010311e+01 5.911961e+01 9.118185e+01 5.546596e+01 7.425529e+01
#>  [596] 5.891790e+01 1.149788e+02 6.629989e+01 5.690471e+01 5.862288e+01
#>  [601] 6.089788e+01 6.038974e+01 6.305073e+01 7.623558e+01 5.756152e+01
#>  [606] 6.935587e+01 6.793078e+01 6.342623e+01 7.705715e+01 6.603835e+01
#>  [611] 6.925525e+01 1.013202e+02 6.107735e+01 2.486285e+02 5.174550e+01
#>  [616] 6.980100e+01 5.929605e+01 5.719392e+08 6.608621e+01 5.783843e+01
#>  [621] 6.419572e+01 5.893540e+01 6.103514e+01 7.303281e+01 1.104732e+02
#>  [626] 5.827685e+01 6.173316e+01 6.412577e+01 6.821700e+01 4.043830e+04
#>  [631] 6.108623e+02 7.327653e+01 6.771455e+01 6.022263e+01 5.662317e+01
#>  [636] 6.990606e+01 7.127302e+01 6.865105e+01 8.043364e+01 6.623463e+01
#>  [641] 5.577887e+01 5.905161e+01 6.037439e+01 6.230951e+01 9.018217e+01
#>  [646] 7.848661e+01 1.783628e+02 7.386493e+01 6.628456e+01 1.132471e+02
#>  [651] 1.811290e+02 6.091581e+01 6.524624e+01 7.818215e+01 1.953050e+03
#>  [656] 4.682425e+03 1.137268e+06 5.466016e+01 7.017503e+01 5.834733e+01
#>  [661] 6.282359e+01 7.101682e+01 6.659646e+01 7.086455e+01 6.972488e+01
#>  [666] 8.386155e+01 6.831278e+01 4.911027e+01 6.821981e+01 5.242923e+01
#>  [671] 5.753757e+01 5.921792e+01 8.786387e+01 5.567323e+01 6.242356e+01
#>  [676] 6.232689e+01 6.509411e+01 5.598936e+01 1.102644e+02 5.866664e+01
#>  [681] 6.234328e+01 6.160800e+01 6.568662e+01 5.646525e+01 6.240395e+01
#>  [686] 9.148557e+01 1.798212e+02 9.228412e+01 1.437340e+02 1.008867e+02
#>  [691] 7.806828e+01 5.512407e+01 6.253670e+01 6.125685e+01 6.701600e+01
#>  [696] 5.895866e+01 8.460496e+01 5.894601e+01 5.526673e+02 5.197363e+01
#>  [701] 5.795454e+01 8.149507e+01 6.225168e+01 8.321274e+01 3.254690e+03
#>  [706] 3.493917e+02 8.621834e+01 6.912757e+01 7.930612e+01 7.343791e+01
#>  [711] 5.512264e+01 1.461541e+02 6.889513e+01 1.447661e+02 6.422162e+01
#>  [716] 8.486048e+01 5.836827e+01 7.036806e+01 1.872938e+02 1.115677e+02
#>  [721] 6.434212e+01 6.007324e+01 5.359186e+01 7.774401e+01 6.226687e+01
#>  [726] 2.394141e+02 5.585235e+01 1.732410e+03 1.020477e+02 2.201612e+02
#>  [731] 8.862664e+01 8.398614e+01 5.413098e+01 6.672163e+01 5.788806e+01
#>  [736] 6.415037e+01 6.148173e+01 9.157334e+02 6.110953e+01 8.256828e+01
#>  [741] 7.202783e+01 3.704136e+02 5.994455e+01 6.643180e+01 8.874157e+01
#>  [746] 7.715521e+01 7.631097e+01 2.454514e+02 6.785611e+01 1.055787e+02
#>  [751] 7.455232e+01 9.033729e+01 7.799602e+01 6.712018e+01 8.080885e+01
#>  [756] 6.262462e+01 7.641637e+01 6.128006e+01 9.627746e+01 8.183355e+01
#>  [761] 6.060415e+01 9.972805e+01 8.185810e+01 8.792399e+01 4.650469e+01
#>  [766] 6.182906e+01 1.125437e+02 5.876266e+01 1.135784e+02 1.715101e+02
#>  [771] 7.364665e+01 7.596474e+01 7.916410e+01 5.839364e+01 9.199817e+01
#>  [776] 1.215591e+02 6.174631e+01 6.388280e+01 7.083703e+01 6.967839e+01
#>  [781] 7.064373e+01 8.721789e+01 6.457355e+01 7.144228e+01 6.260953e+01
#>  [786] 5.644889e+01 5.909554e+01 6.006367e+01 7.214242e+01 6.074594e+01
#>  [791] 6.107368e+01 6.612178e+01 6.402166e+01 6.771553e+01 7.703216e+01
#>  [796] 6.580832e+01 5.053517e+01 6.117166e+01 2.146097e+02 5.095905e+01
#>  [801] 1.356889e+02 6.215584e+01 6.760333e+01 6.952351e+01 7.985327e+01
#>  [806] 7.099456e+01 6.064848e+01 6.006119e+01 6.156207e+01 6.012992e+01
#>  [811] 5.761042e+01 5.838796e+01 1.797573e+02 1.101873e+02 7.467491e+01
#>  [816] 6.124145e+01 5.086818e+02 7.257710e+03 5.574664e+03 1.892330e+03
#>  [821] 7.701695e+02 4.209563e+02 6.575275e+01 1.156430e+02 6.673284e+01
#>  [826] 8.256679e+01 6.038072e+01 6.788486e+01 8.497631e+01 6.630526e+01
#>  [831] 6.117620e+01 6.313130e+01 1.064959e+02 7.818200e+01 6.753850e+01
#>  [836] 8.431660e+01 1.760616e+02 7.247991e+01 6.295988e+01 7.918262e+01
#>  [841] 1.891983e+02 1.750032e+03 4.686567e+02 1.454081e+02 5.451944e+01
#>  [846] 5.714230e+01 5.871944e+01 7.093077e+01 6.451367e+01 7.795562e+01
#>  [851] 6.113899e+01 7.855849e+01 1.457236e+02 9.439329e+01 8.786356e+01
#>  [856] 6.753240e+01 7.015847e+01 4.600827e+04 1.210889e+02 5.927729e+01
#>  [861] 6.768242e+01 6.286027e+01 6.744558e+01 8.814938e+01 5.886557e+01
#>  [866] 7.284648e+01 8.181145e+01 6.678467e+01 1.492642e+02 6.098211e+01
#>  [871] 6.176209e+01 5.545746e+01 7.031233e+01 1.029410e+02 8.871114e+01
#>  [876] 7.192615e+01 8.138049e+01 6.097894e+01 6.129446e+01 6.851957e+01
#>  [881] 6.327637e+01 1.776394e+02 7.266495e+01 7.790274e+01 1.006217e+02
#>  [886] 6.073800e+01 5.893020e+01 8.569824e+01 9.050206e+01 1.003675e+02
#>  [891] 7.042572e+01 6.015718e+01 7.350190e+01 1.001585e+02 6.090253e+01
#>  [896] 5.347870e+01 6.475348e+01 2.084466e+02 5.598449e+01 8.866562e+01
#>  [901] 6.829652e+01 7.470619e+01 7.803194e+01 5.961600e+01 6.049849e+01
#>  [906] 7.107946e+01 6.857642e+01 7.160526e+01 5.626442e+01 5.641297e+01
#>  [911] 6.489413e+01 8.604367e+01 3.092740e+02 1.181467e+02 2.018692e+02
#>  [916] 8.627585e+01 5.720979e+01 8.964357e+01 8.136150e+01 5.688669e+01
#>  [921] 7.655679e+01 5.504569e+01 6.066834e+01 2.524117e+02 7.433771e+01
#>  [926] 1.273764e+02 9.023671e+01 5.744107e+01 6.413659e+01 5.402825e+01
#>  [931] 1.491993e+03 8.040512e+06 5.635693e+01 6.673614e+01 5.802336e+01
#>  [936] 7.891132e+01 6.888848e+01 1.877934e+02 6.710475e+01 6.555380e+01
#>  [941] 7.049005e+01 5.333726e+01 1.074987e+02 6.562347e+01 6.158436e+01
#>  [946] 1.395256e+02 7.409050e+01 8.434382e+01 5.831170e+01 7.117925e+01
#>  [951] 7.628489e+01 5.953351e+01 6.965954e+01 6.557961e+01 5.615804e+01
#>  [956] 7.794695e+01 5.579187e+01 5.588166e+01 6.065389e+01 7.836541e+01
#>  [961] 1.033579e+03 5.514201e+01 5.607961e+01 5.736465e+01 8.306088e+01
#>  [966] 3.137310e+02 5.690842e+01 6.389701e+01 8.490959e+01 6.022528e+01
#>  [971] 6.808598e+01 7.015270e+01 7.031950e+01 5.675008e+01 1.212022e+02
#>  [976] 5.501497e+01 6.519473e+01 6.268693e+01 5.968409e+01 5.668757e+01
#>  [981] 9.890749e+01 6.135731e+01 1.157018e+02 6.979074e+01 6.042470e+01
#>  [986] 1.310081e+02 9.235430e+01 5.885394e+01 6.461179e+01 7.245333e+01
#>  [991] 6.001227e+01 6.633089e+01 7.520003e+01 5.947445e+01 8.528679e+01
#>  [996] 6.018433e+01 6.542002e+01 8.431005e+01 6.199971e+01 6.954027e+01
```
