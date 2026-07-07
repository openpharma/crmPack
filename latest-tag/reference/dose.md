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

  (`proportion` or `numeric`)\
  a value of an independent variable on which dose depends. The
  following recycling rule applies when `samples` is not missing:
  vectors of size 1 will be recycled to the size of the sample (i.e.
  `size(samples)`). Otherwise, `x` must have the same size as the
  sample.

- model:

  (`GeneralModel` or `ModelPseudo`)\
  the model.

- samples:

  (`Samples`)\
  the samples of model's parameters that will be used to compute the
  resulting doses. Can also be missing for some models.

- ...:

  model specific parameters when `samples` are not used.

- grade:

  (`integer`)\
  The toxicity grade for which probabilities are required

- group:

  (`character` or `factor`)\
  for
  [`LogisticLogNormalGrouped`](https://docs.crmpack.org/reference/LogisticLogNormalGrouped-class.md),
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
  [`LogisticLogNormalGrouped`](https://docs.crmpack.org/reference/LogisticLogNormalGrouped-class.md)
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

The `dose()` and [`prob()`](https://docs.crmpack.org/reference/prob.md)
methods are the inverse of each other, for all `dose()` methods for
which its first argument, i.e. a given independent variable that dose
depends on, represents toxicity probability.

## See also

[`doseFunction()`](https://docs.crmpack.org/reference/doseFunction.md),
[`prob()`](https://docs.crmpack.org/reference/prob.md),
[`efficacy()`](https://docs.crmpack.org/reference/efficacy.md).

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
#>  [1] 180.87760 180.87760 180.87760  55.90603  55.90603  55.90603  55.90603
#>  [8] 138.94849 138.94849  73.87671  60.04099 122.81050  41.37296  29.86978
#> [15]  29.08137  27.74076  25.99670  25.99670  52.59565  70.92876

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
#>  [1]     10.07613     10.07613     22.15191 850756.88704     48.50700
#>  [6]     48.50700     48.50700     72.06078     72.06078     72.06078
#> [11]   2590.71936    155.17055    155.17055    155.17055    212.47066
#> [16]    156.91934    234.83714    234.83714    196.82793     90.86247
dose(x = c(0.45, 0.6), model = dlt_model)
#> [1] 144.6624 247.7348
data_ordinal <- .DefaultDataOrdinal()
model <- .DefaultLogisticLogNormalOrdinal()
options <- .DefaultMcmcOptions()
samples <- mcmc(data_ordinal, model, options)

dose(0.25, model, samples, grade = 2L)
#>    [1] 6.084236e+01 6.440508e+01 6.350956e+01 5.804029e+01 6.442973e+01
#>    [6] 8.242140e+01 6.071559e+01 7.842148e+01 5.329533e+01 5.997497e+01
#>   [11] 6.306759e+01 6.128383e+02 1.234477e+02 5.817633e+01 6.514051e+01
#>   [16] 6.084587e+01 6.184076e+01 1.035766e+02 5.796598e+01 6.135721e+01
#>   [21] 6.806631e+01 5.768848e+01 6.435752e+01 5.836043e+01 5.752979e+02
#>   [26] 6.118189e+01 6.635962e+01 1.099310e+02 7.617443e+01 6.701274e+01
#>   [31] 6.267869e+01 6.817706e+01 5.965913e+01 6.178192e+01 5.910995e+01
#>   [36] 7.711193e+01 5.786931e+01 1.225723e+02 1.100328e+02 6.739730e+01
#>   [41] 6.704177e+01 1.384178e+02 1.101130e+02 5.707480e+01 8.352868e+01
#>   [46] 6.193510e+01 6.766575e+01 2.023026e+03 7.849961e+01 5.677594e+01
#>   [51] 6.174945e+01 1.428823e+02 6.293405e+01 6.953205e+01 7.161779e+01
#>   [56] 1.157844e+02 6.354536e+01 4.812078e+01 6.585597e+01 1.097416e+02
#>   [61] 6.185931e+01 9.041202e+01 5.634861e+01 6.376350e+01 8.474827e+01
#>   [66] 8.916441e+01 8.100863e+01 6.983669e+01 5.344330e+01 5.417745e+01
#>   [71] 7.539183e+01 5.253755e+01 7.726710e+01 5.739494e+01 7.258842e+01
#>   [76] 6.075465e+01 5.640065e+01 3.376092e+02 8.777784e+01 5.162264e+01
#>   [81] 5.997141e+01 4.962930e+01 8.468673e+01 5.958393e+01 8.079681e+01
#>   [86] 8.816040e+01 1.026847e+02 6.554810e+01 6.263179e+01 1.060286e+02
#>   [91] 6.135190e+01 6.125682e+01 6.755693e+01 6.026286e+01 7.165184e+01
#>   [96] 6.690762e+01 9.084795e+01 9.556567e+01 6.584538e+01 6.913101e+01
#>  [101] 7.442458e+01 9.214999e+01 5.680937e+01 7.846436e+01 5.946149e+01
#>  [106] 5.281752e+01 5.551371e+01 8.003782e+01 6.499456e+01 8.168125e+01
#>  [111] 1.522945e+02 4.784981e+01 5.995148e+01 7.445335e+01 7.148048e+01
#>  [116] 5.761972e+01 5.461743e+01 6.308088e+02 2.912301e+02 6.447296e+01
#>  [121] 7.224277e+01 5.397173e+01 6.749566e+01 5.653912e+01 1.078222e+02
#>  [126] 9.896098e+01 7.448833e+01 5.535456e+01 9.113815e+01 5.505974e+01
#>  [131] 6.595743e+01 7.059149e+01 5.878682e+01 5.969893e+01 6.364094e+01
#>  [136] 5.755976e+01 6.820184e+01 5.738405e+01 8.424050e+01 8.368062e+01
#>  [141] 7.404808e+01 2.902179e+02 5.013102e+01 7.585641e+01 4.688363e+01
#>  [146] 4.166682e+02 8.052714e+01 1.883071e+02 6.085197e+01 5.415327e+01
#>  [151] 5.640598e+01 6.736257e+01 7.122143e+01 6.500624e+01 6.054760e+01
#>  [156] 6.207316e+01 6.040042e+01 8.799659e+01 6.605193e+01 8.460758e+01
#>  [161] 5.759171e+01 7.347112e+01 6.191802e+01 4.491044e+01 4.920510e+01
#>  [166] 7.310890e+01 5.455803e+01 6.621245e+01 6.639724e+01 2.913849e+02
#>  [171] 6.647944e+01 9.744144e+01 6.791100e+01 6.388955e+01 7.850732e+01
#>  [176] 8.216774e+01 5.471105e+01 7.707658e+01 5.996466e+01 7.390579e+01
#>  [181] 1.172038e+03 1.263823e+02 6.313399e+01 6.074438e+01 6.520834e+01
#>  [186] 7.584771e+01 8.295814e+01 5.543946e+01 5.870268e+01 5.889953e+01
#>  [191] 6.341937e+01 6.329591e+01 5.020715e+01 6.499829e+01 5.607523e+01
#>  [196] 6.980145e+01 8.122076e+01 7.472246e+01 6.918006e+01 1.080182e+02
#>  [201] 6.058727e+01 6.385141e+01 5.617346e+01 7.685335e+01 5.553537e+01
#>  [206] 5.799179e+01 7.750490e+01 5.250177e+01 3.436096e+02 7.136659e+01
#>  [211] 5.995385e+01 6.777155e+01 6.516769e+01 5.996174e+01 8.831690e+01
#>  [216] 9.724343e+01 5.756086e+01 5.909685e+01 5.325839e+01 6.776439e+01
#>  [221] 5.605689e+01 5.298690e+01 6.981977e+01 6.155035e+01 1.219537e+02
#>  [226] 1.170380e+06 6.392702e+02 6.349847e+01 7.323235e+01 9.718045e+01
#>  [231] 1.391000e+02 5.877148e+01 8.946097e+01 7.657936e+01 8.584189e+01
#>  [236] 6.053236e+01 2.624054e+02 1.403750e+02 9.995841e+01 8.914307e+01
#>  [241] 7.519887e+01 5.813575e+01 7.879938e+01 5.769070e+01 7.812379e+01
#>  [246] 6.292319e+01 6.224796e+01 5.593915e+01 6.274661e+01 6.160438e+01
#>  [251] 7.004479e+01 1.712768e+02 5.306752e+01 5.983497e+01 7.167586e+01
#>  [256] 6.063422e+01 6.239031e+01 7.131718e+01 5.151285e+02 1.215364e+02
#>  [261] 5.873055e+01 7.381817e+01 2.711014e+02 7.779515e+01 6.974998e+01
#>  [266] 6.604051e+01 6.148943e+01 6.463744e+01 6.380899e+01 7.047777e+01
#>  [271] 6.565358e+01 6.369994e+01 7.925958e+01 7.298684e+01 6.375347e+01
#>  [276] 5.710641e+01 9.141773e+01 5.904530e+01 6.340860e+01 7.545694e+01
#>  [281] 5.448621e+01 8.575611e+01 6.642066e+01 5.843335e+01 5.762740e+01
#>  [286] 6.034685e+01 7.767774e+01 7.991258e+01 6.616366e+01 6.391499e+01
#>  [291] 5.799954e+01 6.565019e+01 5.641608e+01 6.337633e+01 8.024603e+01
#>  [296] 6.600401e+01 2.829039e+02 1.117057e+02 5.890152e+01 6.081475e+01
#>  [301] 5.986107e+01 8.948019e+01 5.942547e+01 1.044994e+02 6.002382e+01
#>  [306] 6.298590e+01 8.915144e+01 5.711618e+01 7.136198e+01 5.756163e+01
#>  [311] 1.638924e+02 6.323917e+01 5.428503e+01 5.593938e+01 5.538660e+01
#>  [316] 5.557339e+01 1.047090e+02 9.417353e+01 7.782028e+01 6.298521e+01
#>  [321] 6.123762e+01 5.693779e+01 8.462203e+01 7.858217e+01 5.591790e+01
#>  [326] 6.468964e+01 6.540102e+01 6.992792e+01 7.330565e+01 7.729074e+01
#>  [331] 8.527199e+01 1.589874e+03 4.599496e+02 7.931842e+03 8.060539e+01
#>  [336] 7.373153e+01 5.959255e+01 8.127354e+01 1.814186e+02 1.809965e+02
#>  [341] 8.802848e+01 1.481396e+02 2.732298e+02 6.122689e+01 5.443612e+01
#>  [346] 2.727192e+02 1.146491e+03 5.807801e+01 6.171078e+01 5.946082e+01
#>  [351] 5.813521e+01 1.234087e+02 4.923365e+01 4.900502e+02 5.855183e+01
#>  [356] 5.729075e+01 5.873704e+01 6.266565e+01 7.113023e+01 8.375814e+01
#>  [361] 1.293227e+02 5.442576e+02 5.630029e+01 5.522123e+01 6.602841e+01
#>  [366] 6.039336e+01 6.193292e+01 7.536955e+01 5.789079e+01 8.846802e+01
#>  [371] 6.426636e+01 6.451622e+01 5.692182e+01 9.878919e+01 5.766813e+01
#>  [376] 6.850321e+01 6.121164e+01 7.436775e+01 5.934338e+01 7.274312e+01
#>  [381] 5.707157e+01 5.484218e+01 9.153737e+01 7.237381e+01 1.213717e+02
#>  [386] 6.569398e+01 5.073885e+01 6.976218e+01 5.645876e+01 1.017356e+02
#>  [391] 9.355533e+01 6.107599e+01 5.852248e+01 6.194312e+01 6.461160e+01
#>  [396] 6.388590e+01 1.099830e+02 7.792872e+01 1.876876e+02 1.158271e+02
#>  [401] 6.599068e+01 7.408096e+01 6.285570e+01 3.307425e+02 5.800580e+01
#>  [406] 5.995756e+01 6.729338e+01 7.033278e+01 6.378661e+01 6.144858e+01
#>  [411] 6.415835e+01 5.851483e+01 6.590044e+01 6.392821e+01 6.937500e+01
#>  [416] 5.755884e+01 5.719891e+01 5.601290e+01 7.502291e+01 8.742415e+01
#>  [421] 6.238046e+01 6.563952e+01 5.613570e+01 2.046494e+02 5.854189e+03
#>  [426] 5.844580e+01 6.660107e+01 6.185989e+01 6.354418e+01 6.052069e+01
#>  [431] 5.566777e+01 1.120547e+02 6.199354e+01 8.159198e+01 5.444795e+01
#>  [436] 7.080901e+01 7.327581e+01 1.687029e+02 1.057995e+02 7.168233e+01
#>  [441] 8.652079e+01 7.469748e+01 1.003438e+03 7.282780e+01 6.402459e+01
#>  [446] 8.654966e+01 8.060478e+01 1.271666e+02 6.344522e+01 7.266538e+01
#>  [451] 1.284202e+02 5.614421e+01 5.934970e+01 6.183518e+01 7.074471e+01
#>  [456] 6.157729e+01 7.067956e+01 5.679051e+01 5.898663e+01 5.869391e+01
#>  [461] 5.981737e+01 5.821434e+01 6.582847e+01 6.200502e+01 6.196795e+01
#>  [466] 6.630864e+01 6.010299e+01 7.233190e+01 6.736732e+01 6.434559e+01
#>  [471] 9.888588e+01 5.097871e+01 6.377972e+01 9.130783e+01 5.299182e+01
#>  [476] 1.137033e+02 2.011169e+02 5.323573e+01 7.998770e+01 7.058079e+01
#>  [481] 6.165029e+01 9.651744e+01 6.863588e+01 5.902143e+01 1.122341e+02
#>  [486] 9.917285e+01 2.123721e+02 6.505577e+01 5.617271e+01 6.059657e+01
#>  [491] 7.753016e+01 7.315514e+01 6.118410e+01 8.269682e+01 6.550608e+01
#>  [496] 1.396446e+02 5.653333e+01 5.467493e+01 5.639893e+01 7.776714e+01
#>  [501] 6.509808e+01 5.484020e+01 5.706852e+01 5.815363e+01 5.771599e+01
#>  [506] 6.109496e+01 7.642024e+01 5.700156e+01 7.344116e+01 5.705912e+01
#>  [511] 5.418066e+01 5.899012e+01 7.919666e+01 8.616706e+01 6.433432e+01
#>  [516] 6.708943e+01 5.854746e+01 6.465719e+01 5.764099e+01 7.545782e+01
#>  [521] 6.190258e+01 6.237137e+01 8.226557e+01 2.322012e+02 1.504927e+02
#>  [526] 5.888270e+01 6.464931e+01 6.325057e+01 4.982256e+01 6.547103e+01
#>  [531] 6.895786e+01 9.614041e+01 6.608577e+01 8.247552e+01 8.275607e+01
#>  [536] 5.897866e+01 6.252094e+01 8.035873e+01 6.769845e+01 5.610124e+01
#>  [541] 5.418004e+01 5.948615e+01 7.217855e+01 6.601619e+01 8.509302e+01
#>  [546] 6.271203e+01 6.934001e+01 9.044973e+01 1.759834e+02 5.545654e+01
#>  [551] 6.319132e+01 7.128196e+01 8.133802e+01 5.495168e+01 6.210279e+01
#>  [556] 6.191908e+01 8.735013e+01 1.302274e+02 5.906780e+01 5.914537e+01
#>  [561] 6.571638e+01 2.262039e+02 1.339500e+02 5.966360e+01 5.848333e+01
#>  [566] 6.667716e+01 6.034982e+01 6.951997e+01 7.902505e+01 6.060599e+01
#>  [571] 5.847221e+01 8.912968e+01 2.087032e+02 1.553816e+02 1.090943e+02
#>  [576] 8.557753e+01 6.153394e+01 7.597533e+01 2.347248e+02 5.530427e+01
#>  [581] 6.585665e+01 5.880965e+01 6.903605e+01 5.916025e+01 6.065890e+01
#>  [586] 5.741596e+01 5.657413e+01 6.285422e+01 6.257245e+01 6.039884e+01
#>  [591] 5.556541e+01 5.675915e+01 5.431524e+01 1.563382e+02 7.279283e+02
#>  [596] 8.971900e+01 1.800141e+02 9.439269e+01 9.380718e+01 1.020358e+02
#>  [601] 5.965495e+01 6.125659e+01 8.077345e+01 5.689439e+01 8.567097e+01
#>  [606] 1.189538e+02 6.918006e+01 6.210495e+01 7.768280e+01 5.943813e+01
#>  [611] 5.782942e+01 6.252773e+01 1.170857e+02 8.400663e+01 8.578356e+01
#>  [616] 7.944998e+01 5.646996e+01 6.254973e+01 8.810032e+01 5.948722e+01
#>  [621] 6.364619e+01 6.304956e+01 6.290112e+01 6.347853e+01 9.053069e+01
#>  [626] 9.621699e+01 6.737134e+01 6.303536e+01 5.552263e+01 2.635089e+02
#>  [631] 5.518991e+01 4.225977e+02 1.056049e+02 1.137340e+02 6.756183e+01
#>  [636] 6.374034e+01 5.974004e+01 6.718401e+01 6.116162e+01 7.251970e+01
#>  [641] 5.993694e+01 5.622880e+01 6.104376e+01 6.295916e+01 5.658048e+01
#>  [646] 6.437869e+01 5.797681e+01 2.874858e+02 5.033291e+01 5.463754e+01
#>  [651] 7.673348e+01 8.625888e+01 6.918455e+01 6.163565e+01 8.268545e+01
#>  [656] 6.644628e+01 6.100302e+01 6.788507e+01 6.799430e+01 6.317040e+01
#>  [661] 6.744806e+01 6.017101e+01 6.205033e+01 7.395470e+01 1.341064e+02
#>  [666] 6.737453e+01 7.905396e+01 5.572511e+01 5.524007e+01 6.166400e+01
#>  [671] 7.519046e+01 5.833240e+01 5.860640e+01 6.090480e+01 6.635296e+01
#>  [676] 6.584679e+01 7.179104e+01 6.367032e+01 6.360295e+01 5.649583e+01
#>  [681] 6.741893e+01 8.175094e+01 6.100206e+01 5.660194e+01 2.293133e+02
#>  [686] 5.944471e+01 6.479598e+01 6.180517e+01 5.975542e+01 7.274496e+01
#>  [691] 9.733754e+01 8.168857e+01 1.105560e+02 5.802908e+01 3.105950e+02
#>  [696] 6.106414e+01 5.296318e+01 7.059930e+01 1.590943e+03 4.754144e+01
#>  [701] 4.492687e+01 6.235096e+01 6.634969e+01 6.046047e+01 8.677816e+01
#>  [706] 7.692717e+01 6.684074e+01 5.548725e+01 6.454335e+01 7.258265e+01
#>  [711] 6.219988e+01 6.121897e+01 5.787599e+01 6.273826e+01 7.184124e+01
#>  [716] 7.925068e+01 3.416698e+02 6.624499e+01 5.758266e+01 6.740898e+01
#>  [721] 1.015401e+02 7.718016e+02 1.597127e+02 5.483835e+01 6.102703e+01
#>  [726] 8.929962e+01 1.280689e+02 6.050891e+01 5.136608e+01 1.531584e+02
#>  [731] 5.781245e+01 5.320411e+01 6.516823e+01 6.384220e+01 7.109599e+01
#>  [736] 6.205612e+01 8.170673e+01 1.403178e+02 7.817316e+01 6.331721e+01
#>  [741] 5.559774e+01 6.066639e+01 9.607352e+01 7.345731e+01 6.253131e+01
#>  [746] 3.880787e+02 6.563039e+01 6.465367e+01 6.517809e+01 8.478770e+01
#>  [751] 6.816758e+01 7.152817e+01 6.981047e+01 8.784054e+01 5.672468e+01
#>  [756] 5.360937e+01 5.895039e+01 5.704512e+01 1.592246e+02 6.779461e+01
#>  [761] 1.850848e+02 2.022898e+02 6.586206e+01 5.485525e+01 5.996776e+01
#>  [766] 6.156843e+01 6.696648e+01 6.408052e+01 3.123223e+02 1.238802e+02
#>  [771] 5.658427e+01 5.928469e+01 5.962534e+01 6.196746e+01 7.443570e+01
#>  [776] 1.011269e+02 6.345808e+01 7.788321e+01 5.908646e+01 7.226853e+01
#>  [781] 6.287950e+01 5.393149e+01 6.234164e+01 9.216941e+01 1.368788e+23
#>  [786] 3.351781e+05 1.848379e+02 9.529727e+01 7.659711e+01 6.194835e+01
#>  [791] 6.831997e+01 6.613406e+01 6.831008e+01 5.035474e+01 5.040826e+01
#>  [796] 5.479727e+01 6.351247e+01 6.333341e+01 6.988673e+01 8.325468e+01
#>  [801] 9.315788e+01 5.583804e+01 6.173894e+01 5.948984e+01 7.458769e+01
#>  [806] 6.310483e+01 6.750921e+01 5.645922e+01 6.626201e+01 6.475456e+01
#>  [811] 6.464442e+01 7.008089e+01 1.077051e+02 1.075329e+02 6.056121e+01
#>  [816] 5.637919e+01 6.844529e+01 5.713836e+01 6.011491e+01 6.873432e+01
#>  [821] 6.527630e+01 6.524052e+01 4.719872e+01 1.012698e+02 9.993694e+01
#>  [826] 5.606451e+01 6.031373e+01 6.485254e+01 5.461730e+01 1.123804e+02
#>  [831] 7.165919e+01 5.963831e+01 5.592782e+01 7.006409e+01 9.694013e+01
#>  [836] 7.968043e+01 6.597452e+01 5.826204e+01 5.787640e+01 8.708026e+01
#>  [841] 5.196658e+02 7.264589e+01 1.151056e+02 1.104363e+02 6.829647e+01
#>  [846] 9.329511e+01 7.624631e+01 3.121053e+02 9.358079e+01 5.882382e+01
#>  [851] 5.927380e+01 5.747228e+01 6.728847e+01 6.845510e+01 6.507058e+01
#>  [856] 1.022450e+02 6.013918e+01 6.167339e+01 5.949145e+01 6.040305e+01
#>  [861] 5.592095e+01 6.655916e+01 1.156401e+02 6.442669e+01 6.343284e+01
#>  [866] 6.441235e+01 1.775962e+02 1.273056e+02 6.010745e+01 8.942143e+01
#>  [871] 2.090849e+02 7.330970e+01 1.026764e+02 6.247343e+01 5.630105e+01
#>  [876] 5.721013e+01 6.562087e+01 5.342097e+01 2.767061e+02 5.218938e+01
#>  [881] 6.215640e+02 1.219393e+03 2.040917e+02 5.339340e+01 9.088524e+01
#>  [886] 6.383943e+01 9.384908e+01 7.740174e+01 6.681885e+01 5.162684e+02
#>  [891] 6.661847e+01 5.356397e+01 6.139001e+01 8.210092e+01 1.576660e+02
#>  [896] 8.136074e+01 8.816983e+03 7.051344e+01 7.880555e+01 6.033312e+01
#>  [901] 5.535560e+01 1.184837e+02 1.988403e+02 6.100396e+01 1.192591e+02
#>  [906] 5.714821e+01 5.754894e+01 6.428170e+01 5.951857e+01 7.037650e+01
#>  [911] 6.731613e+01 6.197261e+01 9.766013e+01 5.162413e+01 6.258466e+01
#>  [916] 2.297561e+02 5.847483e+01 6.056793e+01 6.186483e+01 6.368381e+01
#>  [921] 6.517136e+01 6.360101e+01 6.131304e+01 8.090730e+01 6.294230e+01
#>  [926] 6.338104e+01 6.696211e+01 7.958635e+01 5.551294e+01 5.765871e+01
#>  [931] 7.805537e+01 6.610808e+01 6.745206e+01 1.388888e+02 4.814289e+01
#>  [936] 1.084716e+02 6.370835e+01 5.851556e+01 6.518505e+01 6.807873e+01
#>  [941] 5.665581e+01 6.962259e+01 6.778972e+01 5.965693e+01 5.849033e+01
#>  [946] 6.323085e+01 1.225099e+02 6.227188e+01 5.979757e+01 6.630504e+01
#>  [951] 1.133625e+02 5.413743e+01 5.463782e+01 1.728545e+02 1.132089e+02
#>  [956] 6.206961e+01 7.973423e+01 1.631107e+02 7.076522e+01 5.580136e+01
#>  [961] 6.030274e+01 6.373493e+01 2.142676e+02 6.849732e+01 5.717941e+01
#>  [966] 5.759919e+01 1.261430e+02 7.100683e+01 5.824332e+01 6.691892e+01
#>  [971] 6.322942e+01 9.709852e+01 5.748671e+01 5.890787e+01 5.971606e+01
#>  [976] 6.426800e+01 7.716856e+01 5.419330e+01 6.587108e+01 1.170867e+02
#>  [981] 5.972468e+01 6.343607e+01 7.432615e+01 5.735037e+01 5.577345e+01
#>  [986] 1.508642e+02 5.772003e+01 6.131489e+01 6.760472e+01 6.109052e+01
#>  [991] 5.727220e+01 1.140859e+02 6.746647e+01 5.572238e+01 5.870255e+01
#>  [996] 6.191132e+01 5.297459e+01 1.409311e+02 7.225813e+01 1.148643e+02
```
