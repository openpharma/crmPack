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
#>  [1] 15.02938 15.76814 15.76814 15.76814 15.76814 31.28534 31.28534 55.27813
#>  [9] 55.27813 55.27813 55.27813 55.27813 55.27813 64.47318 64.47318 72.35447
#> [17] 72.35447 72.35447 72.35447 72.35447

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
#>    [1] 1.029426e+02 1.158173e+02 6.619811e+01 1.193962e+02 6.671173e+01
#>    [6] 6.728552e+01 1.847695e+02 7.433064e+01 7.737161e+01 4.040067e+02
#>   [11] 6.705926e+01 1.004746e+02 5.717493e+01 5.725004e+01 6.337597e+01
#>   [16] 5.837566e+01 9.641887e+01 5.323049e+01 5.801359e+01 5.968040e+01
#>   [21] 6.007173e+01 6.390139e+01 1.306454e+02 5.750518e+01 4.803596e+01
#>   [26] 5.402498e+01 6.550896e+01 6.889526e+01 5.167013e+01 5.661692e+01
#>   [31] 6.066596e+01 9.060353e+01 5.776415e+01 5.794784e+01 6.056464e+01
#>   [36] 6.134511e+01 5.894766e+01 7.370663e+01 6.929190e+01 5.826402e+01
#>   [41] 2.156072e+02 6.979889e+01 7.118325e+01 6.067982e+01 5.875306e+01
#>   [46] 1.763695e+02 5.009526e+02 5.933429e+01 1.343654e+02 1.305703e+02
#>   [51] 5.722022e+01 5.926966e+01 6.250904e+01 9.830680e+01 6.196191e+01
#>   [56] 8.499463e+01 7.505803e+01 5.883306e+01 6.113903e+01 6.132815e+01
#>   [61] 1.310593e+02 7.618544e+01 5.971467e+01 7.031971e+01 7.554061e+01
#>   [66] 1.652650e+02 4.562718e+02 9.909543e+01 6.252046e+01 6.368255e+01
#>   [71] 6.707608e+01 6.553587e+01 7.023570e+01 6.687824e+01 5.876206e+01
#>   [76] 7.782365e+01 9.352494e+01 9.015371e+01 1.214328e+02 5.931385e+01
#>   [81] 5.744670e+01 9.132805e+01 6.907933e+01 7.036644e+01 5.870096e+01
#>   [86] 7.293874e+01 4.855007e+01 2.213829e+02 6.217458e+01 6.128202e+01
#>   [91] 9.211749e+01 6.346105e+01 5.848590e+01 2.539918e+02 7.205619e+01
#>   [96] 6.028396e+01 9.110016e+01 6.130297e+01 5.480168e+01 6.995837e+01
#>  [101] 7.206172e+01 6.238890e+01 1.122254e+02 8.141109e+01 1.403088e+02
#>  [106] 5.842503e+01 7.872429e+01 6.158784e+01 7.320911e+01 8.400924e+01
#>  [111] 5.875531e+01 1.500177e+02 1.196938e+02 5.912389e+01 4.872325e+02
#>  [116] 7.509993e+01 7.483234e+01 5.705062e+01 4.591265e+01 6.194101e+01
#>  [121] 5.877485e+01 5.844285e+01 7.407848e+01 6.573620e+01 1.424204e+02
#>  [126] 1.038529e+02 6.126975e+01 5.918970e+01 5.779078e+01 6.361734e+01
#>  [131] 5.254701e+01 9.470247e+01 1.748804e+02 7.620754e+01 5.614087e+01
#>  [136] 5.688505e+01 6.809124e+01 4.848016e+02 3.211776e+04 7.333012e+07
#>  [141] 7.923404e+04 4.614988e+04 8.286230e+01 5.614221e+01 5.868937e+01
#>  [146] 1.413168e+02 5.581396e+01 5.827717e+01 8.819591e+01 6.831760e+01
#>  [151] 6.337979e+01 6.821914e+01 5.803477e+01 6.906233e+01 7.080970e+01
#>  [156] 5.673468e+01 6.167708e+01 5.947877e+01 6.040522e+01 7.253893e+01
#>  [161] 5.508509e+01 6.300579e+01 5.470487e+01 8.767741e+01 5.654259e+01
#>  [166] 1.405302e+03 2.447985e+06 5.615772e+01 6.362250e+01 7.815324e+01
#>  [171] 1.153334e+02 1.275511e+02 6.543852e+01 6.268427e+01 6.857504e+01
#>  [176] 5.844963e+01 6.039300e+01 6.483707e+01 5.428941e+01 5.765347e+01
#>  [181] 7.430781e+01 5.814162e+01 5.940123e+01 6.956725e+01 5.821137e+01
#>  [186] 1.439435e+02 5.909011e+01 5.973832e+01 5.504986e+01 7.118251e+01
#>  [191] 6.947124e+01 6.718241e+01 5.957927e+01 6.970543e+01 5.855503e+01
#>  [196] 7.988358e+01 9.136460e+01 4.407069e+01 5.839851e+01 6.436749e+01
#>  [201] 6.240447e+01 6.731375e+01 5.790110e+01 7.193473e+01 6.995780e+01
#>  [206] 6.913782e+01 6.179287e+01 5.898722e+01 6.332667e+01 5.603411e+01
#>  [211] 6.771861e+01 8.355434e+01 6.273314e+01 5.589472e+01 5.635635e+01
#>  [216] 1.142987e+02 5.817566e+01 8.572214e+01 5.246572e+01 6.756944e+01
#>  [221] 5.491137e+01 6.019510e+01 6.302053e+01 5.842472e+01 5.775868e+01
#>  [226] 6.229699e+01 4.573527e+01 9.979377e+01 8.720168e+01 5.684054e+01
#>  [231] 6.914125e+01 8.926482e+01 7.124515e+01 1.106172e+02 6.724543e+01
#>  [236] 4.952937e+01 8.286594e+01 2.070748e+02 2.431509e+02 6.401595e+05
#>  [241] 1.132377e+03 2.384074e+02 7.640834e+01 6.322939e+01 6.379421e+01
#>  [246] 7.966795e+01 6.145845e+01 1.397249e+02 6.183857e+01 8.026583e+01
#>  [251] 6.631734e+01 4.925723e+01 8.692281e+01 5.581962e+01 6.216157e+01
#>  [256] 3.108814e+02 6.990069e+01 1.253496e+02 6.279795e+01 5.683550e+01
#>  [261] 5.832625e+01 6.375318e+01 1.593627e+04 7.489363e+01 7.305656e+01
#>  [266] 4.988114e+01 4.995853e+01 8.755415e+01 8.313859e+01 5.946511e+01
#>  [271] 7.477342e+01 8.395143e+01 6.184647e+01 5.942378e+01 8.591196e+01
#>  [276] 6.408309e+01 8.205995e+01 7.392719e+01 8.736575e+01 5.830879e+01
#>  [281] 1.515619e+02 6.773487e+01 5.890790e+01 8.473672e+01 5.753751e+01
#>  [286] 6.475748e+01 6.073550e+01 5.980057e+01 6.438769e+01 6.122714e+01
#>  [291] 5.806397e+01 6.236436e+01 5.634858e+01 7.307786e+01 6.039099e+01
#>  [296] 5.635225e+01 6.116017e+01 5.902676e+01 6.001865e+01 8.151215e+01
#>  [301] 1.063219e+02 5.483986e+01 6.675433e+01 6.479881e+01 5.528322e+01
#>  [306] 1.042334e+02 1.037700e+02 5.277530e+01 7.203189e+01 5.902525e+01
#>  [311] 6.737464e+01 1.688529e+03 1.567555e+02 6.555244e+01 5.930102e+01
#>  [316] 7.681145e+01 5.784843e+01 7.083451e+01 5.786159e+01 5.757870e+01
#>  [321] 6.255234e+01 6.324503e+01 1.226145e+02 1.022944e+02 5.798864e+01
#>  [326] 6.707354e+01 5.880339e+01 6.995778e+01 1.433441e+02 7.462645e+01
#>  [331] 6.288521e+01 7.452070e+01 5.453514e+01 6.099592e+01 5.988651e+01
#>  [336] 6.595046e+01 6.213419e+01 6.276775e+01 9.077606e+01 5.516118e+01
#>  [341] 6.046825e+01 6.659629e+01 6.214364e+01 5.601219e+01 9.469511e+01
#>  [346] 9.259019e+01 5.646784e+01 1.541785e+02 5.159767e+01 5.341149e+01
#>  [351] 6.137707e+01 6.461743e+01 7.547278e+01 7.272692e+01 6.021633e+01
#>  [356] 8.809389e+01 5.905883e+01 6.389957e+01 6.143228e+01 5.730791e+01
#>  [361] 6.156394e+01 7.255908e+01 5.545495e+01 6.465924e+01 8.173286e+01
#>  [366] 5.404789e+01 7.364607e+01 6.162589e+01 6.437957e+01 7.208246e+01
#>  [371] 6.463185e+01 7.606397e+01 5.330024e+01 1.129268e+02 5.529395e+01
#>  [376] 6.092264e+01 6.587983e+01 6.380669e+01 1.105130e+02 5.544342e+01
#>  [381] 6.667117e+01 2.970001e+02 1.811828e+02 1.406461e+05 7.284338e+01
#>  [386] 6.585347e+01 1.727067e+02 2.878981e+02 2.396444e+02 5.196000e+01
#>  [391] 7.066132e+01 7.153788e+01 1.136091e+02 5.825561e+01 7.129004e+01
#>  [396] 1.048883e+02 2.645981e+02 5.910925e+01 7.162368e+01 5.810130e+01
#>  [401] 6.771056e+01 7.745171e+01 1.309655e+02 6.679199e+01 3.008886e+02
#>  [406] 6.126153e+01 6.211450e+01 6.178518e+01 5.718700e+01 5.898598e+01
#>  [411] 6.147850e+01 9.876398e+01 7.326716e+01 5.750201e+01 7.197125e+01
#>  [416] 6.585003e+01 7.994422e+01 6.339899e+01 5.712073e+01 4.772815e+02
#>  [421] 9.894849e+01 5.715210e+01 6.600727e+01 7.333098e+01 6.385813e+01
#>  [426] 8.725937e+01 7.312425e+01 1.598106e+02 3.122613e+03 5.883105e+01
#>  [431] 6.051385e+01 9.986827e+01 1.013937e+02 5.907744e+01 5.813250e+01
#>  [436] 6.655399e+01 6.153905e+01 6.582851e+01 7.904417e+01 6.573608e+01
#>  [441] 7.856538e+01 5.593078e+01 1.037434e+02 7.550349e+01 6.805664e+01
#>  [446] 1.031706e+02 5.826976e+01 6.704900e+01 6.479982e+01 5.843636e+01
#>  [451] 6.378621e+01 6.206507e+01 6.977948e+01 7.117402e+01 6.535670e+01
#>  [456] 5.931968e+01 1.106550e+02 7.873004e+01 6.190845e+01 6.548164e+01
#>  [461] 6.199135e+01 5.710330e+01 5.863618e+01 8.210444e+01 6.263934e+01
#>  [466] 6.328070e+01 6.544216e+01 5.879398e+01 6.578634e+01 6.218035e+01
#>  [471] 6.539459e+01 6.429390e+01 6.273316e+01 8.027028e+01 8.791627e+01
#>  [476] 2.603191e+02 8.193098e+01 1.093518e+02 1.000119e+02 1.400918e+02
#>  [481] 7.146589e+01 1.537204e+02 5.571032e+01 7.415812e+01 5.368001e+01
#>  [486] 5.235544e+01 5.755441e+01 1.373763e+02 6.583144e+01 1.002060e+02
#>  [491] 5.386591e+01 3.312205e+02 1.253983e+02 1.080681e+02 5.789000e+01
#>  [496] 1.045104e+02 5.194542e+01 7.049850e+01 1.724696e+02 6.024399e+01
#>  [501] 5.668307e+01 6.872645e+01 6.368123e+01 6.004065e+01 5.915508e+01
#>  [506] 1.836858e+02 7.183818e+01 7.571136e+01 6.651188e+01 6.298787e+01
#>  [511] 5.858297e+01 6.504568e+01 5.245331e+01 1.399563e+02 6.316940e+01
#>  [516] 5.764698e+01 6.829704e+01 7.748055e+01 6.966857e+01 7.089011e+01
#>  [521] 7.655803e+01 6.950363e+01 6.630395e+01 6.454233e+01 6.798656e+01
#>  [526] 8.012945e+01 6.707527e+01 1.256689e+02 2.875547e+02 6.087136e+01
#>  [531] 6.090586e+01 8.750420e+01 6.917154e+01 7.138789e+01 8.241934e+01
#>  [536] 6.615298e+01 5.570352e+01 6.771490e+01 6.840577e+01 5.814724e+01
#>  [541] 5.699556e+01 6.610768e+01 5.815633e+01 1.148611e+02 6.598504e+01
#>  [546] 7.627780e+01 7.682059e+01 6.332483e+01 7.695153e+01 8.073308e+01
#>  [551] 7.447854e+01 6.442252e+01 6.729428e+01 7.870074e+01 6.387656e+01
#>  [556] 9.306167e+01 3.250738e+02 3.083870e+02 6.193242e+01 5.890609e+01
#>  [561] 6.740466e+01 6.580516e+01 6.819791e+01 8.195331e+01 1.288335e+02
#>  [566] 2.333344e+02 1.876364e+04 1.516916e+02 1.049930e+02 7.859879e+01
#>  [571] 5.360669e+01 9.735554e+01 4.045736e+01 5.104464e+01 7.000956e+01
#>  [576] 5.303335e+01 5.621900e+01 6.815970e+01 6.864005e+01 8.830106e+01
#>  [581] 5.565493e+01 2.180276e+02 5.498612e+01 5.996870e+01 5.896749e+01
#>  [586] 6.567429e+01 5.821740e+01 7.084332e+01 5.590440e+01 1.007530e+02
#>  [591] 6.059903e+01 5.812463e+01 6.274179e+01 5.460904e+01 6.449662e+01
#>  [596] 6.926711e+01 6.498968e+01 6.381284e+01 9.637981e+01 7.287921e+01
#>  [601] 6.010351e+01 7.114181e+01 6.894594e+01 9.884066e+01 6.987531e+01
#>  [606] 9.591536e+01 5.517158e+01 8.729278e+01 7.251433e+01 6.502816e+01
#>  [611] 5.663121e+01 7.144139e+01 6.104121e+01 8.380788e+01 6.691613e+01
#>  [616] 7.481064e+01 5.618900e+01 5.783207e+01 7.244542e+01 5.865610e+01
#>  [621] 5.840281e+01 5.881773e+01 6.214577e+01 6.047870e+01 7.586022e+01
#>  [626] 6.601130e+01 5.700937e+01 6.474090e+01 5.543087e+01 6.009497e+01
#>  [631] 5.356999e+01 6.548638e+01 5.935546e+01 6.321360e+01 5.700692e+01
#>  [636] 1.754285e+02 3.666877e+02 6.715944e+01 6.616346e+01 8.216047e+01
#>  [641] 5.895521e+01 6.406836e+01 8.193119e+01 7.522320e+01 8.586131e+01
#>  [646] 8.080899e+01 6.144531e+01 9.862856e+01 5.735302e+01 7.605481e+01
#>  [651] 5.690429e+01 7.642967e+01 7.093969e+01 6.925619e+01 3.349728e+03
#>  [656] 6.364800e+01 1.000240e+02 6.881072e+01 6.358148e+01 6.546813e+01
#>  [661] 6.513553e+01 6.107877e+01 5.861621e+01 5.932798e+03 4.746376e+03
#>  [666] 1.943354e+02 7.315136e+01 7.168790e+01 5.675802e+01 6.304993e+01
#>  [671] 5.737925e+01 7.070892e+01 6.708458e+01 6.686278e+01 6.188737e+01
#>  [676] 5.613325e+01 8.228266e+01 8.302089e+01 5.788263e+01 6.210459e+01
#>  [681] 7.172210e+01 1.389207e+02 6.770217e+01 6.847943e+01 6.471444e+01
#>  [686] 5.062345e+01 1.830393e+02 6.543632e+01 5.786064e+01 7.057666e+01
#>  [691] 7.059039e+01 4.540078e+01 9.878043e+01 6.096700e+01 6.127559e+01
#>  [696] 6.663489e+01 5.955592e+01 6.032682e+01 7.223766e+01 9.144441e+01
#>  [701] 7.305359e+01 5.665305e+01 2.344719e+02 6.275072e+01 1.073026e+02
#>  [706] 6.048911e+01 5.567427e+01 5.860508e+01 9.966574e+01 7.961070e+01
#>  [711] 1.060087e+02 6.644458e+01 1.017827e+02 5.525500e+01 7.977197e+01
#>  [716] 9.358021e+01 1.859602e+02 8.909168e+01 5.904507e+01 6.846346e+01
#>  [721] 6.065105e+01 7.730017e+01 1.718615e+02 5.877571e+01 5.682943e+01
#>  [726] 6.068644e+01 6.375644e+01 5.917604e+01 6.142429e+01 5.886949e+01
#>  [731] 5.767012e+01 8.905330e+01 5.380148e+01 6.352245e+01 6.426918e+01
#>  [736] 8.468158e+01 6.491794e+01 6.258271e+01 1.321714e+02 5.745143e+01
#>  [741] 5.784963e+01 7.842043e+01 6.427897e+01 5.805658e+01 6.619734e+01
#>  [746] 1.122142e+02 5.648654e+01 5.941826e+01 6.482088e+01 1.156824e+02
#>  [751] 6.305143e+01 5.602533e+01 1.212574e+02 8.735118e+01 1.169664e+02
#>  [756] 6.004082e+01 7.234977e+01 5.817677e+01 5.453057e+01 6.447609e+01
#>  [761] 2.704609e+03 4.959853e+01 6.950120e+01 5.957846e+01 6.388047e+01
#>  [766] 7.719540e+01 5.724060e+01 7.447367e+01 6.135429e+01 8.486277e+01
#>  [771] 6.369685e+01 7.229091e+01 6.976053e+01 8.949898e+01 5.402025e+01
#>  [776] 1.052916e+02 6.620335e+01 9.333226e+01 9.426133e+01 5.364590e+01
#>  [781] 9.840238e+01 8.090201e+01 6.927641e+01 7.833803e+01 5.750108e+01
#>  [786] 5.511601e+01 8.327183e+01 5.650721e+01 6.092624e+01 5.610150e+01
#>  [791] 6.696559e+01 6.642983e+01 5.768342e+01 5.972893e+01 6.055465e+01
#>  [796] 6.453672e+01 4.044767e+01 7.439814e+01 1.232072e+02 3.090378e+02
#>  [801] 9.032077e+01 7.947389e+01 6.118876e+01 7.188047e+01 7.209866e+01
#>  [806] 6.085736e+01 1.899240e+02 1.859971e+02 8.117896e+01 6.572052e+01
#>  [811] 6.628200e+01 6.087545e+01 6.995276e+01 5.377298e+01 5.499173e+01
#>  [816] 6.072899e+01 7.207220e+01 5.954069e+01 6.384943e+01 6.071264e+01
#>  [821] 5.605621e+01 6.618028e+01 7.824394e+01 8.080920e+01 4.034072e+01
#>  [826] 5.654327e+01 7.872979e+01 7.648128e+01 9.628763e+01 6.784153e+01
#>  [831] 6.296075e+01 6.004222e+01 6.651731e+01 7.983862e+01 1.008511e+02
#>  [836] 5.664199e+01 7.788949e+01 5.671544e+01 9.045132e+01 9.455003e+01
#>  [841] 6.060984e+01 6.641046e+01 1.029974e+02 7.152183e+01 6.974423e+01
#>  [846] 7.155880e+01 6.959406e+01 6.817198e+01 1.302695e+02 1.046841e+02
#>  [851] 5.911338e+01 6.713051e+01 6.861926e+01 6.599897e+01 7.956062e+01
#>  [856] 6.238457e+01 8.436144e+01 6.659525e+01 5.245729e+01 5.995503e+01
#>  [861] 6.702919e+01 6.474534e+01 5.368355e+01 6.400994e+01 7.579967e+01
#>  [866] 6.547649e+01 6.019152e+01 6.440696e+01 5.812078e+01 5.953519e+01
#>  [871] 6.147123e+01 5.969110e+01 6.626190e+01 6.268203e+01 6.765382e+01
#>  [876] 6.961921e+01 6.232901e+01 5.144021e+01 5.461933e+01 5.586103e+01
#>  [881] 6.523382e+01 6.644558e+01 1.260388e+02 6.911692e+01 8.659936e+01
#>  [886] 7.614745e+01 6.169580e+01 7.268566e+01 1.091976e+02 6.149613e+01
#>  [891] 6.989388e+01 7.997260e+01 5.707807e+01 6.095179e+01 6.635456e+01
#>  [896] 9.973579e+01 6.107975e+01 8.590374e+01 5.618224e+01 1.067102e+02
#>  [901] 2.584251e+02 6.289158e+02 5.905445e+01 6.314463e+01 5.971987e+01
#>  [906] 6.077185e+01 8.770057e+01 6.714952e+01 6.005881e+01 1.168830e+02
#>  [911] 7.949981e+01 6.248046e+01 6.157491e+01 1.245031e+02 5.872641e+01
#>  [916] 6.204910e+01 6.283349e+01 5.368028e+01 5.834842e+01 7.622649e+01
#>  [921] 6.136126e+01 6.593131e+01 3.288273e+02 7.669518e+01 7.542907e+01
#>  [926] 1.722899e+02 7.913029e+01 1.356101e+02 2.560750e+02 7.926863e+01
#>  [931] 6.801858e+01 6.865171e+01 5.589812e+01 6.496574e+01 6.343128e+01
#>  [936] 3.230701e+02 1.088525e+02 9.133145e+01 5.422511e+01 1.965840e+02
#>  [941] 3.073870e+02 5.914305e+01 6.046396e+01 7.085622e+01 8.014106e+01
#>  [946] 8.413389e+01 6.617726e+01 5.564391e+01 5.833289e+01 6.934811e+01
#>  [951] 5.805571e+01 7.011372e+01 7.356257e+01 1.312386e+02 5.849993e+01
#>  [956] 7.235855e+01 5.226319e+01 7.544831e+01 6.173017e+01 6.545942e+01
#>  [961] 7.101977e+01 6.589109e+01 5.714707e+01 6.157944e+01 6.549187e+01
#>  [966] 3.843860e+02 6.919285e+01 7.960211e+01 6.401292e+01 5.920492e+01
#>  [971] 7.759027e+01 7.158586e+01 1.115170e+02 6.887580e+01 5.933652e+01
#>  [976] 6.238794e+01 5.870701e+01 6.180701e+01 6.590867e+01 7.553676e+01
#>  [981] 6.726792e+01 7.301623e+01 5.565269e+01 1.136317e+02 2.452122e+02
#>  [986] 1.414959e+02 6.230862e+01 6.629197e+01 5.948526e+01 6.461284e+01
#>  [991] 1.295371e+02 1.089485e+02 6.507504e+01 6.495349e+01 6.779848e+01
#>  [996] 6.338119e+01 5.305021e+01 6.457751e+01 6.322612e+01 8.393704e+01
```
