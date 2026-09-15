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
#>  [1]   79.627196  327.854309  227.851349  227.851349 1855.118456   25.034611
#>  [7]    6.214979    6.214979    6.214979   29.181346    8.032186   75.303727
#> [13]   87.051248   44.999333   44.999333   21.677021   21.677021   21.677021
#> [19]   17.642054  107.747388

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
#>    [1] 5.822089e+01 6.346200e+01 6.742574e+01 7.261499e+01 7.416731e+01
#>    [6] 6.042024e+01 6.035665e+01 1.018513e+02 2.576649e+02 6.300271e+01
#>   [11] 6.433967e+01 7.901221e+01 7.206934e+01 1.150674e+02 5.388608e+01
#>   [16] 5.493925e+01 5.695868e+01 7.770484e+01 6.615848e+01 6.375486e+01
#>   [21] 6.727424e+01 6.997467e+01 1.324439e+02 5.905689e+01 6.883184e+01
#>   [26] 6.359492e+01 9.669015e+01 6.401453e+01 6.499031e+01 5.707191e+01
#>   [31] 6.512194e+01 5.458350e+01 6.777623e+01 6.582015e+02 1.024255e+02
#>   [36] 5.103900e+01 9.477297e+01 6.913643e+01 6.391487e+01 1.284435e+02
#>   [41] 8.432934e+01 8.273015e+01 8.944864e+01 7.740084e+01 7.978294e+01
#>   [46] 8.363586e+01 8.080495e+01 8.865403e+01 5.603448e+01 6.191350e+01
#>   [51] 6.117316e+01 7.961911e+01 9.920707e+01 6.901654e+01 7.191637e+01
#>   [56] 7.305456e+01 8.049844e+01 6.149036e+01 5.769327e+01 6.651986e+01
#>   [61] 6.655239e+01 6.625315e+01 5.950284e+01 1.576829e+02 6.234175e+01
#>   [66] 5.605246e+01 7.324742e+01 6.321649e+01 6.139699e+01 6.323098e+01
#>   [71] 8.161139e+01 5.820340e+01 1.192457e+02 6.694705e+01 8.979762e+01
#>   [76] 5.813917e+01 5.862750e+01 5.814728e+01 5.720388e+01 7.964871e+01
#>   [81] 5.837563e+01 5.916386e+01 3.884186e+02 8.626519e+01 6.416881e+01
#>   [86] 6.444965e+01 7.125560e+01 6.251576e+01 6.024310e+01 7.872200e+01
#>   [91] 6.472554e+01 6.726913e+01 5.322012e+01 5.347888e+01 5.342703e+01
#>   [96] 5.641549e+01 8.941121e+01 1.760198e+02 5.090622e+01 9.515195e+01
#>  [101] 5.455851e+01 6.398341e+01 2.099063e+02 7.427097e+01 5.852968e+01
#>  [106] 6.313371e+01 7.903116e+01 5.930829e+01 7.416243e+01 6.160453e+01
#>  [111] 1.246025e+02 1.382824e+02 9.811140e+01 5.732077e+01 5.892084e+01
#>  [116] 6.888391e+01 5.364678e+01 6.249818e+01 9.821311e+01 5.339875e+01
#>  [121] 5.262290e+01 7.873255e+01 6.692879e+01 6.089155e+01 8.655943e+01
#>  [126] 7.040020e+01 5.960545e+01 6.113691e+01 1.190166e+02 1.767198e+02
#>  [131] 5.826861e+01 7.421379e+01 6.466915e+01 5.667034e+01 5.879467e+01
#>  [136] 5.847968e+01 6.217236e+01 6.143071e+01 8.762091e+01 1.058193e+02
#>  [141] 8.479184e+01 5.529216e+01 9.060500e+01 6.437001e+01 6.223014e+01
#>  [146] 5.858574e+01 1.964616e+02 1.214141e+02 1.131274e+02 7.962878e+01
#>  [151] 7.550340e+01 6.066322e+01 6.428223e+01 1.287487e+02 7.391291e+01
#>  [156] 6.790979e+01 7.623666e+01 7.084167e+01 5.564544e+01 6.166657e+01
#>  [161] 5.794790e+01 7.997190e+01 5.401805e+01 4.570133e+01 6.900471e+01
#>  [166] 6.487304e+01 1.788214e+02 6.115488e+01 2.257180e+02 5.682328e+01
#>  [171] 7.649045e+01 1.390466e+02 5.904680e+01 7.953732e+01 8.155719e+01
#>  [176] 8.802869e+01 9.718334e+01 1.000556e+02 6.530157e+01 6.633595e+01
#>  [181] 5.662579e+01 5.077106e+01 7.250355e+01 7.684245e+01 6.903344e+01
#>  [186] 8.710152e+01 5.718557e+01 6.954743e+01 8.643748e+01 4.920026e+01
#>  [191] 5.603994e+01 5.360181e+01 6.176942e+01 5.708974e+01 6.632041e+01
#>  [196] 7.185403e+01 5.171592e+01 6.878366e+01 9.315799e+01 1.123792e+02
#>  [201] 5.418279e+01 8.022463e+01 1.102684e+02 2.464681e+02 9.405675e+01
#>  [206] 4.547779e+01 6.258399e+01 6.090098e+01 5.727675e+01 6.681806e+01
#>  [211] 5.854410e+01 6.080255e+01 6.293148e+01 5.938729e+01 7.065292e+01
#>  [216] 6.415986e+01 7.223009e+01 9.126009e+01 5.624538e+01 4.197261e+02
#>  [221] 4.990188e+01 8.891117e+01 8.085122e+01 6.229446e+01 8.603524e+01
#>  [226] 6.373855e+01 5.604847e+01 6.736195e+01 5.630016e+01 1.345907e+02
#>  [231] 5.912201e+01 5.732817e+01 1.634627e+02 9.507341e+01 1.030472e+02
#>  [236] 6.019113e+01 1.037288e+02 6.167831e+01 6.819666e+01 7.368524e+01
#>  [241] 5.896296e+01 5.664049e+01 1.215661e+02 4.093944e+04 2.258241e+02
#>  [246] 6.168837e+01 1.111514e+03 1.552399e+02 6.448048e+02 1.435416e+02
#>  [251] 7.751321e+01 2.623104e+02 1.187997e+02 6.563502e+01 6.668497e+01
#>  [256] 6.224280e+01 5.866716e+01 6.250132e+01 1.015698e+02 5.749566e+01
#>  [261] 6.420754e+01 7.045913e+01 1.439681e+02 5.768942e+01 7.475451e+01
#>  [266] 9.398026e+01 5.820209e+01 8.913574e+01 6.800190e+01 6.118959e+01
#>  [271] 6.019711e+01 5.879468e+01 5.717228e+01 6.635058e+01 6.026168e+01
#>  [276] 7.493290e+01 5.828044e+01 6.650082e+01 6.526535e+01 9.759953e+01
#>  [281] 5.781021e+01 5.659597e+01 5.949351e+01 6.024823e+01 7.188876e+01
#>  [286] 6.068755e+01 9.246508e+01 6.791549e+01 7.574755e+01 5.798957e+01
#>  [291] 5.677361e+01 6.783076e+01 7.496515e+01 6.504108e+01 6.024311e+01
#>  [296] 6.232406e+01 5.807474e+01 8.349800e+02 8.017858e+01 1.017855e+02
#>  [301] 6.356317e+01 1.122974e+02 8.019786e+01 7.139166e+01 7.087285e+01
#>  [306] 7.126989e+01 7.151651e+01 5.488298e+01 8.399055e+01 1.167602e+02
#>  [311] 6.438050e+01 7.565695e+01 6.594985e+01 6.725155e+01 5.815629e+01
#>  [316] 1.190380e+02 7.850784e+01 6.080172e+01 1.179097e+02 6.656307e+01
#>  [321] 6.073249e+01 8.431436e+01 5.919315e+01 6.187517e+01 5.423012e+01
#>  [326] 1.811038e+02 2.199925e+02 8.349384e+01 1.152692e+02 6.358792e+01
#>  [331] 6.547557e+01 6.056348e+01 6.530423e+01 5.412929e+01 1.635837e+02
#>  [336] 6.382824e+01 5.797420e+01 5.733061e+01 5.402277e+01 6.500633e+01
#>  [341] 6.149444e+01 6.057503e+01 6.245828e+01 7.937641e+01 5.872624e+01
#>  [346] 5.096879e+01 5.695868e+01 7.674203e+01 6.145306e+01 6.113145e+01
#>  [351] 6.183943e+01 5.472230e+01 5.309617e+01 7.512534e+01 9.699206e+01
#>  [356] 5.611336e+01 6.578739e+01 5.916751e+01 6.647333e+01 6.040094e+01
#>  [361] 6.536424e+01 5.991596e+01 7.678279e+01 5.437494e+01 2.662140e+02
#>  [366] 5.373132e+01 1.049912e+02 6.417685e+01 6.699789e+01 6.345316e+01
#>  [371] 7.390069e+01 6.048637e+01 7.553217e+01 7.188344e+01 5.845780e+01
#>  [376] 1.632284e+02 5.688645e+01 5.814328e+01 7.808750e+01 6.688495e+01
#>  [381] 6.523453e+01 6.517841e+01 1.774378e+02 2.826391e+02 6.884038e+01
#>  [386] 6.619036e+01 8.993378e+01 6.404571e+01 6.221687e+01 6.339211e+01
#>  [391] 2.278328e+03 9.745071e+01 6.432528e+01 7.249237e+01 6.284837e+01
#>  [396] 6.609034e+01 6.355042e+01 7.223455e+01 1.191537e+02 1.585114e+02
#>  [401] 5.772045e+01 7.975201e+01 6.449874e+01 1.732577e+02 6.058360e+01
#>  [406] 8.947019e+01 1.087461e+03 6.004207e+01 6.756909e+01 7.807320e+01
#>  [411] 5.650131e+01 4.649551e+02 1.040089e+02 6.192751e+01 8.585929e+01
#>  [416] 5.932897e+01 6.664388e+01 9.884850e+01 7.343189e+01 6.256253e+01
#>  [421] 6.734376e+01 5.260675e+01 5.515299e+01 6.235666e+01 7.519923e+01
#>  [426] 5.642698e+01 4.862777e+01 5.693307e+01 6.622906e+01 6.818235e+01
#>  [431] 6.653297e+01 5.923399e+01 5.749099e+01 7.846566e+01 6.127722e+01
#>  [436] 6.755851e+01 5.744077e+01 6.414163e+01 5.576515e+01 5.843473e+01
#>  [441] 5.851889e+01 7.063017e+01 6.550453e+01 6.812344e+01 1.017397e+02
#>  [446] 6.784588e+01 5.966241e+01 6.087665e+01 6.622419e+01 9.104607e+01
#>  [451] 7.123700e+01 6.504909e+01 5.714848e+01 5.975173e+01 7.367003e+01
#>  [456] 7.201075e+01 7.087273e+01 6.129439e+01 7.331485e+01 6.568503e+01
#>  [461] 5.986252e+01 7.553175e+01 7.560632e+01 8.475580e+01 8.739332e+01
#>  [466] 6.135637e+01 6.059241e+01 6.134362e+01 6.345513e+01 9.102745e+01
#>  [471] 6.521734e+01 6.148670e+01 7.329137e+01 6.594451e+01 5.887594e+01
#>  [476] 5.149350e+01 7.707003e+01 6.626820e+01 6.904436e+01 5.950166e+01
#>  [481] 2.841910e+02 6.122974e+01 6.484058e+01 5.958640e+01 5.547830e+01
#>  [486] 6.846735e+01 5.868814e+01 1.575594e+02 5.807548e+01 6.620826e+01
#>  [491] 6.554527e+01 5.508028e+01 5.476259e+01 6.179414e+01 5.761859e+01
#>  [496] 6.751527e+01 8.366703e+01 6.639804e+01 5.555979e+01 5.702351e+01
#>  [501] 7.111399e+01 5.751267e+01 7.062916e+01 6.597342e+01 6.193812e+01
#>  [506] 7.320390e+01 5.711976e+01 6.640959e+01 7.839587e+01 5.633284e+01
#>  [511] 7.291475e+01 1.742362e+02 5.673358e+01 5.793169e+01 6.052072e+01
#>  [516] 6.070014e+01 5.667673e+01 2.506015e+02 7.776884e+01 7.072929e+01
#>  [521] 7.033674e+01 9.143143e+01 5.730015e+01 6.245412e+01 6.217143e+01
#>  [526] 2.059953e+02 5.915025e+01 6.197355e+01 9.260620e+01 7.462324e+01
#>  [531] 6.075597e+01 9.202827e+01 6.102716e+01 5.969801e+01 6.514366e+01
#>  [536] 7.504626e+01 6.287660e+01 5.946516e+01 7.361269e+01 6.789604e+01
#>  [541] 7.959039e+01 6.679315e+01 1.512562e+02 6.460900e+01 6.845397e+01
#>  [546] 5.751560e+01 6.271749e+01 6.572784e+01 5.914038e+01 6.142302e+01
#>  [551] 5.613725e+01 6.205553e+01 6.705982e+01 7.263611e+01 6.842919e+01
#>  [556] 6.084841e+01 5.441575e+01 7.862527e+01 9.294510e+01 3.318480e+02
#>  [561] 6.077107e+01 1.634041e+02 6.355771e+01 7.053634e+01 5.934755e+01
#>  [566] 6.994423e+01 5.980889e+01 7.455042e+01 6.474734e+01 6.106080e+01
#>  [571] 1.064775e+07 4.777077e+04 7.028791e+01 1.298958e+02 8.738810e+01
#>  [576] 6.068432e+01 6.489225e+01 7.445555e+01 6.694759e+01 6.699010e+01
#>  [581] 6.213131e+01 6.566471e+01 7.327335e+01 6.986129e+01 5.854457e+01
#>  [586] 6.057892e+01 5.888987e+01 7.524900e+01 5.903737e+01 6.825310e+01
#>  [591] 5.600255e+01 1.060231e+02 1.250742e+02 4.817686e+02 5.660324e+01
#>  [596] 5.675852e+01 6.621768e+01 5.561692e+01 4.187682e+01 5.966873e+01
#>  [601] 8.050130e+01 6.733288e+01 6.153371e+01 7.674373e+01 1.040325e+02
#>  [606] 2.973174e+02 6.157145e+01 1.658983e+02 7.145459e+01 1.365081e+02
#>  [611] 7.172701e+01 7.791010e+01 7.284684e+01 6.455873e+01 2.041685e+02
#>  [616] 5.925671e+01 5.637432e+01 6.534394e+01 1.038438e+03 5.571727e+01
#>  [621] 4.827855e+01 6.541353e+01 6.012433e+01 1.024479e+02 6.939683e+01
#>  [626] 8.893739e+01 5.674295e+01 6.036025e+01 1.112810e+02 7.386548e+01
#>  [631] 7.026548e+01 5.863241e+01 1.798396e+03 1.671204e+02 7.632045e+01
#>  [636] 6.162337e+01 6.804991e+01 1.066341e+02 6.481052e+01 6.371777e+01
#>  [641] 6.204424e+01 7.697656e+01 7.703774e+01 1.715569e+02 1.137338e+02
#>  [646] 7.419059e+01 5.419896e+01 7.423360e+01 8.215890e+01 7.186967e+01
#>  [651] 1.159811e+02 6.682804e+01 7.829821e+01 7.467905e+01 5.651520e+01
#>  [656] 7.101559e+01 7.448935e+01 6.990301e+01 5.689433e+01 9.926636e+01
#>  [661] 6.816788e+01 5.679080e+01 5.928817e+01 5.626758e+01 6.102197e+01
#>  [666] 7.216351e+01 6.974623e+01 8.944467e+01 9.347269e+01 7.399272e+01
#>  [671] 1.388473e+02 9.313900e+01 1.068343e+02 6.953800e+01 6.336507e+01
#>  [676] 5.803918e+01 5.955278e+01 8.448794e+01 7.414154e+01 5.602739e+01
#>  [681] 7.472887e+01 1.026470e+02 6.376717e+01 8.149681e+01 6.704525e+01
#>  [686] 5.500425e+01 6.457076e+01 6.216202e+01 1.502939e+02 4.851564e+01
#>  [691] 6.708778e+01 5.855919e+01 5.913037e+01 6.274376e+01 7.029790e+01
#>  [696] 6.301718e+01 5.688428e+01 6.676322e+01 5.867345e+01 5.677353e+01
#>  [701] 5.897009e+01 6.598514e+01 6.836873e+01 6.478678e+01 6.091723e+01
#>  [706] 6.247252e+01 6.002746e+01 6.468989e+01 8.723417e+01 7.167182e+01
#>  [711] 5.881428e+01 5.641520e+01 8.381420e+01 7.089731e+01 6.485840e+01
#>  [716] 6.226157e+01 5.537663e+01 2.176904e+02 5.526318e+01 6.321494e+01
#>  [721] 6.115238e+01 6.293641e+01 7.385895e+01 7.699855e+01 7.835487e+01
#>  [726] 5.961449e+01 5.831750e+01 7.111333e+01 5.226518e+01 6.831957e+01
#>  [731] 7.975808e+01 5.834102e+01 7.542635e+01 6.047410e+01 6.407679e+01
#>  [736] 6.225715e+01 6.319067e+01 6.099033e+01 6.299989e+01 5.191931e+01
#>  [741] 7.837245e+01 1.848151e+02 1.556950e+02 8.411027e+01 5.918246e+01
#>  [746] 5.597097e+01 6.598548e+01 5.634494e+01 5.689279e+01 1.125413e+02
#>  [751] 1.325226e+03 1.196793e+02 8.106406e+01 6.163756e+01 6.895517e+01
#>  [756] 1.115171e+02 6.613624e+01 7.318067e+01 7.809106e+01 8.436507e+01
#>  [761] 2.963242e+02 6.036210e+01 8.937039e+01 7.750273e+01 7.726235e+01
#>  [766] 5.522426e+01 4.820023e+01 8.855087e+01 6.352698e+01 6.610582e+01
#>  [771] 1.154269e+02 1.578756e+02 6.257748e+02 5.855779e+01 5.670274e+01
#>  [776] 6.064394e+01 5.007406e+01 5.741190e+01 6.149393e+03 6.958955e+06
#>  [781] 5.282229e+01 1.153972e+02 8.543756e+01 1.764623e+02 7.410776e+01
#>  [786] 5.536229e+01 6.676735e+01 6.299926e+01 8.631491e+01 1.280798e+02
#>  [791] 5.969974e+01 9.657243e+01 6.670948e+01 9.361050e+01 5.775621e+01
#>  [796] 8.787305e+01 7.076073e+01 6.182609e+01 6.183513e+01 7.109614e+01
#>  [801] 7.177690e+01 5.973958e+01 6.757971e+01 6.131436e+01 5.194681e+01
#>  [806] 8.255155e+01 6.809654e+01 6.470706e+01 6.027853e+01 5.646342e+01
#>  [811] 6.186791e+01 1.084896e+02 5.740406e+01 6.843783e+01 5.766271e+01
#>  [816] 8.566762e+01 1.153980e+03 6.720880e+01 5.244528e+01 8.459668e+01
#>  [821] 8.228878e+01 6.879681e+01 6.273382e+01 7.222497e+01 2.253317e+02
#>  [826] 7.729738e+01 5.680172e+01 6.375103e+01 1.002900e+02 5.745989e+01
#>  [831] 5.668329e+01 1.159197e+02 8.317445e+01 6.534447e+01 6.292788e+01
#>  [836] 6.383252e+01 2.498845e+02 1.041796e+02 5.980769e+01 1.444539e+02
#>  [841] 8.388594e+01 6.998385e+01 7.165345e+01 5.539897e+02 8.869498e+01
#>  [846] 6.067636e+01 8.174693e+01 2.596429e+02 5.844889e+01 1.699691e+02
#>  [851] 6.253188e+01 7.595570e+01 7.785563e+01 5.479758e+01 8.870872e+01
#>  [856] 1.085374e+02 6.978246e+01 7.118816e+01 2.020742e+02 7.189649e+01
#>  [861] 6.205344e+01 6.342747e+01 1.595211e+02 9.419070e+01 5.623417e+01
#>  [866] 6.467884e+01 6.046328e+01 5.427985e+01 6.975896e+01 9.789613e+01
#>  [871] 7.444406e+01 6.672870e+01 6.828798e+01 6.341669e+01 6.147861e+01
#>  [876] 5.981273e+01 6.672668e+01 4.363195e+02 1.232791e+02 7.852541e+01
#>  [881] 5.498527e+01 5.877506e+01 6.151688e+01 6.427232e+01 6.048619e+01
#>  [886] 7.974503e+01 6.042587e+01 5.373110e+01 5.686795e+01 5.437962e+01
#>  [891] 6.121710e+01 5.811708e+01 7.303360e+01 6.074182e+01 5.910933e+01
#>  [896] 5.534777e+01 6.204285e+01 6.244593e+01 6.982193e+01 5.813799e+01
#>  [901] 2.459752e+03 1.122320e+02 7.737844e+01 6.218362e+01 5.782059e+01
#>  [906] 7.585959e+01 6.680515e+01 6.739112e+01 6.366166e+01 6.328134e+01
#>  [911] 6.353496e+01 5.607715e+01 6.933681e+01 6.804051e+01 6.194047e+01
#>  [916] 5.834480e+01 6.020688e+01 8.744201e+01 5.429584e+01 5.501737e+01
#>  [921] 5.555576e+01 8.533282e+01 6.733880e+01 6.290034e+01 3.458847e+03
#>  [926] 4.795977e+03 5.859891e+01 6.053831e+01 7.391374e+01 6.729172e+01
#>  [931] 6.954929e+01 6.635553e+01 6.154203e+01 8.469951e+01 9.381075e+01
#>  [936] 1.455615e+02 3.799814e+02 5.375720e+01 6.419839e+01 7.005336e+01
#>  [941] 5.732458e+01 8.621015e+01 7.271405e+01 5.824334e+01 6.569273e+01
#>  [946] 5.937246e+01 5.914845e+01 6.335754e+01 9.655933e+01 6.199106e+01
#>  [951] 6.133446e+01 5.670076e+01 7.581405e+01 7.352352e+01 7.170909e+01
#>  [956] 6.760650e+01 6.350636e+01 5.870380e+01 6.294491e+01 9.731959e+01
#>  [961] 7.877342e+01 5.657775e+01 6.145765e+01 6.667864e+01 6.406688e+01
#>  [966] 3.294505e+01 7.677068e+01 5.555412e+01 6.962990e+01 7.360280e+01
#>  [971] 6.396714e+01 7.148838e+01 7.614968e+01 6.179327e+01 7.787425e+01
#>  [976] 7.890081e+01 7.053906e+01 6.385268e+01 5.677650e+01 5.675170e+01
#>  [981] 5.704043e+01 6.675702e+01 5.537267e+01 6.103437e+01 6.280041e+01
#>  [986] 5.641174e+01 6.887047e+01 6.762498e+01 3.455349e+02 6.350759e+01
#>  [991] 1.115284e+02 5.730046e+01 5.595905e+01 6.045443e+01 5.861819e+01
#>  [996] 5.965578e+01 5.794728e+01 5.995399e+01 5.923255e+01 5.640068e+01
```
