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
#>  [1] 78.291138  6.769337  6.769337 37.783106 23.175608 70.031476 43.094601
#>  [8] 78.628691 78.628691 78.628691 78.628691 78.628691 78.628691 33.542136
#> [15] 33.542136 80.386675 80.386675 80.386675 80.271763 81.668512

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
#>    [1] 6.183989e+01 6.210366e+01 6.517073e+01 5.440797e+01 8.269425e+03
#>    [6] 2.494045e+02 8.589080e+01 7.916547e+01 5.796363e+01 7.687223e+01
#>   [11] 7.072790e+01 2.002203e+02 6.380309e+01 5.605783e+01 6.017279e+01
#>   [16] 6.396434e+01 6.072692e+01 7.859221e+01 2.236871e+02 1.379303e+02
#>   [21] 9.297080e+01 6.301074e+01 6.465586e+01 5.886955e+01 6.581561e+01
#>   [26] 7.936526e+01 6.802077e+01 5.551265e+01 3.039497e+02 6.826547e+01
#>   [31] 6.655684e+01 5.832419e+01 9.030693e+01 9.601615e+01 6.760267e+01
#>   [36] 6.037666e+01 1.848671e+02 6.634930e+01 9.290286e+01 7.970360e+01
#>   [41] 5.949549e+01 6.586435e+01 6.116609e+01 5.861882e+01 6.364516e+01
#>   [46] 7.854528e+01 5.514601e+01 5.689007e+01 7.016809e+01 6.079096e+01
#>   [51] 1.072750e+02 1.324265e+02 7.871032e+01 6.139384e+01 5.737785e+01
#>   [56] 7.481595e+01 5.095251e+01 5.051060e+01 5.866222e+01 6.356564e+01
#>   [61] 6.600961e+01 6.506191e+01 8.216150e+01 5.955320e+01 7.318110e+01
#>   [66] 9.994684e+01 7.902394e+01 6.391852e+01 6.199239e+01 7.110372e+01
#>   [71] 5.589548e+01 6.903405e+01 5.916750e+01 6.509926e+01 1.216797e+02
#>   [76] 5.446363e+01 9.047354e+01 8.973477e+01 6.714790e+01 5.900906e+01
#>   [81] 5.654903e+01 6.187706e+01 5.873625e+01 1.316924e+02 6.650933e+01
#>   [86] 6.502290e+01 8.651955e+01 6.061528e+01 7.102942e+01 1.092220e+02
#>   [91] 6.365016e+01 8.702322e+01 6.771412e+01 6.069860e+01 7.964045e+01
#>   [96] 1.047008e+02 5.512616e+01 7.736047e+01 6.426377e+01 6.077197e+01
#>  [101] 5.902573e+01 6.492385e+01 5.543330e+01 7.963102e+01 6.597860e+01
#>  [106] 5.492032e+01 5.795331e+01 1.283419e+02 7.500382e+01 6.883695e+01
#>  [111] 7.923506e+01 6.526799e+01 6.143719e+01 7.599420e+01 7.605172e+02
#>  [116] 1.896435e+02 2.145283e+02 6.370804e+01 7.207239e+01 1.437392e+02
#>  [121] 1.315918e+02 6.265469e+01 5.878871e+01 5.701253e+01 1.805938e+02
#>  [126] 5.840799e+01 5.467508e+01 3.012262e+02 1.101829e+02 5.539788e+01
#>  [131] 1.022090e+02 5.823470e+01 8.861005e+01 2.053944e+02 5.753914e+01
#>  [136] 5.814617e+01 1.056637e+02 8.369225e+01 5.577105e+01 7.046481e+01
#>  [141] 1.417719e+02 2.057557e+02 6.660765e+01 1.047412e+02 5.633193e+01
#>  [146] 5.583903e+01 5.876659e+01 5.964093e+01 8.376528e+01 5.943682e+01
#>  [151] 1.606949e+02 5.503493e+01 6.283216e+01 7.401190e+01 7.549292e+01
#>  [156] 5.712852e+01 7.281848e+01 1.384845e+02 6.174592e+01 6.259364e+01
#>  [161] 6.167902e+01 5.867508e+01 1.104906e+02 1.004437e+02 8.223635e+01
#>  [166] 5.738361e+01 7.298931e+01 5.837507e+01 6.065921e+01 6.910797e+01
#>  [171] 7.357787e+01 6.091499e+01 6.492868e+01 5.924850e+01 6.732507e+01
#>  [176] 5.897318e+01 5.252490e+01 5.538189e+01 7.130887e+01 7.060889e+01
#>  [181] 5.903814e+01 9.644623e+01 6.599429e+01 6.785609e+01 6.050639e+01
#>  [186] 7.952881e+01 5.991512e+01 6.711714e+01 6.216847e+01 8.322392e+01
#>  [191] 9.084446e+01 6.444030e+01 5.038243e+01 5.677903e+01 9.722443e+01
#>  [196] 1.166937e+02 7.039635e+01 1.485528e+02 6.216577e+01 5.437985e+01
#>  [201] 6.239607e+01 5.811156e+01 5.692769e+01 5.809789e+01 6.982466e+01
#>  [206] 1.675394e+02 6.092603e+01 6.333984e+01 8.096135e+01 1.543781e+02
#>  [211] 7.162551e+01 6.402231e+01 6.846554e+01 7.549147e+01 6.204146e+01
#>  [216] 6.334222e+01 7.266948e+01 7.387415e+01 6.201690e+01 6.174928e+01
#>  [221] 5.840947e+01 5.407626e+01 1.312315e+02 9.544336e+01 6.082224e+01
#>  [226] 6.646009e+01 6.890711e+01 1.148391e+03 1.957891e+03 1.489228e+02
#>  [231] 1.045129e+02 5.821339e+01 6.504339e+01 5.536080e+01 2.614622e+02
#>  [236] 6.861142e+01 1.002525e+02 2.964703e+03 7.329407e+01 5.545392e+01
#>  [241] 9.575011e+01 6.064090e+01 5.597035e+01 9.842170e+01 7.139862e+01
#>  [246] 6.379535e+01 5.373567e+01 1.881451e+02 7.209521e+01 7.967114e+01
#>  [251] 5.294724e+01 7.753065e+01 1.609098e+02 6.491254e+01 6.603872e+01
#>  [256] 5.457835e+01 1.173960e+02 5.544555e+01 6.311814e+01 7.147602e+01
#>  [261] 5.439967e+01 5.406597e+01 8.458772e+01 5.640742e+01 5.679661e+01
#>  [266] 5.888138e+01 6.156435e+01 6.172004e+01 6.948318e+01 6.698767e+01
#>  [271] 7.021669e+01 1.610639e+02 2.675012e+02 8.016477e+01 6.135761e+01
#>  [276] 8.210460e+01 6.402888e+01 5.946079e+01 1.678797e+02 5.318861e+01
#>  [281] 6.806879e+01 6.899712e+01 6.393385e+01 6.489637e+01 7.043567e+01
#>  [286] 5.625463e+01 7.097953e+01 5.873815e+01 5.219767e+01 1.718655e+02
#>  [291] 8.051979e+01 6.519235e+01 6.021615e+01 8.793069e+01 9.636262e+01
#>  [296] 7.376194e+01 2.464681e+02 6.678793e+01 5.622510e+01 7.988393e+01
#>  [301] 5.991327e+01 6.846719e+01 5.870033e+01 5.921154e+01 6.575735e+01
#>  [306] 6.717622e+01 5.911554e+01 8.966052e+01 9.602439e+01 8.871781e+01
#>  [311] 9.232131e+01 2.572469e+02 1.125944e+02 5.373421e+01 5.528132e+01
#>  [316] 6.116651e+01 5.939377e+01 7.850405e+01 5.561204e+01 7.790567e+01
#>  [321] 4.907557e+02 5.653883e+01 5.617957e+01 6.276993e+01 6.236734e+01
#>  [326] 5.903187e+02 1.662088e+02 8.712441e+01 1.366300e+02 4.475908e+02
#>  [331] 5.559190e+01 7.701101e+01 5.895545e+01 1.177582e+02 1.331740e+02
#>  [336] 7.560794e+01 7.038089e+01 1.318996e+02 5.066677e+01 5.842583e+01
#>  [341] 5.546586e+01 7.313692e+01 6.952489e+01 6.241384e+01 6.149393e+01
#>  [346] 7.349748e+01 7.270573e+01 2.797448e+02 6.560463e+01 6.201444e+01
#>  [351] 6.565539e+01 5.338732e+01 5.734745e+01 5.815403e+01 7.844258e+01
#>  [356] 5.885797e+01 5.316881e+01 7.994780e+01 6.626542e+01 5.940774e+01
#>  [361] 6.442887e+01 9.828691e+01 6.562364e+01 6.392848e+01 7.642050e+01
#>  [366] 6.801269e+01 6.044293e+01 1.039410e+02 6.865002e+01 6.820601e+01
#>  [371] 5.782344e+01 9.591808e+01 1.041962e+02 7.556404e+01 6.680836e+01
#>  [376] 8.459459e+01 8.019165e+01 4.915205e+01 5.943992e+01 6.772883e+01
#>  [381] 7.179378e+01 6.555701e+01 6.584468e+01 6.051388e+01 6.293902e+01
#>  [386] 6.031442e+01 5.928028e+01 8.533678e+01 5.599682e+01 5.882295e+01
#>  [391] 9.686328e+01 2.714751e+02 3.628462e+02 1.950343e+02 1.828759e+10
#>  [396] 6.225261e+01 1.205989e+04 3.104054e+02 7.131788e+01 6.534627e+01
#>  [401] 5.704765e+01 9.141457e+01 8.554291e+01 5.437209e+01 6.021348e+01
#>  [406] 1.219328e+02 5.445978e+01 5.569090e+01 5.653883e+01 1.327251e+02
#>  [411] 5.333077e+02 6.758212e+01 8.935864e+01 5.946925e+01 6.417765e+01
#>  [416] 5.657711e+01 7.373125e+01 1.061997e+02 8.630174e+01 1.152539e+02
#>  [421] 6.889907e+01 7.972958e+01 9.350404e+01 9.072070e+01 2.271252e+02
#>  [426] 7.005883e+01 5.786669e+01 6.232463e+01 7.318144e+01 5.997348e+01
#>  [431] 6.751493e+01 6.356770e+01 5.973483e+01 1.020263e+02 8.692947e+01
#>  [436] 5.297202e+01 7.260317e+01 2.187949e+02 7.272077e+01 6.740786e+01
#>  [441] 5.980469e+01 5.915027e+01 8.815680e+01 5.737867e+01 8.107389e+01
#>  [446] 5.987435e+01 5.714536e+01 5.631711e+01 1.004943e+02 5.428766e+01
#>  [451] 6.333093e+01 6.167586e+01 5.886931e+01 8.527529e+02 3.408742e+02
#>  [456] 6.290566e+01 6.456891e+01 7.081278e+01 7.836065e+01 6.461464e+01
#>  [461] 5.450689e+01 6.020288e+01 7.738300e+01 6.344238e+01 5.701336e+01
#>  [466] 6.982996e+01 5.124227e+01 8.452878e+01 5.783055e+01 6.166962e+01
#>  [471] 6.349725e+01 6.457286e+01 1.037985e+02 2.091567e+02 1.676439e+02
#>  [476] 7.481352e+01 7.614535e+01 5.226452e+01 6.977048e+01 6.251642e+01
#>  [481] 5.936360e+01 7.459011e+01 5.431604e+01 6.570247e+01 6.443457e+01
#>  [486] 8.631902e+01 1.109849e+02 6.968996e+01 5.979325e+01 7.609162e+01
#>  [491] 9.640859e+01 7.775982e+02 1.226489e+02 3.070477e+02 7.169235e+01
#>  [496] 7.653317e+01 6.078473e+01 6.789009e+01 6.075590e+01 5.665763e+01
#>  [501] 5.510813e+01 6.540847e+01 6.223854e+01 5.953868e+01 9.153787e+01
#>  [506] 8.788037e+01 5.662169e+01 5.465718e+01 5.393726e+01 5.957133e+01
#>  [511] 7.166707e+01 5.510070e+01 9.444369e+01 1.577940e+02 6.500274e+01
#>  [516] 6.789615e+01 1.025987e+02 5.310541e+01 6.056552e+01 6.456107e+01
#>  [521] 6.398523e+01 6.578348e+01 1.383880e+02 5.814524e+01 6.821140e+01
#>  [526] 5.814498e+01 5.649890e+01 1.236642e+02 5.789543e+01 7.275158e+01
#>  [531] 1.238234e+02 6.863528e+01 6.612633e+01 6.107562e+01 6.531250e+01
#>  [536] 5.569397e+01 5.914518e+01 6.257251e+01 2.799071e+02 6.015390e+01
#>  [541] 1.105047e+02 1.012855e+02 6.372952e+01 1.055515e+02 6.365726e+01
#>  [546] 6.567444e+01 5.633481e+01 8.550014e+01 6.058528e+01 5.839720e+01
#>  [551] 5.510660e+01 6.341219e+01 6.216557e+01 7.573489e+01 8.273928e+01
#>  [556] 7.879174e+01 7.275406e+01 1.114969e+02 9.564681e+01 7.594728e+01
#>  [561] 9.502516e+01 5.860590e+01 6.222749e+01 5.416366e+01 6.910080e+01
#>  [566] 7.196101e+01 6.785739e+01 7.439434e+01 6.073430e+01 5.933660e+01
#>  [571] 5.812720e+01 5.475880e+05 1.551562e+04 1.509704e+02 6.882478e+01
#>  [576] 6.138116e+01 6.477366e+01 7.871656e+01 7.403582e+01 6.925137e+01
#>  [581] 6.627913e+01 7.257987e+01 6.815778e+01 7.268319e+01 6.241547e+01
#>  [586] 5.659199e+01 1.965684e+02 1.458344e+02 2.215068e+02 7.672641e+01
#>  [591] 8.757705e+01 5.184985e+01 5.074701e+01 6.352067e+01 4.731319e+01
#>  [596] 5.367802e+01 6.328390e+01 5.965246e+01 7.086096e+01 5.895595e+01
#>  [601] 6.982184e+01 5.683365e+01 1.613557e+02 9.355549e+01 6.502061e+01
#>  [606] 6.273087e+01 5.777074e+01 1.356134e+02 1.947993e+02 2.472872e+02
#>  [611] 5.751909e+01 5.989731e+01 5.790483e+01 5.625732e+01 1.308737e+02
#>  [616] 5.725632e+01 1.168505e+02 5.842992e+01 1.132644e+02 7.643319e+01
#>  [621] 5.758673e+01 1.186676e+02 6.460305e+01 6.367900e+01 7.013087e+01
#>  [626] 2.902316e+03 8.685039e+01 6.562917e+01 5.579144e+01 8.063688e+01
#>  [631] 5.537127e+01 7.317154e+01 1.123327e+02 8.710503e+01 7.835774e+01
#>  [636] 2.311006e+02 6.401819e+01 5.938268e+01 9.362566e+01 5.644736e+01
#>  [641] 6.388110e+01 5.847226e+01 5.974455e+01 6.248888e+01 5.277870e+01
#>  [646] 6.511554e+01 6.501290e+01 7.310035e+01 5.945451e+01 5.778540e+01
#>  [651] 5.861971e+01 6.006614e+01 9.524357e+01 7.521569e+01 6.201888e+01
#>  [656] 6.358682e+01 6.642620e+01 2.372144e+02 8.920024e+01 1.107503e+02
#>  [661] 6.217408e+01 7.169839e+01 6.463866e+01 9.075351e+01 1.373715e+02
#>  [666] 6.908168e+01 6.057791e+01 3.034521e+02 6.487369e+01 1.176757e+02
#>  [671] 3.367841e+02 9.986927e+01 6.455744e+01 7.087096e+01 9.008645e+01
#>  [676] 5.798771e+01 5.840992e+01 6.564778e+01 5.730109e+01 6.004331e+01
#>  [681] 7.012723e+01 7.044072e+01 7.146246e+01 6.277323e+01 5.523882e+01
#>  [686] 5.824615e+01 6.870044e+01 5.563961e+01 6.785135e+01 7.519223e+01
#>  [691] 5.502899e+01 7.504486e+01 4.084043e+01 5.749861e+01 1.057268e+02
#>  [696] 5.679306e+01 6.734394e+01 6.177293e+01 6.948645e+01 5.851338e+01
#>  [701] 6.108436e+01 6.159273e+01 6.873354e+01 1.826250e+02 2.182030e+02
#>  [706] 6.000653e+01 5.516972e+01 6.700379e+01 6.163201e+01 5.367959e+01
#>  [711] 6.135346e+01 5.841330e+01 2.035457e+02 5.755178e+01 6.153870e+01
#>  [716] 6.139470e+01 6.124561e+01 7.335390e+01 6.159528e+01 5.278464e+01
#>  [721] 7.113537e+01 6.486006e+01 5.850624e+01 6.818994e+01 6.465215e+01
#>  [726] 5.503155e+01 5.878432e+01 7.505516e+01 5.598794e+01 8.089364e+01
#>  [731] 5.822389e+01 5.910144e+01 6.490618e+01 6.366015e+01 5.994667e+01
#>  [736] 9.612991e+01 6.888151e+01 5.133476e+01 5.396988e+01 5.963224e+01
#>  [741] 5.551152e+01 9.635925e+01 1.138012e+02 7.678551e+01 5.829341e+01
#>  [746] 6.920364e+01 5.581839e+01 6.587349e+01 9.209027e+01 6.806568e+01
#>  [751] 8.267248e+01 7.351975e+01 6.102106e+01 5.521553e+01 6.117976e+01
#>  [756] 6.854516e+01 7.000607e+01 5.664301e+01 6.491569e+01 1.082711e+02
#>  [761] 6.447019e+01 9.208353e+01 6.712912e+01 6.309838e+01 5.825276e+01
#>  [766] 5.504607e+01 6.434829e+01 6.869563e+01 7.101873e+01 5.785961e+01
#>  [771] 6.358220e+01 8.691115e+01 6.430284e+01 7.348423e+01 4.944324e+01
#>  [776] 2.204056e+02 6.357515e+01 6.134329e+01 6.226037e+01 6.573832e+01
#>  [781] 9.025585e+01 1.096650e+02 5.052275e+01 7.192958e+01 5.968506e+01
#>  [786] 1.180214e+02 6.553681e+01 2.370886e+02 9.584675e+01 7.331662e+01
#>  [791] 5.198948e+01 6.443414e+01 6.905640e+01 6.683674e+01 6.900778e+01
#>  [796] 5.418898e+01 6.484555e+01 1.241674e+02 7.451957e+01 7.861694e+01
#>  [801] 4.035096e+02 1.563758e+02 1.102386e+02 5.928164e+01 8.982230e+01
#>  [806] 5.541810e+02 6.700352e+01 6.584636e+01 7.456100e+01 5.372287e+01
#>  [811] 1.231188e+02 1.169121e+02 5.939525e+01 1.103615e+02 5.729011e+01
#>  [816] 8.950429e+01 6.088589e+01 6.041903e+01 6.142502e+01 6.026299e+01
#>  [821] 7.262242e+01 7.976322e+01 5.530229e+01 1.439534e+02 7.035025e+01
#>  [826] 5.954124e+01 6.391603e+01 5.730484e+01 1.041244e+02 6.029655e+01
#>  [831] 6.174012e+01 4.466788e+02 5.090265e+01 1.111975e+02 6.502777e+01
#>  [836] 6.939618e+01 7.022565e+01 3.409427e+02 6.067692e+01 5.183799e+01
#>  [841] 9.219357e+01 7.267179e+01 5.979083e+01 6.122042e+01 7.999957e+01
#>  [846] 5.834337e+01 5.334011e+01 5.569919e+01 5.750159e+01 6.616494e+01
#>  [851] 1.501412e+04 2.948830e+02 5.907646e+01 8.417777e+01 7.429899e+01
#>  [856] 5.645914e+01 5.187675e+01 8.175495e+01 6.449558e+01 1.505358e+02
#>  [861] 7.478616e+01 7.518479e+01 5.991531e+01 8.335997e+01 5.991548e+01
#>  [866] 7.324971e+01 6.382738e+01 6.152298e+01 5.742547e+01 6.002957e+01
#>  [871] 7.595717e+01 5.733295e+01 6.621205e+01 6.238263e+01 7.747518e+01
#>  [876] 9.007241e+01 6.527645e+01 8.722868e+01 7.904496e+01 1.691192e+02
#>  [881] 7.558566e+01 2.051162e+02 8.482816e+01 8.729235e+01 5.409082e+01
#>  [886] 5.403519e+02 1.262486e+02 6.931675e+01 2.673031e+02 5.899062e+01
#>  [891] 7.171832e+01 1.497864e+02 1.266455e+02 5.963611e+01 1.722790e+02
#>  [896] 5.968858e+01 8.396531e+01 6.654397e+01 5.911140e+01 5.864935e+01
#>  [901] 5.866938e+01 9.071542e+01 5.696937e+01 7.546481e+01 6.066800e+01
#>  [906] 6.759720e+01 1.143554e+02 1.046984e+02 3.083788e+02 2.030988e+02
#>  [911] 7.576892e+01 5.876194e+01 6.226962e+01 1.175870e+02 8.345149e+01
#>  [916] 6.167067e+01 5.974762e+01 5.292684e+01 6.231316e+01 6.970425e+01
#>  [921] 6.122519e+01 5.550529e+01 6.183911e+01 6.830323e+01 6.311761e+01
#>  [926] 6.032138e+01 5.983942e+01 9.102261e+01 8.543575e+01 7.650533e+01
#>  [931] 2.233055e+02 9.518002e+03 4.320082e+03 2.375000e+02 6.340495e+01
#>  [936] 7.294168e+01 3.908664e+01 6.527288e+01 5.728467e+01 5.254570e+02
#>  [941] 6.215501e+01 1.014859e+02 7.022639e+01 5.909861e+01 6.598461e+01
#>  [946] 5.830551e+01 6.985062e+01 6.182588e+01 9.785631e+01 6.851426e+01
#>  [951] 5.525252e+01 7.162367e+01 5.897249e+01 8.078884e+01 6.576539e+01
#>  [956] 1.122075e+02 7.354488e+01 5.751130e+01 7.351358e+01 5.949803e+01
#>  [961] 6.289380e+01 6.078516e+01 5.434088e+01 6.656151e+01 7.234327e+01
#>  [966] 6.962688e+01 1.221871e+02 6.975927e+01 8.145110e+01 5.430650e+01
#>  [971] 5.453392e+01 6.713896e+01 6.141891e+01 6.248645e+01 5.598550e+01
#>  [976] 6.084419e+01 1.408196e+03 2.376372e+07 7.338998e+01 6.238962e+01
#>  [981] 2.208722e+02 5.696473e+01 6.772815e+01 5.570384e+01 7.580304e+01
#>  [986] 9.017342e+01 3.525232e+03 1.141127e+02 9.141452e+01 5.941290e+01
#>  [991] 6.306556e+01 5.742379e+01 6.739947e+01 5.848377e+01 6.564177e+01
#>  [996] 6.494413e+01 5.737672e+01 5.754645e+01 6.058004e+01 8.460310e+01
```
