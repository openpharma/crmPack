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
#>  [1]  28.15854  28.15854  28.15854  28.15854  28.15854  28.15854  55.73025
#>  [8]  55.73025  10.02135 173.84444  29.74972  29.74972  62.69909 155.91245
#> [15] 155.91245  65.90743  65.90743  68.85340  68.85340  68.85340

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
#>    [1] 6.864818e+01 7.039608e+01 6.113175e+01 5.817780e+01 6.845481e+01
#>    [6] 6.189636e+01 5.621242e+01 6.920788e+01 6.323139e+01 5.822300e+01
#>   [11] 5.689716e+01 8.150805e+01 6.257986e+01 7.262590e+01 6.916568e+01
#>   [16] 6.369195e+01 7.764651e+01 1.407806e+03 1.397487e+02 5.001854e+01
#>   [21] 6.705419e+01 7.837961e+01 6.829898e+01 6.214018e+01 6.079600e+01
#>   [26] 7.120614e+01 7.884626e+01 1.118691e+02 7.928429e+01 6.295475e+01
#>   [31] 6.536073e+01 6.664014e+01 6.663510e+01 6.203586e+01 8.449971e+01
#>   [36] 7.481493e+01 1.341687e+02 5.813191e+01 8.701727e+01 5.326871e+01
#>   [41] 5.863826e+01 6.284497e+01 6.117696e+01 5.536154e+01 6.247979e+01
#>   [46] 5.699326e+01 5.762635e+01 6.169083e+01 2.041083e+02 8.766436e+01
#>   [51] 4.276582e+01 5.850327e+01 5.944103e+01 3.619021e+01 6.588210e+01
#>   [56] 5.731137e+01 4.690079e+02 7.160068e+01 8.379023e+01 1.478278e+02
#>   [61] 6.511868e+01 9.486703e+01 6.046194e+01 6.152367e+01 7.095067e+01
#>   [66] 1.283557e+02 5.659238e+01 7.424987e+01 1.006172e+02 9.931694e+01
#>   [71] 1.023763e+02 8.000738e+01 5.760799e+01 6.694434e+01 7.906873e+01
#>   [76] 1.550654e+02 6.053199e+01 7.711288e+01 1.228098e+02 7.247952e+02
#>   [81] 5.552866e+01 6.392253e+01 6.620929e+01 7.079807e+01 1.071012e+02
#>   [86] 6.183049e+01 1.413824e+02 7.475746e+01 6.269630e+01 5.854545e+01
#>   [91] 6.321308e+01 7.694205e+01 7.115093e+01 7.030294e+01 8.062451e+01
#>   [96] 6.456232e+01 6.612880e+01 6.623625e+01 1.303962e+02 1.475852e+02
#>  [101] 1.115795e+02 7.284547e+01 7.957620e+01 7.640027e+01 1.031971e+02
#>  [106] 6.183620e+01 6.129186e+01 6.599253e+01 1.071456e+02 1.661639e+02
#>  [111] 5.907673e+01 1.074173e+02 5.793404e+01 5.865916e+01 1.171983e+02
#>  [116] 6.696131e+01 7.470122e+01 6.718110e+01 8.476413e+01 7.570804e+01
#>  [121] 6.124247e+01 5.481313e+01 6.232339e+01 6.466763e+01 6.281017e+01
#>  [126] 6.061241e+01 7.072338e+01 6.416203e+01 6.014680e+01 7.151621e+01
#>  [131] 6.469679e+01 8.428927e+01 7.883909e+01 5.843399e+01 3.519668e+02
#>  [136] 7.386416e+01 5.414047e+01 5.295819e+01 7.731108e+01 1.978165e+02
#>  [141] 6.191904e+01 3.482406e+01 9.694856e+01 7.456134e+01 5.237323e+01
#>  [146] 6.385461e+01 6.957600e+01 6.243707e+01 7.167197e+01 5.562092e+01
#>  [151] 1.785101e+02 7.461581e+01 5.769085e+01 1.522328e+02 7.477338e+01
#>  [156] 8.931972e+01 1.258572e+02 5.906672e+01 5.888470e+01 5.808340e+01
#>  [161] 7.240066e+01 1.251264e+02 6.764968e+01 6.890934e+01 6.583364e+01
#>  [166] 5.736109e+01 7.801512e+01 7.034497e+01 6.527393e+01 5.669545e+01
#>  [171] 5.763574e+01 6.001731e+01 1.594163e+02 6.702898e+01 7.226707e+01
#>  [176] 6.508727e+01 5.972945e+01 1.620904e+02 5.878090e+01 1.929906e+02
#>  [181] 7.505623e+01 5.723411e+01 5.948631e+01 5.922045e+01 2.414705e+02
#>  [186] 5.212607e+02 1.295792e+02 6.145939e+01 6.622322e+01 3.048204e+02
#>  [191] 3.362485e+02 7.711485e+01 1.193507e+02 7.462700e+01 6.145530e+01
#>  [196] 5.777330e+01 1.062259e+02 6.729489e+01 5.996846e+01 7.299190e+01
#>  [201] 6.460057e+01 1.824067e+02 6.547438e+01 5.906659e+01 5.831217e+01
#>  [206] 7.358856e+01 6.131683e+01 6.917955e+01 2.218337e+02 5.801439e+01
#>  [211] 7.566392e+01 7.808757e+01 7.426027e+02 5.691339e+01 1.119467e+02
#>  [216] 6.067937e+01 7.779934e+01 8.099825e+01 5.688435e+01 6.220894e+01
#>  [221] 6.893033e+01 5.888760e+01 6.069806e+01 6.105139e+01 3.507188e+02
#>  [226] 5.848050e+01 8.765159e+01 8.030711e+01 6.571983e+01 8.054604e+01
#>  [231] 7.349596e+01 6.775613e+01 8.664834e+01 6.461406e+01 6.903894e+01
#>  [236] 1.199641e+02 8.947516e+01 5.702443e+01 5.781373e+01 5.165428e+01
#>  [241] 6.426094e+01 1.798449e+02 1.110444e+02 8.301901e+01 7.284671e+01
#>  [246] 6.627571e+01 6.703771e+01 3.154315e+02 5.455277e+01 1.403086e+04
#>  [251] 5.497793e+01 5.617178e+01 5.766514e+01 7.075494e+01 6.376294e+01
#>  [256] 7.208337e+01 1.139476e+02 1.354870e+02 1.892626e+02 8.504592e+01
#>  [261] 1.554299e+02 5.421811e+01 5.706292e+01 7.059399e+01 9.023509e+01
#>  [266] 7.738337e+01 7.265320e+01 6.003171e+01 6.165089e+01 5.506148e+01
#>  [271] 6.279467e+01 8.198477e+01 6.101499e+01 5.748717e+01 5.795753e+01
#>  [276] 6.161506e+01 6.130526e+01 8.342574e+01 6.080739e+01 6.150481e+01
#>  [281] 5.911619e+01 5.976183e+01 6.704421e+01 6.087130e+01 1.109688e+02
#>  [286] 6.912983e+01 1.133745e+02 6.949711e+01 6.456615e+01 8.471318e+01
#>  [291] 7.552364e+01 7.545058e+01 5.517902e+01 5.571951e+01 5.916473e+01
#>  [296] 6.766464e+01 2.070588e+02 5.606705e+01 6.143489e+01 5.700932e+01
#>  [301] 6.424417e+01 6.092352e+01 5.878597e+01 5.626627e+01 6.588861e+01
#>  [306] 6.248726e+01 7.083552e+01 6.547247e+01 9.578277e+01 8.096642e+01
#>  [311] 6.744955e+01 6.304001e+01 5.708627e+01 6.344315e+01 7.008440e+01
#>  [316] 6.263315e+01 1.017974e+02 6.054020e+01 5.705155e+01 1.133521e+02
#>  [321] 9.164079e+01 7.990275e+01 5.924225e+01 5.695191e+01 8.500471e+01
#>  [326] 5.994763e+01 6.323663e+01 7.866614e+01 7.213401e+01 6.730470e+01
#>  [331] 6.482952e+01 7.278106e+01 6.268723e+01 5.771965e+01 5.759232e+01
#>  [336] 6.065077e+01 7.116982e+01 9.544513e+01 5.472295e+01 6.965915e+01
#>  [341] 1.547001e+02 1.231143e+02 1.206905e+02 5.932316e+01 5.958456e+01
#>  [346] 5.533690e+01 9.204758e+01 6.092791e+01 7.997694e+01 5.921086e+01
#>  [351] 1.049543e+02 8.998488e+01 7.724947e+01 5.969469e+01 6.165643e+01
#>  [356] 6.167745e+01 5.512172e+01 5.256706e+01 6.482808e+01 7.767436e+01
#>  [361] 7.659459e+01 6.510579e+01 1.037435e+02 1.013862e+02 6.678396e+01
#>  [366] 1.481941e+02 7.279921e+01 5.917718e+01 7.830693e+01 8.270870e+01
#>  [371] 6.540462e+01 6.143403e+01 4.624464e+02 8.632248e+01 6.749658e+01
#>  [376] 9.234610e+01 1.254453e+02 5.031447e+02 1.074638e+03 6.677352e+01
#>  [381] 1.619910e+02 6.417046e+01 6.138845e+01 5.760967e+01 5.598189e+01
#>  [386] 5.752651e+01 8.730596e+01 5.865872e+01 5.589197e+01 1.825929e+02
#>  [391] 1.709747e+02 8.540034e+01 6.745455e+01 5.689814e+01 4.732831e+02
#>  [396] 5.356535e+01 6.348434e+01 5.496496e+01 5.814758e+01 9.186803e+01
#>  [401] 6.162915e+01 6.219966e+01 6.053540e+01 6.957844e+01 1.416246e+02
#>  [406] 6.632701e+01 5.689335e+01 5.932516e+01 6.491854e+01 6.293548e+01
#>  [411] 5.744469e+01 6.389377e+01 1.057045e+02 8.137386e+01 6.390672e+01
#>  [416] 5.708961e+01 6.837425e+01 5.114536e+01 8.787001e+01 6.570753e+01
#>  [421] 1.880679e+02 6.972957e+01 9.284796e+01 2.471282e+02 1.052089e+02
#>  [426] 6.851205e+01 7.585998e+01 1.038654e+02 8.007704e+01 6.129885e+01
#>  [431] 7.675677e+01 8.161587e+01 5.364110e+01 5.317331e+01 5.893913e+01
#>  [436] 5.786874e+01 5.564057e+01 6.334159e+01 6.546514e+01 8.627464e+01
#>  [441] 6.581049e+01 7.269135e+01 6.171859e+01 6.482057e+01 8.691834e+01
#>  [446] 5.638133e+01 2.211792e+02 5.155421e+01 5.609494e+01 5.560781e+01
#>  [451] 1.134294e+02 1.470110e+02 5.558813e+01 6.102008e+01 1.233706e+02
#>  [456] 5.440426e+01 5.996883e+01 6.078229e+01 7.003257e+01 4.528367e+01
#>  [461] 5.953544e+01 5.198449e+01 6.148650e+01 8.809260e+01 6.161446e+01
#>  [466] 6.974894e+01 8.014899e+01 6.554774e+01 5.764316e+01 5.696101e+01
#>  [471] 7.976025e+01 6.102605e+01 6.526267e+01 8.999865e+01 5.763126e+01
#>  [476] 1.351832e+02 5.505351e+01 6.631920e+01 7.077648e+01 6.350160e+01
#>  [481] 6.016272e+01 6.167190e+01 5.758069e+01 7.177289e+01 6.632225e+01
#>  [486] 5.839519e+01 6.638420e+01 1.356571e+02 6.241127e+01 6.588674e+01
#>  [491] 6.068300e+01 5.537090e+01 5.747281e+01 5.921009e+01 6.400779e+01
#>  [496] 5.872759e+01 9.055544e+01 7.042351e+01 6.220707e+01 5.998963e+01
#>  [501] 5.551914e+01 6.170219e+01 6.608473e+01 6.283267e+01 6.258209e+01
#>  [506] 5.698086e+01 6.567816e+01 6.328279e+01 6.424249e+01 6.488507e+01
#>  [511] 5.679505e+02 6.092859e+01 7.554704e+01 7.606017e+01 9.186827e+01
#>  [516] 7.183532e+01 5.738089e+01 8.599983e+01 7.822999e+01 6.502161e+01
#>  [521] 2.505064e+02 7.459168e+01 6.711942e+01 7.605888e+01 5.955248e+01
#>  [526] 2.256362e+02 1.072232e+02 2.090665e+02 6.097031e+01 6.130411e+01
#>  [531] 5.962689e+01 7.449944e+01 6.021484e+01 4.813366e+01 7.056293e+01
#>  [536] 7.888279e+01 1.287868e+02 6.833953e+01 5.896850e+01 9.958620e+01
#>  [541] 7.551013e+01 6.609760e+01 6.448822e+01 5.748532e+01 5.978723e+01
#>  [546] 5.990039e+01 6.560850e+01 5.784892e+01 6.274939e+01 6.771302e+01
#>  [551] 6.371433e+01 5.883043e+01 5.701523e+01 5.954534e+01 5.555857e+01
#>  [556] 5.809592e+01 6.144741e+01 6.304223e+01 8.677574e+01 8.357116e+01
#>  [561] 5.461309e+01 7.412748e+01 6.407754e+01 6.136388e+01 1.077249e+02
#>  [566] 5.798178e+01 7.705766e+01 8.694440e+01 7.278392e+01 6.443480e+01
#>  [571] 5.891678e+01 5.869478e+01 6.104740e+01 4.939891e+01 8.629989e+01
#>  [576] 6.568549e+01 6.804236e+01 5.721827e+01 2.618283e+02 5.443353e+01
#>  [581] 6.474473e+01 5.843776e+01 1.093319e+02 1.358683e+02 8.119530e+01
#>  [586] 5.350152e+01 8.825553e+01 5.291185e+01 5.479540e+01 6.211908e+01
#>  [591] 6.376570e+01 6.002687e+01 5.892502e+01 6.742375e+01 2.974629e+02
#>  [596] 6.428269e+01 6.016909e+01 7.285126e+01 8.255452e+01 7.667196e+01
#>  [601] 8.089864e+01 8.230357e+01 6.688663e+01 6.079533e+01 6.671651e+01
#>  [606] 5.951642e+01 1.213573e+02 5.764583e+01 8.643823e+01 5.839991e+01
#>  [611] 1.845015e+02 4.022115e+02 8.356838e+01 5.921084e+01 5.990011e+01
#>  [616] 5.938270e+01 6.377657e+01 5.666091e+01 8.535942e+01 9.973419e+01
#>  [621] 6.597776e+01 6.608828e+01 1.341180e+02 5.817465e+01 6.513981e+01
#>  [626] 5.851825e+01 6.940238e+01 6.527505e+01 5.943410e+01 9.133048e+01
#>  [631] 7.262262e+01 7.387009e+01 2.773226e+02 7.313634e+01 6.795667e+01
#>  [636] 5.481374e+01 9.025172e+01 8.457421e+01 5.991044e+01 7.105934e+01
#>  [641] 6.895282e+01 6.980357e+01 7.699282e+01 7.124083e+01 5.500402e+01
#>  [646] 8.038743e+01 7.381556e+01 6.523869e+01 1.076903e+02 9.065205e+01
#>  [651] 6.693020e+01 6.063838e+01 6.717017e+01 6.460265e+01 6.230497e+01
#>  [656] 9.458289e+01 6.047443e+01 5.836593e+01 6.482909e+01 5.811693e+01
#>  [661] 5.910949e+01 6.817229e+01 1.192039e+02 7.914797e+01 8.162198e+01
#>  [666] 1.420044e+02 5.728207e+01 7.793302e+01 5.536061e+01 5.705174e+01
#>  [671] 5.289766e+01 7.316309e+01 2.108886e+02 3.676957e+03 9.037180e+01
#>  [676] 7.513436e+01 5.846400e+01 6.611945e+01 6.466841e+01 4.025671e+03
#>  [681] 4.791332e+03 1.404747e+02 6.563884e+01 5.344636e+01 7.739914e+01
#>  [686] 6.222204e+01 6.096625e+01 6.814718e+01 1.305162e+02 4.945906e+02
#>  [691] 4.689768e+02 1.022585e+02 3.415831e+02 8.764392e+01 9.157931e+01
#>  [696] 5.735415e+01 5.758157e+01 6.712237e+01 6.295161e+01 5.580360e+01
#>  [701] 6.213315e+01 6.405077e+01 8.476680e+01 6.205465e+01 8.090606e+01
#>  [706] 5.620459e+01 5.970405e+01 5.883339e+01 1.135550e+02 5.690018e+01
#>  [711] 6.262069e+01 5.823799e+01 8.286759e+01 5.440002e+01 5.696342e+01
#>  [716] 2.833100e+04 1.056213e+02 6.905579e+01 5.967009e+01 6.239488e+01
#>  [721] 6.125535e+01 6.711666e+01 5.802584e+01 6.720550e+01 6.132425e+01
#>  [726] 6.823408e+01 6.576039e+01 5.693225e+01 6.948313e+01 6.509578e+01
#>  [731] 1.154171e+02 6.516509e+01 5.740514e+01 5.821739e+01 6.328220e+01
#>  [736] 8.331677e+01 6.587172e+01 9.673551e+01 5.972972e+01 6.054906e+01
#>  [741] 6.691918e+01 6.843502e+01 5.349610e+01 5.672229e+01 5.607996e+01
#>  [746] 8.976076e+01 1.002260e+02 6.390690e+01 9.817799e+01 4.861482e+01
#>  [751] 7.387598e+01 6.005814e+01 2.000497e+02 7.069345e+01 6.268974e+01
#>  [756] 6.572088e+01 7.687149e+01 7.349384e+01 5.800050e+01 6.901854e+01
#>  [761] 1.858038e+02 1.104827e+02 7.495323e+01 6.822777e+01 5.954985e+01
#>  [766] 5.837479e+01 6.251443e+01 7.422900e+01 6.050780e+01 4.618019e+02
#>  [771] 7.383220e+06 1.373849e+02 5.652678e+01 9.679722e+01 6.635569e+01
#>  [776] 6.001690e+01 9.246077e+01 1.559168e+02 3.418368e+02 1.212656e+02
#>  [781] 5.927038e+01 5.911809e+01 6.388411e+01 5.672445e+01 5.741192e+01
#>  [786] 5.660695e+01 5.587725e+01 5.912051e+01 5.584712e+01 1.809004e+02
#>  [791] 6.103306e+01 6.316206e+01 5.686449e+01 6.110355e+01 6.137997e+01
#>  [796] 6.857717e+01 7.178635e+01 1.109312e+03 6.998453e+01 6.553730e+01
#>  [801] 7.232773e+01 6.545926e+01 5.560144e+01 5.837205e+01 6.235399e+01
#>  [806] 7.071875e+01 7.877141e+01 6.845598e+01 5.377740e+01 7.224749e+01
#>  [811] 6.502128e+01 6.301520e+01 7.275661e+01 3.634107e+02 1.323916e+02
#>  [816] 8.598417e+01 1.045568e+02 1.728253e+02 7.276790e+01 6.270061e+01
#>  [821] 5.717518e+01 9.173022e+02 2.789239e+02 9.363992e+01 7.897715e+01
#>  [826] 5.621292e+01 5.392839e+01 5.600338e+01 6.146480e+01 6.008968e+01
#>  [831] 1.524758e+02 7.025776e+01 6.248504e+01 5.695326e+01 6.289021e+01
#>  [836] 7.287941e+01 1.010050e+02 6.788550e+01 5.686525e+01 5.641372e+01
#>  [841] 7.497692e+01 7.475032e+01 6.510970e+01 5.832157e+01 6.047747e+01
#>  [846] 2.731522e+02 8.284514e+01 6.197278e+01 6.408427e+01 9.760017e+01
#>  [851] 6.879329e+01 7.071093e+01 9.119271e+01 6.201656e+01 5.626307e+01
#>  [856] 6.615874e+01 5.570989e+01 8.072564e+01 1.146724e+02 7.588702e+01
#>  [861] 7.610730e+01 1.005219e+02 6.567271e+01 1.030852e+02 6.002459e+01
#>  [866] 5.496788e+01 6.156913e+01 8.030646e+01 6.374163e+01 7.028195e+01
#>  [871] 7.310392e+01 8.362768e+01 6.292563e+01 5.714051e+01 7.094868e+01
#>  [876] 8.760283e+01 1.116788e+02 1.480369e+02 1.615030e+02 6.190997e+01
#>  [881] 6.324189e+01 6.032899e+01 6.203164e+01 5.395188e+01 9.855995e+01
#>  [886] 5.860122e+01 5.714613e+01 6.113063e+01 6.266702e+01 6.374638e+01
#>  [891] 1.042615e+02 8.422204e+01 5.159467e+01 6.313535e+01 6.748165e+01
#>  [896] 8.509142e+01 9.092388e+01 4.205022e+02 8.892688e+01 1.367472e+02
#>  [901] 6.708024e+01 6.476865e+01 6.237596e+01 6.423385e+01 6.182258e+01
#>  [906] 6.107318e+01 6.234808e+01 1.050386e+02 9.892130e+01 9.682332e+01
#>  [911] 6.189119e+01 5.389487e+01 6.466888e+01 7.150729e+01 1.217121e+02
#>  [916] 5.486120e+01 5.990043e+01 7.886801e+01 1.926767e+02 7.134744e+01
#>  [921] 5.991731e+01 6.670558e+01 5.690561e+01 5.915470e+01 5.914089e+01
#>  [926] 7.601902e+01 6.653011e+01 2.202871e+02 7.043857e+01 7.438675e+01
#>  [931] 6.017739e+01 7.936021e+01 1.193136e+02 5.823185e+01 6.359105e+01
#>  [936] 5.932930e+01 1.574530e+02 7.230978e+01 1.322393e+02 6.178402e+01
#>  [941] 1.469744e+02 1.351501e+02 7.477094e+01 7.694934e+01 9.456256e+01
#>  [946] 8.427019e+01 1.433679e+02 5.591730e+01 5.537337e+01 5.994647e+01
#>  [951] 6.508820e+01 1.228885e+03 1.058848e+02 6.297639e+01 7.618551e+01
#>  [956] 7.122496e+01 1.090862e+02 8.488686e+01 6.338665e+01 8.137629e+01
#>  [961] 5.768203e+01 8.806500e+01 6.085842e+01 7.725616e+01 6.338303e+01
#>  [966] 6.610257e+01 7.552576e+01 5.909134e+01 4.691518e+01 5.986632e+01
#>  [971] 6.396638e+01 6.704213e+01 6.625540e+01 5.282453e+02 1.661238e+02
#>  [976] 2.493488e+02 6.560398e+01 7.862081e+01 6.447330e+01 6.113710e+01
#>  [981] 6.801616e+01 7.386023e+01 7.584238e+01 6.080680e+01 6.652750e+01
#>  [986] 6.182093e+01 4.919142e+01 5.962223e+01 5.948470e+01 6.224465e+01
#>  [991] 7.132968e+01 1.166715e+02 2.239731e+03 2.383098e+05 2.077833e+02
#>  [996] 6.491279e+01 6.435883e+01 5.628199e+01 4.582999e+03 5.600803e+01
```
