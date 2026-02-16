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
#>  [1]  30.82776  30.82776  47.77127  47.77127  47.77127 139.73020  13.71625
#>  [8]  40.81182  40.81182  40.81182  40.81182  40.81182  40.81182  66.38251
#> [15]  76.96457 101.79775 101.79775 102.84634 102.84634 102.84634

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
#>    [1] 6.608116e+01 6.034263e+01 6.626792e+01 6.022072e+01 7.289129e+01
#>    [6] 6.624158e+01 7.933536e+01 5.985657e+01 6.002867e+01 7.192222e+01
#>   [11] 7.072368e+01 1.176185e+02 6.244930e+01 6.926818e+01 6.263600e+01
#>   [16] 6.492878e+01 1.303947e+02 6.593181e+01 6.197820e+01 7.386447e+01
#>   [21] 1.701195e+02 6.530718e+01 9.258476e+01 6.117187e+01 7.621837e+01
#>   [26] 8.932635e+01 8.710612e+02 7.242703e+01 6.023244e+01 5.061945e+02
#>   [31] 7.309654e+01 7.551161e+01 7.887432e+01 6.822217e+01 6.158665e+01
#>   [36] 7.939164e+01 6.169851e+01 5.493228e+01 6.084365e+01 6.550586e+01
#>   [41] 5.885207e+01 5.321528e+01 6.943084e+01 1.077458e+02 6.570761e+01
#>   [46] 5.441235e+01 9.699303e+01 1.339251e+02 5.819474e+01 8.311294e+01
#>   [51] 5.938298e+01 1.151346e+02 9.899063e+01 6.246453e+01 6.158994e+01
#>   [56] 5.951556e+01 6.435709e+01 6.345371e+01 7.565226e+01 6.243745e+01
#>   [61] 7.180663e+01 1.031539e+02 9.998284e+01 5.403300e+01 9.652776e+01
#>   [66] 6.304357e+01 6.142411e+01 6.807441e+01 7.528605e+01 1.031761e+02
#>   [71] 5.783764e+01 5.721060e+01 6.661586e+01 5.824252e+01 6.706438e+01
#>   [76] 1.944830e+02 8.421853e+01 8.572214e+01 6.224001e+01 9.319767e+01
#>   [81] 7.109861e+01 1.336578e+03 7.331951e+01 1.027739e+02 5.981053e+01
#>   [86] 8.809709e+01 5.681415e+01 5.802097e+01 6.825034e+01 6.424482e+01
#>   [91] 8.424795e+01 7.485791e+01 5.899259e+01 1.913280e+02 6.718466e+01
#>   [96] 5.768764e+01 6.156667e+01 5.875197e+01 5.983832e+01 6.555314e+01
#>  [101] 6.716477e+01 1.261210e+02 9.694491e+01 9.118314e+01 6.084569e+01
#>  [106] 5.961375e+01 6.740383e+01 8.999010e+01 6.193221e+01 5.861934e+01
#>  [111] 6.632927e+01 6.399934e+01 1.714544e+02 8.298448e+01 8.813919e+01
#>  [116] 6.574154e+01 7.097156e+01 5.357730e+01 8.222051e+01 6.919361e+01
#>  [121] 5.081022e+01 6.429689e+01 5.696211e+01 9.171679e+01 1.704517e+02
#>  [126] 3.767988e+02 5.317644e+01 7.036788e+01 5.845220e+01 5.786848e+01
#>  [131] 4.414407e+02 8.672822e+01 8.345412e+01 6.016496e+01 6.955701e+01
#>  [136] 8.018424e+01 7.848036e+01 4.951358e+01 9.154306e+01 5.450400e+01
#>  [141] 1.510354e+02 5.664083e+01 5.903898e+01 6.037986e+01 1.327134e+02
#>  [146] 6.213219e+01 5.844350e+01 6.099025e+01 6.079511e+01 5.625935e+01
#>  [151] 5.666273e+01 5.742295e+01 6.173183e+01 6.786770e+01 5.324881e+01
#>  [156] 5.471940e+01 6.157498e+01 6.170150e+01 5.853214e+01 6.095080e+01
#>  [161] 6.226052e+01 9.378367e+01 6.162735e+01 5.346124e+01 6.368098e+01
#>  [166] 8.683537e+01 5.748820e+01 1.004841e+02 6.123474e+01 2.015406e+02
#>  [171] 9.384740e+01 6.415163e+01 6.371364e+01 8.740784e+01 5.898240e+01
#>  [176] 8.268819e+01 6.598308e+01 6.276612e+01 9.263815e+01 7.130950e+01
#>  [181] 6.120041e+01 6.173166e+02 4.133855e+02 2.340276e+02 7.600475e+01
#>  [186] 8.473148e+01 1.429642e+02 1.714462e+02 6.217212e+01 5.128974e+01
#>  [191] 5.568845e+01 7.204299e+01 5.959268e+01 6.160006e+01 6.416356e+01
#>  [196] 6.141202e+01 6.128657e+01 6.429033e+01 6.802873e+01 8.609134e+01
#>  [201] 8.914520e+01 6.565222e+01 6.993481e+01 7.632325e+01 2.230436e+02
#>  [206] 6.162773e+01 6.045464e+01 5.671436e+01 5.129015e+01 5.301779e+01
#>  [211] 6.646018e+01 7.527368e+01 8.510899e+01 8.836925e+01 6.034449e+01
#>  [216] 7.088861e+01 9.711664e+01 5.923309e+01 3.187318e+02 1.820281e+03
#>  [221] 6.833082e+01 7.484830e+01 8.591810e+01 6.418458e+01 5.701627e+01
#>  [226] 7.212912e+01 3.436647e+02 9.595489e+01 8.356688e+01 6.060770e+01
#>  [231] 5.930668e+01 5.729471e+01 5.595120e+01 6.269536e+01 5.259939e+01
#>  [236] 1.463486e+02 6.940016e+02 6.387231e+01 2.652608e+02 1.767729e+02
#>  [241] 7.283463e+02 7.653960e+01 7.022006e+01 6.114501e+01 5.773145e+01
#>  [246] 9.285737e+01 7.121759e+01 1.501481e+02 6.772686e+01 7.425637e+01
#>  [251] 6.172725e+01 6.204588e+01 4.957111e+02 8.315841e+01 5.739538e+01
#>  [256] 6.912936e+01 5.560732e+01 6.368703e+01 4.157509e+02 6.269797e+01
#>  [261] 6.415359e+01 7.433555e+01 8.639134e+01 6.331620e+01 7.206479e+01
#>  [266] 5.618886e+01 5.904290e+01 1.139994e+02 6.157606e+01 6.320847e+01
#>  [271] 5.746635e+01 7.188592e+01 5.812303e+01 6.276745e+01 5.919922e+01
#>  [276] 5.382847e+01 7.260329e+01 7.095579e+01 8.814255e+01 9.108329e+01
#>  [281] 6.012585e+01 7.364334e+01 7.023458e+01 6.735263e+01 7.236749e+01
#>  [286] 2.073440e+02 7.001177e+01 9.501130e+01 7.576096e+01 6.304919e+01
#>  [291] 6.784296e+01 7.634032e+01 3.549995e+03 6.117184e+01 5.829053e+01
#>  [296] 5.819109e+01 5.492588e+01 6.763360e+01 5.987763e+01 6.195101e+01
#>  [301] 5.373169e+01 5.821633e+01 6.067994e+01 5.544539e+01 5.968757e+01
#>  [306] 6.033125e+01 5.818388e+01 5.891448e+01 6.165213e+01 5.801012e+01
#>  [311] 6.940159e+01 5.689988e+01 5.980330e+01 6.803958e+01 5.754557e+01
#>  [316] 5.698937e+01 6.157919e+01 1.801289e+02 9.062685e+01 6.614371e+01
#>  [321] 8.383377e+01 2.943231e+02 5.435184e+01 6.815091e+01 7.951329e+01
#>  [326] 9.028482e+01 6.783672e+01 6.403081e+01 2.363255e+02 1.451622e+02
#>  [331] 7.033080e+01 1.418023e+02 3.052960e+02 5.825140e+01 5.486791e+01
#>  [336] 7.906111e+01 6.604952e+01 6.418501e+01 6.006357e+01 6.485768e+01
#>  [341] 6.451575e+01 5.777111e+01 6.258706e+01 6.237223e+01 6.910254e+01
#>  [346] 5.598007e+01 1.383914e+02 5.491367e+01 6.243882e+01 7.967350e+01
#>  [351] 5.951914e+01 6.568518e+01 1.216187e+02 8.192933e+01 5.519255e+01
#>  [356] 5.859135e+01 5.496987e+01 5.553946e+01 6.013833e+01 1.050389e+02
#>  [361] 6.105757e+01 6.342850e+01 1.166360e+02 6.105882e+01 6.278471e+01
#>  [366] 1.420412e+02 7.660708e+01 7.629712e+01 6.818391e+01 7.399338e+01
#>  [371] 1.804842e+02 5.975307e+01 6.171606e+01 5.758883e+01 5.394054e+01
#>  [376] 6.618400e+01 7.455129e+01 5.540463e+01 8.816462e+01 1.278266e+03
#>  [381] 1.124768e+02 7.862385e+01 6.447735e+01 6.418620e+01 6.697739e+01
#>  [386] 5.856491e+01 7.784945e+01 6.935853e+01 9.147255e+01 6.087152e+01
#>  [391] 6.930122e+01 7.019449e+01 5.331465e+01 8.842414e+01 9.038991e+01
#>  [396] 5.739555e+01 1.921412e+02 1.397949e+02 5.916497e+01 8.570195e+01
#>  [401] 6.662662e+01 9.490919e+01 1.254874e+02 5.378861e+01 7.834360e+01
#>  [406] 6.647401e+01 8.895891e+01 5.578940e+01 9.756001e+01 6.397359e+01
#>  [411] 7.322901e+01 7.143677e+01 5.725710e+01 5.920419e+01 6.296916e+01
#>  [416] 6.274316e+01 5.650438e+01 6.540185e+01 7.411660e+01 5.793632e+01
#>  [421] 1.104577e+02 6.261602e+01 5.245926e+01 1.888892e+02 5.901159e+01
#>  [426] 6.732922e+01 6.230406e+01 6.281116e+01 9.172273e+01 5.625059e+01
#>  [431] 8.096084e+01 8.018933e+01 5.956205e+01 1.330334e+02 5.414097e+02
#>  [436] 1.906579e+02 1.356513e+02 1.020439e+02 2.586015e+02 6.248132e+01
#>  [441] 6.991176e+01 1.269193e+02 3.133435e+02 6.550816e+01 7.430329e+01
#>  [446] 5.952145e+01 8.235904e+01 1.531386e+02 5.963760e+01 6.189469e+01
#>  [451] 5.644026e+01 6.803125e+01 6.060608e+01 5.964184e+01 5.654879e+01
#>  [456] 1.276819e+02 7.926881e+01 8.658141e+01 6.773158e+01 1.239048e+02
#>  [461] 5.571922e+01 6.089683e+01 1.176661e+02 6.171517e+01 6.509861e+01
#>  [466] 6.190978e+01 5.461839e+01 6.322963e+01 1.123552e+02 6.666078e+01
#>  [471] 7.480104e+01 5.306267e+01 5.891365e+01 1.223197e+02 1.095599e+02
#>  [476] 6.684662e+02 6.425350e+01 7.371964e+01 6.121124e+01 7.713801e+02
#>  [481] 1.065365e+02 1.058668e+02 6.367830e+01 5.678012e+01 9.435452e+01
#>  [486] 5.968284e+01 6.495324e+01 7.196526e+01 5.898328e+01 6.204281e+01
#>  [491] 6.936292e+01 5.883943e+01 5.638386e+01 6.038297e+01 7.760267e+01
#>  [496] 6.709188e+01 6.386976e+01 1.762398e+02 5.845494e+01 5.094694e+01
#>  [501] 7.536217e+01 8.900937e+01 2.986172e+02 1.162299e+02 1.982631e+03
#>  [506] 5.544024e+01 1.057735e+02 7.236366e+01 5.939150e+01 6.217229e+01
#>  [511] 6.493535e+01 6.011581e+01 5.983026e+01 6.567710e+01 6.298431e+01
#>  [516] 6.670650e+01 8.821695e+01 1.142994e+03 4.786501e+03 1.038918e+02
#>  [521] 2.030206e+02 6.524587e+01 6.015621e+01 6.903530e+01 7.030658e+01
#>  [526] 8.097159e+01 8.332269e+01 5.230238e+01 5.552164e+01 7.432224e+01
#>  [531] 5.838936e+01 2.646994e+02 6.048371e+01 6.064959e+01 7.986279e+01
#>  [536] 9.000400e+01 4.800579e+01 6.492072e+01 1.096080e+02 5.906072e+01
#>  [541] 7.233815e+01 5.042078e+01 1.066658e+02 6.939836e+01 6.221515e+01
#>  [546] 6.602893e+01 6.370586e+01 6.070177e+01 7.189142e+01 5.828489e+01
#>  [551] 6.415127e+01 7.715916e+01 1.724609e+04 2.168953e+02 7.623474e+01
#>  [556] 4.699992e+01 4.949133e+01 5.782569e+01 6.628440e+01 2.746471e+02
#>  [561] 1.175240e+02 3.320753e+02 5.496202e+01 5.847450e+01 6.118521e+01
#>  [566] 5.783819e+01 1.118335e+02 3.047909e+02 5.883057e+01 5.775508e+01
#>  [571] 9.807041e+01 5.467499e+01 5.804003e+01 1.858508e+02 5.966135e+01
#>  [576] 8.414079e+01 5.501096e+01 5.919567e+01 7.503097e+01 2.599863e+03
#>  [581] 5.961619e+02 3.660971e+02 6.917233e+03 1.881106e+03 1.117249e+02
#>  [586] 9.267331e+01 5.663665e+01 5.271939e+01 6.434029e+01 6.899578e+01
#>  [591] 1.061191e+02 5.475701e+01 5.566464e+01 6.523292e+01 5.502333e+01
#>  [596] 6.318086e+01 6.604901e+01 5.816603e+01 5.841873e+01 5.375437e+01
#>  [601] 7.849736e+01 5.917574e+01 6.526418e+01 5.805345e+01 6.273769e+01
#>  [606] 5.871290e+01 6.150609e+01 5.946553e+01 6.283273e+01 1.781374e+02
#>  [611] 4.743708e+01 5.975938e+01 6.736987e+01 6.450645e+01 6.191782e+01
#>  [616] 9.029063e+01 1.146648e+02 9.381792e+01 9.320969e+01 6.369918e+01
#>  [621] 6.295484e+01 6.400875e+01 7.915447e+01 6.451767e+01 5.532605e+01
#>  [626] 6.715125e+01 5.817626e+01 7.671089e+01 8.361940e+01 7.192970e+01
#>  [631] 5.922311e+01 3.391596e+02 4.221991e+03 8.679108e+02 1.291366e+05
#>  [636] 1.555462e+03 3.476547e+02 8.959516e+01 5.879290e+01 6.502431e+01
#>  [641] 6.793606e+01 6.644996e+01 5.977529e+01 1.058604e+02 1.693621e+02
#>  [646] 5.721631e+01 6.883821e+01 6.581993e+01 6.351311e+01 6.713518e+01
#>  [651] 6.240620e+01 7.245350e+01 5.823576e+01 5.465312e+01 6.405012e+01
#>  [656] 6.684802e+01 1.073408e+02 6.487196e+01 9.092569e+01 9.629264e+01
#>  [661] 6.248215e+01 1.098516e+02 1.117733e+02 6.378797e+01 5.728194e+01
#>  [666] 1.146641e+02 2.822661e+02 1.211545e+02 5.876581e+01 6.752440e+01
#>  [671] 7.699227e+01 5.848100e+01 7.263855e+01 6.220601e+01 6.404775e+01
#>  [676] 5.942598e+01 7.191629e+01 8.256183e+01 5.963785e+01 6.514839e+01
#>  [681] 5.840228e+01 5.843897e+01 5.465631e+01 6.892830e+01 5.691785e+01
#>  [686] 8.138755e+01 6.332923e+01 1.397454e+02 6.033028e+01 6.132800e+01
#>  [691] 5.783866e+01 5.946660e+01 6.023911e+01 5.643252e+01 6.734423e+01
#>  [696] 5.435365e+01 8.073880e+01 6.685083e+01 5.876771e+01 6.001945e+01
#>  [701] 6.267940e+01 6.730841e+01 5.392952e+01 5.762152e+01 8.379798e+01
#>  [706] 1.253001e+02 6.252270e+01 6.273964e+01 8.490460e+01 7.648103e+02
#>  [711] 3.069877e+07 6.267693e+01 5.733499e+01 7.873581e+01 6.835674e+01
#>  [716] 4.838208e+01 5.567083e+01 1.291604e+02 6.534589e+01 6.112629e+01
#>  [721] 5.760632e+01 7.207667e+01 7.246755e+01 5.862782e+01 7.151123e+01
#>  [726] 7.096906e+01 5.831937e+01 5.721880e+01 7.770506e+01 6.815525e+01
#>  [731] 6.976832e+01 6.629932e+01 5.201908e+01 7.383417e+01 5.930887e+01
#>  [736] 5.643421e+01 4.105000e+02 7.113288e+01 7.421297e+01 5.802531e+01
#>  [741] 7.477576e+01 5.913171e+01 8.102195e+01 7.355889e+01 6.429864e+01
#>  [746] 5.656464e+01 5.214387e+01 8.896747e+01 1.621575e+02 6.136822e+01
#>  [751] 1.014747e+02 8.327081e+01 6.867166e+01 2.571909e+02 5.487919e+01
#>  [756] 1.015902e+02 1.256853e+02 5.795568e+01 6.469873e+01 6.268316e+01
#>  [761] 8.070017e+01 5.826517e+01 1.084450e+02 1.074561e+02 8.792186e+01
#>  [766] 1.136934e+02 6.514310e+01 5.181019e+01 7.894093e+01 4.243357e+02
#>  [771] 2.611218e+02 1.003372e+02 2.920033e+02 5.255193e+02 6.037949e+01
#>  [776] 5.954197e+01 5.402571e+01 1.656704e+02 6.771115e+01 8.991701e+01
#>  [781] 5.959289e+01 6.050061e+01 1.631968e+02 5.916656e+01 5.690538e+01
#>  [786] 5.990564e+01 7.787720e+01 8.562488e+01 5.905606e+01 6.605501e+01
#>  [791] 8.635025e+01 6.808938e+01 6.300052e+01 6.033341e+01 7.717770e+01
#>  [796] 9.567488e+01 6.601923e+01 6.028438e+01 7.337748e+01 5.773991e+01
#>  [801] 5.831146e+01 6.057238e+01 5.811683e+01 6.679257e+01 5.724584e+01
#>  [806] 9.667615e+01 8.562255e+01 7.161263e+01 5.594441e+01 8.782390e+01
#>  [811] 7.444101e+01 6.612949e+01 1.690581e+02 6.985815e+01 6.860331e+01
#>  [816] 6.417431e+01 5.744763e+01 6.013685e+01 6.335429e+01 8.430534e+01
#>  [821] 7.575101e+01 7.364881e+01 8.326398e+01 7.844434e+01 1.211436e+02
#>  [826] 5.415266e+01 6.615883e+01 6.215038e+01 9.274520e+01 6.557215e+01
#>  [831] 7.623251e+01 1.629541e+02 5.479938e+01 5.427783e+01 5.985950e+01
#>  [836] 6.425044e+01 6.299901e+01 6.269714e+01 6.464060e+01 5.316070e+01
#>  [841] 7.175572e+01 6.541998e+01 7.548790e+01 6.147336e+01 5.886424e+01
#>  [846] 6.036568e+01 6.040436e+01 5.974004e+01 6.133928e+01 7.091019e+01
#>  [851] 5.277864e+01 6.615516e+01 5.164903e+01 7.736912e+01 3.372944e+02
#>  [856] 5.492706e+01 5.350890e+01 7.576635e+01 9.523807e+01 5.323174e+01
#>  [861] 9.397466e+01 6.257439e+01 8.338014e+01 2.285789e+02 5.735436e+01
#>  [866] 1.362125e+02 6.622411e+01 6.564443e+01 6.996778e+01 6.323554e+01
#>  [871] 6.045695e+01 1.470929e+02 5.694580e+01 5.721772e+01 5.596079e+01
#>  [876] 8.074963e+01 6.215091e+01 6.429827e+01 1.782873e+02 6.595529e+01
#>  [881] 6.661134e+01 6.188023e+01 5.943638e+01 6.218259e+01 6.140479e+01
#>  [886] 1.681556e+02 7.314229e+01 1.094326e+02 4.706698e+01 1.050962e+02
#>  [891] 7.036030e+01 5.645320e+01 6.162854e+01 6.484742e+01 8.628875e+01
#>  [896] 5.661784e+01 6.785578e+01 4.988237e+01 5.235189e+01 6.096590e+01
#>  [901] 8.009341e+01 7.584390e+01 6.617970e+01 6.435392e+01 8.201091e+01
#>  [906] 3.854091e+02 5.196595e+01 7.090270e+01 5.356365e+01 6.264188e+01
#>  [911] 1.428099e+02 7.298727e+01 5.629073e+01 4.879386e+01 7.295347e+01
#>  [916] 1.219046e+02 5.579260e+01 5.866056e+01 7.158827e+01 6.647133e+01
#>  [921] 6.055034e+01 6.879912e+01 5.704147e+01 5.961812e+01 6.073040e+01
#>  [926] 6.014867e+01 2.320944e+02 6.728337e+01 1.285759e+02 5.656600e+01
#>  [931] 6.637381e+01 2.019545e+02 9.498357e+01 5.914589e+01 8.399553e+01
#>  [936] 7.510395e+01 9.243061e+01 6.994143e+01 7.806169e+01 1.009598e+02
#>  [941] 6.569192e+01 6.138398e+01 5.719513e+01 1.242136e+02 8.372829e+01
#>  [946] 6.338886e+01 5.742459e+01 7.191908e+01 5.721617e+01 5.762011e+01
#>  [951] 5.974838e+01 6.280476e+01 5.746116e+01 6.834125e+01 5.603034e+01
#>  [956] 1.772357e+02 5.992661e+01 5.741887e+01 5.968146e+01 6.294715e+01
#>  [961] 6.611240e+01 6.521763e+01 5.500263e+01 6.418479e+01 5.192746e+01
#>  [966] 5.562709e+01 6.560917e+01 6.985803e+01 6.467177e+01 9.818424e+01
#>  [971] 5.846402e+01 5.932661e+01 7.871137e+01 7.211747e+01 1.537243e+02
#>  [976] 1.042278e+02 3.454894e+02 5.857005e+03 1.643392e+02 2.303542e+02
#>  [981] 6.464150e+01 7.939571e+01 5.904144e+01 8.778842e+01 5.797734e+01
#>  [986] 9.091048e+01 3.759083e+02 5.957262e+01 6.758778e+01 6.606569e+01
#>  [991] 7.830550e+01 6.064006e+01 4.401770e+02 5.976848e+01 7.569343e+01
#>  [996] 7.931822e+01 6.381130e+01 7.172781e+01 5.871203e+01 6.471064e+01
```
