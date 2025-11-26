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
#>  [1] 35.02908 35.02908 27.99349 27.99349 27.99349 27.99349 27.99349 23.14185
#>  [9] 30.25691 30.25691 10.60781 10.60781 10.60781 14.97994 37.46594 30.76891
#> [17] 30.76891 30.76891 58.38263 58.38263

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
#>    [1] 2.651959e+02 5.726275e+01 6.023714e+01 6.693599e+01 1.646159e+03
#>    [6] 2.052987e+02 1.292043e+02 5.758504e+01 5.570453e+01 6.893778e+01
#>   [11] 6.418765e+01 6.452224e+01 5.583263e+01 6.166494e+01 6.002577e+01
#>   [16] 5.981801e+01 6.235928e+01 5.928947e+01 7.835477e+01 7.066314e+01
#>   [21] 6.139754e+01 1.111582e+02 5.983641e+01 8.258128e+01 7.578700e+01
#>   [26] 5.917609e+01 5.796792e+01 6.178824e+01 6.890954e+01 6.561355e+01
#>   [31] 8.649048e+01 5.961019e+01 9.822982e+01 6.880085e+01 7.444702e+01
#>   [36] 1.042625e+02 5.413391e+01 7.123502e+01 6.077546e+01 9.141000e+01
#>   [41] 8.976381e+01 8.185575e+01 6.478993e+01 8.947242e+01 6.335653e+01
#>   [46] 6.000869e+01 6.030863e+01 2.162343e+02 1.398796e+02 1.131910e+02
#>   [51] 7.128271e+01 5.435824e+01 5.750564e+01 5.867509e+01 5.684897e+01
#>   [56] 5.941710e+01 7.394789e+01 7.237662e+01 5.765923e+01 7.411116e+01
#>   [61] 9.376209e+01 1.046706e+02 6.708083e+01 6.968393e+01 6.846038e+01
#>   [66] 6.810558e+01 6.281904e+01 5.624267e+01 5.438527e+01 6.829144e+01
#>   [71] 5.878248e+01 1.174100e+02 5.841595e+01 1.162977e+02 1.338843e+02
#>   [76] 5.204956e+01 5.629334e+01 5.576581e+01 6.840576e+01 1.025635e+02
#>   [81] 6.167423e+01 5.448364e+02 2.662685e+02 5.919983e+01 6.020118e+01
#>   [86] 6.986937e+01 7.027168e+01 8.091289e+01 5.955547e+01 8.017404e+01
#>   [91] 5.619692e+01 5.694852e+01 6.100848e+01 6.058529e+01 6.689922e+01
#>   [96] 6.641276e+01 4.694938e+01 2.694210e+02 1.296185e+03 9.749110e+01
#>  [101] 5.692604e+01 6.892517e+01 5.555070e+01 7.181770e+01 1.320924e+02
#>  [106] 5.871443e+01 6.653562e+01 7.093998e+01 6.212856e+01 8.634408e+01
#>  [111] 7.613225e+01 9.560873e+01 5.536699e+01 5.975079e+01 9.582271e+01
#>  [116] 8.220030e+01 5.400776e+01 5.964442e+01 6.804383e+01 5.848851e+01
#>  [121] 5.989962e+01 6.579159e+01 5.849505e+01 6.483760e+01 6.096342e+01
#>  [126] 6.618824e+01 5.479789e+01 8.517337e+01 7.547419e+01 5.568033e+01
#>  [131] 6.230257e+01 6.414512e+01 7.853710e+01 6.560837e+01 7.037273e+01
#>  [136] 7.907368e+01 6.750550e+01 1.824340e+02 6.127980e+01 5.961245e+01
#>  [141] 6.371263e+01 6.474076e+01 1.003746e+02 6.544083e+01 7.071867e+01
#>  [146] 8.143457e+01 7.120932e+01 9.979767e+01 6.758040e+01 7.127729e+01
#>  [151] 6.217501e+01 1.388877e+02 6.539579e+01 6.339206e+01 6.140472e+01
#>  [156] 6.216740e+01 1.015514e+02 5.845068e+01 9.614087e+01 1.489421e+02
#>  [161] 3.189498e+01 3.093671e+02 6.291147e+01 6.758937e+01 7.298354e+01
#>  [166] 5.830879e+01 6.243179e+01 6.117192e+01 5.893131e+01 6.396587e+01
#>  [171] 8.322616e+01 5.910741e+01 5.748344e+01 5.902462e+01 7.316785e+01
#>  [176] 5.944933e+01 6.536496e+01 6.670732e+01 5.962527e+01 1.208525e+02
#>  [181] 5.003724e+01 1.239314e+02 5.522403e+01 6.493311e+01 6.727266e+01
#>  [186] 7.063669e+01 9.803381e+01 7.424502e+01 6.207112e+01 6.873161e+01
#>  [191] 1.063086e+02 7.852791e+01 6.091352e+01 6.063283e+01 6.566418e+01
#>  [196] 1.094023e+02 6.934842e+01 6.466319e+01 6.515646e+01 6.967536e+01
#>  [201] 6.715116e+01 6.768426e+01 6.838340e+01 6.801164e+01 6.424382e+01
#>  [206] 6.104757e+01 6.446648e+01 8.815120e+01 6.610545e+01 7.287133e+01
#>  [211] 6.247098e+01 1.413090e+02 2.756783e+02 6.580353e+01 7.592900e+01
#>  [216] 5.780044e+01 8.041012e+01 5.584565e+01 6.202055e+01 2.147500e+02
#>  [221] 1.208838e+02 5.323875e+01 7.689740e+01 7.622836e+01 1.159188e+02
#>  [226] 5.303763e+01 6.638791e+01 6.986089e+01 6.033430e+01 5.704582e+01
#>  [231] 1.430210e+02 6.156487e+01 7.685952e+01 8.299733e+01 5.967701e+01
#>  [236] 1.456241e+02 6.052873e+01 1.134114e+02 8.397565e+01 2.760963e+02
#>  [241] 8.386068e+01 6.763004e+01 1.874518e+02 6.791310e+01 7.262762e+01
#>  [246] 5.599247e+01 8.451461e+01 5.781171e+01 5.897403e+01 6.622216e+01
#>  [251] 5.787380e+01 7.999084e+01 5.376419e+01 5.661096e+01 7.835815e+01
#>  [256] 6.392702e+01 5.858475e+01 5.654453e+01 6.245563e+01 5.905420e+01
#>  [261] 6.031987e+01 5.756671e+01 1.283907e+02 5.781338e+01 6.200766e+01
#>  [266] 2.847352e+02 9.416348e+01 1.114217e+02 6.491069e+01 6.840318e+01
#>  [271] 5.775613e+01 6.547376e+01 8.743533e+01 9.997113e+01 1.537676e+02
#>  [276] 6.224691e+01 6.188745e+01 6.446216e+01 6.269806e+01 6.481688e+01
#>  [281] 6.349710e+01 8.584034e+01 7.757508e+01 2.655504e+02 3.198723e+02
#>  [286] 6.552079e+01 6.453787e+01 5.588916e+01 6.458255e+01 5.845544e+01
#>  [291] 8.456475e+01 6.135896e+01 5.520183e+01 1.144156e+02 5.555863e+01
#>  [296] 6.057403e+01 6.019193e+01 1.192147e+02 9.289586e+01 1.158217e+02
#>  [301] 5.871667e+01 1.020063e+03 5.675592e+01 6.053135e+01 8.167769e+01
#>  [306] 1.277332e+02 1.003309e+02 5.775167e+01 9.739452e+01 7.323506e+01
#>  [311] 5.783252e+01 7.070128e+01 5.874750e+01 7.352239e+01 6.209191e+01
#>  [316] 5.443812e+01 5.854832e+01 6.912088e+01 6.285017e+01 5.649193e+01
#>  [321] 5.880236e+01 6.011840e+01 7.092778e+01 9.158385e+01 7.290708e+01
#>  [326] 8.500960e+01 6.311400e+01 5.923432e+01 5.727124e+01 6.400191e+01
#>  [331] 1.184852e+02 7.448772e+01 6.502620e+01 1.865294e+02 2.894339e+02
#>  [336] 4.959584e+01 7.132134e+01 6.580201e+01 5.414651e+01 9.142675e+01
#>  [341] 5.831246e+01 1.574686e+02 5.429880e+01 6.011382e+01 6.596126e+01
#>  [346] 9.364753e+01 7.789616e+01 5.954483e+01 6.889786e+01 9.060391e+01
#>  [351] 7.147244e+01 3.844694e+03 1.311191e+02 6.729202e+01 1.056567e+03
#>  [356] 7.328281e+01 7.201824e+01 6.701398e+01 8.510791e+01 9.961024e+01
#>  [361] 1.235253e+02 7.829376e+01 5.729875e+01 1.294375e+02 6.009478e+01
#>  [366] 6.252637e+01 5.342660e+01 6.051219e+01 7.547101e+01 3.758538e+02
#>  [371] 1.852399e+02 7.066751e+01 6.588730e+01 5.971645e+01 6.268778e+01
#>  [376] 2.641856e+02 7.251066e+01 5.459583e+01 5.632029e+01 8.885714e+01
#>  [381] 2.109183e+03 6.075916e+01 6.690055e+01 1.099428e+02 8.273926e+01
#>  [386] 5.973094e+01 5.763451e+01 7.157104e+01 5.844056e+01 6.178165e+01
#>  [391] 5.863076e+01 6.252431e+01 6.347812e+01 7.571532e+01 1.052348e+02
#>  [396] 6.156696e+01 5.990244e+01 6.053952e+01 6.917950e+01 1.022882e+02
#>  [401] 7.867686e+01 5.730171e+01 5.034516e+01 6.100211e+01 3.326139e+02
#>  [406] 6.004789e+01 6.656809e+01 7.571229e+01 9.250768e+01 5.593242e+01
#>  [411] 6.765658e+01 2.798644e+03 6.695016e+01 6.335252e+01 5.572959e+01
#>  [416] 6.702942e+01 5.570556e+02 3.001705e+03 6.758727e+02 8.850124e+01
#>  [421] 5.381442e+01 1.288722e+02 3.094481e+02 8.835031e+01 6.195998e+01
#>  [426] 6.157881e+01 8.080541e+01 6.526092e+01 5.742415e+01 7.994659e+01
#>  [431] 6.400670e+01 1.502545e+02 4.206331e+02 6.300736e+01 6.238655e+01
#>  [436] 5.668940e+01 5.970498e+01 6.062369e+01 5.807247e+01 7.063479e+01
#>  [441] 6.374564e+01 9.501622e+01 6.099061e+01 5.905361e+01 5.675476e+01
#>  [446] 5.985630e+01 6.402906e+01 5.974598e+01 6.275530e+01 5.676909e+01
#>  [451] 6.007240e+01 5.839269e+01 5.999074e+01 6.230634e+01 6.677855e+01
#>  [456] 1.043201e+02 3.671668e+01 7.103368e+01 5.788976e+01 6.326505e+01
#>  [461] 5.762547e+02 5.551818e+01 5.785383e+01 6.318267e+01 7.086391e+01
#>  [466] 5.793726e+01 5.980882e+01 8.689392e+01 9.213770e+01 1.210480e+02
#>  [471] 6.154292e+01 5.712451e+01 5.903137e+01 2.016671e+02 1.359502e+02
#>  [476] 5.233025e+01 6.995567e+01 8.623014e+01 6.596776e+01 5.934826e+01
#>  [481] 6.111775e+01 2.161878e+02 6.697158e+01 6.365983e+01 5.913432e+01
#>  [486] 6.387932e+01 6.082925e+01 6.336457e+01 8.602940e+01 5.866152e+02
#>  [491] 6.343381e+01 5.917055e+01 2.524341e+02 1.149426e+02 9.634546e+01
#>  [496] 6.057466e+01 6.124295e+01 6.982862e+01 6.224271e+01 7.983652e+01
#>  [501] 1.079470e+02 6.103332e+01 5.962969e+01 8.109643e+01 6.700711e+01
#>  [506] 8.669409e+01 9.923481e+01 1.371408e+02 8.587903e+01 6.151323e+01
#>  [511] 6.567851e+01 6.574526e+01 6.531812e+01 8.467239e+01 5.310901e+01
#>  [516] 7.980679e+01 1.023812e+02 6.863512e+01 5.948892e+01 6.766523e+01
#>  [521] 5.963790e+01 5.903684e+01 1.290362e+02 9.463884e+01 2.278393e+09
#>  [526] 1.081446e+08 7.304074e+01 7.601364e+01 6.165619e+01 7.408052e+01
#>  [531] 1.488012e+03 2.033361e+04 7.338549e+01 6.540215e+01 5.995187e+01
#>  [536] 6.621252e+01 5.642521e+01 9.172204e+01 6.393721e+01 7.995505e+01
#>  [541] 6.738952e+01 3.449223e+02 6.102013e+01 5.454611e+01 5.907799e+01
#>  [546] 5.458786e+01 6.482954e+01 6.079305e+01 5.749851e+01 6.390641e+01
#>  [551] 6.916617e+01 7.536975e+01 9.720914e+01 6.666859e+01 5.468253e+01
#>  [556] 5.935231e+01 9.946660e+01 6.178826e+01 1.099777e+02 7.854097e+01
#>  [561] 6.491956e+01 6.033722e+01 8.217982e+01 1.031082e+02 5.866966e+01
#>  [566] 6.320668e+01 5.727604e+01 2.396186e+02 5.914215e+01 1.042069e+02
#>  [571] 5.672943e+01 7.413677e+01 5.803853e+01 7.346915e+01 7.302763e+01
#>  [576] 8.328349e+01 6.117861e+01 6.171800e+01 5.587643e+01 6.132417e+01
#>  [581] 5.748230e+01 6.178281e+01 7.170113e+01 6.454339e+01 7.206414e+01
#>  [586] 5.760901e+01 7.647993e+01 6.954876e+01 6.609468e+01 6.971745e+01
#>  [591] 6.423395e+01 5.660727e+01 7.426639e+01 2.143983e+02 6.803651e+01
#>  [596] 8.789953e+01 6.795301e+01 5.721551e+01 8.859536e+01 2.299758e+02
#>  [601] 6.026985e+01 9.591723e+01 8.713056e+01 8.930513e+01 9.314178e+01
#>  [606] 5.928255e+01 6.087043e+01 8.378387e+01 9.678518e+01 1.565686e+02
#>  [611] 7.037941e+01 7.254615e+01 8.762681e+01 6.343492e+01 1.149819e+03
#>  [616] 5.235676e+01 1.845707e+02 5.599750e+01 6.987796e+01 6.435737e+01
#>  [621] 5.977594e+01 7.359690e+01 6.540704e+01 8.320220e+01 6.081145e+01
#>  [626] 1.499791e+02 5.371071e+01 6.632726e+01 6.097585e+01 5.659034e+01
#>  [631] 6.091960e+01 6.084831e+01 6.194975e+01 5.993795e+01 5.905761e+01
#>  [636] 6.841172e+01 9.105614e+01 6.940130e+01 5.859605e+01 6.232603e+01
#>  [641] 7.158180e+01 6.724682e+01 7.277534e+01 5.915513e+01 1.081612e+02
#>  [646] 6.549625e+01 7.375292e+01 6.752424e+01 5.495247e+01 7.629043e+01
#>  [651] 7.962300e+01 7.483846e+01 1.104248e+04 2.988629e+03 1.573681e+02
#>  [656] 6.914916e+01 5.452145e+01 5.627759e+01 1.314999e+02 6.281199e+01
#>  [661] 6.837508e+01 6.657774e+01 7.087369e+01 7.349174e+01 6.293778e+01
#>  [666] 6.987723e+01 7.530790e+01 7.322568e+02 6.873151e+02 3.443275e+03
#>  [671] 1.210353e+02 5.114969e+01 6.262985e+01 1.732625e+02 9.900353e+01
#>  [676] 6.218168e+01 6.603799e+01 1.113124e+05 9.138260e+01 9.880469e+01
#>  [681] 6.480115e+01 6.754981e+01 5.633943e+01 8.682790e+01 6.120318e+01
#>  [686] 6.620159e+01 1.055678e+02 1.264926e+02 1.035556e+02 5.641359e+01
#>  [691] 6.337162e+01 8.392680e+01 6.461173e+01 6.153198e+01 5.937267e+01
#>  [696] 6.593672e+01 6.687592e+01 6.353450e+01 1.064045e+02 5.890996e+01
#>  [701] 9.209639e+01 1.110866e+02 6.125698e+01 8.380981e+01 3.669911e+02
#>  [706] 3.175836e+02 5.971239e+01 9.658944e+01 7.452571e+01 5.635069e+01
#>  [711] 1.275239e+02 9.822970e+01 7.823404e+01 6.400708e+01 6.493504e+01
#>  [716] 5.675456e+01 5.738718e+01 6.840732e+01 6.881469e+01 8.397559e+01
#>  [721] 7.194836e+01 6.327085e+01 6.640090e+01 3.976015e+02 6.449418e+01
#>  [726] 7.389653e+01 7.542262e+01 7.157415e+01 7.952282e+01 6.808152e+01
#>  [731] 5.839597e+01 4.789668e+01 4.918024e+01 6.457873e+01 6.094484e+01
#>  [736] 6.438290e+01 1.000186e+02 6.214635e+01 5.207498e+01 5.918243e+01
#>  [741] 6.955681e+01 6.579027e+01 1.569542e+02 5.736011e+01 2.273691e+02
#>  [746] 6.541716e+01 6.235836e+01 6.211971e+01 6.452465e+01 7.752493e+01
#>  [751] 5.976433e+01 9.966989e+01 5.960315e+01 6.770859e+01 7.244776e+01
#>  [756] 5.737609e+01 6.181458e+01 6.897429e+01 7.173674e+01 7.688717e+01
#>  [761] 7.697106e+01 6.796162e+01 8.134984e+01 3.748596e+03 6.647271e+01
#>  [766] 7.313143e+01 9.715202e+01 6.070471e+01 1.101739e+02 7.605371e+01
#>  [771] 6.568672e+01 1.145260e+02 7.343004e+01 5.782948e+01 6.278453e+01
#>  [776] 6.648406e+01 1.443137e+02 1.815884e+02 5.700288e+01 5.887116e+01
#>  [781] 6.466407e+01 2.868807e+02 6.318249e+01 6.612578e+01 9.569301e+01
#>  [786] 1.727374e+02 2.233730e+02 6.939372e+01 9.779893e+01 1.060869e+02
#>  [791] 6.288612e+01 5.501672e+01 8.444885e+01 7.459789e+01 5.912247e+01
#>  [796] 6.490047e+01 6.181948e+01 8.144604e+01 6.514176e+01 1.421046e+02
#>  [801] 7.560819e+01 7.011371e+01 6.141982e+01 8.047069e+01 7.745609e+01
#>  [806] 8.755806e+01 6.106907e+01 6.362178e+01 7.366850e+01 5.530499e+01
#>  [811] 7.558880e+01 5.843559e+01 6.038143e+01 5.522150e+01 5.809189e+01
#>  [816] 7.866500e+01 5.685909e+01 5.540445e+01 5.881506e+01 5.382924e+01
#>  [821] 1.383865e+02 7.898001e+01 1.345801e+02 6.326698e+01 5.035123e+01
#>  [826] 6.729091e+01 2.831666e+02 1.362173e+02 7.896997e+01 6.745358e+01
#>  [831] 7.735401e+01 8.555800e+01 6.101755e+01 7.074560e+01 1.753437e+02
#>  [836] 5.461714e+01 7.426627e+01 6.260237e+01 9.503160e+01 6.175176e+01
#>  [841] 6.452995e+01 6.596093e+01 1.339707e+02 5.937856e+01 8.675467e+01
#>  [846] 7.248625e+01 1.741517e+02 5.681747e+01 5.682206e+01 5.952937e+01
#>  [851] 6.008116e+01 6.358811e+01 9.080051e+01 6.799280e+01 5.757547e+01
#>  [856] 1.158070e+02 1.630248e+02 6.100317e+01 5.443086e+01 6.180158e+01
#>  [861] 6.149980e+01 6.132414e+01 6.535600e+01 7.172506e+01 6.605893e+01
#>  [866] 5.310856e+01 7.114179e+01 5.933656e+01 4.407569e+01 5.131567e+01
#>  [871] 5.564989e+01 9.687389e+01 6.056189e+01 5.977921e+01 6.868936e+01
#>  [876] 6.221275e+01 8.091798e+01 6.772507e+01 7.654895e+01 2.735663e+02
#>  [881] 7.305671e+01 5.662147e+01 6.636400e+01 2.202361e+02 9.168753e+01
#>  [886] 1.224374e+02 1.101868e+02 1.131707e+02 7.983957e+01 4.478176e+02
#>  [891] 5.790061e+01 6.017488e+01 5.675406e+01 1.281804e+02 7.278471e+01
#>  [896] 6.865800e+01 1.058706e+02 9.925140e+01 9.177294e+01 5.538998e+01
#>  [901] 2.684223e+02 1.153548e+02 5.295743e+01 9.043585e+01 6.371439e+01
#>  [906] 7.158894e+01 5.729921e+01 1.073627e+02 2.752614e+02 1.645455e+02
#>  [911] 7.774944e+01 6.212533e+01 1.640323e+02 6.345847e+01 5.872178e+01
#>  [916] 6.002972e+01 1.289323e+02 9.169123e+01 6.542647e+01 5.873026e+01
#>  [921] 6.508934e+01 5.692109e+01 6.037956e+01 7.634979e+01 7.833420e+01
#>  [926] 5.712486e+01 6.463980e+01 5.895453e+01 6.187221e+01 6.514745e+01
#>  [931] 9.668879e+01 5.647940e+01 1.163207e+02 8.426911e+01 5.651571e+01
#>  [936] 6.224130e+01 6.631604e+01 5.719326e+01 1.869760e+02 1.213026e+02
#>  [941] 1.010901e+02 9.200223e+01 7.888435e+01 6.173114e+01 5.601499e+01
#>  [946] 1.203397e+02 6.350970e+01 9.494733e+01 6.562375e+01 5.388056e+01
#>  [951] 5.373117e+01 8.316356e+01 5.554808e+01 5.796712e+01 7.927693e+01
#>  [956] 6.327537e+02 1.275027e+02 1.260635e+02 1.267114e+02 1.618371e+02
#>  [961] 5.788803e+01 8.975802e+01 8.031813e+01 5.167420e+01 9.538505e+01
#>  [966] 7.127957e+01 6.653350e+01 5.418550e+01 6.005000e+01 6.512484e+01
#>  [971] 5.935449e+01 6.666948e+01 6.055615e+01 6.339853e+01 9.957993e+01
#>  [976] 6.582973e+01 5.731850e+01 6.089726e+01 7.635786e+01 5.709802e+01
#>  [981] 8.475318e+01 8.366105e+01 5.384070e+01 7.200591e+01 6.128180e+01
#>  [986] 6.217133e+01 8.713501e+01 6.348986e+01 6.237905e+01 7.822411e+01
#>  [991] 9.242743e+01 5.300998e+01 7.241594e+01 5.236372e+01 5.855045e+01
#>  [996] 6.074560e+01 1.084012e+02 6.230604e+01 5.633217e+01 9.238312e+01
```
