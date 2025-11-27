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
#>  [1] 157.76630  49.10009  33.29373  38.10883  42.71730  30.82150  30.82150
#>  [8]  30.82150  36.41321  36.41321 137.31646 137.31646  22.22343  22.22343
#> [15]  22.22343  94.81048  94.81048  94.81048  44.18159  44.18159

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
#>    [1] 5.682414e+01 6.550195e+01 6.341541e+01 6.001199e+01 6.318794e+01
#>    [6] 5.265376e+01 6.909438e+01 6.024935e+01 6.026454e+01 5.531741e+01
#>   [11] 5.876760e+01 6.602327e+01 7.470101e+01 7.018495e+01 6.761824e+01
#>   [16] 6.035232e+01 6.148681e+01 6.708881e+01 5.776012e+01 7.045319e+01
#>   [21] 5.559971e+01 5.673487e+01 6.293610e+01 7.717742e+01 5.595805e+01
#>   [26] 6.843169e+01 5.356561e+01 6.297735e+01 1.336561e+02 5.885166e+01
#>   [31] 6.359298e+01 6.092464e+01 2.334922e+02 5.792139e+01 5.190462e+01
#>   [36] 9.474214e+01 1.030156e+02 6.762741e+01 7.087818e+01 7.857154e+01
#>   [41] 7.269886e+01 6.507760e+01 6.903430e+01 5.933461e+01 5.243090e+01
#>   [46] 5.607260e+01 6.064325e+01 8.030780e+01 6.132625e+01 7.166788e+01
#>   [51] 6.150266e+01 6.321047e+01 2.132609e+02 6.402645e+01 5.579678e+01
#>   [56] 1.276370e+02 6.649707e+01 5.505388e+01 1.140248e+02 1.000205e+02
#>   [61] 9.960001e+01 1.642550e+02 1.304748e+02 7.794852e+01 5.806549e+01
#>   [66] 9.678620e+01 9.423107e+01 5.942347e+01 5.806268e+02 6.940405e+01
#>   [71] 6.689784e+01 7.012353e+01 6.432474e+01 6.726554e+01 5.843684e+01
#>   [76] 5.955335e+01 7.135708e+02 6.795748e+01 6.392529e+01 6.036901e+01
#>   [81] 6.812387e+01 2.127297e+02 1.491605e+02 1.191099e+02 1.741586e+03
#>   [86] 3.492403e+02 5.239833e+01 6.035010e+01 5.433409e+01 5.287565e+01
#>   [91] 7.048951e+01 2.008807e+02 5.434522e+01 3.769001e+02 7.128647e+01
#>   [96] 5.738964e+01 9.762241e+01 6.498473e+01 6.352040e+01 6.047943e+01
#>  [101] 5.955545e+01 8.259351e+01 6.975725e+01 5.911232e+01 5.577523e+01
#>  [106] 6.539186e+01 9.027878e+01 5.806502e+01 6.025620e+01 6.198739e+01
#>  [111] 6.432854e+01 6.037542e+01 5.932984e+01 5.805807e+01 7.807314e+01
#>  [116] 7.252930e+01 6.016457e+01 6.322694e+01 6.964705e+01 9.628105e+01
#>  [121] 6.450854e+01 5.762992e+01 9.190898e+01 5.712470e+01 1.061422e+02
#>  [126] 9.881083e+01 4.133192e+02 5.875017e+01 1.036393e+02 6.703833e+01
#>  [131] 5.674785e+01 5.497514e+01 5.019625e+01 5.919079e+01 1.214039e+02
#>  [136] 6.169090e+01 5.898202e+01 4.861505e+01 7.095130e+01 6.158666e+01
#>  [141] 8.673922e+01 6.086190e+01 5.732377e+01 9.261897e+01 4.514691e+01
#>  [146] 2.454346e+02 1.053069e+02 6.262186e+01 7.583981e+01 6.391479e+01
#>  [151] 1.072897e+02 6.645581e+01 7.062820e+01 6.274053e+01 6.069831e+01
#>  [156] 7.176242e+01 5.845305e+01 5.539329e+01 5.847932e+01 7.188235e+01
#>  [161] 4.517971e+01 5.130585e+01 8.294321e+01 1.094557e+02 5.807440e+01
#>  [166] 6.080455e+01 5.630512e+01 6.991870e+01 6.337413e+01 6.812046e+01
#>  [171] 2.975607e+02 6.759407e+01 5.766434e+01 5.667833e+01 6.523034e+01
#>  [176] 1.819939e+02 1.288135e+02 1.070510e+02 2.796278e+03 8.892072e+01
#>  [181] 7.551208e+01 1.078187e+02 9.576317e+01 5.326706e+01 6.374805e+01
#>  [186] 1.004472e+02 5.706024e+01 1.679194e+02 8.173052e+01 5.812147e+01
#>  [191] 6.089014e+01 6.079698e+01 9.715557e+01 1.127769e+02 6.979889e+01
#>  [196] 6.210338e+01 5.906570e+01 1.696892e+02 5.549112e+01 7.015327e+01
#>  [201] 2.353096e+02 2.350684e+02 6.787881e+01 8.804155e+01 8.725101e+01
#>  [206] 6.078138e+01 7.450699e+01 7.126488e+01 7.841276e+01 1.165350e+02
#>  [211] 6.013108e+01 6.839929e+01 5.988904e+01 4.489683e+01 8.649506e+01
#>  [216] 7.943638e+01 7.666987e+01 5.199941e+01 6.405881e+01 7.429866e+01
#>  [221] 5.428869e+01 2.762863e+03 6.393394e+01 6.110628e+01 6.261618e+01
#>  [226] 8.265129e+01 5.803316e+01 7.112425e+01 5.951352e+01 6.497917e+01
#>  [231] 6.576337e+01 6.697657e+01 1.203697e+02 5.775702e+01 7.944087e+01
#>  [236] 5.854339e+01 7.275121e+01 6.608469e+01 6.259618e+01 8.539605e+01
#>  [241] 7.003717e+01 8.099893e+01 6.284619e+01 6.549293e+01 6.305267e+01
#>  [246] 6.617675e+01 7.914404e+01 1.039041e+02 5.559773e+01 5.796401e+01
#>  [251] 5.940539e+01 6.433277e+01 5.142564e+01 8.548614e+01 6.535659e+01
#>  [256] 8.460396e+01 5.967582e+01 7.783476e+01 7.475021e+01 5.754180e+01
#>  [261] 6.283811e+01 6.863475e+01 9.030107e+01 6.046875e+01 1.060951e+02
#>  [266] 5.730099e+01 6.651022e+01 5.512104e+01 7.501253e+01 8.712431e+01
#>  [271] 6.581862e+01 5.642442e+01 5.373630e+01 6.040643e+01 5.864354e+01
#>  [276] 8.489837e+01 7.893584e+01 5.926617e+01 5.835809e+01 5.928070e+01
#>  [281] 6.731681e+01 5.564656e+01 7.809586e+01 7.911148e+01 6.173304e+01
#>  [286] 6.491795e+01 6.262727e+01 5.419439e+01 1.310013e+02 7.892711e+01
#>  [291] 2.853721e+02 1.296792e+02 3.012235e+02 8.425132e+01 7.079562e+01
#>  [296] 5.853449e+01 1.652618e+02 5.568293e+01 1.306344e+02 7.948134e+01
#>  [301] 6.096114e+01 8.424383e+01 7.823657e+01 6.029612e+01 5.833689e+01
#>  [306] 5.391724e+01 7.956542e+01 5.603018e+01 6.179928e+01 5.985950e+01
#>  [311] 6.586340e+01 6.442250e+01 9.707418e+01 5.697080e+01 6.006513e+01
#>  [316] 5.709466e+01 7.751236e+01 5.704650e+01 6.141473e+01 5.426054e+01
#>  [321] 7.081293e+01 7.531644e+01 6.846768e+01 6.479569e+01 7.969691e+01
#>  [326] 7.087760e+01 5.605353e+01 8.329727e+01 6.302478e+01 5.730362e+01
#>  [331] 9.701521e+01 1.656766e+02 6.974291e+01 1.121237e+02 6.415715e+01
#>  [336] 7.322056e+01 6.139522e+01 7.604242e+01 3.833270e+01 6.845207e+01
#>  [341] 6.601258e+01 7.506728e+01 6.011243e+01 5.524431e+01 7.191650e+01
#>  [346] 5.987254e+01 6.792683e+01 5.901269e+01 1.178397e+02 7.613016e+01
#>  [351] 5.692016e+01 2.473764e+02 6.768158e+01 5.436209e+01 2.603483e+02
#>  [356] 5.481208e+01 5.348870e+01 9.366358e+01 6.189893e+01 5.751608e+01
#>  [361] 6.592104e+01 8.845805e+01 5.638508e+01 6.488147e+01 1.048814e+02
#>  [366] 5.061608e+01 1.489092e+02 6.152837e+01 1.128070e+02 1.311033e+02
#>  [371] 6.500538e+01 7.367496e+01 6.360810e+01 9.444888e+01 8.720687e+01
#>  [376] 6.287553e+01 2.259158e+02 6.622555e+01 5.770268e+01 6.092028e+01
#>  [381] 7.578377e+01 1.064704e+02 7.676483e+01 7.717854e+01 6.043343e+01
#>  [386] 1.157979e+02 5.996893e+01 7.227297e+01 8.872108e+01 9.913361e+01
#>  [391] 2.408255e+02 5.850649e+01 7.579863e+01 4.587444e+02 1.349999e+02
#>  [396] 6.368860e+01 5.606888e+01 4.636465e+01 6.831049e+01 7.713163e+01
#>  [401] 7.251336e+01 5.900187e+01 8.078742e+01 2.968914e+02 1.333979e+02
#>  [406] 6.114878e+01 3.125362e+02 2.532286e+02 6.108543e+01 6.296896e+01
#>  [411] 8.157021e+01 6.667900e+01 7.205555e+01 6.264239e+01 6.254254e+01
#>  [416] 5.868177e+01 7.137580e+01 5.809309e+01 5.721551e+01 5.479902e+01
#>  [421] 5.772056e+01 5.837778e+01 5.695121e+01 6.563496e+01 6.281227e+01
#>  [426] 1.136051e+02 6.479653e+01 5.822804e+01 5.891417e+01 5.915634e+01
#>  [431] 5.879512e+01 6.193561e+01 6.626503e+01 6.065739e+01 5.645381e+01
#>  [436] 3.739867e+07 6.226675e+01 6.515913e+01 9.548690e+01 7.109783e+01
#>  [441] 6.540943e+01 6.093814e+01 5.724599e+01 7.763493e+01 7.162960e+01
#>  [446] 1.202771e+02 8.878899e+01 5.137245e+01 1.969794e+02 8.511682e+01
#>  [451] 1.666633e+02 9.810709e+01 1.142569e+02 5.910683e+01 7.617868e+01
#>  [456] 7.388532e+01 3.582718e+02 6.070461e+01 8.373435e+01 5.598976e+01
#>  [461] 1.185790e+02 7.202344e+01 3.228270e+02 5.751880e+01 5.880177e+01
#>  [466] 8.579017e+01 5.730918e+01 1.054251e+02 8.280004e+01 5.367884e+01
#>  [471] 4.879966e+01 5.899421e+01 6.253253e+01 8.960530e+01 6.714782e+01
#>  [476] 8.312472e+01 7.128831e+01 6.571694e+01 6.100366e+01 5.874965e+01
#>  [481] 9.162103e+01 3.288407e+02 2.609781e+02 8.953444e+01 7.157749e+01
#>  [486] 6.310270e+01 6.097446e+01 9.919947e+01 1.129157e+02 5.946645e+01
#>  [491] 6.053514e+01 5.987820e+01 6.422997e+01 7.404709e+01 3.951608e+02
#>  [496] 5.606708e+01 8.623645e+01 8.182240e+01 1.364278e+02 7.996776e+01
#>  [501] 7.100451e+01 6.227982e+01 7.128934e+01 5.558390e+01 6.020488e+01
#>  [506] 5.963899e+01 7.140196e+01 7.841586e+01 1.860507e+02 6.719786e+01
#>  [511] 6.231396e+01 6.495267e+01 5.843503e+01 6.054007e+01 7.577073e+01
#>  [516] 4.822969e+02 9.416997e+01 5.339519e+01 7.847347e+01 1.397731e+02
#>  [521] 1.124870e+02 6.316366e+01 6.102118e+01 7.936433e+01 5.873726e+01
#>  [526] 8.369674e+01 5.938451e+01 5.905588e+01 6.074524e+01 7.186991e+01
#>  [531] 7.210736e+01 9.104604e+01 5.825110e+01 1.273002e+02 7.319703e+01
#>  [536] 5.959024e+01 5.857297e+01 7.762929e+01 2.079010e+02 7.265245e+01
#>  [541] 1.044724e+02 7.485531e+01 6.052605e+01 5.978848e+01 7.416854e+01
#>  [546] 5.667950e+01 2.569955e+02 6.022661e+01 6.281673e+01 6.549847e+01
#>  [551] 6.051516e+01 9.723869e+01 8.210275e+01 6.252966e+01 7.441827e+01
#>  [556] 3.796212e+02 6.417132e+01 4.879105e+01 5.457882e+01 6.118507e+01
#>  [561] 5.187507e+01 9.337399e+01 7.218510e+01 5.838125e+01 8.187116e+01
#>  [566] 6.344550e+01 7.006835e+01 6.023226e+01 7.090106e+01 6.406138e+01
#>  [571] 8.421208e+01 3.561519e+02 6.794360e+01 5.520437e+01 5.822283e+01
#>  [576] 5.950447e+01 6.604479e+01 1.133134e+02 6.742226e+01 5.800560e+01
#>  [581] 7.750659e+01 1.049517e+02 1.803658e+02 8.074816e+01 1.102330e+02
#>  [586] 6.631826e+01 8.144774e+01 6.710036e+01 7.893443e+01 7.328761e+01
#>  [591] 9.424627e+01 7.476189e+01 6.006003e+01 8.444229e+01 7.312189e+01
#>  [596] 4.460036e+01 1.911483e+02 1.181073e+02 7.439173e+01 1.177667e+02
#>  [601] 6.574541e+01 7.022311e+01 5.736224e+01 5.828291e+01 9.727228e+01
#>  [606] 6.849301e+01 7.664242e+01 5.862049e+01 2.811091e+02 5.581975e+01
#>  [611] 1.003583e+02 5.439802e+01 6.525589e+01 6.740069e+01 5.765019e+01
#>  [616] 5.772408e+01 8.576448e+01 6.113542e+01 7.791914e+01 5.531059e+01
#>  [621] 6.386731e+01 6.154967e+01 6.906391e+01 1.483837e+02 4.911877e+02
#>  [626] 1.202447e+02 8.451507e+01 5.953988e+01 7.567294e+01 5.824291e+01
#>  [631] 6.389456e+01 6.020206e+01 8.354224e+01 6.993889e+01 5.743867e+01
#>  [636] 6.919145e+01 8.804016e+01 1.475177e+02 5.629853e+01 7.621562e+01
#>  [641] 5.808928e+01 9.540951e+01 6.285303e+01 6.301775e+01 7.685739e+01
#>  [646] 5.561591e+01 6.195416e+01 5.979502e+01 2.035407e+02 2.408294e+02
#>  [651] 5.654691e+01 6.367922e+01 5.499404e+01 1.224926e+12 3.723817e+04
#>  [656] 8.270625e+01 6.097025e+01 5.974778e+01 5.997371e+01 6.276744e+01
#>  [661] 6.315204e+01 5.954797e+01 6.313286e+01 6.029696e+01 6.644507e+01
#>  [666] 7.297111e+01 1.516557e+02 6.314993e+01 6.675833e+02 1.121599e+02
#>  [671] 6.252901e+01 6.475755e+01 1.338592e+02 7.926924e+01 7.701894e+01
#>  [676] 6.024673e+01 8.849354e+01 6.130565e+01 1.160671e+02 6.644259e+01
#>  [681] 8.370870e+01 6.572947e+01 5.904502e+01 6.488350e+01 6.291380e+01
#>  [686] 6.017748e+01 7.603480e+01 5.766292e+01 8.276853e+01 6.442414e+01
#>  [691] 5.574497e+01 7.484845e+01 1.046570e+02 1.895132e+02 7.308977e+01
#>  [696] 5.977738e+01 6.222879e+01 5.900439e+01 5.800658e+01 7.089964e+01
#>  [701] 5.751106e+01 1.219301e+02 7.525691e+01 5.897607e+01 1.001031e+02
#>  [706] 6.903765e+01 5.496977e+01 5.562845e+01 6.628249e+01 9.531194e+01
#>  [711] 1.494796e+02 7.221748e+01 6.849510e+01 1.699829e+02 1.344621e+04
#>  [716] 6.786586e+01 5.319403e+01 6.802731e+01 6.580481e+01 5.599022e+01
#>  [721] 6.550108e+01 6.105223e+01 6.597678e+01 1.521345e+02 8.212738e+01
#>  [726] 3.452054e+02 1.413091e+02 6.611538e+01 4.542671e+02 1.846069e+05
#>  [731] 5.094437e+01 6.368324e+01 5.840626e+01 3.052028e+02 5.579177e+01
#>  [736] 7.949771e+01 5.917503e+01 6.615072e+01 1.097874e+02 8.591153e+01
#>  [741] 6.145453e+01 5.480578e+01 1.119804e+02 6.753661e+01 6.043695e+01
#>  [746] 9.396741e+01 5.929519e+01 7.323014e+01 2.377781e+02 6.058561e+01
#>  [751] 6.451969e+01 6.780752e+01 7.213415e+01 7.973660e+01 6.396817e+01
#>  [756] 6.420422e+01 5.386922e+01 7.832128e+01 5.802350e+01 7.558986e+01
#>  [761] 7.852404e+01 6.140240e+01 7.368326e+01 6.426908e+01 6.212461e+01
#>  [766] 6.067289e+01 5.632427e+01 6.733933e+01 6.771389e+01 5.841775e+01
#>  [771] 1.099342e+02 8.288704e+01 1.866466e+02 6.699265e+01 3.951213e+02
#>  [776] 1.011359e+02 6.249961e+01 5.269943e+01 6.282664e+01 5.915637e+01
#>  [781] 8.303750e+01 7.220326e+01 1.134889e+02 6.607107e+01 8.312015e+01
#>  [786] 5.823655e+01 7.081151e+01 1.106597e+02 6.748278e+01 5.573307e+01
#>  [791] 5.834259e+01 6.048217e+01 5.551641e+01 9.873220e+01 6.222448e+01
#>  [796] 9.517794e+01 9.101612e+01 5.242806e+01 5.799844e+01 1.332420e+02
#>  [801] 2.146733e+02 6.434610e+01 1.255153e+02 6.658739e+01 6.031428e+01
#>  [806] 6.645814e+01 6.535464e+01 5.793013e+01 5.920005e+01 6.200611e+01
#>  [811] 8.551627e+01 5.870926e+01 6.668909e+01 9.233029e+01 1.056974e+02
#>  [816] 1.027931e+02 5.239641e+01 6.519328e+01 6.459365e+01 5.740272e+01
#>  [821] 1.330620e+02 6.463081e+01 5.962197e+01 6.142285e+01 6.189094e+01
#>  [826] 5.802741e+01 5.726324e+01 6.133477e+01 8.190359e+01 8.099866e+01
#>  [831] 6.672900e+01 6.054717e+01 5.988228e+01 6.014253e+01 1.154440e+02
#>  [836] 5.591169e+01 5.800410e+01 7.446418e+01 8.330278e+01 5.231198e+01
#>  [841] 5.638280e+01 5.582513e+01 6.269120e+01 8.267161e+01 7.759351e+01
#>  [846] 6.258371e+01 1.263057e+02 7.617381e+01 9.722629e+01 5.890701e+01
#>  [851] 6.405203e+01 6.793793e+01 1.547436e+02 8.500659e+01 1.840100e+02
#>  [856] 5.719981e+01 6.992165e+01 6.307063e+02 7.742497e+01 6.211499e+01
#>  [861] 7.037233e+01 5.952795e+01 6.438571e+01 5.160819e+01 6.058094e+01
#>  [866] 6.294729e+01 1.044111e+02 1.078558e+02 6.554202e+01 7.616304e+01
#>  [871] 2.656396e+02 6.606515e+01 5.908382e+01 5.765553e+01 5.411608e+01
#>  [876] 7.569042e+01 7.753364e+01 5.347474e+01 6.043478e+01 6.423502e+01
#>  [881] 7.371170e+01 6.067192e+01 6.271530e+01 8.647838e+01 1.628423e+02
#>  [886] 2.072195e+02 1.362405e+02 6.691266e+01 1.616796e+02 7.051142e+01
#>  [891] 5.586364e+01 1.136809e+02 6.043924e+01 2.851686e+02 6.452717e+01
#>  [896] 6.045446e+01 6.609989e+01 8.593304e+01 6.612194e+01 7.780661e+01
#>  [901] 2.448303e+02 8.563993e+01 6.450984e+01 7.576476e+01 6.124660e+01
#>  [906] 6.291900e+01 7.352396e+01 6.303262e+01 1.704146e+02 4.394531e+01
#>  [911] 5.581665e+01 9.116256e+01 1.227545e+02 2.575905e+06 1.074261e+02
#>  [916] 5.632917e+01 6.682234e+01 6.284415e+01 6.991897e+01 6.782281e+01
#>  [921] 7.806652e+01 6.401502e+01 6.340757e+01 8.675592e+01 9.642662e+01
#>  [926] 6.413515e+01 7.716432e+01 6.951102e+01 5.613215e+01 5.950270e+01
#>  [931] 5.910190e+01 6.320809e+01 1.704105e+02 7.968745e+01 6.503424e+01
#>  [936] 6.186765e+01 5.992937e+01 5.619040e+01 6.022223e+01 6.404782e+01
#>  [941] 6.290467e+01 6.034881e+01 6.275093e+01 6.040601e+01 5.732400e+01
#>  [946] 1.748982e+02 5.676176e+01 6.686339e+01 5.742870e+01 8.227753e+01
#>  [951] 9.721600e+01 1.181119e+02 6.241347e+01 6.533595e+01 6.457400e+01
#>  [956] 6.452893e+01 5.923238e+01 6.153572e+01 5.314779e+01 6.207922e+01
#>  [961] 5.981219e+01 6.324999e+01 6.446515e+01 6.658935e+01 6.887543e+01
#>  [966] 7.855420e+01 7.003862e+01 1.281442e+02 6.055536e+01 6.499532e+01
#>  [971] 5.798393e+01 5.816893e+01 7.011230e+01 1.707882e+02 3.074252e+02
#>  [976] 5.900973e+01 8.283119e+01 6.406163e+01 5.936511e+01 6.090713e+01
#>  [981] 6.523249e+01 9.371090e+01 5.190579e+01 6.226712e+01 1.005398e+03
#>  [986] 8.052078e+01 6.340056e+01 1.638074e+02 3.278822e+04 2.827749e+02
#>  [991] 5.899060e+01 1.033792e+02 6.915735e+01 5.733233e+01 5.991829e+01
#>  [996] 6.084659e+01 7.959760e+01 6.687114e+01 6.205133e+01 9.482990e+01
```
