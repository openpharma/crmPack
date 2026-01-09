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
#>  [1]  26.88134  91.47799  91.47799  91.47799  91.47799 137.95472 137.95472
#>  [8] 137.95472 137.95472 137.95472 137.95472 137.95472 137.95472  31.91824
#> [15]  31.91824  31.91824  31.91824  31.91824 132.84418 220.75407

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
#>    [1] 8.997239e+01 7.143618e+01 6.284528e+01 5.703996e+01 5.990443e+01
#>    [6] 5.906142e+01 1.371219e+02 7.518643e+01 1.042842e+02 6.239650e+01
#>   [11] 5.710460e+01 6.283400e+01 5.498061e+01 6.008739e+01 6.075970e+01
#>   [16] 7.063220e+01 6.942586e+01 8.851308e+01 6.040192e+01 5.829670e+01
#>   [21] 6.656252e+01 7.136207e+01 5.987633e+01 6.679869e+01 6.629442e+01
#>   [26] 8.262445e+01 5.954781e+01 7.230408e+01 7.267974e+01 6.172480e+01
#>   [31] 6.820380e+01 6.072477e+01 8.388789e+01 7.583656e+01 5.715745e+01
#>   [36] 6.215774e+01 5.683784e+01 6.410492e+01 9.362144e+01 2.091727e+02
#>   [41] 6.537001e+01 5.839633e+01 5.945342e+01 6.393407e+01 5.553494e+01
#>   [46] 5.632813e+01 6.766737e+01 5.908722e+01 6.454278e+01 6.090722e+01
#>   [51] 6.234646e+01 7.186320e+01 7.620044e+01 5.802962e+01 6.997083e+01
#>   [56] 5.927812e+01 1.351635e+02 5.970195e+01 1.156095e+02 7.767250e+01
#>   [61] 6.833796e+01 5.983795e+01 1.271166e+02 7.030226e+01 6.013359e+01
#>   [66] 6.945035e+01 5.528571e+01 8.216549e+01 6.615735e+01 7.869700e+01
#>   [71] 1.518766e+02 1.399303e+02 6.596049e+01 5.975203e+01 5.982802e+01
#>   [76] 6.611703e+01 6.660438e+01 6.356503e+01 7.087636e+01 1.315501e+02
#>   [81] 5.934677e+01 7.765434e+01 6.587722e+01 5.832364e+01 6.698486e+01
#>   [86] 7.880904e+01 1.073017e+02 7.600004e+01 1.056283e+02 7.052369e+01
#>   [91] 8.040958e+01 9.938725e+01 5.939319e+01 1.984121e+02 1.314982e+02
#>   [96] 6.929758e+01 1.983930e+02 7.141855e+01 5.485641e+01 9.659977e+01
#>  [101] 6.771943e+01 6.244185e+01 6.306989e+01 6.270190e+01 6.153499e+01
#>  [106] 7.331280e+01 6.398770e+01 1.992636e+02 6.934820e+01 6.601123e+01
#>  [111] 6.815286e+01 9.225080e+01 9.454723e+02 7.694644e+01 5.435512e+01
#>  [116] 6.818087e+01 6.849343e+01 6.739330e+01 7.764304e+01 7.206717e+01
#>  [121] 1.079117e+02 8.213750e+01 1.211204e+02 1.251380e+02 9.573368e+01
#>  [126] 1.286603e+02 7.832896e+01 6.807273e+01 9.271559e+01 5.546058e+01
#>  [131] 1.073014e+02 1.168628e+02 7.482230e+02 1.404657e+02 6.441151e+01
#>  [136] 6.818451e+01 8.930588e+01 5.809203e+01 1.129536e+02 5.763025e+01
#>  [141] 5.686495e+01 6.093732e+01 1.214670e+02 7.416122e+01 5.956813e+01
#>  [146] 9.279823e+01 6.212324e+02 5.843454e+01 1.118565e+02 7.583808e+01
#>  [151] 1.288419e+02 2.499658e+02 7.329737e+01 6.235448e+01 8.974202e+01
#>  [156] 7.581155e+01 7.300680e+01 5.191051e+01 6.824187e+01 8.001823e+01
#>  [161] 7.473991e+01 5.888737e+01 5.738154e+01 5.623830e+01 5.934991e+01
#>  [166] 6.492431e+01 7.413414e+01 8.913621e+01 5.214009e+01 6.874977e+01
#>  [171] 6.065711e+01 5.735066e+01 7.138757e+01 7.198082e+01 9.166398e+01
#>  [176] 1.121462e+02 5.321279e+01 5.676706e+01 1.152379e+02 6.543900e+01
#>  [181] 5.628445e+01 5.877600e+01 9.879349e+01 5.518837e+01 6.732707e+01
#>  [186] 8.378819e+01 5.890040e+01 7.088877e+01 5.829496e+01 6.451734e+01
#>  [191] 8.749722e+01 6.268440e+01 7.143906e+01 5.656868e+01 6.457964e+01
#>  [196] 7.673364e+01 5.567069e+01 1.313991e+04 1.095829e+02 6.380143e+01
#>  [201] 7.402198e+01 6.041522e+01 7.229281e+01 6.957781e+01 5.964214e+01
#>  [206] 5.957162e+01 6.979473e+01 6.044217e+01 5.437928e+01 7.993004e+01
#>  [211] 1.428638e+02 5.294727e+01 6.750762e+01 6.314169e+01 6.435956e+01
#>  [216] 5.982468e+01 6.515564e+01 7.041670e+01 7.666089e+01 8.134583e+01
#>  [221] 1.102754e+02 5.362993e+01 5.532244e+01 6.819939e+02 8.435695e+01
#>  [226] 6.506481e+01 6.907214e+01 6.160961e+01 5.645976e+01 1.191012e+02
#>  [231] 8.315875e+01 1.123947e+02 5.891331e+01 7.582733e+01 7.502520e+01
#>  [236] 7.729971e+01 6.322624e+01 6.783228e+01 5.680682e+01 1.210460e+02
#>  [241] 6.115163e+01 6.077771e+01 6.456094e+01 6.092583e+01 5.636194e+01
#>  [246] 1.292811e+02 1.689923e+02 6.099553e+01 6.573045e+01 6.797129e+01
#>  [251] 6.376544e+01 7.205078e+01 6.375483e+01 7.924027e+01 9.214219e+01
#>  [256] 7.170337e+01 7.104532e+01 1.271873e+02 1.114199e+02 6.243329e+01
#>  [261] 6.112239e+01 7.004185e+01 6.549410e+01 6.340303e+01 5.675047e+01
#>  [266] 6.496037e+01 7.780799e+01 7.676506e+01 5.788134e+01 8.075204e+01
#>  [271] 5.844012e+01 6.456186e+01 3.315781e+02 3.803221e+02 5.296291e+01
#>  [276] 1.062834e+02 2.774301e+03 5.711187e+01 1.142683e+02 5.858055e+01
#>  [281] 2.476070e+02 2.280957e+02 7.079575e+01 6.518693e+01 6.019401e+01
#>  [286] 6.531577e+01 6.202583e+01 5.646939e+01 6.477980e+01 5.948590e+01
#>  [291] 5.865374e+01 9.931596e+01 5.545739e+01 1.441998e+02 2.803898e+02
#>  [296] 8.829387e+01 8.197063e+01 5.716041e+01 9.262736e+01 6.525629e+01
#>  [301] 5.794447e+01 6.408790e+01 7.339260e+01 7.568896e+01 5.398557e+01
#>  [306] 5.457442e+01 8.281865e+01 6.218011e+01 6.161054e+01 7.241383e+01
#>  [311] 6.107781e+01 6.264717e+01 1.941215e+02 6.985298e+01 9.433841e+01
#>  [316] 6.627776e+01 6.484770e+01 7.559277e+01 1.236926e+02 1.112373e+04
#>  [321] 6.337629e+01 6.812209e+01 6.162528e+01 6.191965e+01 7.964725e+01
#>  [326] 5.583178e+01 6.407256e+01 8.355075e+01 4.484818e+01 6.872524e+01
#>  [331] 6.105364e+01 7.067650e+01 6.469409e+01 8.238640e+01 5.114967e+01
#>  [336] 5.638787e+01 5.925629e+01 6.601824e+01 5.991517e+01 5.824965e+01
#>  [341] 5.718188e+01 6.418497e+01 9.771833e+01 6.983395e+01 6.121073e+01
#>  [346] 5.883587e+01 6.453156e+01 5.746035e+01 6.455560e+01 6.149511e+01
#>  [351] 5.828335e+01 5.420185e+01 5.851027e+01 5.617742e+01 6.533335e+01
#>  [356] 7.690707e+01 6.513863e+01 5.407153e+01 6.159003e+01 6.448640e+01
#>  [361] 8.845848e+01 2.560160e+02 7.569333e+01 6.400168e+01 5.285580e+01
#>  [366] 7.372530e+01 6.772932e+01 7.581007e+01 6.490058e+01 5.944363e+01
#>  [371] 1.268665e+02 1.275016e+02 5.202156e+01 5.804638e+01 1.067290e+02
#>  [376] 7.309733e+01 5.931328e+01 6.069318e+01 7.623880e+01 5.865970e+01
#>  [381] 7.231043e+01 5.264683e+01 2.156098e+02 1.227060e+02 3.342444e+04
#>  [386] 5.891830e+01 5.915449e+01 5.454171e+01 3.066544e+02 6.576879e+01
#>  [391] 1.019084e+02 5.693261e+01 6.351354e+01 6.027621e+01 6.439025e+01
#>  [396] 5.674151e+01 7.312313e+01 4.128319e+02 1.259168e+02 1.263213e+05
#>  [401] 5.191070e+01 5.739806e+01 7.848542e+01 9.393910e+01 7.254362e+01
#>  [406] 7.282004e+01 6.069123e+01 5.829278e+01 6.473711e+01 8.752529e+01
#>  [411] 6.424389e+01 7.755267e+01 6.404376e+01 5.767100e+01 8.468824e+01
#>  [416] 6.499820e+01 9.728360e+01 8.138185e+01 6.734274e+01 5.659914e+01
#>  [421] 9.852347e+01 6.090051e+01 6.665196e+01 6.826014e+01 6.044866e+01
#>  [426] 9.167776e+01 5.500639e+01 6.335039e+01 5.870069e+01 5.649193e+01
#>  [431] 1.500511e+02 6.165577e+01 6.454439e+01 5.577055e+01 6.361955e+01
#>  [436] 5.807926e+01 5.710663e+01 1.168238e+02 5.864734e+01 7.462906e+01
#>  [441] 7.194996e+01 6.503412e+01 5.493369e+01 6.001064e+01 9.220850e+01
#>  [446] 5.017207e+01 4.772527e+01 5.185095e+01 1.601991e+02 4.165034e+01
#>  [451] 1.629345e+02 8.631352e+01 6.905872e+01 6.534897e+01 5.943246e+01
#>  [456] 7.675694e+01 7.842571e+01 5.977238e+01 5.246778e+01 5.953377e+01
#>  [461] 6.957893e+01 1.192882e+02 6.011809e+01 8.160502e+01 1.126542e+02
#>  [466] 7.689337e+02 1.204736e+02 1.945590e+02 8.734850e+02 5.690227e+01
#>  [471] 6.204117e+01 3.633040e+02 8.940016e+01 7.432386e+01 6.449627e+01
#>  [476] 5.865040e+01 1.312027e+02 3.871789e+06 2.240372e+02 3.009106e+02
#>  [481] 1.944692e+02 3.000273e+05 5.658801e+01 6.332931e+01 2.095781e+02
#>  [486] 1.530643e+02 2.444013e+02 6.348906e+01 6.028396e+01 6.257942e+01
#>  [491] 6.607287e+01 7.399426e+01 7.242565e+01 1.207533e+02 5.764279e+01
#>  [496] 6.128768e+01 6.366032e+01 7.742215e+01 6.253149e+01 1.578738e+02
#>  [501] 5.476300e+01 5.752355e+01 5.848868e+01 6.980356e+01 6.580776e+01
#>  [506] 9.126658e+01 1.896685e+02 1.212468e+02 6.332454e+01 8.019764e+01
#>  [511] 5.660413e+01 1.124453e+02 7.783323e+01 8.102284e+01 5.886020e+01
#>  [516] 6.044496e+01 1.465557e+02 5.872361e+01 6.550937e+01 6.863601e+01
#>  [521] 6.149886e+01 1.174867e+02 6.713995e+01 2.157131e+02 7.085818e+01
#>  [526] 6.138015e+01 7.573333e+01 8.973651e+01 6.028747e+01 6.032271e+01
#>  [531] 6.273093e+01 6.295855e+01 6.077850e+01 9.722371e+01 6.479437e+01
#>  [536] 5.901572e+01 6.207472e+01 7.384616e+01 6.225079e+01 5.749748e+01
#>  [541] 7.215368e+01 7.005602e+01 6.428375e+01 6.937291e+01 6.066716e+01
#>  [546] 6.301595e+01 6.510456e+01 5.921246e+01 5.972419e+01 6.279041e+01
#>  [551] 1.100092e+02 8.338373e+01 7.245547e+01 5.658889e+01 8.438380e+01
#>  [556] 6.727382e+01 1.187487e+02 6.635929e+01 5.524039e+01 7.039520e+01
#>  [561] 7.305201e+01 7.223533e+01 5.862260e+01 5.978958e+01 5.748624e+01
#>  [566] 2.161427e+02 6.984566e+01 6.117705e+01 1.036755e+02 1.431240e+02
#>  [571] 7.050375e+01 5.436892e+01 8.767246e+01 8.114813e+01 8.217489e+01
#>  [576] 6.442086e+01 6.200131e+01 6.692123e+01 6.537774e+01 6.251795e+01
#>  [581] 7.647662e+01 6.548668e+01 7.769084e+01 5.915816e+01 5.753964e+01
#>  [586] 5.613492e+01 6.876783e+01 6.655343e+01 5.710185e+01 5.472259e+01
#>  [591] 6.010257e+01 5.969474e+01 7.937734e+01 6.016712e+01 6.169065e+01
#>  [596] 6.615094e+01 7.072586e+01 6.426646e+01 8.862265e+01 6.229980e+01
#>  [601] 6.959165e+01 6.032977e+01 6.406794e+01 5.628682e+01 7.067434e+01
#>  [606] 7.090100e+01 5.918975e+01 5.586864e+01 7.781791e+01 6.116329e+01
#>  [611] 6.257228e+01 7.347976e+01 6.444543e+01 5.779520e+01 1.420762e+02
#>  [616] 5.829001e+01 7.866125e+01 6.063724e+01 1.246153e+02 5.830988e+03
#>  [621] 6.943469e+01 6.868101e+01 8.030583e+01 6.967208e+01 6.327984e+01
#>  [626] 8.733626e+01 5.713914e+01 7.766629e+01 7.855537e+01 6.658086e+01
#>  [631] 7.413358e+01 5.385362e+01 7.188289e+01 7.023747e+01 6.503090e+01
#>  [636] 7.765364e+01 5.555561e+01 8.038848e+01 5.859499e+01 6.359748e+01
#>  [641] 8.652893e+01 5.386905e+01 6.280285e+01 5.773065e+01 1.903197e+02
#>  [646] 8.746294e+01 6.644138e+01 6.352600e+01 6.806871e+01 2.019010e+02
#>  [651] 1.067289e+02 6.519152e+01 5.908781e+01 6.276080e+01 1.180886e+02
#>  [656] 7.232193e+01 7.931693e+01 6.334030e+01 6.169077e+01 1.885673e+02
#>  [661] 7.718369e+01 6.527570e+01 1.054138e+02 9.336368e+01 5.674324e+01
#>  [666] 6.986989e+01 6.962345e+01 8.522577e+01 5.206129e+01 1.328317e+02
#>  [671] 5.653901e+01 9.555311e+01 7.181667e+01 5.455855e+01 8.442961e+01
#>  [676] 6.423119e+01 6.082532e+01 1.163220e+02 9.412307e+01 5.720758e+01
#>  [681] 5.904922e+01 1.066612e+02 6.900359e+01 1.193643e+02 5.403742e+01
#>  [686] 2.560920e+02 7.859402e+02 2.859413e+02 6.620894e+01 5.861459e+01
#>  [691] 1.799790e+03 6.281535e+01 5.756082e+01 6.517738e+01 5.695800e+01
#>  [696] 6.605051e+01 5.476105e+01 6.318468e+01 7.323357e+01 6.147737e+01
#>  [701] 6.239996e+01 5.947077e+01 7.130567e+01 6.182833e+01 5.731536e+01
#>  [706] 7.191684e+01 5.651313e+01 5.546574e+01 6.819998e+01 6.353917e+01
#>  [711] 6.853118e+01 1.055891e+02 1.007923e+02 8.572911e+01 4.982618e+01
#>  [716] 7.202929e+01 5.782722e+01 7.802333e+01 7.227633e+01 6.066272e+01
#>  [721] 8.912987e+01 1.259105e+02 5.900504e+01 6.411935e+01 6.180559e+01
#>  [726] 6.578291e+01 6.015244e+01 7.743306e+01 8.032215e+01 6.387840e+01
#>  [731] 7.083194e+01 6.782308e+01 7.322069e+01 7.874589e+01 5.655620e+01
#>  [736] 1.648158e+02 6.863655e+01 9.018507e+01 7.814557e+01 1.271437e+03
#>  [741] 1.728504e+02 5.821692e+01 6.103210e+01 6.363037e+01 6.066696e+01
#>  [746] 7.158190e+01 5.690519e+01 1.062092e+02 7.494786e+01 6.271390e+01
#>  [751] 5.982461e+01 2.445754e+02 1.102844e+02 5.736250e+01 6.125848e+01
#>  [756] 6.021833e+01 6.734260e+01 5.808100e+01 5.720675e+01 6.084825e+01
#>  [761] 5.850200e+01 6.132222e+01 6.347639e+01 5.790859e+01 5.943722e+01
#>  [766] 7.093101e+01 8.697551e+01 5.766271e+01 7.499869e+01 7.389688e+01
#>  [771] 6.331203e+01 6.088573e+01 6.886362e+01 1.188510e+02 8.338625e+01
#>  [776] 8.089840e+01 8.176387e+01 6.344628e+01 6.743604e+01 5.684082e+01
#>  [781] 6.013182e+01 5.959100e+01 7.060947e+01 1.074736e+02 2.007357e+02
#>  [786] 6.845687e+01 5.854611e+01 6.354162e+01 5.826118e+01 5.725296e+01
#>  [791] 6.407237e+01 5.455574e+01 5.507920e+01 6.642331e+01 8.699964e+01
#>  [796] 5.866403e+01 7.738632e+01 7.280974e+01 6.953529e+01 6.205147e+01
#>  [801] 6.414905e+01 3.636381e+02 5.690819e+01 6.531462e+01 6.485750e+01
#>  [806] 8.055471e+01 5.666734e+01 7.421541e+01 5.743862e+01 6.678550e+01
#>  [811] 8.979772e+01 8.132828e+01 8.626486e+01 7.024230e+01 7.059402e+01
#>  [816] 6.121544e+01 6.437067e+01 1.235772e+02 5.609567e+01 6.067450e+01
#>  [821] 7.971800e+01 7.673069e+01 7.805160e+01 6.180865e+01 5.684707e+01
#>  [826] 8.504053e+01 6.094969e+01 5.946115e+01 8.377848e+01 1.502569e+02
#>  [831] 1.032208e+02 2.224216e+02 3.733306e+03 1.398040e+04 7.123429e+02
#>  [836] 1.101496e+02 8.465171e+01 6.267748e+01 6.199898e+01 5.567682e+01
#>  [841] 7.498384e+01 5.819887e+01 7.983736e+01 5.417581e+01 6.526976e+01
#>  [846] 7.404966e+01 1.430078e+02 5.128138e+01 4.949178e+01 5.280485e+01
#>  [851] 1.640705e+02 6.429237e+01 8.921162e+01 5.651381e+01 4.592892e+02
#>  [856] 1.470587e+02 5.656168e+01 7.451295e+01 6.719322e+01 1.098580e+02
#>  [861] 6.377388e+01 9.940741e+01 6.496019e+01 6.721556e+01 6.127237e+01
#>  [866] 5.243360e+01 5.641622e+01 6.681470e+01 5.752522e+01 7.888569e+01
#>  [871] 6.071036e+01 8.360009e+01 6.667460e+01 5.692513e+01 5.784382e+01
#>  [876] 6.893391e+01 7.516533e+01 6.483941e+01 5.745869e+01 1.024005e+02
#>  [881] 1.556407e+02 5.628195e+01 1.365540e+02 5.358507e+08 1.080790e+02
#>  [886] 5.728162e+01 9.087776e+01 5.462076e+01 7.281670e+01 5.783312e+01
#>  [891] 6.811397e+01 7.202207e+01 6.397900e+01 9.044687e+01 5.768588e+01
#>  [896] 5.945074e+01 8.598005e+01 7.141630e+01 5.816417e+01 6.129963e+01
#>  [901] 5.935885e+01 6.663292e+01 6.374554e+01 6.066933e+01 7.485910e+01
#>  [906] 6.423480e+01 6.161961e+01 1.101061e+02 6.859430e+01 7.859064e+01
#>  [911] 1.005877e+02 7.501114e+01 9.158893e+01 7.325059e+01 6.108935e+01
#>  [916] 6.616126e+01 5.736336e+01 6.450940e+01 6.558343e+01 5.981435e+01
#>  [921] 7.365056e+01 5.464736e+01 7.260111e+01 7.304116e+01 8.870240e+01
#>  [926] 7.445101e+01 5.128344e+01 7.691967e+01 7.023106e+01 8.000282e+01
#>  [931] 5.074179e+01 9.231758e+01 6.079216e+01 7.133955e+01 4.718961e+01
#>  [936] 5.957188e+01 6.107174e+01 6.689777e+01 7.003422e+01 2.600857e+02
#>  [941] 6.032894e+01 9.559015e+01 7.339782e+01 6.584374e+01 5.745856e+01
#>  [946] 6.352644e+01 1.189173e+02 5.792143e+01 5.609019e+01 7.020800e+01
#>  [951] 5.056942e+01 7.243704e+01 2.788274e+02 8.107089e+01 1.535378e+03
#>  [956] 2.883347e+03 6.163546e+01 8.063719e+01 7.592216e+01 9.063573e+01
#>  [961] 5.686330e+01 6.527501e+01 6.371817e+01 6.450678e+01 6.063197e+01
#>  [966] 5.690288e+01 5.555993e+01 6.457738e+01 6.135485e+01 5.679262e+01
#>  [971] 6.574163e+01 6.322818e+01 7.915393e+01 8.618616e+01 5.897915e+01
#>  [976] 9.565510e+01 5.076826e+01 5.767910e+01 5.999337e+01 8.403627e+01
#>  [981] 6.364197e+01 6.827131e+01 1.140504e+02 1.208231e+03 8.162287e+01
#>  [986] 7.336876e+01 5.418537e+01 6.487788e+01 8.575006e+01 5.595101e+01
#>  [991] 2.221309e+02 6.746631e+01 5.669868e+01 6.288651e+01 7.044560e+01
#>  [996] 6.466612e+01 6.101551e+01 5.776385e+01 2.984135e+02 5.616853e+01
```
