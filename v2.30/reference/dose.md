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
#>  [1]  58.82372 207.02105  11.43579  22.37021  36.99007  36.99007  36.99007
#>  [8]  73.95837  73.95837  73.95837 148.88687 340.89479 340.89479 340.89479
#> [15] 340.89479 340.89479  51.32976  79.33807  79.33807  79.33807

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
#>    [1] 5.376078e+01 5.397708e+01 6.361250e+01 6.642437e+01 7.369504e+01
#>    [6] 7.350023e+01 5.969304e+01 6.062221e+01 9.415424e+01 9.593007e+01
#>   [11] 9.150051e+01 5.855225e+01 9.843584e+01 7.891324e+01 8.632699e+01
#>   [16] 4.651994e+02 6.171547e+01 6.028388e+01 6.676918e+01 6.240286e+01
#>   [21] 7.374548e+01 6.703005e+01 1.697859e+02 6.299597e+01 8.066654e+01
#>   [26] 5.353360e+01 1.943944e+02 5.917752e+01 6.586229e+01 1.014843e+02
#>   [31] 5.556414e+01 6.396268e+01 1.464899e+02 7.429873e+01 1.066233e+02
#>   [36] 5.782422e+01 7.367585e+01 7.576628e+01 7.235248e+01 6.481757e+01
#>   [41] 6.089268e+01 7.542857e+01 5.414756e+01 7.202788e+02 9.411185e+01
#>   [46] 6.053960e+01 5.696234e+01 6.004450e+01 1.419712e+02 6.635423e+01
#>   [51] 5.804125e+01 6.676444e+01 6.064585e+01 5.840213e+01 5.527593e+01
#>   [56] 9.602636e+01 5.861480e+01 4.796305e+09 1.325421e+07 2.576105e+02
#>   [61] 5.850933e+01 6.081656e+01 1.119215e+02 6.266009e+01 7.817199e+01
#>   [66] 5.979307e+01 6.041381e+01 8.672535e+01 6.825297e+01 5.816135e+01
#>   [71] 5.936556e+01 6.144059e+01 6.306536e+01 7.359088e+01 5.844491e+01
#>   [76] 7.122298e+01 5.575808e+01 5.298900e+01 6.662139e+01 4.958172e+01
#>   [81] 6.031160e+01 5.914016e+01 7.507689e+01 5.889855e+01 6.562967e+01
#>   [86] 4.763384e+01 6.396788e+01 7.730507e+01 6.825572e+01 6.633217e+01
#>   [91] 6.626787e+01 7.119359e+01 5.342608e+01 5.625274e+01 5.902492e+01
#>   [96] 7.338706e+01 6.273577e+01 6.130239e+01 6.691148e+01 9.644348e+01
#>  [101] 6.066156e+01 6.365388e+01 6.567279e+01 5.258902e+01 6.190468e+01
#>  [106] 6.276153e+01 1.062181e+02 8.018476e+01 7.376078e+01 7.660868e+01
#>  [111] 6.004975e+01 5.466239e+01 6.666160e+01 6.887907e+01 6.031136e+01
#>  [116] 6.018378e+01 6.878723e+01 8.096293e+01 5.365750e+01 7.523782e+01
#>  [121] 1.980045e+02 6.871335e+01 5.944377e+01 7.120211e+01 7.375040e+01
#>  [126] 7.616945e+01 8.573201e+01 8.261891e+01 5.809468e+01 9.556519e+01
#>  [131] 5.786419e+01 9.643302e+01 8.692553e+01 5.935238e+01 7.058668e+01
#>  [136] 8.490477e+01 5.902418e+01 5.811411e+01 6.206357e+01 6.246817e+01
#>  [141] 7.301547e+01 5.955063e+01 5.410795e+01 6.746092e+01 1.134150e+02
#>  [146] 6.443769e+01 6.896030e+01 6.429556e+01 8.485212e+01 1.148566e+02
#>  [151] 6.806845e+01 1.799053e+02 7.852203e+01 8.586817e+01 5.731997e+01
#>  [156] 5.599435e+01 6.735789e+01 7.885472e+01 5.591313e+01 8.795827e+01
#>  [161] 6.392007e+01 7.056240e+01 1.188913e+02 8.424847e+01 7.641405e+01
#>  [166] 1.011149e+02 7.798030e+01 6.392282e+01 7.395767e+01 7.427785e+01
#>  [171] 5.485505e+01 9.126541e+01 1.353509e+02 5.797161e+01 6.127054e+01
#>  [176] 6.647851e+01 6.168022e+01 7.475218e+01 7.556884e+01 9.058207e+01
#>  [181] 5.314767e+01 5.513639e+01 6.992890e+01 7.362708e+01 5.876357e+01
#>  [186] 1.342469e+02 6.432540e+01 6.291846e+01 6.981822e+01 5.841473e+01
#>  [191] 6.010142e+01 6.530502e+01 7.460028e+01 5.635001e+01 5.920344e+01
#>  [196] 6.809383e+01 7.317822e+01 1.174770e+02 9.745958e+01 6.144888e+01
#>  [201] 8.128864e+02 1.597799e+02 7.074387e+01 6.276272e+01 6.753699e+01
#>  [206] 6.544154e+01 6.048206e+01 5.685942e+01 5.648757e+01 9.533084e+01
#>  [211] 7.157961e+01 6.132127e+01 6.673846e+01 1.098456e+02 6.660424e+01
#>  [216] 9.883084e+01 1.738102e+02 2.158930e+02 5.610029e+01 6.566298e+01
#>  [221] 5.663477e+01 5.459977e+01 6.165763e+01 1.376118e+02 1.388573e+02
#>  [226] 3.944936e+02 8.310272e+01 5.649598e+01 6.334237e+01 6.107527e+01
#>  [231] 6.524820e+01 5.665605e+01 6.485317e+01 7.004941e+01 8.741741e+01
#>  [236] 6.945031e+01 8.633402e+01 6.229807e+01 5.403917e+01 6.597720e+01
#>  [241] 6.311485e+01 5.971146e+01 6.079788e+01 6.524973e+01 6.848833e+01
#>  [246] 6.243937e+01 7.494482e+01 8.461032e+01 1.603801e+02 5.655520e+01
#>  [251] 5.885811e+01 6.119337e+01 1.179628e+02 5.961709e+01 1.735585e+02
#>  [256] 6.697882e+01 9.411174e+01 7.634471e+01 7.891007e+01 9.658050e+01
#>  [261] 7.112289e+01 7.041625e+01 6.291837e+01 5.818884e+01 6.256320e+01
#>  [266] 6.158613e+01 5.942303e+01 5.758808e+01 6.530591e+01 6.172721e+01
#>  [271] 6.362526e+01 1.126421e+02 6.832950e+01 8.092707e+01 6.146702e+01
#>  [276] 6.333565e+01 6.730113e+01 6.265042e+01 6.879326e+01 7.640536e+01
#>  [281] 7.271145e+01 6.038011e+01 6.640456e+01 5.683806e+01 6.417633e+01
#>  [286] 5.980010e+01 5.752783e+01 6.284800e+01 6.191038e+01 6.617306e+01
#>  [291] 7.550106e+01 6.242454e+01 5.938300e+01 5.840646e+01 5.523357e+01
#>  [296] 8.439148e+01 6.803421e+01 1.867790e+02 6.535542e+01 6.916354e+01
#>  [301] 6.736608e+01 6.500110e+01 6.596043e+01 1.197872e+02 6.311620e+01
#>  [306] 6.579412e+01 6.281015e+01 5.889515e+01 7.146638e+01 7.618229e+01
#>  [311] 5.758578e+01 7.084840e+01 6.602302e+01 5.767455e+01 9.347349e+01
#>  [316] 5.441717e+01 5.965803e+01 6.234620e+01 7.039777e+01 5.802752e+01
#>  [321] 8.246727e+01 8.408933e+01 5.679321e+01 5.202897e+01 1.166500e+02
#>  [326] 5.791031e+01 2.828793e+02 5.871864e+01 5.874343e+01 5.446565e+01
#>  [331] 7.368065e+01 5.717262e+01 8.067827e+01 7.165059e+01 6.620413e+01
#>  [336] 3.268956e+02 1.225524e+02 8.138842e+01 6.011421e+01 6.253446e+01
#>  [341] 5.685245e+01 7.674309e+01 5.833166e+01 8.651152e+01 5.273357e+01
#>  [346] 1.489142e+03 7.440148e+01 7.361416e+01 1.191072e+02 7.235085e+01
#>  [351] 1.676588e+02 5.527299e+01 6.826918e+01 1.287071e+02 7.283574e+01
#>  [356] 6.775657e+01 6.359716e+01 1.080531e+02 6.939979e+01 6.580734e+01
#>  [361] 6.482564e+01 5.901680e+01 7.751649e+01 1.153866e+02 9.686388e+01
#>  [366] 8.642198e+01 6.174123e+01 6.200335e+01 5.845284e+01 5.894714e+01
#>  [371] 5.992558e+01 5.184096e+01 7.659195e+01 1.177952e+02 6.626033e+01
#>  [376] 8.553191e+01 5.545145e+01 5.883118e+01 5.962650e+01 1.288778e+02
#>  [381] 6.569777e+01 7.614097e+01 6.638698e+01 6.307535e+01 6.566030e+01
#>  [386] 6.836999e+01 7.424965e+01 6.509959e+01 6.211808e+01 7.839510e+01
#>  [391] 6.749528e+01 6.742132e+01 5.781907e+01 6.106026e+01 5.264275e+01
#>  [396] 8.630458e+01 6.311296e+02 6.283714e+01 6.759814e+01 6.509587e+01
#>  [401] 6.109644e+01 6.849500e+01 6.123751e+01 6.064130e+01 1.332315e+02
#>  [406] 5.854707e+01 1.524006e+02 5.853184e+01 5.737932e+01 1.005316e+02
#>  [411] 7.921964e+01 5.533479e+01 7.661485e+02 8.726388e+01 6.036781e+01
#>  [416] 5.552029e+01 8.352667e+01 8.831392e+01 7.405204e+01 1.457698e+02
#>  [421] 6.211819e+01 5.671082e+01 6.399746e+01 1.685694e+02 4.782681e+02
#>  [426] 5.870803e+01 1.858663e+02 6.627560e+01 6.858137e+01 6.137352e+01
#>  [431] 2.512699e+02 6.179388e+01 7.173973e+01 5.339414e+01 5.146556e+01
#>  [436] 8.602604e+01 6.648419e+04 7.716586e+01 5.056267e+01 5.270723e+01
#>  [441] 6.119727e+01 1.036687e+02 8.544404e+01 6.526501e+01 7.045505e+01
#>  [446] 6.930619e+01 5.778467e+01 7.303114e+01 7.304833e+01 6.355405e+01
#>  [451] 6.108555e+01 6.167599e+01 9.596818e+01 7.551725e+01 1.072590e+02
#>  [456] 6.876780e+01 6.367777e+01 8.808288e+01 8.586657e+01 6.354928e+01
#>  [461] 7.161352e+01 5.930211e+01 6.700011e+01 5.845722e+01 6.610809e+01
#>  [466] 6.332453e+01 1.524004e+02 5.441794e+01 1.027794e+02 6.435385e+01
#>  [471] 5.583564e+01 6.115207e+01 7.823992e+01 6.306303e+01 8.982542e+01
#>  [476] 2.143496e+02 5.006800e+01 8.630869e+01 1.354051e+06 8.168070e+01
#>  [481] 6.395975e+01 7.371422e+01 6.680078e+01 6.013840e+01 5.617753e+01
#>  [486] 1.413212e+02 5.983398e+01 6.446760e+01 5.783635e+01 9.548994e+01
#>  [491] 6.561833e+01 6.379336e+01 7.384017e+01 6.836951e+01 5.588177e+01
#>  [496] 6.805257e+01 5.509882e+01 5.949198e+01 1.061732e+02 6.151459e+01
#>  [501] 6.002461e+01 5.782767e+01 6.103288e+01 5.979302e+01 2.233186e+02
#>  [506] 7.821426e+01 5.895116e+01 5.344048e+01 6.480323e+01 7.130598e+01
#>  [511] 6.862344e+01 5.590561e+01 1.023194e+02 8.512711e+01 1.315626e+02
#>  [516] 5.805596e+01 5.641706e+01 9.711197e+01 1.275265e+02 8.377892e+01
#>  [521] 5.575556e+01 5.577088e+01 1.762211e+02 6.465340e+02 4.343663e+02
#>  [526] 5.606535e+01 5.547794e+01 8.972436e+01 6.305613e+01 4.522766e+02
#>  [531] 5.552244e+01 8.284977e+01 6.480867e+01 7.180121e+01 7.896064e+01
#>  [536] 3.512484e+02 7.709909e+01 5.578237e+01 6.343016e+01 9.657728e+01
#>  [541] 8.644092e+02 8.522472e+01 5.328389e+02 5.301003e+01 1.822393e+02
#>  [546] 7.056513e+01 8.171715e+01 6.814987e+01 6.302118e+01 6.183040e+01
#>  [551] 6.104358e+01 6.516973e+01 6.146214e+01 5.588726e+01 1.012969e+02
#>  [556] 8.015674e+01 6.405293e+01 8.369722e+01 5.365109e+01 7.755615e+01
#>  [561] 5.547822e+01 4.799354e+02 8.792291e+01 1.017326e+02 7.618203e+01
#>  [566] 8.164678e+01 7.516382e+01 1.055871e+02 7.169492e+01 7.532299e+01
#>  [571] 1.143496e+02 6.543034e+01 1.192603e+02 6.392432e+01 6.058029e+01
#>  [576] 8.240588e+01 6.680987e+01 6.723020e+01 6.133122e+01 9.345038e+01
#>  [581] 6.265609e+01 3.407586e+04 9.419061e+01 1.325756e+02 5.565345e+01
#>  [586] 1.062860e+02 5.603899e+01 6.247510e+01 8.908617e+01 5.708624e+01
#>  [591] 1.187133e+02 1.679555e+02 1.006035e+02 7.100896e+01 8.613732e+01
#>  [596] 5.733187e+01 7.542003e+01 8.300709e+01 6.768132e+01 5.744036e+01
#>  [601] 7.054751e+02 1.346713e+02 5.970258e+01 5.919757e+01 5.524038e+01
#>  [606] 6.471855e+01 1.009588e+02 1.059450e+02 6.298043e+01 6.920575e+01
#>  [611] 7.673379e+01 6.030074e+01 5.708851e+01 5.429969e+01 6.207959e+01
#>  [616] 5.709864e+01 5.343822e+01 8.693851e+01 8.325270e+01 7.768021e+01
#>  [621] 5.985399e+01 5.909379e+01 5.608500e+01 5.873928e+01 8.685699e+01
#>  [626] 6.170950e+01 5.919891e+01 6.177542e+01 6.568419e+01 1.224960e+02
#>  [631] 5.629972e+01 9.245257e+01 6.239675e+01 5.996428e+01 5.923587e+01
#>  [636] 8.241318e+01 1.325967e+02 6.606303e+01 1.640705e+02 6.516619e+01
#>  [641] 6.767697e+01 7.758019e+01 7.585072e+01 6.084219e+01 6.968633e+01
#>  [646] 9.555794e+01 5.427630e+01 1.280362e+02 9.234776e+01 9.593704e+01
#>  [651] 5.562461e+01 5.430558e+01 8.105761e+01 9.561967e+01 7.542475e+01
#>  [656] 7.391133e+01 8.745223e+01 5.866146e+01 5.797926e+01 6.553136e+01
#>  [661] 7.118377e+01 6.128020e+01 7.100377e+01 6.080887e+01 5.586290e+01
#>  [666] 6.756426e+01 7.169373e+01 6.103340e+01 6.523288e+01 6.621355e+01
#>  [671] 5.953559e+01 1.190386e+02 6.711761e+01 5.268117e+01 6.323829e+01
#>  [676] 7.024375e+01 5.716101e+01 7.419015e+01 6.144124e+01 5.939899e+01
#>  [681] 7.478650e+01 7.096111e+01 5.149180e+01 8.830256e+01 6.104272e+01
#>  [686] 6.676823e+01 1.007039e+02 5.498708e+01 6.614086e+01 5.777524e+01
#>  [691] 1.165943e+02 7.688011e+01 8.675958e+01 6.431862e+01 5.997338e+01
#>  [696] 6.190673e+01 1.021242e+02 6.134312e+01 6.051694e+01 5.876590e+01
#>  [701] 5.664655e+01 5.370202e+01 5.472630e+01 6.311228e+01 6.246344e+01
#>  [706] 5.704822e+01 6.368068e+01 1.568376e+02 7.299732e+01 7.008653e+01
#>  [711] 6.008375e+01 5.642402e+01 1.005448e+02 5.937336e+01 1.538412e+02
#>  [716] 6.307869e+01 6.300216e+01 7.500859e+01 7.071066e+01 5.321846e+01
#>  [721] 6.204029e+01 1.351683e+02 7.082759e+01 5.491201e+01 5.845282e+01
#>  [726] 7.004712e+01 8.411516e+01 5.141245e+01 5.382019e+01 7.631955e+01
#>  [731] 8.815401e+01 6.491929e+01 6.697392e+01 7.623047e+01 6.882424e+01
#>  [736] 1.400199e+02 5.686370e+01 1.191556e+02 6.969240e+01 5.913252e+01
#>  [741] 6.389229e+01 6.107860e+01 5.958890e+01 5.800640e+01 6.077632e+01
#>  [746] 2.001009e+03 5.690943e+01 5.732021e+01 4.310772e+01 1.048241e+02
#>  [751] 5.383341e+01 6.134183e+01 9.889014e+01 8.150603e+01 6.926756e+01
#>  [756] 5.542071e+02 5.316026e+01 5.594379e+01 1.089173e+02 5.963002e+01
#>  [761] 8.433805e+01 6.092928e+01 1.186210e+02 5.433743e+01 5.961343e+01
#>  [766] 8.288273e+01 5.713243e+01 5.894591e+01 6.042066e+01 6.793436e+01
#>  [771] 7.384939e+01 9.910022e+01 5.733227e+01 6.178880e+01 6.203928e+01
#>  [776] 7.079679e+01 6.174001e+01 7.389874e+01 2.075808e+02 7.994456e+01
#>  [781] 8.468524e+01 8.344347e+01 5.866859e+01 6.189105e+01 6.951317e+01
#>  [786] 1.036925e+02 6.122425e+01 7.417449e+01 6.408568e+01 6.917681e+01
#>  [791] 1.263895e+02 5.320577e+01 1.137783e+02 6.281953e+01 1.256872e+02
#>  [796] 5.885534e+01 6.651508e+01 6.120662e+01 5.854161e+01 5.621628e+01
#>  [801] 5.407376e+01 5.998231e+01 8.192688e+01 7.191854e+01 1.271977e+02
#>  [806] 6.800156e+01 6.282545e+01 6.476286e+01 8.273977e+01 1.135829e+02
#>  [811] 2.539179e+02 6.245392e+01 5.302361e+01 7.588587e+01 1.049336e+02
#>  [816] 1.670009e+03 1.275632e+02 7.446147e+01 6.158241e+01 6.778473e+01
#>  [821] 6.021220e+01 7.502793e+01 6.065334e+01 5.765487e+01 5.501923e+01
#>  [826] 7.160805e+01 6.852944e+01 6.065436e+01 5.351754e+01 5.410634e+02
#>  [831] 9.890272e+01 5.122745e+01 6.624629e+01 6.196922e+01 7.093236e+01
#>  [836] 3.025666e+02 1.295200e+02 5.955106e+01 5.445902e+01 5.169223e+01
#>  [841] 6.737980e+01 5.475443e+01 7.018815e+01 6.243597e+01 5.834233e+01
#>  [846] 5.874829e+01 1.006739e+02 2.487831e+02 5.372023e+02 7.754738e+01
#>  [851] 5.693438e+01 8.675510e+01 6.305212e+01 8.717157e+01 7.057484e+01
#>  [856] 5.998416e+01 5.819284e+01 5.873314e+01 6.154673e+01 6.544653e+01
#>  [861] 8.140903e+01 6.723145e+01 5.737135e+01 1.009175e+02 5.945565e+01
#>  [866] 7.294179e+01 7.952145e+01 9.584982e+01 5.810797e+01 6.260964e+01
#>  [871] 5.720052e+01 5.971306e+01 6.187981e+01 8.036075e+01 7.183750e+01
#>  [876] 7.566966e+01 6.982567e+01 1.327582e+02 9.468197e+01 7.164424e+01
#>  [881] 6.265953e+01 6.477180e+01 6.565053e+01 8.282002e+01 6.326408e+01
#>  [886] 8.686805e+01 7.320290e+01 7.833326e+01 6.985254e+01 1.927735e+02
#>  [891] 3.060637e+02 1.575675e+02 5.544263e+01 6.753161e+01 9.147837e+01
#>  [896] 1.396894e+02 7.354572e+01 9.255414e+01 7.050499e+01 7.739546e+01
#>  [901] 6.434811e+01 6.924286e+01 6.477383e+01 5.889901e+01 6.590021e+01
#>  [906] 6.831008e+01 6.624751e+01 6.173862e+01 7.070260e+01 6.893451e+01
#>  [911] 5.942105e+01 6.477773e+01 9.372766e+01 8.603027e+01 7.606395e+01
#>  [916] 5.572000e+01 6.087489e+01 5.977937e+01 6.452931e+01 5.786390e+01
#>  [921] 5.126623e+01 5.169783e+01 2.034816e+02 5.859000e+01 5.865628e+01
#>  [926] 1.123311e+02 1.253514e+02 1.557275e+02 1.372615e+02 2.082393e+02
#>  [931] 6.631731e+01 8.893555e+01 8.283486e+01 1.269570e+02 6.057645e+01
#>  [936] 1.863952e+02 1.170862e+02 6.376180e+01 6.439814e+01 6.097660e+01
#>  [941] 6.999518e+01 6.367693e+01 5.622640e+01 6.305468e+01 5.126584e+01
#>  [946] 6.123176e+01 6.925208e+01 7.512361e+01 5.971511e+01 8.136188e+01
#>  [951] 5.494756e+01 2.133789e+02 7.817383e+01 5.817342e+01 6.303833e+01
#>  [956] 5.474282e+01 6.854629e+01 5.614798e+01 6.314199e+01 5.702934e+01
#>  [961] 5.960143e+01 7.329869e+01 7.136669e+01 6.699901e+01 5.803391e+01
#>  [966] 7.056302e+01 5.813475e+01 7.119959e+01 7.045533e+01 1.443265e+02
#>  [971] 1.158932e+02 5.288539e+01 7.254021e+01 5.993867e+01 1.200741e+02
#>  [976] 2.272054e+02 5.862685e+01 8.173561e+01 7.399260e+01 8.120098e+01
#>  [981] 5.952445e+01 5.322909e+01 6.621949e+01 1.449205e+03 1.317957e+02
#>  [986] 5.271705e+01 6.224266e+01 7.464117e+01 1.434990e+02 5.519602e+01
#>  [991] 5.865262e+01 6.393040e+01 8.875286e+01 5.991500e+01 6.012936e+01
#>  [996] 8.013417e+01 6.401671e+01 6.424150e+01 6.242369e+01 8.050978e+01
```
