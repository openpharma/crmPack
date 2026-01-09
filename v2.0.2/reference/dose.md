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
#>  [1]  47.18402  47.18402  47.18402  44.85705  44.85705  35.93385  35.93385
#>  [8]  35.93385  81.96863  81.96863  81.96863  81.96863  81.96863  58.69007
#> [15]  58.69007 164.99907 156.09966 156.09966 156.09966  22.29185

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
#>    [1] 6.410860e+01 5.688515e+01 7.345137e+01 5.947624e+01 5.854254e+01
#>    [6] 6.139090e+01 7.672394e+01 5.136119e+01 5.063523e+01 6.852195e+01
#>   [11] 6.425127e+01 7.732086e+01 9.424644e+01 7.309046e+01 5.947901e+01
#>   [16] 7.135266e+01 8.167573e+01 1.060961e+02 5.959301e+01 6.265282e+01
#>   [21] 1.303472e+02 3.307936e+02 1.531800e+02 6.086686e+01 7.084413e+01
#>   [26] 5.803017e+01 9.986026e+01 2.114714e+02 1.435751e+02 6.142451e+01
#>   [31] 6.186316e+01 7.456700e+01 5.824849e+01 6.295377e+01 3.378384e+02
#>   [36] 2.328199e+02 6.786933e+01 5.289774e+01 5.974365e+01 6.328343e+01
#>   [41] 6.080801e+01 7.323059e+01 7.274644e+01 8.483046e+01 5.444132e+01
#>   [46] 4.926843e+01 8.490948e+01 6.590246e+01 6.213979e+01 2.069499e+02
#>   [51] 6.884751e+01 7.773979e+01 7.657870e+01 5.589540e+01 6.209495e+01
#>   [56] 5.386167e+01 6.152820e+01 6.499378e+01 6.023057e+01 7.425150e+01
#>   [61] 3.072054e+02 7.042376e+01 8.096934e+01 5.909664e+01 1.277633e+02
#>   [66] 1.872587e+02 6.924038e+01 6.194760e+01 1.754088e+02 6.602466e+01
#>   [71] 6.819262e+01 1.097561e+02 6.703109e+01 1.026123e+02 6.228149e+01
#>   [76] 6.090941e+01 7.138994e+01 7.068048e+01 5.712252e+01 1.224224e+02
#>   [81] 5.506700e+01 6.338492e+01 5.772116e+01 6.354409e+01 7.589269e+01
#>   [86] 2.552073e+03 1.161928e+02 6.760818e+03 1.827824e+02 3.394370e+02
#>   [91] 5.606722e+01 1.142764e+02 6.907154e+01 7.443792e+01 8.681253e+01
#>   [96] 1.238844e+02 1.357776e+02 5.540758e+01 8.428079e+01 7.012353e+01
#>  [101] 5.320422e+01 5.725713e+01 6.058514e+01 5.951358e+01 6.775061e+01
#>  [106] 6.039193e+01 1.678407e+02 6.549427e+01 5.950127e+01 1.173930e+02
#>  [111] 6.617005e+01 6.204031e+01 5.947147e+01 3.050328e+02 8.465407e+01
#>  [116] 1.738886e+03 7.644863e+01 6.605576e+01 1.035224e+02 1.975469e+02
#>  [121] 1.351563e+03 5.721869e+01 6.263322e+01 5.616738e+01 6.841776e+01
#>  [126] 7.788096e+01 5.545445e+01 9.052348e+01 1.721269e+02 1.662800e+02
#>  [131] 1.772861e+02 5.878768e+01 1.028951e+02 6.619120e+01 6.968694e+01
#>  [136] 2.381739e+02 5.971594e+01 4.926226e+01 1.081908e+02 6.604656e+01
#>  [141] 6.716258e+01 5.991023e+01 8.603225e+01 7.491953e+01 6.037128e+01
#>  [146] 6.224119e+01 6.677766e+01 8.618762e+01 5.112708e+01 5.410624e+01
#>  [151] 6.815319e+01 1.249512e+02 6.619757e+01 6.972825e+01 5.947911e+01
#>  [156] 7.044367e+01 6.256516e+01 6.984765e+01 6.932562e+01 6.094858e+01
#>  [161] 8.003867e+01 7.047598e+01 6.062903e+01 5.604802e+01 3.905477e+02
#>  [166] 1.020938e+02 2.043596e+02 5.678679e+01 3.326031e+02 2.527076e+02
#>  [171] 5.736110e+01 6.713405e+01 1.021997e+02 6.403893e+01 7.601300e+01
#>  [176] 8.209691e+01 7.816966e+01 5.934739e+01 6.536400e+01 5.371148e+01
#>  [181] 5.941019e+01 6.092863e+01 5.739158e+01 5.865746e+01 6.041631e+01
#>  [186] 8.366099e+01 5.921685e+01 6.726216e+01 6.885776e+01 5.956667e+01
#>  [191] 7.610727e+01 7.640185e+01 6.221186e+01 5.832288e+01 1.182917e+02
#>  [196] 5.351884e+01 7.386029e+01 8.705912e+02 6.684765e+01 5.884209e+01
#>  [201] 1.918904e+02 6.019039e+01 6.780665e+01 5.802628e+01 1.445710e+02
#>  [206] 1.077114e+05 6.372802e+01 8.515455e+01 3.857539e+02 6.007526e+01
#>  [211] 6.466739e+01 6.106574e+01 6.852651e+01 1.192789e+02 5.840733e+01
#>  [216] 6.456823e+01 7.922822e+01 6.731457e+01 6.977093e+01 6.094201e+01
#>  [221] 6.258931e+01 1.018270e+02 7.458337e+01 7.225438e+01 7.772735e+01
#>  [226] 6.477147e+01 6.084805e+01 6.729073e+01 6.476321e+01 6.569049e+01
#>  [231] 2.261918e+02 1.082313e+02 9.658853e+01 6.505289e+01 6.853007e+01
#>  [236] 3.357464e+02 6.168019e+01 5.403737e+01 5.936791e+01 3.485087e+02
#>  [241] 6.307451e+01 6.970958e+01 1.179053e+02 1.808236e+02 1.165859e+02
#>  [246] 5.760054e+01 1.146959e+02 2.051513e+02 6.224443e+01 5.457090e+01
#>  [251] 8.474660e+01 6.019615e+01 8.647699e+01 5.813058e+01 5.998652e+01
#>  [256] 5.724504e+01 5.368468e+01 6.419649e+01 8.743736e+01 5.356311e+01
#>  [261] 1.070959e+02 6.022240e+01 6.595301e+01 6.891193e+01 5.961011e+01
#>  [266] 6.177879e+01 5.778367e+01 8.635861e+01 2.008717e+02 9.176891e+01
#>  [271] 6.244570e+01 8.560227e+01 8.955320e+01 1.118879e+02 7.640397e+01
#>  [276] 7.889511e+01 6.192947e+01 7.841557e+01 6.448241e+01 9.552433e+01
#>  [281] 8.816977e+01 7.939929e+01 6.697713e+01 6.411869e+01 6.199404e+01
#>  [286] 1.376505e+02 1.679402e+02 2.023503e+02 6.476085e+01 5.252196e+01
#>  [291] 6.152474e+01 6.861449e+01 5.940834e+01 7.780040e+01 7.273727e+01
#>  [296] 5.767401e+01 6.181239e+01 6.224052e+01 7.356349e+01 7.866535e+01
#>  [301] 5.611514e+01 1.213563e+02 5.532473e+01 6.446078e+01 6.603265e+01
#>  [306] 5.681597e+01 1.334210e+02 1.343154e+02 5.929653e+01 6.863428e+01
#>  [311] 6.375303e+01 5.761372e+01 7.461175e+01 5.569141e+01 6.275565e+01
#>  [316] 7.489188e+01 7.745477e+01 6.033380e+01 6.507081e+01 6.171338e+01
#>  [321] 9.333988e+01 8.356248e+01 7.230861e+01 5.284225e+01 5.008961e+01
#>  [326] 4.764369e+01 5.997511e+01 5.643445e+01 5.980631e+01 1.283642e+02
#>  [331] 5.910058e+01 7.356094e+01 8.923261e+01 5.447817e+01 6.332356e+02
#>  [336] 6.464003e+01 7.583083e+01 6.646524e+01 5.816808e+01 6.042447e+01
#>  [341] 6.024090e+01 5.966746e+01 5.884185e+01 8.007858e+01 5.649107e+01
#>  [346] 5.580487e+01 6.277009e+01 9.283166e+01 7.306677e+01 7.565104e+01
#>  [351] 2.875994e+02 6.144848e+01 1.709060e+02 6.378068e+01 7.139241e+01
#>  [356] 6.256503e+01 1.323801e+02 6.493519e+01 6.107582e+01 6.004063e+01
#>  [361] 6.872757e+01 6.578344e+01 8.264970e+01 4.990044e+01 1.065143e+02
#>  [366] 6.356674e+01 6.385370e+01 5.427244e+01 8.276258e+01 7.581586e+01
#>  [371] 1.090504e+02 1.116659e+02 7.474809e+01 6.902555e+01 6.225505e+01
#>  [376] 8.331945e+01 7.550471e+01 1.589905e+02 8.461029e+01 2.756827e+02
#>  [381] 2.635054e+04 2.654498e+02 8.276298e+01 8.876202e+01 1.479678e+02
#>  [386] 4.339427e+04 1.087418e+02 6.077424e+01 5.963434e+01 7.988443e+01
#>  [391] 6.651113e+01 7.015091e+01 7.159422e+01 9.001480e+01 5.644976e+01
#>  [396] 6.017972e+01 7.256238e+01 7.545636e+01 1.190232e+02 2.618053e+02
#>  [401] 6.104922e+01 5.952497e+01 6.244223e+01 6.004082e+01 9.965287e+01
#>  [406] 9.635439e+01 6.503778e+01 6.122480e+01 5.716275e+01 5.723882e+01
#>  [411] 8.184207e+01 6.517114e+01 6.630681e+01 8.186074e+01 5.687397e+01
#>  [416] 2.846976e+02 6.258560e+01 7.218659e+01 7.484888e+01 6.392644e+01
#>  [421] 6.159874e+01 6.925693e+01 6.700245e+01 6.329688e+01 5.903060e+01
#>  [426] 5.672043e+01 6.586405e+01 1.024562e+02 1.675041e+02 1.274404e+02
#>  [431] 4.958346e+02 6.686869e+01 8.554529e+01 5.839065e+01 7.328009e+01
#>  [436] 2.226594e+02 5.572717e+02 8.065303e+01 7.720256e+01 5.924407e+01
#>  [441] 5.721625e+01 8.002276e+01 6.413875e+01 7.155274e+01 6.125155e+01
#>  [446] 6.100689e+01 6.715105e+01 7.307722e+01 6.744922e+01 5.908175e+01
#>  [451] 6.654618e+02 8.877431e+01 5.986516e+01 5.404242e+01 6.203929e+01
#>  [456] 6.569416e+01 1.381679e+02 7.126139e+01 6.478479e+01 6.417851e+01
#>  [461] 6.726300e+01 7.296147e+01 6.469293e+01 1.364145e+02 8.282950e+01
#>  [466] 5.941696e+02 6.264610e+01 6.772260e+01 1.148205e+02 1.423283e+02
#>  [471] 5.786241e+01 1.149285e+02 6.791120e+01 6.346583e+01 5.946891e+01
#>  [476] 6.003680e+01 6.039358e+01 6.907395e+01 5.595132e+01 7.452138e+01
#>  [481] 6.179375e+01 5.854385e+01 4.205217e+02 4.492492e+01 1.059746e+02
#>  [486] 7.845206e+01 1.021774e+02 5.374892e+01 5.739919e+01 5.819771e+01
#>  [491] 1.119754e+02 1.261294e+02 1.392860e+02 8.603100e+01 5.910548e+01
#>  [496] 7.401463e+01 7.429016e+01 7.869373e+01 5.749309e+01 6.632453e+01
#>  [501] 6.757057e+01 6.426899e+01 6.557784e+01 1.195916e+02 6.350957e+01
#>  [506] 6.348384e+01 9.705357e+01 6.894041e+01 5.990939e+01 6.837430e+01
#>  [511] 1.584644e+02 6.469690e+01 5.831499e+01 6.648027e+01 6.576109e+01
#>  [516] 5.735630e+01 5.700667e+01 6.837513e+01 1.272537e+02 6.564064e+01
#>  [521] 6.388545e+01 6.487632e+01 1.230364e+02 2.995749e+02 3.108014e+02
#>  [526] 5.900755e+01 6.790829e+01 1.849077e+02 1.121984e+02 5.475720e+01
#>  [531] 5.382422e+01 6.366619e+01 5.316409e+01 1.034277e+02 6.039791e+01
#>  [536] 6.418527e+01 1.065606e+02 6.686564e+01 8.247162e+01 5.257810e+01
#>  [541] 6.993016e+01 5.735476e+01 8.902688e+01 5.624313e+01 5.924072e+01
#>  [546] 5.923518e+01 5.880913e+01 8.318821e+01 6.029846e+01 6.787481e+01
#>  [551] 6.044493e+01 5.629897e+01 6.346170e+01 8.185177e+01 3.188951e+02
#>  [556] 6.349726e+01 7.175704e+01 6.160169e+01 6.045009e+01 8.246565e+01
#>  [561] 6.858390e+01 6.258919e+01 6.193212e+01 7.388708e+01 5.663299e+01
#>  [566] 5.858336e+01 5.568235e+01 5.971083e+01 5.999654e+01 5.825300e+01
#>  [571] 6.334473e+01 5.655547e+01 6.613681e+01 6.672792e+01 6.335043e+01
#>  [576] 6.038909e+01 6.052775e+01 5.864452e+01 5.949823e+01 6.364538e+01
#>  [581] 9.440751e+02 3.352523e+02 1.139726e+02 5.627816e+01 6.465391e+01
#>  [586] 5.646266e+01 6.081090e+01 7.545856e+01 7.187652e+01 5.039676e+01
#>  [591] 7.803585e+01 6.046238e+01 7.814466e+01 5.799068e+01 5.822847e+01
#>  [596] 5.773561e+01 5.671978e+01 6.711609e+01 5.986003e+01 5.797115e+01
#>  [601] 1.496912e+02 6.638993e+01 5.934039e+01 9.650755e+01 5.800497e+01
#>  [606] 1.178571e+02 5.732509e+01 5.406465e+01 5.011328e+02 1.179953e+02
#>  [611] 7.610161e+01 4.942392e+01 7.412145e+01 5.228627e+01 6.067229e+01
#>  [616] 5.814706e+01 7.169864e+01 5.995742e+01 6.759753e+01 7.048885e+01
#>  [621] 1.921432e+02 6.657995e+01 9.788748e+01 5.787786e+01 6.155737e+01
#>  [626] 6.292074e+01 6.368279e+01 7.511786e+01 8.057557e+01 8.097800e+01
#>  [631] 5.464271e+01 1.005633e+02 5.520950e+01 6.249328e+01 1.037500e+02
#>  [636] 1.102584e+02 5.599421e+01 8.129114e+01 5.402376e+01 6.793339e+01
#>  [641] 6.926342e+01 7.272716e+01 8.808223e+01 7.461338e+01 7.080037e+01
#>  [646] 1.067566e+02 5.633955e+01 5.948107e+01 7.112343e+01 5.398348e+01
#>  [651] 7.627418e+01 5.641204e+01 8.393023e+01 6.456057e+01 6.818323e+01
#>  [656] 7.854576e+01 5.948025e+01 6.865753e+01 1.467457e+02 2.726771e+02
#>  [661] 5.757387e+01 6.072104e+01 7.133217e+01 9.151734e+01 7.168435e+01
#>  [666] 8.147430e+01 6.386900e+01 9.900408e+01 5.532566e+01 5.828068e+01
#>  [671] 5.447982e+01 1.258703e+02 3.658571e+02 7.476184e+01 5.853816e+01
#>  [676] 1.006703e+02 5.480247e+01 5.700382e+01 6.074771e+01 6.156767e+01
#>  [681] 7.038831e+01 6.401695e+01 6.203308e+01 6.431962e+01 7.358773e+01
#>  [686] 6.142237e+01 6.069637e+01 8.335409e+01 1.187709e+02 5.904879e+01
#>  [691] 8.530424e+01 6.144348e+01 7.222895e+01 8.864488e+02 5.920420e+01
#>  [696] 5.430111e+01 6.296727e+01 6.348038e+01 6.128979e+01 7.243500e+01
#>  [701] 1.003757e+02 6.770751e+01 5.374276e+01 7.518721e+01 8.692343e+01
#>  [706] 8.551320e+01 6.899977e+01 7.520725e+01 5.668437e+01 6.762041e+01
#>  [711] 1.682665e+02 6.368860e+01 5.737712e+01 7.330353e+01 7.221579e+01
#>  [716] 5.214543e+01 5.677926e+01 5.852486e+01 9.317046e+01 6.039370e+01
#>  [721] 7.734804e+01 6.084330e+01 9.076327e+01 6.023570e+01 6.995626e+01
#>  [726] 7.754938e+01 9.670017e+01 6.390484e+01 3.791272e+02 2.502092e+02
#>  [731] 7.632966e+01 6.644277e+01 6.242705e+01 6.187513e+01 6.427022e+01
#>  [736] 8.283772e+01 6.166850e+01 5.947451e+01 5.833655e+01 6.538841e+01
#>  [741] 5.313825e+01 5.731280e+01 8.641188e+01 5.808104e+01 5.594750e+01
#>  [746] 1.093758e+02 6.628355e+01 5.772332e+01 6.182129e+01 5.892896e+01
#>  [751] 5.699038e+01 5.622461e+01 5.689647e+01 6.177727e+01 1.058354e+02
#>  [756] 9.593609e+01 6.582323e+01 9.975760e+01 7.148306e+01 6.819439e+01
#>  [761] 6.011587e+01 1.824350e+02 6.129135e+01 6.228428e+01 6.770995e+01
#>  [766] 7.459714e+01 1.121629e+02 6.136446e+01 8.526837e+01 7.654788e+01
#>  [771] 8.502527e+01 7.128422e+01 6.043014e+01 6.150174e+01 5.930323e+01
#>  [776] 6.985603e+01 5.873412e+01 5.836087e+01 5.851491e+01 1.403788e+02
#>  [781] 9.641942e+01 5.969305e+01 5.743629e+01 6.234099e+01 6.029667e+01
#>  [786] 6.280619e+01 6.057912e+01 6.480088e+01 8.921911e+01 9.414984e+01
#>  [791] 7.552826e+01 6.479766e+01 6.036029e+01 7.056333e+01 5.419889e+01
#>  [796] 6.327724e+01 2.707225e+02 8.855133e+01 6.298634e+01 5.999093e+01
#>  [801] 5.625178e+01 8.274029e+01 7.444390e+01 7.591892e+01 7.020074e+01
#>  [806] 6.273429e+01 6.122537e+01 8.398943e+04 1.083392e+02 8.014020e+01
#>  [811] 5.968232e+01 7.283095e+01 6.523820e+01 5.917468e+01 6.130143e+01
#>  [816] 9.926243e+01 6.595270e+01 8.282412e+01 5.991631e+01 6.874077e+01
#>  [821] 6.323760e+01 6.225915e+01 5.318760e+01 6.731804e+01 6.261195e+01
#>  [826] 7.119546e+01 5.910632e+01 6.159074e+01 6.245324e+01 5.781907e+01
#>  [831] 5.165976e+01 6.276816e+01 6.524388e+01 7.314539e+01 5.640615e+01
#>  [836] 5.265446e+01 5.691313e+01 7.948430e+01 5.872723e+01 6.368379e+01
#>  [841] 6.549446e+02 7.332341e+11 1.568326e+02 5.771664e+02 5.994161e+01
#>  [846] 5.877845e+01 6.288618e+01 5.989282e+01 7.162446e+01 5.324204e+01
#>  [851] 8.764330e+01 6.729630e+01 6.298282e+01 6.171325e+01 7.450267e+01
#>  [856] 1.629398e+02 7.713310e+01 5.541460e+01 4.465057e+02 6.204257e+01
#>  [861] 6.506266e+01 6.121204e+01 5.937671e+01 5.915525e+01 6.055407e+01
#>  [866] 6.771105e+01 6.036319e+01 9.157522e+01 7.954562e+01 7.333640e+01
#>  [871] 6.949526e+01 7.747624e+01 6.542312e+01 5.935874e+01 7.216807e+01
#>  [876] 6.341072e+01 6.122467e+01 6.534354e+01 6.810130e+01 6.782878e+01
#>  [881] 7.057327e+01 6.138781e+01 5.827618e+01 6.618000e+01 6.483554e+01
#>  [886] 1.129174e+02 5.451196e+01 5.674832e+01 7.150343e+01 6.770593e+01
#>  [891] 7.695364e+01 6.635251e+01 5.986075e+01 6.908836e+01 9.476838e+01
#>  [896] 5.948486e+01 5.590726e+01 6.742410e+01 1.199399e+02 5.344696e+01
#>  [901] 6.423859e+01 5.947832e+01 6.549622e+01 6.350341e+01 8.343201e+01
#>  [906] 5.907382e+01 6.143673e+01 6.243713e+01 1.596247e+02 5.604561e+01
#>  [911] 8.140798e+01 5.826242e+01 6.834802e+01 5.665084e+01 1.297990e+02
#>  [916] 4.832105e+01 5.432053e+01 7.923190e+01 7.272577e+01 2.687412e+02
#>  [921] 6.137530e+01 6.021692e+01 5.608121e+01 8.366833e+01 7.355871e+01
#>  [926] 7.346799e+01 6.365059e+01 5.770336e+01 6.361421e+01 9.315515e+01
#>  [931] 6.906047e+01 6.539412e+01 5.536035e+01 8.199435e+01 6.056266e+01
#>  [936] 9.894903e+01 5.451546e+01 1.017724e+02 2.583858e+03 2.527303e+03
#>  [941] 3.144823e+02 8.790725e+01 5.448523e+01 1.054196e+02 7.124942e+01
#>  [946] 1.075393e+02 6.123266e+01 5.787713e+01 3.094585e+02 6.705646e+01
#>  [951] 7.802579e+01 6.412050e+01 5.853309e+01 5.876750e+01 5.903842e+01
#>  [956] 5.857381e+01 5.610515e+01 6.395593e+01 6.894307e+01 8.800604e+01
#>  [961] 5.821862e+01 9.393134e+01 1.465655e+02 5.502217e+01 5.360732e+01
#>  [966] 5.953203e+01 6.139660e+01 6.529961e+01 6.244965e+01 1.433430e+02
#>  [971] 5.857122e+01 6.614321e+01 9.232834e+01 9.334401e+01 6.851901e+01
#>  [976] 6.470509e+01 8.844372e+01 7.517946e+01 5.937131e+01 6.077419e+01
#>  [981] 6.197749e+01 5.931322e+01 5.540940e+01 6.624199e+01 7.225236e+01
#>  [986] 7.902134e+01 6.566648e+01 6.126427e+01 7.441510e+01 5.770027e+01
#>  [991] 5.993810e+01 5.730865e+01 5.901021e+01 7.939101e+01 6.691434e+01
#>  [996] 5.429217e+01 3.888329e+02 2.175294e+02 5.551903e+01 1.626883e+02
```
