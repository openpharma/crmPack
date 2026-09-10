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
#>  [1]   122.63774   122.63774   550.20484   550.20484   550.20484   660.35342
#>  [7]   174.69984   174.69984   174.69984 74684.52803    92.02816    80.80936
#> [13]    71.63020   137.40321   137.40321   137.40321   137.40321   153.72089
#> [19]    59.10338    59.10338

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
#>    [1] 6.205937e+01 2.314284e+02 6.965846e+01 1.131500e+02 8.507487e+01
#>    [6] 5.808981e+01 6.214369e+01 9.361088e+01 2.046362e+02 1.416487e+02
#>   [11] 1.444826e+02 1.561526e+02 5.665675e+01 8.582799e+02 5.321807e+01
#>   [16] 7.828011e+01 6.915177e+01 6.123821e+01 5.501010e+01 1.241661e+02
#>   [21] 5.926201e+01 5.881974e+01 6.055351e+01 7.906304e+01 7.912065e+01
#>   [26] 6.282735e+01 5.543364e+01 6.526441e+01 5.952803e+01 5.559640e+01
#>   [31] 1.069626e+02 8.154143e+01 7.829335e+01 5.439400e+01 5.853053e+01
#>   [36] 5.931152e+01 6.040458e+01 5.852612e+01 6.750542e+01 7.491976e+01
#>   [41] 6.938621e+01 1.421613e+02 7.232967e+02 8.126063e+02 6.332771e+01
#>   [46] 5.957537e+01 7.305190e+01 7.449215e+01 5.084931e+01 5.531942e+01
#>   [51] 6.749389e+01 6.647824e+01 5.698868e+01 6.211243e+01 9.322537e+01
#>   [56] 5.590876e+01 6.601822e+01 5.793283e+01 7.340125e+01 5.319651e+01
#>   [61] 8.017868e+01 7.493465e+01 6.232856e+01 6.666160e+01 6.323839e+01
#>   [66] 7.621281e+01 6.160091e+01 6.097936e+01 5.632981e+01 6.139546e+01
#>   [71] 5.984773e+01 7.088416e+01 7.291250e+01 5.679434e+01 5.912077e+01
#>   [76] 6.363155e+01 5.762728e+01 6.615992e+01 1.261446e+02 6.188606e+01
#>   [81] 5.993111e+01 6.872120e+01 6.228327e+01 7.613792e+01 1.387212e+02
#>   [86] 1.253139e+02 6.624834e+01 1.236118e+02 1.106277e+02 5.636112e+01
#>   [91] 8.422724e+01 6.154851e+01 6.709222e+01 7.998986e+01 9.251739e+01
#>   [96] 7.690200e+01 6.036332e+01 5.784121e+01 5.575577e+01 6.620850e+01
#>  [101] 9.696652e+01 9.910174e+01 6.048264e+01 6.457333e+01 6.812912e+01
#>  [106] 5.971240e+01 5.852897e+01 5.827019e+01 6.567578e+01 6.369834e+01
#>  [111] 6.305981e+01 9.122495e+01 1.637306e+02 6.027870e+01 5.222912e+01
#>  [116] 3.823666e+02 6.266336e+01 5.495941e+01 1.620140e+02 7.447755e+02
#>  [121] 6.268296e+01 5.749379e+01 5.844615e+01 1.141351e+02 5.884595e+01
#>  [126] 8.167776e+01 6.861347e+01 8.792283e+01 8.722350e+01 6.084133e+01
#>  [131] 9.078503e+01 6.087930e+01 6.147653e+01 5.810352e+01 6.268141e+01
#>  [136] 5.902555e+01 4.802969e+02 5.814738e+01 6.760827e+01 7.738250e+01
#>  [141] 5.520508e+01 5.597637e+01 8.033818e+01 6.411011e+01 6.124177e+01
#>  [146] 1.426404e+02 7.186836e+01 6.957007e+01 5.745960e+01 7.231518e+01
#>  [151] 9.511313e+01 6.311234e+01 5.895864e+01 1.469871e+02 7.372767e+01
#>  [156] 8.419028e+01 5.771379e+01 6.871286e+01 5.603276e+01 4.611826e+05
#>  [161] 7.054060e+01 6.272175e+01 5.771362e+01 8.068220e+01 1.882385e+03
#>  [166] 5.813368e+01 6.435146e+01 5.618004e+01 8.521958e+01 1.057959e+02
#>  [171] 6.344868e+01 7.099673e+01 1.602213e+02 6.905843e+01 7.167626e+01
#>  [176] 6.144421e+01 6.390646e+01 4.980945e+02 1.274285e+03 8.940012e+01
#>  [181] 5.840829e+01 6.573735e+01 6.244782e+01 6.979345e+01 1.423397e+02
#>  [186] 2.224075e+02 7.765165e+01 8.446691e+01 6.790014e+01 5.194266e+01
#>  [191] 5.500160e+01 6.379579e+01 7.739268e+01 1.056482e+02 1.543431e+02
#>  [196] 6.069457e+01 4.663105e+01 7.257712e+01 7.679118e+01 7.210663e+01
#>  [201] 7.120864e+01 8.702014e+01 6.330352e+01 5.643180e+01 6.546800e+01
#>  [206] 1.226393e+02 7.458513e+01 8.288259e+01 5.743603e+03 6.323836e+01
#>  [211] 6.068413e+01 5.925748e+01 7.883025e+01 8.015655e+01 1.550008e+02
#>  [216] 2.033928e+03 6.968594e+01 5.989487e+01 6.117436e+01 5.699326e+01
#>  [221] 5.881106e+01 6.513909e+01 7.260871e+01 8.300966e+01 1.821120e+02
#>  [226] 6.248579e+01 8.705540e+01 2.165655e+02 8.286865e+01 6.112730e+01
#>  [231] 5.497440e+01 6.073796e+01 6.190836e+01 5.683451e+01 5.498758e+01
#>  [236] 7.702259e+01 5.973971e+01 6.981187e+01 6.340538e+01 5.871664e+01
#>  [241] 5.970520e+01 7.614460e+01 5.984775e+01 5.552364e+01 5.664792e+01
#>  [246] 8.678241e+02 3.858753e+02 1.238609e+02 7.238107e+01 5.344974e+01
#>  [251] 6.130720e+01 7.909076e+01 6.121952e+01 5.868871e+01 8.852708e+01
#>  [256] 5.788335e+01 7.298689e+01 6.414209e+01 6.522066e+01 5.594041e+01
#>  [261] 6.616585e+01 1.105029e+02 6.314458e+01 6.043431e+01 9.446471e+01
#>  [266] 3.269393e+02 5.954026e+01 6.040816e+01 6.062766e+01 5.585130e+01
#>  [271] 8.123491e+01 1.216529e+02 6.572979e+01 6.326968e+01 7.495686e+01
#>  [276] 6.607503e+01 5.936953e+01 5.468471e+01 5.710612e+01 6.360629e+01
#>  [281] 1.012354e+02 5.487019e+01 5.366874e+01 6.155081e+01 8.352529e+01
#>  [286] 8.416366e+01 6.436412e+01 5.476788e+01 6.903178e+01 5.749569e+01
#>  [291] 1.037175e+02 8.011999e+01 1.875069e+02 7.473134e+01 7.905315e+01
#>  [296] 5.560677e+01 6.362985e+01 5.672372e+01 1.854368e+02 7.424264e+01
#>  [301] 6.068632e+01 7.944325e+01 6.603836e+01 6.021991e+01 2.244620e+02
#>  [306] 7.361536e+01 8.351802e+01 9.478378e+01 6.272000e+01 1.276491e+02
#>  [311] 6.151518e+01 5.769167e+01 1.515433e+02 6.125755e+01 6.198793e+01
#>  [316] 5.818891e+01 5.785256e+01 5.987785e+01 6.483534e+01 7.380991e+01
#>  [321] 1.472981e+02 6.077339e+01 6.386858e+01 6.851072e+01 6.641526e+01
#>  [326] 6.226706e+01 1.206217e+02 7.011011e+01 7.757561e+01 6.321388e+01
#>  [331] 9.064807e+01 5.534543e+01 5.803718e+01 6.153987e+01 5.589776e+01
#>  [336] 5.959000e+01 8.963621e+01 5.478575e+01 6.449414e+01 7.269158e+01
#>  [341] 7.659755e+01 5.514687e+01 1.089373e+06 1.541847e+03 7.200210e+01
#>  [346] 6.856517e+01 1.248724e+02 5.660901e+01 6.285941e+01 4.182707e+02
#>  [351] 1.061451e+02 5.992809e+01 6.313537e+01 5.885595e+01 5.684251e+01
#>  [356] 5.890295e+01 8.626592e+01 8.241949e+01 2.056687e+02 7.742461e+01
#>  [361] 6.206921e+01 5.764249e+01 6.031944e+01 7.560863e+01 5.562416e+01
#>  [366] 5.327746e+01 6.093209e+01 5.959386e+01 5.873022e+01 5.786292e+01
#>  [371] 6.643668e+01 8.567735e+01 7.068667e+01 6.718624e+01 6.799532e+01
#>  [376] 8.647990e+01 9.629663e+01 6.810949e+01 6.319030e+01 6.129595e+01
#>  [381] 6.036918e+01 5.769089e+01 5.553329e+01 6.258380e+01 6.048741e+01
#>  [386] 5.427039e+01 1.000245e+02 6.292999e+01 8.273871e+01 1.766231e+02
#>  [391] 5.890842e+01 5.913368e+01 7.090050e+01 5.109787e+01 5.375384e+01
#>  [396] 7.835615e+01 7.698410e+01 1.031961e+02 1.180520e+02 6.791516e+01
#>  [401] 5.481561e+01 7.761177e+01 7.965291e+01 6.064213e+01 7.250632e+01
#>  [406] 1.362174e+02 6.163797e+01 1.164663e+02 1.438283e+02 1.609927e+02
#>  [411] 8.730582e+01 6.438794e+01 6.127563e+01 5.919171e+01 2.304533e+03
#>  [416] 1.348863e+04 5.589312e+01 3.696116e+02 5.430836e+01 3.806177e+02
#>  [421] 6.527844e+01 6.499586e+01 6.953224e+01 5.762916e+01 5.889739e+01
#>  [426] 9.393238e+01 8.173332e+01 5.250416e+01 5.721547e+01 5.985879e+01
#>  [431] 9.608141e+01 6.698915e+01 6.351168e+01 5.794995e+01 5.727403e+01
#>  [436] 5.583084e+01 1.250439e+02 5.665221e+01 8.876505e+01 6.179857e+01
#>  [441] 7.931050e+01 5.594776e+01 5.860272e+01 5.340964e+01 6.807655e+01
#>  [446] 8.036018e+02 5.843378e+01 7.943421e+01 5.455340e+01 6.980862e+01
#>  [451] 1.011293e+02 7.463723e+01 6.773184e+01 6.327948e+01 5.890718e+01
#>  [456] 6.671680e+01 6.583840e+01 8.822965e+01 5.732887e+01 5.962349e+01
#>  [461] 7.346621e+01 8.733497e+01 1.167250e+02 8.038634e+01 7.970222e+01
#>  [466] 6.440665e+01 8.336886e+01 9.371044e+01 6.210869e+01 5.911242e+01
#>  [471] 5.697054e+01 5.815977e+01 1.056143e+02 5.763721e+01 9.161422e+01
#>  [476] 7.037850e+01 5.763453e+01 6.178245e+01 6.047609e+01 6.398477e+01
#>  [481] 6.422630e+01 6.045429e+01 6.011621e+01 5.558701e+01 7.143570e+01
#>  [486] 5.886114e+01 7.041845e+01 7.159177e+01 5.712174e+01 7.537676e+01
#>  [491] 6.545312e+01 7.963726e+01 6.172255e+01 5.836562e+01 2.697765e+02
#>  [496] 9.663078e+01 5.901154e+01 6.620305e+01 6.379029e+01 2.132621e+02
#>  [501] 5.851037e+01 6.462507e+01 5.907353e+01 7.111132e+01 7.248460e+01
#>  [506] 5.965971e+01 6.896280e+01 8.695845e+01 5.872983e+01 2.624598e+02
#>  [511] 1.018582e+02 6.273041e+01 6.734532e+01 5.346794e+01 1.889526e+02
#>  [516] 5.237017e+01 9.829778e+01 1.181736e+02 6.660841e+01 6.047442e+01
#>  [521] 5.215478e+01 6.564782e+01 6.023177e+01 6.120521e+01 1.930212e+02
#>  [526] 1.310972e+02 5.606648e+01 6.656637e+01 6.735071e+01 6.719330e+01
#>  [531] 6.241467e+01 6.460666e+01 8.530849e+01 5.846603e+01 6.077874e+01
#>  [536] 5.810949e+01 5.787442e+01 5.584504e+01 9.287100e+01 1.093113e+02
#>  [541] 1.552649e+02 5.773133e+01 5.909362e+01 5.743123e+01 1.221188e+02
#>  [546] 7.965675e+01 5.727092e+01 1.021994e+02 4.809704e+01 5.973750e+01
#>  [551] 6.159093e+01 5.782151e+01 6.184503e+01 2.157223e+02 7.674738e+01
#>  [556] 6.148701e+01 6.102016e+01 5.655368e+01 5.737210e+01 7.152792e+01
#>  [561] 6.475350e+01 7.192130e+01 7.291437e+01 6.873503e+01 6.743547e+01
#>  [566] 6.618138e+01 6.136622e+01 5.714599e+01 7.985024e+01 6.827773e+01
#>  [571] 1.258102e+02 8.411083e+01 5.737916e+01 6.005055e+01 5.860393e+01
#>  [576] 7.659447e+01 6.387443e+01 6.250644e+01 9.408911e+01 6.329323e+01
#>  [581] 5.774859e+01 7.924173e+01 7.899553e+01 7.978980e+01 1.845877e+02
#>  [586] 5.276835e+01 1.173088e+02 8.742225e+01 7.177563e+01 6.465102e+01
#>  [591] 8.122192e+01 6.065198e+01 5.986430e+01 6.096659e+01 1.509310e+02
#>  [596] 1.428787e+02 5.994190e+01 1.100396e+02 8.932300e+01 1.812566e+02
#>  [601] 1.924039e+02 8.252134e+01 5.631435e+01 5.508361e+01 3.137317e+02
#>  [606] 9.812590e+01 7.082117e+01 5.981675e+01 5.812666e+01 1.156510e+02
#>  [611] 7.019811e+01 6.664833e+01 2.093894e+02 5.466724e+01 6.253724e+01
#>  [616] 5.610978e+01 9.001690e+01 5.252410e+01 1.381979e+02 5.876481e+01
#>  [621] 5.917622e+01 9.604545e+01 5.423009e+01 6.535395e+01 6.225545e+01
#>  [626] 5.765652e+01 6.350362e+01 5.810235e+01 7.923385e+01 8.296584e+01
#>  [631] 5.571305e+01 9.026451e+01 6.946150e+01 9.492382e+01 5.687460e+01
#>  [636] 6.622323e+01 6.392135e+01 6.962457e+01 6.209636e+01 5.903325e+01
#>  [641] 5.604995e+01 6.441044e+01 6.895794e+01 1.058264e+02 6.654677e+01
#>  [646] 6.182178e+01 6.087600e+01 6.507936e+01 7.921096e+01 6.449424e+01
#>  [651] 6.391718e+01 1.161790e+02 8.227407e+01 5.529174e+01 5.990237e+01
#>  [656] 8.570599e+01 6.209703e+01 1.188138e+02 6.280810e+01 6.871029e+01
#>  [661] 6.448161e+01 6.025702e+01 8.009488e+01 9.653730e+02 1.209527e+02
#>  [666] 5.250757e+01 7.081156e+01 6.436599e+01 6.237671e+01 6.614861e+01
#>  [671] 9.289998e+01 5.565192e+01 6.351136e+01 7.194932e+01 8.791829e+01
#>  [676] 8.972478e+01 7.327367e+01 6.072704e+01 6.498868e+01 1.309074e+02
#>  [681] 5.907785e+01 5.809506e+01 6.939438e+01 7.628906e+01 7.431868e+01
#>  [686] 6.047893e+01 5.947293e+01 6.021189e+01 1.235414e+02 1.314130e+02
#>  [691] 6.122171e+01 5.732427e+01 7.386914e+01 6.198111e+01 5.797180e+01
#>  [696] 6.315478e+01 6.686470e+01 7.127693e+01 7.846435e+01 1.302814e+02
#>  [701] 8.268732e+01 7.498791e+01 5.997818e+01 3.169063e+02 5.861313e+02
#>  [706] 8.255618e+01 6.049786e+01 6.525431e+01 7.445131e+01 6.926794e+01
#>  [711] 7.930821e+01 4.870298e+01 6.308835e+01 5.897000e+01 1.038286e+02
#>  [716] 8.031923e+01 5.741833e+01 1.220952e+02 6.216708e+01 5.883935e+01
#>  [721] 6.412211e+01 6.684189e+01 8.623169e+01 7.854103e+01 5.802786e+01
#>  [726] 5.722159e+01 7.218631e+01 5.749962e+01 5.733452e+01 5.771191e+01
#>  [731] 5.835067e+01 6.045255e+01 6.661494e+01 5.836873e+01 6.565545e+01
#>  [736] 1.065933e+02 8.356922e+01 1.375253e+02 6.034911e+01 6.652570e+01
#>  [741] 6.595682e+01 7.020313e+01 8.325061e+01 5.454807e+01 8.211495e+01
#>  [746] 9.287317e+01 7.619060e+01 6.546827e+01 1.058700e+02 6.468276e+01
#>  [751] 7.222305e+01 1.528435e+02 1.074545e+02 7.573116e+01 5.517374e+01
#>  [756] 6.395354e+01 7.509660e+01 1.946344e+02 7.233276e+01 5.230830e+01
#>  [761] 5.837224e+01 5.790056e+01 9.513161e+01 5.475156e+01 6.114612e+01
#>  [766] 5.560646e+01 2.224346e+02 6.238841e+01 5.591923e+01 1.308945e+02
#>  [771] 1.053783e+02 8.556174e+01 8.332991e+01 5.733590e+01 1.511171e+02
#>  [776] 2.903563e+02 5.254037e+01 6.116532e+01 5.695605e+01 6.419226e+01
#>  [781] 5.527358e+01 5.973039e+01 5.433814e+01 6.842563e+01 7.095931e+01
#>  [786] 5.936127e+01 5.867064e+01 6.227595e+01 5.773562e+01 6.273146e+01
#>  [791] 6.376069e+01 1.374353e+02 5.276123e+01 7.119303e+01 5.985720e+01
#>  [796] 6.286900e+01 6.113452e+01 6.936945e+01 6.948130e+01 4.420434e+02
#>  [801] 7.934825e+01 5.230606e+01 5.777548e+01 5.828419e+01 6.527965e+01
#>  [806] 7.285389e+01 6.591342e+01 6.697767e+01 6.467219e+01 6.029071e+01
#>  [811] 6.476849e+01 5.737254e+01 5.806875e+01 5.949203e+01 5.680635e+01
#>  [816] 6.576108e+01 7.141639e+01 8.020771e+01 1.015765e+02 5.761476e+01
#>  [821] 1.367491e+02 9.457918e+01 7.266212e+01 8.969247e+01 6.350380e+01
#>  [826] 5.049270e+01 5.998975e+01 5.663789e+01 5.809440e+01 1.254194e+02
#>  [831] 6.359725e+01 6.529722e+01 2.130785e+03 1.070632e+02 5.774187e+01
#>  [836] 6.297040e+01 5.895512e+01 6.106272e+01 6.210626e+02 5.339517e+01
#>  [841] 6.277359e+01 5.963216e+01 5.376649e+01 6.915211e+01 6.349268e+01
#>  [846] 7.581453e+01 6.360571e+01 5.748514e+01 5.907978e+01 7.003020e+01
#>  [851] 1.318799e+02 1.101204e+02 6.098379e+01 6.662685e+01 6.482130e+01
#>  [856] 9.328295e+01 5.995082e+01 6.613593e+01 6.162812e+01 7.008088e+01
#>  [861] 8.626080e+01 9.801356e+01 6.432991e+01 1.041769e+02 8.631505e+01
#>  [866] 8.275700e+01 7.000354e+01 6.756618e+01 6.056329e+01 5.882238e+01
#>  [871] 6.731639e+01 5.770783e+01 5.889918e+01 6.373275e+01 8.509082e+01
#>  [876] 5.187932e+01 7.679572e+01 5.761591e+01 7.278470e+01 7.161484e+01
#>  [881] 7.868782e+01 5.336978e+01 1.086878e+02 1.241145e+02 7.860159e+01
#>  [886] 1.159176e+02 1.408693e+02 7.503291e+01 8.722224e+01 5.999347e+01
#>  [891] 7.452936e+01 6.622186e+01 6.043398e+01 7.117478e+01 9.852717e+01
#>  [896] 6.782077e+01 8.028006e+01 3.577161e+02 9.573352e+01 8.664405e+01
#>  [901] 1.095321e+02 1.493768e+02 2.069267e+02 6.539001e+01 7.171280e+01
#>  [906] 5.850174e+01 6.596132e+01 7.057639e+01 5.943768e+01 5.683098e+01
#>  [911] 6.502130e+01 5.928925e+01 5.482643e+01 6.606238e+01 5.834571e+01
#>  [916] 8.900860e+01 1.475772e+02 1.260678e+02 7.025261e+01 6.469224e+01
#>  [921] 7.147269e+01 7.709889e+01 8.316717e+01 5.908413e+01 5.728895e+01
#>  [926] 6.051460e+01 6.375592e+01 6.264065e+01 8.250760e+01 5.130009e+01
#>  [931] 7.300345e+01 5.593364e+01 6.840207e+01 5.883878e+01 6.794529e+01
#>  [936] 5.558553e+01 8.049851e+01 5.786756e+01 7.076139e+01 7.053450e+01
#>  [941] 7.428941e+01 6.652483e+01 7.176173e+01 7.666245e+01 2.405257e+02
#>  [946] 6.605752e+01 6.033147e+01 6.475166e+01 5.740178e+01 8.990591e+01
#>  [951] 5.838607e+01 5.992848e+01 6.626598e+01 6.313344e+01 7.346081e+01
#>  [956] 7.956390e+01 7.556333e+01 6.087327e+01 5.427027e+01 6.582058e+01
#>  [961] 5.964599e+01 7.355338e+01 8.070252e+01 8.037866e+01 5.826346e+01
#>  [966] 8.676752e+01 6.616744e+01 9.184239e+01 1.984009e+02 5.571954e+01
#>  [971] 6.285885e+01 1.975765e+02 6.589029e+01 6.246497e+01 1.160921e+02
#>  [976] 1.006727e+02 6.697150e+01 5.731725e+01 5.851561e+01 6.407045e+01
#>  [981] 2.274156e+02 6.748188e+01 6.443145e+01 9.725886e+01 5.571024e+01
#>  [986] 5.999188e+01 5.637703e+01 5.885380e+01 8.222541e+01 5.821298e+01
#>  [991] 6.728766e+01 6.606987e+01 4.279900e+02 7.064941e+01 7.925374e+01
#>  [996] 7.667880e+01 6.585927e+01 8.200040e+01 1.086921e+02 7.523626e+01
```
