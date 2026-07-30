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
#>  [1]  40.21609  40.21609  40.21609  40.21609  55.75257  45.20490  45.20490
#>  [8]  81.49271  71.58878  71.58878  71.58878  76.56002  76.56002  76.56002
#> [15]  76.56002  76.56002  76.56002  76.56002  79.94617 221.43328

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
#>    [1] 5.708881e+01 6.606207e+01 6.655758e+01 5.332387e+01 6.620985e+01
#>    [6] 6.359090e+01 6.846490e+01 8.370835e+01 6.359362e+01 2.834625e+02
#>   [11] 8.594689e+01 8.128129e+01 6.381974e+01 6.926246e+01 5.493640e+01
#>   [16] 7.905103e+01 6.298044e+01 6.547067e+01 6.371527e+01 5.583205e+01
#>   [21] 6.877795e+01 6.250433e+01 6.240803e+01 6.089991e+01 6.048181e+01
#>   [26] 6.406121e+01 5.749362e+01 6.562153e+01 7.307613e+01 7.194669e+01
#>   [31] 5.879394e+01 7.442153e+01 7.990089e+01 6.559782e+01 7.072631e+01
#>   [36] 7.231688e+01 9.913885e+01 6.042503e+01 7.546130e+01 5.331481e+01
#>   [41] 6.353213e+01 5.562476e+01 2.767991e+02 8.003764e+01 7.071911e+01
#>   [46] 6.018300e+01 6.772115e+01 6.095402e+01 7.339084e+01 6.914446e+01
#>   [51] 6.385185e+01 6.978899e+01 6.272049e+01 6.814395e+01 7.280840e+01
#>   [56] 5.676035e+01 8.317802e+01 6.059410e+01 8.314191e+01 6.403973e+01
#>   [61] 7.075196e+01 6.815570e+01 5.627552e+01 6.093917e+01 6.769503e+01
#>   [66] 5.639607e+01 6.694036e+01 7.776155e+01 6.487523e+01 5.821054e+01
#>   [71] 6.366387e+01 1.016348e+05 4.269701e+02 6.457610e+01 6.377228e+01
#>   [76] 5.631460e+01 5.680603e+01 6.883494e+01 5.983007e+01 5.790068e+01
#>   [81] 1.103608e+02 9.968145e+01 5.405465e+01 9.278265e+01 8.598553e+01
#>   [86] 2.620777e+02 7.505496e+01 9.925095e+01 6.464287e+01 1.150057e+02
#>   [91] 1.028802e+02 6.598525e+01 6.270565e+01 5.595697e+01 6.239154e+01
#>   [96] 6.120110e+01 5.957081e+01 5.884467e+01 5.632931e+01 6.428316e+01
#>  [101] 7.082549e+01 8.568808e+01 8.073695e+01 5.877290e+01 6.945424e+01
#>  [106] 1.221916e+02 5.919727e+01 8.416284e+01 1.277140e+02 7.794216e+01
#>  [111] 6.175943e+01 7.442846e+01 1.776483e+02 7.520835e+01 5.991380e+01
#>  [116] 5.829692e+01 1.165742e+02 4.966736e+01 7.093010e+01 6.595282e+01
#>  [121] 6.973815e+01 5.541607e+01 5.652534e+01 6.615609e+01 5.904382e+01
#>  [126] 7.197591e+01 2.342744e+02 1.034882e+02 6.158712e+01 7.441541e+01
#>  [131] 5.633065e+01 5.894452e+01 6.606587e+01 6.636544e+01 6.262421e+01
#>  [136] 6.565135e+01 8.130723e+01 6.341331e+01 4.836982e+01 5.975691e+01
#>  [141] 8.255350e+01 5.544681e+01 5.952667e+01 5.985992e+01 1.159441e+02
#>  [146] 5.937674e+01 5.806006e+01 7.748877e+01 7.054075e+01 5.805714e+01
#>  [151] 6.719596e+01 5.897552e+01 6.851829e+01 3.289408e+03 8.727171e+01
#>  [156] 8.200032e+01 2.513049e+02 2.017019e+04 1.556254e+03 1.404756e+02
#>  [161] 1.556054e+02 6.447046e+01 9.087163e+01 5.651619e+01 5.466012e+01
#>  [166] 7.861636e+01 5.536429e+01 5.706392e+01 6.040981e+01 5.796707e+01
#>  [171] 6.965370e+01 9.896611e+01 5.924496e+01 8.192397e+01 6.736649e+01
#>  [176] 6.232328e+01 7.407263e+01 6.240909e+01 5.916413e+01 6.240235e+01
#>  [181] 7.688868e+01 6.216929e+01 9.247071e+01 6.142590e+01 5.738861e+01
#>  [186] 1.069357e+02 1.604501e+02 6.231775e+01 5.471174e+01 6.653435e+01
#>  [191] 5.980750e+01 4.885702e+01 5.782087e+01 5.960899e+01 5.397919e+01
#>  [196] 5.753771e+01 7.772534e+01 5.609032e+01 5.804208e+01 5.819032e+01
#>  [201] 1.708803e+02 1.997814e+02 2.505700e+01 7.041609e+02 4.834171e+01
#>  [206] 5.711832e+01 5.147694e+01 5.372485e+01 6.628899e+01 6.949018e+01
#>  [211] 7.539966e+01 6.397419e+01 5.871178e+01 6.234408e+01 5.896846e+01
#>  [216] 6.259060e+02 5.619510e+01 5.998380e+01 8.754741e+01 5.942554e+01
#>  [221] 6.321342e+01 8.284427e+01 5.739109e+01 9.111246e+01 3.374305e+02
#>  [226] 6.285481e+01 5.439477e+01 6.666901e+01 7.297998e+01 6.004689e+01
#>  [231] 6.910224e+01 6.642962e+01 6.326835e+01 5.545604e+01 8.550917e+01
#>  [236] 8.668217e+01 5.790880e+01 1.693829e+02 5.672813e+01 7.439384e+01
#>  [241] 8.401498e+01 5.773367e+01 6.724320e+01 6.381122e+01 8.997410e+01
#>  [246] 5.952447e+01 5.296806e+01 5.867192e+01 5.978638e+01 6.383709e+01
#>  [251] 7.561803e+01 7.729008e+01 1.473867e+02 8.259392e+01 8.502758e+01
#>  [256] 6.912304e+01 7.188954e+01 1.557931e+02 6.963825e+01 5.857728e+01
#>  [261] 6.668209e+01 6.757842e+01 5.633933e+01 6.364449e+01 5.985085e+01
#>  [266] 6.133994e+01 6.355276e+01 6.511118e+01 7.106217e+01 5.216525e+02
#>  [271] 4.820590e+01 6.162273e+01 5.059421e+01 5.714294e+01 5.616074e+01
#>  [276] 6.884617e+01 6.072542e+01 5.697778e+01 7.893293e+01 6.457380e+01
#>  [281] 7.299852e+01 5.955545e+01 6.293304e+01 6.113308e+01 5.559858e+01
#>  [286] 6.174040e+01 5.835876e+01 1.104920e+02 9.553293e+01 9.576097e+01
#>  [291] 5.884284e+01 8.842934e+01 1.346050e+02 8.529009e+01 5.690868e+01
#>  [296] 6.390109e+01 6.509177e+01 7.887412e+01 6.918776e+01 6.451061e+01
#>  [301] 5.708406e+01 9.149447e+01 1.220316e+02 5.859252e+01 1.285428e+02
#>  [306] 6.622868e+01 8.133190e+01 9.810332e+01 6.989704e+01 5.971186e+01
#>  [311] 6.631212e+01 6.505863e+01 6.677970e+01 1.209733e+02 5.722497e+01
#>  [316] 6.011634e+01 5.634772e+01 8.858845e+01 6.275858e+01 6.496237e+01
#>  [321] 6.032476e+01 5.883940e+01 6.347107e+01 1.244081e+02 6.727996e+01
#>  [326] 7.797090e+01 6.227609e+01 6.769890e+01 5.587862e+01 5.680039e+01
#>  [331] 6.432250e+01 6.938949e+01 7.447720e+01 7.035870e+01 1.064211e+02
#>  [336] 6.837922e+01 7.623773e+01 7.197604e+01 6.123283e+01 5.924940e+01
#>  [341] 5.767000e+01 6.143668e+01 5.836053e+01 7.004851e+01 6.351943e+01
#>  [346] 9.895151e+01 6.350359e+01 6.025221e+01 9.488267e+01 5.772503e+01
#>  [351] 7.753726e+01 6.941903e+01 6.437033e+01 6.211807e+01 5.640300e+01
#>  [356] 6.964170e+01 8.389624e+01 7.682565e+01 5.566390e+01 7.051817e+01
#>  [361] 1.112399e+02 6.237975e+01 8.359241e+01 5.783114e+01 7.584270e+01
#>  [366] 5.712012e+01 6.153441e+01 5.718027e+01 5.690904e+01 5.873049e+01
#>  [371] 5.246109e+02 1.725944e+02 6.190901e+01 1.132897e+02 6.488010e+01
#>  [376] 5.898276e+01 8.011689e+01 5.495034e+01 5.902387e+01 6.316584e+01
#>  [381] 6.034432e+01 7.471155e+01 8.911053e+01 5.855832e+01 6.968247e+01
#>  [386] 6.393501e+01 8.657288e+01 5.292137e+01 7.525693e+01 5.994614e+01
#>  [391] 5.776467e+01 6.031440e+01 5.834659e+01 5.580174e+01 6.198049e+01
#>  [396] 5.033246e+01 5.418790e+01 1.980532e+03 7.022889e+01 5.584445e+01
#>  [401] 6.498096e+01 9.633736e+01 1.178767e+02 7.062276e+01 6.720882e+01
#>  [406] 6.730464e+01 8.980996e+01 5.611354e+01 5.897106e+01 6.916859e+01
#>  [411] 1.109554e+02 2.998297e+02 5.216784e+01 6.170562e+01 9.398688e+01
#>  [416] 7.749429e+01 1.010915e+02 1.298190e+02 8.852337e+01 1.054322e+02
#>  [421] 6.724934e+01 5.696636e+01 1.616324e+02 6.059574e+01 7.646524e+01
#>  [426] 1.044286e+02 6.386513e+01 6.521970e+01 6.313505e+01 5.810179e+01
#>  [431] 5.795705e+01 6.104348e+01 5.985394e+01 6.273303e+01 5.791155e+01
#>  [436] 5.649923e+01 5.685824e+01 5.879704e+01 7.087641e+01 6.361512e+01
#>  [441] 8.936038e+01 6.214383e+01 5.924912e+01 6.263799e+01 5.599291e+02
#>  [446] 9.042918e+01 6.628150e+01 1.471944e+03 5.468513e+01 6.487321e+01
#>  [451] 7.076747e+01 5.862569e+01 8.305847e+01 6.562120e+01 1.460960e+02
#>  [456] 8.168299e+01 8.729210e+01 9.151484e+01 6.768506e+01 6.660406e+01
#>  [461] 5.908263e+01 2.408299e+02 6.242151e+01 5.775624e+01 7.370925e+01
#>  [466] 6.863996e+01 5.740517e+01 8.744612e+01 6.823832e+01 5.526577e+01
#>  [471] 5.979322e+01 4.094434e+02 4.991746e+01 5.774250e+01 6.171916e+01
#>  [476] 6.063453e+01 6.140173e+01 8.491332e+01 8.262527e+01 1.123632e+02
#>  [481] 1.042082e+02 5.833921e+01 7.684633e+01 1.183217e+02 6.334924e+01
#>  [486] 8.874538e+01 5.374460e+01 4.476381e+02 1.056353e+02 5.137137e+01
#>  [491] 6.436559e+01 6.319523e+01 1.390806e+02 1.681076e+02 5.729710e+01
#>  [496] 7.208704e+01 5.963410e+01 1.151342e+02 2.012717e+02 1.010782e+03
#>  [501] 8.684687e+01 7.782856e+01 6.629989e+01 1.322283e+02 6.819576e+01
#>  [506] 7.694449e+01 1.103606e+02 6.894424e+01 6.873347e+01 6.195407e+01
#>  [511] 6.152616e+01 7.727189e+01 5.677496e+01 7.075742e+01 6.557865e+01
#>  [516] 5.858250e+01 1.202927e+02 5.916755e+01 8.796723e+01 1.192965e+02
#>  [521] 6.975668e+01 6.193306e+01 6.389910e+01 6.773624e+01 6.357502e+01
#>  [526] 5.576402e+01 8.798258e+01 6.287204e+01 8.420689e+01 6.514294e+01
#>  [531] 5.516112e+01 8.314596e+01 6.838242e+01 8.921175e+01 3.985280e+02
#>  [536] 1.169890e+02 9.275173e+01 9.210534e+01 7.558769e+01 5.191296e+01
#>  [541] 8.036111e+01 6.231929e+01 5.440848e+01 5.685034e+01 6.257586e+01
#>  [546] 5.347329e+01 7.458430e+01 5.716182e+01 7.559634e+01 6.844526e+01
#>  [551] 6.411077e+01 6.574853e+01 7.335110e+01 8.634660e+01 5.921190e+01
#>  [556] 6.409865e+01 6.289851e+01 5.806874e+01 6.014385e+01 7.190611e+02
#>  [561] 5.764898e+01 8.456123e+01 5.844950e+01 6.808738e+01 5.279849e+01
#>  [566] 1.599649e+02 5.971664e+01 6.312463e+01 6.937152e+01 5.543667e+01
#>  [571] 6.309646e+01 5.820666e+01 6.068752e+01 5.856139e+01 5.773339e+01
#>  [576] 5.681808e+01 5.798934e+01 5.537576e+01 1.084668e+02 1.085312e+02
#>  [581] 8.192907e+01 5.291417e+01 6.135244e+01 6.732459e+01 6.599963e+01
#>  [586] 6.445434e+01 9.051496e+01 6.805756e+01 1.009878e+02 8.732518e+01
#>  [591] 6.123754e+01 6.218776e+01 5.976547e+01 5.972513e+01 6.768133e+01
#>  [596] 8.032419e+01 7.330561e+01 8.159729e+01 6.572461e+01 1.904003e+02
#>  [601] 4.484352e+01 6.453509e+01 6.231489e+01 5.568162e+01 6.172066e+01
#>  [606] 6.297527e+01 6.029972e+01 8.301555e+01 6.136717e+01 6.431250e+01
#>  [611] 6.266614e+02 6.891060e+01 6.035646e+01 8.016641e+01 7.158421e+01
#>  [616] 5.849957e+01 5.974252e+01 6.758730e+01 6.269189e+01 6.296073e+01
#>  [621] 6.117720e+01 6.022822e+01 6.180704e+01 8.241159e+01 5.996310e+01
#>  [626] 2.283269e+02 5.793776e+01 6.728845e+01 7.888820e+01 7.453638e+01
#>  [631] 6.390202e+01 6.225866e+01 9.092191e+01 5.737876e+01 6.995395e+01
#>  [636] 5.580681e+01 5.976984e+01 5.784443e+01 6.113002e+01 6.123154e+01
#>  [641] 5.863468e+01 1.117335e+02 1.231656e+02 5.938931e+01 6.197085e+01
#>  [646] 9.761671e+01 6.068978e+01 9.724628e+01 6.045025e+01 7.509559e+01
#>  [651] 5.105400e+01 8.674288e+01 7.122821e+01 5.378599e+01 8.233802e+01
#>  [656] 5.970053e+01 4.252859e+02 5.929585e+01 8.493352e+01 6.027678e+01
#>  [661] 5.445504e+01 7.111457e+01 5.146637e+01 6.569209e+01 5.349943e+01
#>  [666] 6.175588e+01 1.207365e+02 7.752144e+01 5.814314e+01 7.739775e+01
#>  [671] 7.676832e+01 5.716893e+01 7.519654e+01 6.011124e+01 7.265036e+01
#>  [676] 5.726453e+01 9.712372e+01 7.373480e+01 5.893499e+01 9.953700e+01
#>  [681] 6.196650e+01 6.837514e+01 5.912928e+01 6.322233e+01 2.119247e+02
#>  [686] 5.512738e+01 7.619601e+01 7.200400e+01 8.502579e+01 5.805045e+01
#>  [691] 6.705368e+01 6.407460e+01 6.468502e+01 7.083761e+01 9.100412e+01
#>  [696] 4.801542e+01 5.096597e+01 6.903434e+01 5.331815e+01 4.422016e+01
#>  [701] 5.636454e+01 1.299375e+02 6.335199e+01 7.482055e+01 6.229605e+01
#>  [706] 1.425919e+02 6.704896e+01 7.531243e+01 6.322559e+01 6.483901e+01
#>  [711] 5.691779e+01 6.825628e+01 5.356775e+01 1.964018e+02 7.638451e+01
#>  [716] 6.190467e+01 6.833264e+01 5.977726e+01 7.914895e+01 6.038313e+01
#>  [721] 5.651441e+01 7.378136e+01 7.141127e+01 7.993862e+01 1.245719e+02
#>  [726] 7.291647e+01 6.619006e+01 6.291685e+01 7.483818e+01 7.058455e+01
#>  [731] 6.244120e+01 1.742802e+02 6.447175e+01 5.663456e+01 6.590013e+01
#>  [736] 7.860152e+01 8.295511e+01 5.928899e+01 6.201101e+01 6.737685e+01
#>  [741] 9.506651e+01 7.239680e+01 1.869611e+02 9.552805e+01 1.011503e+02
#>  [746] 8.526247e+01 5.585184e+01 6.931379e+01 5.997769e+01 7.551608e+01
#>  [751] 5.664812e+01 6.857016e+01 1.019368e+02 5.864764e+01 6.085596e+01
#>  [756] 2.105321e+02 6.953046e+01 6.194715e+01 7.859411e+01 6.505062e+01
#>  [761] 6.500044e+01 6.986110e+01 5.702238e+01 1.006739e+02 5.988739e+01
#>  [766] 8.897405e+01 6.435784e+01 6.977432e+01 6.562129e+01 5.719004e+01
#>  [771] 5.828761e+01 6.549069e+01 6.778835e+01 5.607260e+01 5.442342e+01
#>  [776] 5.890915e+01 2.682411e+02 5.860744e+01 5.955593e+01 9.871054e+01
#>  [781] 6.344085e+01 6.138610e+01 5.644155e+01 5.655040e+01 6.094789e+01
#>  [786] 7.299305e+01 6.390275e+01 5.573548e+01 5.879515e+01 6.707761e+01
#>  [791] 6.620532e+01 4.956287e+01 8.976752e+01 8.888835e+01 1.101028e+02
#>  [796] 2.465722e+02 4.177788e+02 2.949602e+02 8.686922e+01 8.634348e+01
#>  [801] 7.399545e+01 7.291425e+01 6.149480e+01 6.495001e+01 9.887317e+01
#>  [806] 1.247266e+02 7.922264e+01 8.172525e+01 6.536141e+01 6.497116e+01
#>  [811] 6.035880e+01 1.025426e+02 6.016829e+01 6.797736e+01 6.409407e+01
#>  [816] 6.577589e+01 5.971256e+01 5.956693e+01 9.630056e+01 6.042492e+01
#>  [821] 8.339181e+01 6.757434e+01 7.026961e+01 6.171600e+01 6.587058e+01
#>  [826] 5.819576e+01 4.681860e+02 5.531214e+01 1.856134e+03 2.106583e+02
#>  [831] 6.286481e+01 5.750008e+01 6.424845e+01 8.612368e+01 5.691564e+01
#>  [836] 6.631983e+01 5.730854e+01 6.965914e+01 6.437809e+01 5.830864e+01
#>  [841] 6.411667e+01 7.609523e+01 1.199835e+02 7.126324e+01 8.946212e+01
#>  [846] 2.322983e+02 6.270366e+01 9.002599e+01 9.082287e+01 6.279611e+01
#>  [851] 5.842263e+01 6.140533e+01 6.295236e+01 6.181787e+01 6.172711e+01
#>  [856] 6.222875e+01 6.281266e+01 7.982737e+01 6.846627e+01 1.133413e+02
#>  [861] 5.766835e+01 5.935619e+01 6.019728e+01 6.123962e+01 6.010414e+01
#>  [866] 6.364009e+01 8.407701e+01 8.301081e+01 9.479161e+01 6.829092e+01
#>  [871] 6.141190e+01 7.738581e+01 5.994322e+01 2.229697e+02 1.540689e+02
#>  [876] 1.623530e+02 6.463287e+01 7.058568e+01 6.249712e+01 2.433749e+02
#>  [881] 5.526307e+01 5.832974e+01 5.566180e+01 5.985271e+01 9.077550e+01
#>  [886] 8.047353e+01 5.964633e+01 6.689396e+01 9.096513e+01 5.968313e+01
#>  [891] 6.502031e+01 6.954470e+01 6.470121e+01 6.447648e+01 6.793272e+01
#>  [896] 7.906296e+01 6.582922e+01 5.926879e+01 6.108520e+01 6.793151e+01
#>  [901] 5.673962e+01 6.057567e+01 6.181756e+01 7.016549e+01 8.283634e+01
#>  [906] 6.815391e+01 6.650926e+01 7.372107e+01 1.292347e+04 7.614725e+01
#>  [911] 5.903297e+01 8.698541e+01 5.282437e+01 6.184468e+01 6.488349e+01
#>  [916] 6.700375e+01 5.857909e+01 6.240995e+01 6.341998e+01 5.923486e+01
#>  [921] 5.003919e+01 7.102221e+01 5.670660e+01 6.555306e+01 5.378593e+01
#>  [926] 6.373027e+01 7.478007e+01 7.720049e+01 5.649548e+01 1.109953e+02
#>  [931] 3.311450e+02 7.419727e+01 5.883469e+01 6.581980e+01 1.521256e+02
#>  [936] 1.660989e+02 5.523405e+01 6.158339e+01 6.182143e+01 6.388332e+01
#>  [941] 6.036684e+01 5.702107e+01 9.052975e+01 3.241803e+02 5.901143e+01
#>  [946] 5.807677e+01 7.501551e+01 5.542311e+01 6.317289e+01 6.104272e+01
#>  [951] 6.459525e+01 5.694143e+01 6.247035e+01 7.079765e+01 6.216215e+01
#>  [956] 5.661393e+01 6.078076e+01 5.918580e+01 6.488894e+01 6.535957e+01
#>  [961] 6.260752e+01 1.371755e+02 5.384752e+01 1.243261e+02 6.248056e+01
#>  [966] 5.917140e+01 8.278038e+01 5.825253e+01 5.968935e+01 6.194912e+01
#>  [971] 5.692605e+01 1.577737e+02 5.965501e+01 6.740770e+01 6.668638e+01
#>  [976] 1.512534e+02 6.235110e+01 5.615646e+01 5.922252e+01 2.672410e+02
#>  [981] 8.463514e+01 5.845857e+01 6.595097e+01 6.795405e+01 6.364588e+01
#>  [986] 5.472419e+01 9.014713e+01 5.816243e+01 6.649095e+01 1.314887e+02
#>  [991] 1.455053e+02 7.484303e+03 1.020513e+02 5.445620e+01 7.231462e+01
#>  [996] 1.486549e+03 1.081617e+15 1.307218e+12 2.213488e+02 6.802598e+01
```
