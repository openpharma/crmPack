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
#>  [1] 100.47955 100.47955 100.47955 100.47955  45.82904  17.51505  17.51505
#>  [8]  46.21283  96.14541  62.24033  55.35209  72.17750  29.39142  29.39142
#> [15]  29.39142  20.08512  22.17647  16.80189  16.80189  18.03365

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
#>    [1] 8.400061e+01 7.736317e+01 6.064841e+01 5.842148e+01 6.286254e+01
#>    [6] 6.034605e+01 6.388750e+01 5.844367e+01 1.209685e+02 5.886244e+01
#>   [11] 5.767280e+01 1.018468e+02 1.081109e+02 8.541000e+01 6.253894e+02
#>   [16] 1.150611e+03 5.922368e+01 7.523605e+01 5.710309e+01 9.483074e+01
#>   [21] 6.097422e+01 7.613457e+01 1.075663e+02 6.047654e+01 5.731905e+01
#>   [26] 8.125326e+01 1.093057e+02 5.451305e+01 6.105615e+01 6.917807e+01
#>   [31] 1.992029e+02 6.371228e+01 5.396348e+01 2.066150e+02 1.959148e+02
#>   [36] 5.782267e+01 7.417969e+01 6.589087e+01 5.385583e+01 5.827111e+01
#>   [41] 9.527372e+01 5.445643e+01 1.144456e+02 5.127731e+01 5.787148e+01
#>   [46] 6.290703e+01 6.382445e+01 5.748179e+01 5.605766e+01 7.080126e+01
#>   [51] 5.785204e+01 6.978652e+01 7.813089e+01 5.335330e+01 5.815304e+01
#>   [56] 5.604402e+01 6.858640e+01 5.594126e+01 7.833942e+01 5.957427e+01
#>   [61] 8.808140e+01 8.380069e+01 5.415799e+01 7.649457e+01 6.667259e+01
#>   [66] 6.229484e+01 6.670569e+01 9.827657e+01 1.612776e+02 2.476976e+02
#>   [71] 6.028727e+01 5.908931e+01 6.772845e+01 6.170871e+01 5.923148e+01
#>   [76] 6.204006e+01 6.602954e+01 1.147208e+02 6.022337e+01 5.970503e+01
#>   [81] 6.894380e+01 6.445700e+01 6.024072e+01 5.973375e+01 9.382062e+01
#>   [86] 6.024542e+01 3.372598e+02 6.313501e+01 6.472640e+01 5.811412e+01
#>   [91] 5.763037e+01 6.973990e+01 9.939813e+01 7.223618e+01 5.447619e+01
#>   [96] 5.694030e+01 5.877764e+01 5.799584e+01 6.040939e+01 5.944634e+01
#>  [101] 5.789955e+01 5.978484e+01 9.414937e+01 8.543759e+01 1.341931e+02
#>  [106] 1.265938e+02 6.166368e+01 6.455646e+01 8.263113e+01 6.527821e+01
#>  [111] 8.796477e+01 5.588002e+01 2.221540e+02 1.217010e+02 1.599811e+02
#>  [116] 7.225547e+01 8.326950e+01 7.296261e+01 7.024659e+01 8.813301e+01
#>  [121] 5.334617e+01 6.054247e+01 4.726814e+02 7.038237e+01 8.024393e+01
#>  [126] 6.195710e+01 6.830398e+01 5.816416e+01 6.748728e+01 4.669954e+02
#>  [131] 7.032550e+01 6.968253e+01 5.916561e+01 5.378469e+01 5.650302e+01
#>  [136] 1.063866e+02 6.090078e+01 7.366565e+01 1.374298e+02 7.141340e+01
#>  [141] 6.179758e+01 6.234227e+01 9.982874e+01 7.086039e+01 5.788101e+01
#>  [146] 1.135999e+02 5.316287e+01 6.493741e+01 5.420949e+01 2.197610e+02
#>  [151] 9.260736e+01 8.912091e+01 5.345576e+01 5.786643e+01 3.299791e+02
#>  [156] 9.621732e+01 6.892416e+01 5.670825e+01 5.823190e+01 6.282162e+01
#>  [161] 7.866896e+01 1.613259e+02 6.755288e+01 5.730074e+01 7.445241e+01
#>  [166] 7.342608e+01 7.040640e+01 5.475559e+01 7.122122e+01 7.824225e+01
#>  [171] 7.244146e+01 6.417379e+01 7.289512e+01 5.940058e+01 6.466563e+01
#>  [176] 5.749083e+01 5.804382e+01 7.120037e+01 8.895087e+01 7.921218e+01
#>  [181] 6.068617e+01 6.329073e+01 5.891398e+01 6.778369e+01 7.576623e+01
#>  [186] 6.097784e+01 8.083959e+01 5.894784e+01 6.137258e+01 6.110345e+01
#>  [191] 6.391660e+01 5.567806e+01 1.560922e+02 7.118098e+01 6.004828e+01
#>  [196] 1.385761e+02 5.620802e+01 6.500879e+01 5.705863e+01 1.650142e+02
#>  [201] 6.156360e+01 7.062003e+01 1.179481e+02 8.748499e+01 4.329772e+01
#>  [206] 5.733402e+01 6.013898e+01 5.888072e+01 5.529689e+01 6.537444e+01
#>  [211] 1.538805e+02 5.927447e+01 7.061086e+01 5.924908e+01 5.982743e+01
#>  [216] 7.957190e+01 6.753777e+01 7.537257e+01 5.642301e+01 6.414310e+01
#>  [221] 6.193073e+01 5.779960e+01 6.476561e+01 6.630679e+01 6.529134e+01
#>  [226] 6.510966e+01 6.636931e+01 5.570052e+01 6.105135e+01 7.435448e+01
#>  [231] 5.902012e+01 6.062392e+01 7.437902e+01 7.306408e+01 1.162588e+02
#>  [236] 6.830083e+01 8.518983e+01 6.307047e+01 8.307164e+01 1.045746e+02
#>  [241] 1.733632e+03 6.147685e+01 7.781919e+01 5.962628e+01 6.580613e+01
#>  [246] 6.923889e+01 5.554511e+01 4.057589e+01 1.268998e+02 8.969345e+01
#>  [251] 7.349629e+01 5.569468e+01 1.119750e+02 6.614893e+01 8.060345e+01
#>  [256] 9.282048e+01 6.164147e+01 7.261684e+01 9.769935e+01 1.004025e+02
#>  [261] 8.146594e+01 7.523225e+01 5.772825e+01 6.093239e+01 7.036128e+01
#>  [266] 6.552002e+01 6.238859e+01 5.988435e+01 6.467839e+01 6.411392e+01
#>  [271] 6.004160e+01 1.094166e+02 8.818872e+01 6.471123e+01 7.763711e+01
#>  [276] 5.406923e+01 6.185909e+01 6.081056e+01 1.067161e+02 6.379424e+01
#>  [281] 8.444809e+01 9.823739e+01 4.362702e+03 1.158515e+02 5.826946e+01
#>  [286] 6.530816e+01 7.086470e+01 6.322298e+01 7.333852e+01 5.423797e+01
#>  [291] 6.862247e+01 2.697553e+02 5.327707e+01 1.624749e+02 8.820580e+01
#>  [296] 1.002569e+02 3.594452e+02 7.411825e+01 7.626808e+01 5.279764e+01
#>  [301] 5.904833e+01 6.158725e+03 5.920543e+01 5.822221e+01 8.954740e+01
#>  [306] 6.472107e+01 5.800239e+01 7.982621e+01 1.307917e+02 6.026262e+01
#>  [311] 6.366501e+01 1.351615e+02 7.705107e+01 7.876483e+01 1.119095e+02
#>  [316] 5.849959e+01 5.911429e+01 1.860126e+02 1.036950e+02 7.496259e+02
#>  [321] 8.502724e+01 5.864090e+01 7.267376e+01 5.388260e+01 5.852287e+01
#>  [326] 6.527533e+01 1.313865e+03 3.401830e+05 7.106709e+01 1.161470e+02
#>  [331] 5.567273e+01 8.011167e+01 6.257831e+01 4.548406e+01 6.882975e+01
#>  [336] 6.738805e+01 8.542259e+01 5.933771e+01 5.958378e+01 5.807866e+01
#>  [341] 6.415693e+01 6.560502e+01 6.435786e+01 5.138477e+01 6.224141e+01
#>  [346] 6.182801e+01 9.687228e+01 6.473780e+01 6.068113e+01 5.797136e+01
#>  [351] 1.144924e+02 5.583343e+01 5.994681e+01 7.084982e+01 5.834870e+01
#>  [356] 7.253227e+01 6.386944e+01 6.134718e+01 6.029853e+01 7.923729e+01
#>  [361] 5.916349e+01 1.099827e+02 8.423761e+01 7.501627e+01 5.640440e+01
#>  [366] 7.242607e+01 5.845790e+01 6.059989e+01 7.629290e+01 5.849249e+01
#>  [371] 3.830093e+02 4.530161e+01 6.711050e+01 1.096892e+02 6.054150e+01
#>  [376] 6.779748e+01 5.738480e+01 1.807315e+02 5.987986e+01 6.085960e+01
#>  [381] 6.179190e+01 6.288549e+01 5.817095e+01 5.843306e+01 5.013548e+01
#>  [386] 4.934258e+01 2.508683e+02 6.059644e+01 7.164723e+01 1.106497e+02
#>  [391] 7.011105e+01 6.065388e+01 5.498771e+01 6.096017e+01 5.671653e+01
#>  [396] 6.066895e+01 5.838066e+01 1.015269e+02 5.370472e+01 6.380093e+02
#>  [401] 1.053352e+02 6.074783e+01 6.920070e+01 5.563298e+01 7.573711e+01
#>  [406] 9.559943e+01 1.750989e+02 6.261436e+01 5.922613e+01 7.127061e+01
#>  [411] 3.058629e+02 6.318663e+01 6.252008e+01 6.203341e+01 5.867862e+01
#>  [416] 6.405651e+01 7.647253e+01 1.111748e+02 6.837871e+01 6.229870e+01
#>  [421] 6.420221e+01 1.039207e+02 3.076617e+04 7.191206e+01 7.183883e+01
#>  [426] 5.881532e+01 6.153729e+01 6.406440e+01 2.104032e+02 5.054124e+01
#>  [431] 9.089129e+01 2.463936e+02 5.684377e+01 5.036216e+01 6.187464e+01
#>  [436] 1.782652e+02 7.640281e+01 1.127255e+02 5.997589e+01 7.321912e+01
#>  [441] 5.597565e+01 5.669697e+01 6.720225e+01 6.026362e+01 6.631154e+01
#>  [446] 6.522859e+01 6.832173e+01 4.643314e+01 9.243335e+01 5.765565e+01
#>  [451] 1.540098e+02 7.913316e+01 6.747017e+01 7.061630e+01 7.898000e+01
#>  [456] 6.166344e+01 5.623248e+01 9.655511e+01 6.888388e+01 8.137110e+01
#>  [461] 1.671145e+02 6.924087e+01 6.861087e+01 1.075454e+02 7.060792e+01
#>  [466] 4.148112e+01 5.739424e+01 5.704692e+01 6.291064e+01 1.147855e+02
#>  [471] 8.119151e+01 5.666117e+01 8.637558e+01 5.714943e+01 5.938649e+01
#>  [476] 1.013234e+02 2.431298e+02 5.416153e+01 5.926372e+01 5.806192e+01
#>  [481] 8.040513e+01 5.787848e+01 6.306104e+01 6.507037e+01 5.899462e+01
#>  [486] 7.766759e+01 6.565831e+01 5.357208e+01 5.754732e+01 5.718528e+01
#>  [491] 6.549233e+01 1.166673e+03 2.987685e+02 9.979010e+01 5.523566e+01
#>  [496] 7.029123e+01 6.264392e+01 4.623064e+01 6.322793e+01 5.706574e+01
#>  [501] 6.197771e+01 1.870069e+03 4.352476e+01 8.234511e+01 6.558525e+01
#>  [506] 1.210247e+02 8.759496e+01 5.108745e+01 7.258315e+01 7.285713e+01
#>  [511] 6.040668e+01 6.691873e+01 6.422419e+01 6.557546e+01 7.097347e+01
#>  [516] 5.720280e+01 8.543110e+01 5.740312e+01 6.891363e+01 5.668166e+01
#>  [521] 5.420314e+01 6.138384e+01 1.707186e+02 5.570262e+01 7.022920e+01
#>  [526] 5.996780e+01 5.891998e+01 6.299169e+01 5.838212e+01 1.027138e+02
#>  [531] 5.970311e+01 6.185494e+01 6.317843e+01 6.676966e+01 7.329642e+01
#>  [536] 6.817661e+01 5.922782e+02 6.222051e+01 5.848571e+01 6.439211e+01
#>  [541] 1.216543e+02 5.695620e+01 5.954185e+01 2.753365e+02 7.696724e+01
#>  [546] 8.510929e+01 5.623160e+01 6.046860e+01 5.535228e+01 9.512416e+01
#>  [551] 5.673218e+01 5.822356e+01 6.281861e+01 6.282261e+01 7.517492e+01
#>  [556] 5.778230e+01 7.031925e+01 2.426652e+02 7.191980e+01 6.410327e+01
#>  [561] 6.182094e+01 7.767029e+01 7.350702e+01 6.737382e+01 6.274201e+01
#>  [566] 5.690884e+01 5.634299e+01 5.604775e+01 6.700798e+01 1.080114e+02
#>  [571] 6.246743e+01 6.052401e+01 7.845649e+01 6.769029e+01 6.544533e+01
#>  [576] 7.769559e+01 1.601442e+04 1.041984e+02 5.670319e+01 5.542800e+01
#>  [581] 5.906785e+01 6.242227e+01 6.137957e+01 1.370266e+02 6.327300e+01
#>  [586] 5.990396e+01 5.772500e+01 5.872715e+01 5.382765e+01 8.423609e+01
#>  [591] 5.740978e+01 6.230701e+01 5.451433e+01 5.752691e+01 7.533645e+01
#>  [596] 6.231271e+01 6.081377e+01 1.745091e+02 6.097973e+01 6.989023e+01
#>  [601] 7.033597e+01 5.209327e+01 8.678506e+01 6.860015e+01 1.409213e+02
#>  [606] 6.160345e+01 9.324482e+01 7.276736e+01 6.620240e+01 5.914599e+01
#>  [611] 1.034521e+02 5.231117e+01 6.886806e+01 6.082712e+01 7.288145e+01
#>  [616] 6.572481e+01 6.368612e+01 1.853042e+02 6.163474e+01 6.296534e+01
#>  [621] 6.736985e+01 6.558823e+01 8.777970e+01 6.992130e+01 5.955940e+01
#>  [626] 5.915786e+01 4.991895e+01 6.292755e+01 6.048612e+01 6.559670e+01
#>  [631] 8.022658e+01 7.773143e+01 9.114029e+01 6.385400e+01 5.770780e+01
#>  [636] 7.197512e+01 1.064338e+02 1.789675e+02 1.883719e+02 6.041139e+01
#>  [641] 6.855437e+01 5.911383e+01 7.255842e+01 5.709784e+01 7.003931e+01
#>  [646] 5.708208e+01 5.600574e+01 7.747524e+01 8.170358e+01 5.433265e+01
#>  [651] 6.533522e+01 5.496173e+01 1.150816e+02 1.653471e+02 8.337285e+01
#>  [656] 8.033849e+01 6.189463e+01 1.086483e+02 1.330870e+02 6.403979e+01
#>  [661] 5.664681e+01 5.793344e+01 6.733870e+01 5.998992e+01 5.595543e+01
#>  [666] 4.947195e+01 7.799604e+01 8.264651e+01 1.209257e+02 6.926602e+01
#>  [671] 5.949898e+01 5.695795e+01 7.014380e+01 6.162008e+01 6.516625e+01
#>  [676] 9.348687e+01 5.331701e+01 1.357515e+02 6.535744e+01 9.476117e+01
#>  [681] 5.780024e+01 7.479962e+01 5.765929e+01 9.631819e+01 8.505383e+01
#>  [686] 6.479354e+01 6.247610e+01 6.016317e+01 5.713734e+01 6.747424e+01
#>  [691] 6.289548e+01 7.391298e+01 5.845458e+01 1.208570e+02 6.100793e+01
#>  [696] 6.289570e+01 5.883837e+01 6.071431e+01 5.920762e+01 6.039637e+01
#>  [701] 6.445981e+02 6.618064e+01 7.718815e+01 5.585770e+01 5.680147e+01
#>  [706] 1.322728e+02 4.697081e+02 5.020197e+02 7.334974e+01 1.399706e+02
#>  [711] 5.182630e+01 5.812769e+01 5.731333e+01 5.782698e+01 6.088111e+01
#>  [716] 5.792772e+01 6.911585e+01 9.548946e+01 5.956017e+01 6.324731e+01
#>  [721] 1.034490e+02 6.117417e+01 6.583014e+01 5.842025e+01 9.510833e+01
#>  [726] 6.380453e+01 6.017498e+01 8.977514e+01 6.003934e+01 6.779845e+01
#>  [731] 7.371814e+01 8.300385e+01 6.598017e+01 2.435055e+02 6.601706e+01
#>  [736] 5.796670e+01 6.532272e+01 5.900473e+01 6.956945e+01 6.375991e+01
#>  [741] 5.653370e+01 7.287113e+01 6.503875e+01 6.829562e+01 6.512041e+01
#>  [746] 6.247632e+01 1.822573e+02 5.828896e+01 7.230828e+01 6.994707e+01
#>  [751] 5.912857e+01 6.396658e+01 6.409282e+01 5.985454e+01 5.863153e+01
#>  [756] 5.906694e+01 7.056650e+01 1.015332e+02 1.273914e+02 9.835572e+01
#>  [761] 6.306502e+01 8.378735e+01 6.410881e+01 6.279909e+01 6.479103e+01
#>  [766] 5.856946e+01 5.601596e+01 7.565806e+01 6.691471e+01 5.980731e+01
#>  [771] 1.140989e+02 5.670593e+01 7.790372e+01 6.405338e+01 6.434887e+01
#>  [776] 7.967648e+01 5.118810e+01 8.064305e+01 6.787703e+01 7.705842e+01
#>  [781] 6.506255e+01 5.674356e+01 6.321196e+01 5.864682e+01 6.102127e+01
#>  [786] 6.666473e+01 6.113104e+01 6.828309e+01 9.545671e+01 1.111220e+02
#>  [791] 6.677053e+01 1.489358e+02 6.926189e+01 7.650751e+01 5.840656e+01
#>  [796] 6.582986e+01 6.972163e+01 8.617863e+01 7.080388e+01 5.697569e+01
#>  [801] 5.323770e+01 1.508177e+02 6.458975e+01 5.812250e+01 1.076801e+02
#>  [806] 7.294233e+03 1.007947e+10 5.779383e+01 7.551514e+01 5.333028e+01
#>  [811] 6.119842e+01 8.612494e+01 6.278966e+01 1.355903e+02 1.826717e+02
#>  [816] 4.027052e+02 5.978334e+01 6.317271e+01 6.327136e+01 6.796426e+01
#>  [821] 1.245510e+02 1.408995e+02 5.724927e+01 5.569294e+01 7.643372e+01
#>  [826] 8.495820e+01 5.873804e+01 9.531720e+01 7.992606e+01 5.323821e+01
#>  [831] 5.808703e+01 7.349438e+01 1.029281e+02 5.619383e+01 6.131901e+01
#>  [836] 8.425204e+01 6.405525e+01 7.242804e+01 9.864117e+01 6.189350e+01
#>  [841] 5.941015e+01 8.331675e+01 5.602608e+01 5.573702e+01 8.080647e+01
#>  [846] 7.832097e+01 5.945578e+01 7.169708e+01 5.873849e+01 1.131878e+02
#>  [851] 5.756430e+01 8.162811e+01 6.731589e+01 5.605926e+01 1.573205e+02
#>  [856] 5.931801e+01 7.911236e+01 1.082693e+02 7.961169e+01 8.142016e+01
#>  [861] 8.143081e+01 5.227278e+01 6.096760e+01 5.543069e+01 6.088236e+01
#>  [866] 6.192464e+01 6.444724e+01 5.729834e+01 8.630427e+01 6.471624e+01
#>  [871] 1.329121e+02 6.129683e+01 5.886942e+01 6.500342e+01 5.831750e+01
#>  [876] 6.012970e+01 1.909993e+02 6.034327e+01 6.348162e+01 7.282432e+01
#>  [881] 6.758554e+01 6.535590e+01 6.081876e+01 8.359564e+01 5.883490e+01
#>  [886] 6.409404e+01 7.404233e+01 6.527557e+01 7.089904e+01 6.943415e+01
#>  [891] 7.951641e+01 7.495148e+01 5.017912e+03 5.757927e+01 6.733549e+01
#>  [896] 5.704523e+01 1.961073e+02 6.305876e+01 7.963909e+01 7.134995e+01
#>  [901] 6.350586e+01 5.819096e+01 6.542488e+01 7.267782e+01 3.020394e+03
#>  [906] 6.630031e+01 6.612582e+01 6.131998e+01 7.650159e+01 6.889428e+01
#>  [911] 5.614611e+01 6.582158e+01 6.017117e+01 1.170429e+02 5.848373e+01
#>  [916] 3.221510e+02 1.160238e+02 9.637180e+01 6.403809e+01 5.671372e+01
#>  [921] 6.559802e+01 1.025806e+02 1.008924e+02 8.285472e+01 8.631795e+01
#>  [926] 7.493812e+01 5.428333e+01 6.470204e+01 6.112181e+01 5.624054e+01
#>  [931] 6.867448e+01 7.090701e+01 5.758010e+01 5.924491e+01 6.735804e+01
#>  [936] 6.186796e+01 5.936896e+01 5.557018e+01 6.003846e+01 5.905878e+01
#>  [941] 6.592304e+01 7.335708e+01 6.553154e+01 6.091934e+01 5.462748e+01
#>  [946] 7.736948e+01 5.675017e+01 5.896093e+01 6.179347e+01 5.821319e+01
#>  [951] 7.987148e+01 9.526813e+01 5.893948e+01 6.311392e+01 6.033272e+01
#>  [956] 8.637173e+01 5.816630e+01 6.171482e+01 5.672213e+01 6.719886e+01
#>  [961] 6.493927e+01 6.583099e+01 5.695391e+01 5.731950e+01 1.595167e+07
#>  [966] 2.822010e+02 5.723113e+01 5.318210e+01 5.775695e+01 5.420911e+01
#>  [971] 6.281042e+01 6.892118e+01 6.360501e+01 6.386278e+01 5.959695e+01
#>  [976] 6.014361e+01 8.629539e+01 6.002634e+01 1.081461e+02 5.949196e+01
#>  [981] 5.994435e+01 8.914817e+01 6.145838e+01 5.600142e+01 5.700578e+01
#>  [986] 5.299850e+01 6.213892e+01 6.186446e+01 7.726535e+01 8.758905e+02
#>  [991] 2.652390e+02 9.975330e+01 1.052682e+02 6.182874e+01 1.004710e+02
#>  [996] 5.772562e+01 7.505561e+01 5.513419e+01 5.571391e+01 8.288437e+01
```
