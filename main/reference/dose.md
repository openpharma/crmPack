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
#>  [1]  52.29325  52.29325  18.22510  39.28094 326.94279 326.94279 326.94279
#>  [8]  61.34120  99.62455  80.38508 140.36726 140.36726  32.76993  32.76993
#> [15]  13.10616  78.53931  85.77135 283.42312 283.42312 129.66462

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
#>    [1] 6.343266e+01 5.622383e+01 6.471690e+01 7.557567e+01 6.381009e+01
#>    [6] 6.075099e+01 6.826015e+01 5.974097e+01 5.935854e+01 7.088871e+01
#>   [11] 5.286102e+01 7.351514e+01 1.195857e+02 6.438393e+01 6.483211e+01
#>   [16] 8.897938e+01 5.675334e+01 5.182616e+01 7.107650e+01 6.316627e+01
#>   [21] 7.569119e+01 5.671304e+01 6.674112e+01 1.575500e+02 1.129238e+02
#>   [26] 5.797415e+01 6.618842e+01 5.702871e+01 6.079314e+01 6.473954e+01
#>   [31] 5.721181e+01 6.145706e+01 5.703419e+01 6.177660e+01 6.088530e+01
#>   [36] 6.618108e+01 5.660946e+01 6.254082e+01 5.361076e+01 7.894059e+01
#>   [41] 7.787103e+02 1.025010e+02 5.815592e+01 6.440923e+01 6.628236e+01
#>   [46] 6.685383e+01 6.124895e+01 5.964216e+01 5.949687e+01 8.153368e+01
#>   [51] 5.817891e+01 1.047506e+02 5.524185e+01 5.259288e+01 8.735841e+01
#>   [56] 6.378040e+01 6.401381e+01 5.580964e+01 6.084765e+01 1.136625e+03
#>   [61] 8.039664e+01 1.156807e+03 7.778513e+01 5.937026e+01 6.913215e+01
#>   [66] 5.514223e+01 6.733653e+01 5.885472e+01 5.992591e+01 6.200333e+01
#>   [71] 7.877204e+01 7.701269e+01 7.106406e+01 7.812950e+01 7.336943e+01
#>   [76] 7.509308e+01 5.682654e+01 7.519953e+01 6.285672e+01 6.406664e+01
#>   [81] 6.203612e+01 5.846054e+01 8.085777e+01 5.407457e+01 6.193600e+01
#>   [86] 7.618131e+01 5.693019e+01 1.145444e+02 5.641103e+01 6.437884e+01
#>   [91] 5.672934e+01 1.017978e+02 5.354328e+01 1.199397e+02 1.140947e+02
#>   [96] 5.917773e+01 5.904788e+01 7.049513e+01 5.462176e+01 6.527596e+01
#>  [101] 7.343921e+01 1.078071e+02 9.089182e+01 6.132392e+01 6.380917e+01
#>  [106] 5.954367e+01 5.900370e+01 6.385464e+01 5.448334e+01 7.129365e+01
#>  [111] 7.547689e+02 1.205857e+02 6.671981e+01 4.200891e+02 5.939382e+01
#>  [116] 5.815692e+01 6.014204e+01 6.595258e+01 6.231107e+01 5.444064e+01
#>  [121] 8.394185e+01 8.745676e+01 7.373017e+01 9.650226e+01 6.950006e+01
#>  [126] 6.794516e+01 6.017201e+01 9.670622e+01 1.047538e+02 8.490047e+01
#>  [131] 5.738324e+01 6.396954e+01 8.083373e+01 6.568237e+01 6.084769e+01
#>  [136] 6.177657e+01 1.927689e+02 7.999465e+01 5.874736e+01 6.059530e+01
#>  [141] 8.477329e+01 1.007650e+02 9.813396e+01 7.470523e+01 7.995014e+01
#>  [146] 6.536306e+01 7.127345e+01 1.038705e+02 1.215510e+02 5.748766e+01
#>  [151] 5.767741e+01 1.417896e+02 6.336021e+01 2.849831e+02 9.865410e+01
#>  [156] 5.849264e+01 1.446637e+02 8.797886e+01 1.623528e+02 9.206252e+01
#>  [161] 1.003381e+02 5.379820e+01 5.946986e+01 6.670019e+01 6.468374e+01
#>  [166] 6.634175e+01 6.139104e+01 6.985316e+01 5.530010e+01 7.349421e+01
#>  [171] 6.432591e+01 6.190075e+01 9.200671e+01 5.621233e+01 5.931308e+01
#>  [176] 8.540717e+01 5.682421e+01 6.583531e+01 8.343017e+01 6.858052e+01
#>  [181] 5.558696e+01 5.359729e+01 6.289713e+01 6.205820e+01 1.179506e+02
#>  [186] 3.032444e+04 7.304900e+01 5.755914e+01 5.903639e+01 5.323273e+01
#>  [191] 6.104828e+01 6.658327e+01 6.602690e+01 1.157140e+02 5.526997e+01
#>  [196] 2.125336e+02 5.909981e+01 8.530448e+01 1.256527e+02 5.618240e+01
#>  [201] 1.069156e+02 5.762192e+01 1.351641e+02 8.111226e+01 8.228629e+01
#>  [206] 7.442905e+01 5.892689e+01 7.303344e+01 5.813701e+01 9.524277e+01
#>  [211] 1.409916e+02 1.689653e+06 7.799345e+01 6.830094e+01 7.878209e+01
#>  [216] 6.059529e+02 4.940207e+02 5.463223e+01 9.655972e+01 5.638348e+01
#>  [221] 7.988160e+01 6.039816e+01 6.211935e+01 6.522672e+01 5.114043e+01
#>  [226] 5.829999e+01 8.030897e+01 8.842753e+01 1.959976e+02 5.700359e+01
#>  [231] 6.061025e+01 6.847863e+01 6.299567e+01 5.311429e+01 5.506890e+01
#>  [236] 8.220678e+01 6.135793e+01 6.232386e+01 5.600567e+01 5.722350e+01
#>  [241] 8.517320e+01 3.608199e+06 7.215939e+01 7.009373e+01 7.897991e+01
#>  [246] 6.341138e+01 6.246505e+01 8.172121e+01 5.909112e+01 8.424559e+01
#>  [251] 9.894604e+01 6.783217e+01 7.409996e+01 6.093741e+01 7.404833e+01
#>  [256] 9.244504e+01 6.658071e+01 7.382194e+01 1.292497e+02 2.331740e+02
#>  [261] 6.506541e+01 5.762018e+01 6.877089e+01 5.620350e+01 6.875739e+01
#>  [266] 6.153105e+01 1.245705e+03 7.864175e+01 7.005216e+01 2.741782e+03
#>  [271] 1.040536e+02 9.404860e+01 5.772616e+01 1.073141e+02 6.036715e+01
#>  [276] 1.343416e+02 6.574428e+01 6.102083e+01 8.265330e+01 2.715408e+02
#>  [281] 7.592794e+01 8.532544e+01 5.730868e+01 6.948649e+01 1.064721e+02
#>  [286] 5.668463e+01 9.812016e+01 1.130090e+02 1.854682e+02 6.021634e+01
#>  [291] 1.030869e+02 5.688345e+01 1.189618e+02 5.537278e+01 8.372146e+01
#>  [296] 6.956521e+01 5.778126e+01 6.714654e+01 6.340898e+01 7.488233e+01
#>  [301] 9.161606e+01 7.016749e+01 5.591511e+01 6.568259e+01 5.338838e+01
#>  [306] 6.270534e+01 6.522623e+01 6.160281e+01 7.225358e+01 8.775788e+01
#>  [311] 1.511980e+02 7.758017e+01 6.266367e+01 5.924091e+01 6.399738e+01
#>  [316] 5.658596e+01 7.770733e+01 5.926506e+01 6.731381e+01 5.812504e+02
#>  [321] 5.867332e+01 6.384527e+01 5.919386e+01 6.457192e+01 7.485083e+01
#>  [326] 6.614447e+01 7.607570e+01 3.356838e+02 1.666250e+02 6.339305e+01
#>  [331] 4.727922e+02 6.737689e+01 5.916411e+01 1.039039e+02 9.290079e+01
#>  [336] 5.779116e+01 6.549564e+01 6.528020e+01 9.169776e+01 8.645874e+01
#>  [341] 7.931786e+01 1.026358e+02 8.309658e+01 5.657982e+01 6.611664e+01
#>  [346] 6.344209e+01 7.534763e+01 7.683645e+01 7.074680e+01 8.470275e+01
#>  [351] 5.658379e+01 6.166030e+01 9.424423e+01 7.531631e+01 1.084351e+02
#>  [356] 1.108087e+02 6.306125e+01 9.629289e+02 8.468248e+01 5.826768e+01
#>  [361] 9.842873e+01 7.071196e+01 5.848132e+01 6.655493e+01 5.969817e+01
#>  [366] 6.188207e+01 7.132359e+01 6.849746e+01 8.412714e+01 6.123075e+01
#>  [371] 6.450825e+01 1.096169e+02 8.806162e+01 6.574683e+01 5.640849e+03
#>  [376] 6.571728e+01 5.713300e+01 6.205426e+01 5.643588e+01 5.850063e+01
#>  [381] 6.094661e+01 6.925236e+01 5.804472e+01 6.081055e+01 6.038638e+01
#>  [386] 7.901220e+01 6.683770e+01 8.876110e+01 5.951139e+01 1.179693e+02
#>  [391] 7.757696e+01 7.352887e+01 5.466799e+01 6.012272e+01 6.123024e+01
#>  [396] 2.462750e+02 5.856401e+01 7.307450e+01 6.377543e+01 7.236262e+01
#>  [401] 5.296398e+01 7.633397e+01 4.945752e+01 7.714505e+01 1.358950e+02
#>  [406] 1.907928e+02 6.807584e+01 7.574447e+01 5.606005e+01 7.018378e+01
#>  [411] 5.812794e+01 1.006021e+02 6.052259e+01 1.009613e+02 6.658466e+01
#>  [416] 1.248723e+02 1.774529e+02 5.239526e+01 5.615242e+01 9.636781e+01
#>  [421] 9.603068e+01 5.939299e+01 6.169618e+01 6.272829e+01 5.153662e+01
#>  [426] 5.802355e+01 3.430167e+02 1.347244e+02 9.217937e+01 5.042709e+01
#>  [431] 9.906439e+01 8.052606e+01 6.747182e+01 1.362481e+02 6.434548e+01
#>  [436] 1.738185e+02 7.329625e+01 6.013608e+01 6.736025e+01 7.083359e+01
#>  [441] 5.468078e+01 6.074117e+01 5.309920e+01 5.554571e+01 6.074791e+01
#>  [446] 6.635805e+01 6.141490e+01 1.537240e+02 6.181499e+01 7.114099e+01
#>  [451] 1.053988e+02 6.321175e+01 6.838900e+01 8.040545e+01 5.331715e+01
#>  [456] 6.304613e+01 1.254671e+02 7.491919e+01 6.804430e+01 6.371093e+01
#>  [461] 6.696132e+01 5.952248e+01 8.299176e+01 5.693931e+01 7.074272e+01
#>  [466] 7.250770e+01 5.710182e+01 6.533823e+01 6.011336e+01 7.594615e+01
#>  [471] 5.707242e+01 1.552441e+02 3.601830e+02 1.823805e+02 2.945138e+02
#>  [476] 6.998848e+01 6.700831e+01 1.175177e+02 6.633625e+01 7.021912e+01
#>  [481] 7.187332e+01 8.164859e+01 3.159400e+02 6.278442e+01 7.236398e+01
#>  [486] 6.697984e+01 5.958865e+01 5.552151e+01 5.889310e+01 5.788936e+01
#>  [491] 6.476433e+01 1.795719e+02 7.077544e+01 1.083368e+02 6.572337e+01
#>  [496] 6.169214e+01 5.860416e+01 7.130224e+01 6.673127e+01 1.801644e+02
#>  [501] 5.969758e+01 5.939045e+01 8.023713e+01 5.901988e+01 1.263416e+02
#>  [506] 1.012222e+02 7.102996e+01 6.034250e+01 8.947329e+01 9.323620e+01
#>  [511] 5.357560e+01 6.409799e+01 2.000410e+02 5.740252e+01 5.951795e+01
#>  [516] 6.684738e+01 5.416071e+01 5.099472e+01 1.656739e+02 7.445881e+01
#>  [521] 5.344435e+01 5.798106e+01 6.213840e+01 7.287891e+01 7.209737e+01
#>  [526] 5.637465e+01 6.396258e+01 6.089668e+01 6.010718e+01 5.703808e+01
#>  [531] 2.938591e+02 1.141855e+02 6.889861e+01 6.403760e+01 9.234394e+01
#>  [536] 7.073293e+01 6.067983e+01 6.985896e+01 6.255493e+01 8.365544e+01
#>  [541] 7.529075e+01 6.894182e+01 6.182501e+01 7.219047e+01 6.308928e+01
#>  [546] 7.648221e+01 2.257846e+02 5.280272e+01 6.597903e+01 6.818192e+01
#>  [551] 6.409284e+01 6.470221e+01 6.694832e+01 8.959611e+01 6.000549e+02
#>  [556] 7.938662e+01 5.280695e+01 5.689439e+01 6.283552e+01 6.712929e+01
#>  [561] 6.414365e+01 6.040116e+01 6.261803e+01 6.882842e+01 5.791332e+01
#>  [566] 1.600110e+02 1.988446e+03 5.892827e+01 5.944188e+01 5.540872e+01
#>  [571] 5.722289e+01 7.613809e+01 6.692971e+01 9.192091e+01 6.480221e+01
#>  [576] 6.109835e+01 6.028513e+01 5.772794e+01 3.725737e+02 7.843165e+01
#>  [581] 6.317683e+01 6.496476e+01 1.183091e+02 9.660765e+01 6.912907e+01
#>  [586] 5.812077e+01 7.737014e+01 6.583126e+01 6.640323e+01 7.227306e+01
#>  [591] 9.386507e+01 5.265649e+01 6.671727e+01 6.607807e+01 7.866650e+01
#>  [596] 6.439613e+01 6.741722e+01 8.571410e+01 7.625868e+01 6.823737e+01
#>  [601] 6.103746e+01 5.260631e+01 5.710733e+01 1.450242e+02 6.414589e+01
#>  [606] 8.060822e+01 6.598193e+01 7.560475e+01 5.757227e+01 1.218488e+02
#>  [611] 5.658357e+01 6.970990e+01 6.203449e+01 1.091741e+02 8.193134e+01
#>  [616] 1.088605e+02 5.959002e+01 9.011089e+01 8.174924e+01 1.173541e+03
#>  [621] 1.606543e+03 1.154947e+03 6.054618e+01 9.317160e+01 1.045417e+02
#>  [626] 6.197269e+01 6.333411e+01 5.722631e+01 6.679127e+01 7.107081e+01
#>  [631] 5.973606e+01 8.080893e+01 7.817672e+01 1.076981e+02 6.079385e+01
#>  [636] 7.704603e+01 8.090183e+01 5.591859e+01 4.570995e+01 1.662348e+02
#>  [641] 6.042671e+01 5.139091e+01 5.640451e+01 6.653368e+01 3.098823e+02
#>  [646] 5.561531e+02 3.949931e+02 5.607430e+01 1.078437e+02 5.614655e+01
#>  [651] 6.166818e+01 5.839663e+01 6.892624e+01 8.375806e+01 6.053942e+01
#>  [656] 6.140082e+01 6.121378e+01 6.142156e+01 8.994615e+01 3.135274e+03
#>  [661] 3.633603e+03 1.242231e+02 7.391965e+01 2.737869e+05 6.590356e+05
#>  [666] 1.452652e+02 1.367164e+03 2.481661e+04 1.435310e+02 1.763153e+02
#>  [671] 7.512111e+01 5.926781e+01 6.610956e+01 6.348755e+01 5.967072e+01
#>  [676] 4.633561e+01 5.554925e+01 5.579079e+01 4.760555e+01 9.835511e+01
#>  [681] 6.623436e+01 8.563588e+02 6.091681e+01 6.062050e+01 6.016200e+01
#>  [686] 9.532514e+01 7.771734e+01 6.384543e+01 6.780243e+01 8.076988e+01
#>  [691] 4.335549e+01 5.456861e+01 8.453620e+01 5.877236e+01 5.589991e+01
#>  [696] 5.477271e+01 6.446972e+01 7.784073e+01 7.141202e+01 5.553720e+01
#>  [701] 8.445585e+01 6.867145e+01 7.290494e+01 1.740200e+02 6.803880e+01
#>  [706] 6.461906e+01 5.790490e+01 6.737571e+01 7.209920e+01 5.584540e+01
#>  [711] 6.593021e+01 6.454863e+01 8.700337e+01 6.434225e+01 7.941490e+01
#>  [716] 1.086497e+02 2.031415e+02 9.629460e+01 6.327759e+01 1.256215e+02
#>  [721] 5.839119e+01 8.959186e+01 7.019627e+01 7.718995e+01 6.247960e+01
#>  [726] 7.169596e+01 9.787857e+01 7.287979e+01 5.543232e+01 3.087616e+02
#>  [731] 1.538524e+04 7.445935e+01 6.009356e+01 6.824104e+01 6.085857e+01
#>  [736] 5.883462e+01 6.861649e+01 6.426108e+01 7.382831e+01 5.794041e+01
#>  [741] 6.364490e+01 6.235845e+01 8.878372e+01 5.335402e+01 5.744235e+01
#>  [746] 6.693766e+01 7.223183e+01 7.631166e+01 1.477482e+02 6.255105e+01
#>  [751] 5.754112e+01 6.001148e+01 6.133333e+01 5.830109e+01 5.363286e+01
#>  [756] 2.093406e+02 1.095519e+02 4.160285e+03 7.481978e+04 4.228134e+08
#>  [761] 6.545188e+02 7.699559e+01 9.337258e+01 5.163242e+01 5.925589e+01
#>  [766] 6.673517e+01 6.267143e+01 5.802945e+01 6.033657e+01 5.976574e+01
#>  [771] 6.644836e+01 6.356644e+01 5.912927e+01 7.582759e+01 5.932623e+01
#>  [776] 6.112854e+01 5.599282e+01 1.761468e+02 7.893217e+01 7.483509e+01
#>  [781] 4.947573e+01 1.919383e+02 7.036661e+01 8.898480e+01 6.056653e+01
#>  [786] 9.562978e+01 6.904333e+01 3.035393e+02 1.081489e+02 5.862933e+01
#>  [791] 1.029565e+02 6.350009e+01 5.852348e+01 6.843131e+01 5.487648e+01
#>  [796] 7.380940e+01 7.494218e+01 8.480506e+01 7.086319e+01 6.144392e+01
#>  [801] 8.890922e+01 6.724035e+01 7.060891e+01 7.098685e+01 5.896559e+01
#>  [806] 5.804108e+01 6.619296e+01 6.423029e+01 6.613721e+01 5.877553e+01
#>  [811] 5.873186e+01 6.942226e+01 7.189342e+01 3.868182e+02 5.052317e+01
#>  [816] 1.028966e+02 6.114730e+01 5.297609e+01 1.406014e+02 9.310577e+01
#>  [821] 7.265120e+01 9.646225e+01 5.463936e+01 6.644890e+01 7.112651e+01
#>  [826] 6.608229e+01 2.781099e+02 5.866705e+01 1.948414e+02 7.523559e+01
#>  [831] 6.880500e+01 8.834396e+01 6.763409e+01 6.612156e+01 5.932930e+01
#>  [836] 6.730050e+01 8.499723e+01 6.479721e+01 5.872692e+01 5.676946e+01
#>  [841] 8.024013e+01 1.025824e+02 1.080728e+02 5.680602e+01 5.648975e+01
#>  [846] 6.014056e+01 6.427190e+01 6.842816e+01 5.629196e+01 5.690447e+01
#>  [851] 5.893512e+01 5.769787e+01 6.785376e+01 6.794514e+01 5.982746e+01
#>  [856] 9.446697e+01 6.997538e+01 5.488595e+01 6.588845e+01 1.042446e+02
#>  [861] 4.824174e+01 5.918466e+01 5.808022e+01 1.281420e+02 1.404175e+02
#>  [866] 5.692457e+01 1.368019e+02 9.829665e+03 7.943822e+01 7.714765e+01
#>  [871] 5.583414e+01 5.585904e+01 6.085605e+01 5.924466e+01 8.067781e+01
#>  [876] 5.942930e+01 8.552359e+01 3.742708e+02 7.606350e+01 8.614722e+01
#>  [881] 6.326897e+01 1.741047e+02 1.370248e+02 5.466150e+01 6.307627e+01
#>  [886] 7.641622e+01 1.254227e+02 9.796011e+01 6.618393e+01 6.416327e+01
#>  [891] 1.465763e+02 1.658917e+02 3.292351e+02 6.205819e+01 6.512441e+01
#>  [896] 8.006523e+01 6.945597e+01 8.036993e+01 7.383944e+01 6.909270e+01
#>  [901] 6.919548e+01 6.176148e+01 6.467981e+01 5.969059e+01 1.211002e+02
#>  [906] 7.219085e+01 6.570297e+01 5.657286e+01 5.792010e+01 5.815020e+01
#>  [911] 6.182308e+01 6.743914e+01 1.314228e+02 1.327843e+02 9.627771e+01
#>  [916] 5.695984e+01 6.277486e+01 8.304020e+01 5.819662e+01 5.872365e+01
#>  [921] 9.995020e+01 2.589684e+02 8.496806e+01 6.768244e+01 1.471531e+04
#>  [926] 6.363806e+01 5.988852e+01 5.344341e+01 5.199013e+01 6.871837e+01
#>  [931] 1.190783e+02 6.001045e+01 6.003473e+01 6.453946e+01 7.208649e+01
#>  [936] 7.487976e+01 1.141306e+02 6.930029e+01 9.063495e+01 6.210373e+01
#>  [941] 5.863224e+01 1.611005e+03 1.131279e+02 5.591952e+01 6.459010e+01
#>  [946] 9.280048e+01 5.351495e+01 8.027738e+01 6.124125e+01 6.396618e+01
#>  [951] 6.293354e+01 7.028205e+01 6.641508e+01 5.590680e+01 5.913488e+01
#>  [956] 1.060033e+02 6.123862e+01 6.083960e+01 5.800646e+01 5.624996e+01
#>  [961] 5.538577e+01 9.305244e+01 5.531572e+01 6.235660e+01 5.566444e+01
#>  [966] 7.360731e+01 9.153280e+01 3.658010e+02 1.213710e+04 5.536161e+01
#>  [971] 6.587745e+01 9.044558e+01 3.955334e+02 5.311449e+01 6.785960e+01
#>  [976] 5.945630e+01 6.016489e+01 6.122164e+01 5.719663e+01 1.594784e+02
#>  [981] 6.307775e+01 6.962750e+01 6.119026e+01 6.052806e+01 1.293194e+02
#>  [986] 1.531701e+02 7.497157e+01 6.662713e+01 6.296120e+01 7.042558e+01
#>  [991] 7.369689e+01 5.402215e+01 5.487355e+01 6.294531e+01 8.942449e+01
#>  [996] 7.500433e+01 1.213596e+02 1.224457e+02 5.665102e+01 6.138441e+01
```
