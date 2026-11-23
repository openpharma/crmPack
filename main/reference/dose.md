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
#>  [1] 105.12964 217.99185 217.99185 196.89849  81.89994  81.89994  46.56917
#>  [8]  46.56917  46.56917  18.25803 250.73721 156.61810 156.61810 156.61810
#> [15] 446.23630 133.89699 133.89699  30.74651  30.74651 141.36710

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
#>    [1] 7.014895e+01 5.782481e+01 5.273146e+01 5.850870e+01 8.186962e+01
#>    [6] 1.633563e+02 6.753404e+01 8.427397e+01 9.345830e+01 1.175065e+02
#>   [11] 6.736165e+01 7.664803e+01 9.571681e+01 1.365498e+02 6.032978e+01
#>   [16] 8.221847e+01 7.380334e+01 1.127511e+02 1.850850e+02 5.954922e+01
#>   [21] 1.006646e+02 5.970166e+01 6.101727e+01 5.806821e+01 5.741927e+01
#>   [26] 6.070961e+01 7.313833e+01 5.946708e+01 5.970732e+01 8.509678e+01
#>   [31] 8.483391e+01 1.123685e+02 6.133567e+01 7.072449e+01 1.166402e+02
#>   [36] 1.028600e+02 5.833994e+01 6.127506e+01 7.298242e+01 6.043083e+01
#>   [41] 6.038874e+01 6.012332e+01 6.060385e+01 5.269638e+01 7.595552e+01
#>   [46] 7.861605e+01 5.654589e+01 5.719229e+01 7.143011e+01 1.061110e+02
#>   [51] 6.462337e+01 7.070683e+01 1.105646e+02 6.463706e+01 7.063387e+01
#>   [56] 8.425908e+01 6.381017e+01 6.369800e+01 6.587087e+01 7.008183e+01
#>   [61] 6.438578e+01 6.129249e+01 6.146848e+01 4.805928e+02 7.668962e+01
#>   [66] 5.449932e+01 9.464932e+01 7.003486e+01 1.545320e+02 5.914935e+01
#>   [71] 6.557363e+01 9.291403e+01 6.074955e+01 1.089770e+02 6.270551e+01
#>   [76] 5.937672e+01 5.674035e+01 5.509675e+01 9.826021e+01 6.099626e+01
#>   [81] 6.230932e+01 6.068923e+01 6.072561e+01 1.629126e+02 7.457541e+01
#>   [86] 9.568791e+01 2.109456e+02 9.981548e+01 7.040457e+01 5.650578e+01
#>   [91] 6.921854e+01 6.472924e+01 1.062514e+02 1.286064e+02 5.959205e+01
#>   [96] 1.295489e+02 5.454319e+01 5.551297e+01 6.565573e+01 1.074737e+02
#>  [101] 5.738421e+01 7.278054e+01 4.290482e+01 7.328197e+01 5.417938e+01
#>  [106] 6.233669e+01 6.663398e+01 1.088696e+02 7.574530e+01 5.364026e+01
#>  [111] 6.062254e+01 5.942412e+01 7.402720e+01 1.676062e+02 8.799639e+01
#>  [116] 8.253377e+01 5.531946e+01 7.767625e+01 7.926330e+01 6.817578e+01
#>  [121] 5.675431e+01 6.471414e+01 6.096896e+01 6.814548e+01 5.816104e+01
#>  [126] 5.605596e+01 5.950966e+01 5.112596e+01 5.154226e+01 7.004297e+01
#>  [131] 1.551831e+02 6.821205e+01 8.172298e+01 3.370753e+02 6.081259e+01
#>  [136] 5.058839e+01 5.448409e+01 7.553826e+01 6.296996e+01 6.953939e+01
#>  [141] 5.880182e+01 9.040467e+01 9.801810e+01 6.355602e+01 1.005691e+02
#>  [146] 5.840440e+01 1.038366e+02 9.620608e+01 5.852052e+01 8.712257e+01
#>  [151] 6.817475e+01 1.784439e+02 9.821658e+01 7.464395e+01 6.230988e+01
#>  [156] 5.978211e+01 6.243121e+01 7.070124e+01 1.758236e+02 5.787007e+01
#>  [161] 5.826648e+01 6.669570e+01 8.311591e+01 6.605001e+01 5.642599e+01
#>  [166] 5.463865e+01 8.308534e+01 8.926595e+01 7.365964e+01 1.089015e+02
#>  [171] 1.306544e+02 5.961375e+01 9.633359e+01 6.433476e+01 6.228367e+01
#>  [176] 5.693561e+01 8.634718e+01 9.828031e+01 6.058671e+01 7.072785e+01
#>  [181] 5.660622e+01 6.691906e+01 6.361918e+01 5.534023e+01 7.501184e+01
#>  [186] 7.352788e+01 6.592210e+01 5.510222e+01 5.862893e+01 6.104532e+01
#>  [191] 6.347040e+01 9.550278e+01 7.494140e+01 7.573725e+01 6.179156e+01
#>  [196] 5.837559e+01 8.299172e+01 7.076707e+01 1.139309e+02 6.822747e+03
#>  [201] 6.298626e+01 5.752260e+01 5.625881e+01 7.755744e+01 5.800897e+01
#>  [206] 5.421289e+01 6.315244e+01 6.797674e+01 5.878309e+01 6.367949e+01
#>  [211] 8.929230e+01 6.292744e+01 7.954927e+01 5.959028e+01 1.165108e+02
#>  [216] 7.978226e+01 6.202062e+01 6.177273e+01 1.986096e+02 5.230652e+01
#>  [221] 2.031957e+02 5.260493e+01 8.769811e+01 7.847533e+01 5.970220e+01
#>  [226] 6.139007e+01 6.982625e+01 6.270202e+01 6.270395e+01 1.283459e+02
#>  [231] 8.018950e+01 7.558584e+01 6.121991e+01 5.979509e+01 6.195032e+01
#>  [236] 6.285592e+01 6.765737e+01 6.104546e+01 6.340641e+01 6.795031e+01
#>  [241] 6.558297e+01 6.691853e+01 5.979732e+01 5.272393e+01 1.545347e+03
#>  [246] 1.538352e+02 5.870144e+01 6.765295e+01 6.634848e+01 6.134703e+01
#>  [251] 6.276672e+01 8.787838e+01 7.433281e+01 2.186838e+02 1.882380e+02
#>  [256] 3.340037e+02 6.051959e+01 6.501287e+01 5.795718e+01 5.662501e+01
#>  [261] 8.378222e+01 1.545573e+02 6.365435e+01 1.419942e+02 6.200935e+01
#>  [266] 6.085061e+01 5.638670e+01 7.521478e+01 6.648965e+01 6.710684e+01
#>  [271] 1.911722e+03 6.287859e+01 5.668571e+01 6.072736e+01 6.582519e+01
#>  [276] 5.314052e+01 5.998521e+01 8.584537e+01 9.087835e+01 5.985243e+01
#>  [281] 6.766728e+01 5.624884e+01 6.264021e+01 6.538228e+01 5.671538e+01
#>  [286] 6.054097e+01 5.931170e+01 6.845082e+01 9.304352e+01 7.968436e+01
#>  [291] 1.064618e+02 1.079173e+02 6.337097e+01 9.236937e+01 1.178208e+02
#>  [296] 1.042097e+02 5.598223e+01 5.939306e+01 8.184638e+01 7.814700e+01
#>  [301] 6.159004e+01 1.043200e+02 3.488572e+02 7.284387e+01 6.404399e+01
#>  [306] 6.099591e+01 6.759578e+01 7.833510e+01 6.315777e+01 6.458406e+01
#>  [311] 5.716043e+01 7.266484e+01 5.571829e+01 5.929643e+01 9.605518e+01
#>  [316] 5.919048e+01 5.853156e+01 6.065209e+01 6.468616e+01 7.914885e+01
#>  [321] 7.175181e+01 8.717471e+01 1.576094e+02 8.075842e+01 5.798894e+01
#>  [326] 5.598553e+01 6.229300e+01 6.096755e+01 5.302480e+01 6.496058e+01
#>  [331] 6.037136e+01 1.697226e+02 5.858167e+01 6.185439e+01 5.993588e+01
#>  [336] 5.161744e+01 7.298947e+01 4.361645e+02 6.952298e+01 6.614713e+01
#>  [341] 6.311914e+01 6.657417e+01 9.876984e+01 6.325467e+01 7.509542e+01
#>  [346] 6.103602e+01 5.578888e+01 8.266328e+01 5.122453e+01 6.598260e+01
#>  [351] 6.013648e+01 8.337774e+01 6.295375e+01 6.194156e+01 7.344045e+01
#>  [356] 1.253388e+02 5.767631e+01 5.797047e+01 7.606413e+01 5.690403e+01
#>  [361] 6.395447e+01 6.151858e+01 6.450161e+01 6.330664e+01 1.023966e+02
#>  [366] 1.629368e+05 1.097680e+02 1.172872e+03 5.228459e+01 6.177522e+01
#>  [371] 6.054619e+01 6.736787e+01 6.859423e+01 5.964359e+01 5.486163e+01
#>  [376] 6.281408e+01 5.912015e+01 7.853519e+01 7.132799e+01 5.845161e+01
#>  [381] 8.364070e+01 6.620128e+01 5.955170e+01 5.950464e+01 5.503668e+01
#>  [386] 1.880398e+02 6.057018e+01 7.896047e+01 6.177832e+01 8.817089e+01
#>  [391] 6.914083e+01 5.482614e+01 7.174046e+01 9.672048e+01 7.885372e+01
#>  [396] 7.584016e+01 6.737848e+01 5.590999e+01 9.632576e+01 4.912816e+01
#>  [401] 6.438987e+01 3.861028e+02 1.275256e+03 6.350766e+01 6.840228e+01
#>  [406] 7.266303e+01 5.623460e+01 7.792342e+01 8.521003e+01 6.580412e+01
#>  [411] 5.978369e+01 6.285536e+01 1.008293e+02 5.635368e+01 6.705920e+01
#>  [416] 6.582009e+01 1.967046e+02 1.030341e+02 6.602807e+01 7.716342e+01
#>  [421] 5.458975e+03 3.387910e+03 3.046682e+03 6.454789e+01 5.851075e+01
#>  [426] 6.754789e+01 6.901187e+01 5.196192e+02 4.065190e+02 9.981906e+02
#>  [431] 4.895165e+17 6.798738e+04 3.370612e+03 6.294317e+01 5.773922e+01
#>  [436] 6.258108e+01 6.217120e+01 6.692445e+01 5.857482e+01 5.869241e+01
#>  [441] 1.035761e+02 6.489664e+01 2.716617e+02 9.288345e+02 3.974594e+01
#>  [446] 8.076365e+01 4.418778e+01 5.768943e+01 7.526914e+01 1.383025e+02
#>  [451] 7.816823e+01 5.966521e+01 7.457422e+01 6.220532e+01 6.735523e+01
#>  [456] 5.712969e+01 6.613866e+01 6.178722e+01 5.643966e+01 6.156008e+01
#>  [461] 5.856330e+01 5.961705e+01 6.070676e+01 1.458278e+02 6.532282e+01
#>  [466] 6.467193e+01 5.864858e+01 5.955018e+01 8.380156e+01 7.266327e+01
#>  [471] 2.074619e+02 7.601869e+01 6.079912e+01 1.472508e+03 9.476367e+01
#>  [476] 6.662748e+01 7.613338e+01 2.308764e+03 7.848762e+01 6.267422e+01
#>  [481] 7.587398e+01 8.308638e+01 8.342252e+01 7.872915e+01 6.822064e+01
#>  [486] 5.931304e+01 6.060892e+01 6.284753e+01 6.318724e+01 6.546017e+01
#>  [491] 6.423319e+01 5.761050e+01 1.009593e+02 9.846756e+01 5.924519e+01
#>  [496] 7.416268e+01 8.249121e+01 5.578124e+01 8.213600e+01 5.885331e+01
#>  [501] 5.450915e+01 5.931417e+01 5.708502e+01 5.974045e+01 7.084092e+01
#>  [506] 6.537377e+01 8.092909e+01 8.188392e+01 7.549244e+01 6.053198e+01
#>  [511] 6.328191e+01 7.322016e+01 1.073570e+02 8.811470e+01 8.151987e+01
#>  [516] 6.482614e+01 5.917679e+01 6.531256e+01 5.809598e+01 6.366909e+01
#>  [521] 7.548250e+01 6.426671e+01 9.884296e+01 5.399604e+01 1.145949e+02
#>  [526] 3.772243e+01 7.024945e+01 6.985582e+01 5.963792e+01 1.592230e+02
#>  [531] 8.782812e+01 6.445177e+01 5.332392e+01 7.109810e+01 8.070053e+01
#>  [536] 5.960888e+01 5.952446e+01 7.669284e+01 1.403084e+02 5.899109e+01
#>  [541] 5.732546e+01 4.659539e+01 6.494354e+01 6.292243e+01 1.029733e+02
#>  [546] 4.954077e+01 1.428315e+02 5.834081e+01 8.168870e+01 1.614924e+02
#>  [551] 5.384252e+01 5.441443e+01 9.831050e+01 6.799445e+01 7.178573e+01
#>  [556] 7.022646e+01 9.202581e+01 7.734431e+01 6.605561e+01 6.565219e+01
#>  [561] 1.160474e+02 9.150557e+01 7.163317e+01 6.386730e+01 9.281334e+01
#>  [566] 1.121995e+02 6.139231e+01 5.988463e+01 5.679413e+01 5.893170e+01
#>  [571] 5.755492e+01 6.663736e+01 8.357124e+01 2.531403e+02 5.997846e+01
#>  [576] 6.911754e+01 5.712782e+01 1.080100e+02 5.166046e+02 5.657403e+01
#>  [581] 7.339722e+01 8.533930e+01 4.670991e+01 4.968877e+01 9.301411e+01
#>  [586] 5.818232e+01 7.882677e+01 8.633620e+01 6.226383e+01 5.759038e+01
#>  [591] 5.822288e+01 6.117785e+01 5.885970e+01 6.230678e+01 8.461797e+01
#>  [596] 5.828578e+01 6.652064e+01 1.044142e+02 5.753206e+01 1.522822e+02
#>  [601] 9.637635e+01 5.730824e+01 7.833103e+01 5.850718e+01 6.367207e+01
#>  [606] 5.388807e+01 2.063959e+02 5.683393e+01 7.146777e+01 9.632591e+01
#>  [611] 1.872925e+02 1.170705e+02 6.440734e+01 5.902384e+01 1.679381e+02
#>  [616] 9.189269e+01 1.001966e+02 6.905364e+01 9.224817e+01 1.114127e+02
#>  [621] 5.735258e+01 5.909276e+01 8.051119e+01 9.974800e+01 7.528150e+01
#>  [626] 6.574832e+01 5.529029e+01 6.027512e+01 7.952001e+01 5.094384e+01
#>  [631] 1.213861e+02 6.385260e+01 6.432809e+01 5.855403e+01 5.816856e+01
#>  [636] 5.903232e+01 9.260861e+01 6.059138e+01 6.139601e+01 5.938708e+01
#>  [641] 6.268071e+01 5.824705e+01 7.370711e+01 6.739058e+01 6.402704e+01
#>  [646] 7.300905e+01 7.274271e+01 5.264785e+01 5.850019e+01 6.114546e+01
#>  [651] 5.690026e+01 1.097287e+02 5.628832e+01 2.019027e+02 5.575157e+01
#>  [656] 7.425640e+01 6.011510e+01 1.428812e+03 1.711737e+02 5.434376e+01
#>  [661] 6.309342e+01 5.288880e+01 6.879801e+01 7.005599e+01 6.351395e+01
#>  [666] 1.094336e+02 5.820127e+01 9.985118e+01 5.579991e+01 6.535471e+01
#>  [671] 5.508502e+01 5.626398e+01 6.020113e+02 1.105553e+02 1.049313e+02
#>  [676] 5.401446e+01 5.569811e+01 5.643608e+01 6.008145e+01 6.857719e+01
#>  [681] 1.174451e+02 6.516679e+01 7.122260e+01 5.634522e+01 5.981304e+01
#>  [686] 6.210866e+01 6.083509e+01 5.652913e+01 1.143808e+04 1.269516e+02
#>  [691] 6.082026e+01 6.463936e+01 6.273122e+01 5.780455e+01 5.938630e+01
#>  [696] 6.727634e+01 6.511947e+01 6.090880e+01 5.532069e+01 7.871813e+01
#>  [701] 7.093748e+01 7.545230e+01 6.880141e+01 7.087473e+01 1.441705e+02
#>  [706] 5.516060e+01 5.496775e+01 1.960385e+02 5.799784e+01 6.341194e+01
#>  [711] 5.897550e+01 5.861143e+01 6.635324e+01 6.183753e+01 7.275136e+01
#>  [716] 6.112289e+01 1.128125e+02 7.152506e+01 5.915536e+01 7.293450e+01
#>  [721] 7.330600e+01 6.161197e+01 1.257861e+02 2.380720e+02 8.274064e+01
#>  [726] 1.084688e+02 9.442185e+01 6.237707e+01 1.011566e+02 5.738876e+01
#>  [731] 6.841884e+01 5.356265e+01 6.936468e+01 1.084166e+02 2.537680e+02
#>  [736] 1.173589e+02 6.433271e+01 5.461438e+01 7.413743e+01 5.812781e+01
#>  [741] 8.135537e+01 5.013753e+01 5.692735e+01 7.383571e+01 8.042046e+01
#>  [746] 6.652299e+01 8.881133e+01 6.886806e+01 7.455180e+01 7.036615e+01
#>  [751] 6.782885e+01 7.397841e+01 5.962824e+01 7.417804e+01 5.565475e+01
#>  [756] 8.971344e+01 1.032348e+02 5.033608e+02 8.019838e+01 7.376581e+01
#>  [761] 6.926576e+01 6.917519e+01 9.303427e+01 2.211756e+02 7.907181e+01
#>  [766] 6.547673e+01 9.077784e+01 5.941992e+01 9.933174e+01 1.171779e+02
#>  [771] 7.995595e+01 5.713964e+01 6.577521e+01 5.615008e+01 1.537394e+02
#>  [776] 5.770259e+01 1.097724e+02 5.904633e+01 9.410037e+01 1.059176e+02
#>  [781] 6.195999e+01 5.813081e+01 6.408546e+01 7.920061e+01 6.255139e+01
#>  [786] 7.393623e+01 9.295742e+01 6.034513e+01 8.571984e+01 5.612018e+01
#>  [791] 8.210012e+01 1.170084e+02 6.948711e+01 6.342648e+01 7.632345e+01
#>  [796] 6.152897e+01 1.232004e+02 7.728818e+01 6.733654e+01 6.139593e+01
#>  [801] 7.460862e+01 2.035586e+02 6.819722e+01 6.374640e+01 1.385667e+02
#>  [806] 6.181697e+01 8.566382e+01 7.916145e+01 1.486246e+02 6.866800e+01
#>  [811] 6.065100e+01 5.577814e+01 6.285617e+01 8.738625e+01 2.471884e+02
#>  [816] 5.802046e+01 7.255804e+01 6.528386e+01 5.736273e+01 5.485343e+01
#>  [821] 7.307760e+01 6.367832e+01 8.320157e+01 8.457001e+01 7.125340e+01
#>  [826] 6.659146e+01 7.335682e+01 6.399963e+01 6.183821e+01 6.285624e+01
#>  [831] 9.366009e+01 1.684143e+02 5.937519e+01 6.563701e+01 6.601544e+01
#>  [836] 6.740902e+01 5.980655e+01 6.447812e+01 1.179351e+02 5.934986e+01
#>  [841] 1.413396e+02 8.167426e+01 2.013431e+02 8.422835e+01 1.031992e+02
#>  [846] 6.192358e+01 5.727085e+01 1.364974e+02 6.826265e+01 8.148270e+01
#>  [851] 6.690777e+01 6.682489e+01 7.157960e+01 6.060047e+01 5.744149e+01
#>  [856] 5.657791e+01 5.883391e+01 6.540279e+01 5.683995e+01 6.561037e+01
#>  [861] 5.389734e+01 9.484269e+01 5.918461e+01 7.798548e+01 7.697204e+01
#>  [866] 1.098584e+02 9.147327e+01 5.619568e+01 7.300207e+01 7.292213e+01
#>  [871] 6.823170e+01 5.813978e+01 5.987486e+01 8.952858e+01 6.103914e+01
#>  [876] 5.806595e+01 6.877506e+01 6.644821e+01 6.834921e+01 8.602204e+01
#>  [881] 6.171860e+01 3.441748e+04 6.228872e+01 6.198363e+01 6.473830e+01
#>  [886] 6.672726e+01 6.534679e+01 6.027705e+01 1.090363e+02 5.409206e+01
#>  [891] 9.777774e+01 5.742344e+01 8.180282e+01 9.129334e+01 6.344590e+01
#>  [896] 7.948869e+01 7.430220e+01 3.783738e+02 6.737264e+01 5.593365e+01
#>  [901] 6.697662e+01 6.394241e+01 5.985227e+01 5.910712e+01 6.096671e+01
#>  [906] 6.027605e+01 8.705446e+01 5.808065e+01 9.100272e+01 7.177135e+01
#>  [911] 8.361443e+01 5.791229e+01 7.186718e+01 6.064328e+01 6.128910e+01
#>  [916] 5.992318e+01 8.322724e+01 7.938440e+02 8.068460e+01 6.159853e+01
#>  [921] 7.162927e+01 6.382915e+01 6.255006e+01 6.813779e+01 6.071972e+01
#>  [926] 6.463283e+01 9.203367e+01 1.363011e+02 6.828961e+01 5.878574e+01
#>  [931] 5.995452e+01 6.273223e+01 6.479201e+01 6.196229e+01 6.823649e+01
#>  [936] 5.945765e+01 8.222673e+01 6.204668e+01 6.185225e+01 8.285201e+01
#>  [941] 1.413753e+03 9.995368e+01 1.125677e+02 5.965320e+01 7.436943e+01
#>  [946] 1.102667e+02 1.184303e+02 8.627768e+01 6.185563e+01 1.259417e+02
#>  [951] 9.734311e+01 6.763482e+01 6.925109e+01 6.156491e+01 5.787735e+01
#>  [956] 6.570692e+01 1.143201e+02 5.860025e+01 5.685115e+01 6.262209e+01
#>  [961] 7.031718e+01 7.262026e+01 6.735260e+01 5.716879e+01 5.848337e+01
#>  [966] 6.480892e+01 6.266459e+01 6.884565e+01 9.341753e+01 5.532212e+01
#>  [971] 6.290675e+01 8.825810e+01 9.828279e+01 6.541767e+01 6.264397e+01
#>  [976] 6.298328e+01 5.701008e+01 5.923442e+01 7.115350e+01 9.918972e+01
#>  [981] 1.346695e+02 9.550379e+01 3.071845e+02 1.032860e+02 2.131135e+02
#>  [986] 7.372631e+01 6.348926e+01 5.586984e+01 7.102313e+01 8.963062e+01
#>  [991] 5.791408e+01 6.007298e+01 8.921728e+01 6.671837e+01 1.790217e+02
#>  [996] 7.131933e+01 6.373881e+01 8.882040e+01 6.574552e+03 6.528969e+01
```
