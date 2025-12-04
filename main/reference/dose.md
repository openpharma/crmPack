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
#>  [1]  33.37124  33.37124  21.18668  21.18668  20.19763  24.64845  24.64845
#>  [8]  58.02573  58.02573  58.02573  30.96439  36.67901  36.67901  36.67901
#> [15]  71.18699 176.93536 176.93536 176.93536 176.93536 176.93536

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
#>    [1] 6.858500e+01 4.937453e+01 1.027747e+02 5.371182e+01 6.133415e+01
#>    [6] 6.096818e+01 6.197473e+01 6.973882e+01 6.022298e+01 6.916061e+01
#>   [11] 1.245139e+02 1.682054e+02 6.414660e+01 8.177614e+01 5.337159e+01
#>   [16] 8.961451e+01 6.280316e+01 8.092232e+01 5.664146e+01 7.540585e+01
#>   [21] 8.209376e+01 5.788320e+01 6.325591e+01 1.133282e+02 6.485155e+01
#>   [26] 1.027658e+02 1.416669e+02 2.353437e+02 6.836410e+01 7.588063e+01
#>   [31] 2.045169e+02 6.345059e+01 6.646200e+01 1.047324e+02 1.354639e+02
#>   [36] 6.421385e+01 5.510142e+01 1.683823e+02 8.632116e+01 5.139171e+01
#>   [41] 5.791055e+01 6.241072e+01 7.518140e+01 6.051776e+01 8.649367e+01
#>   [46] 5.105875e+01 6.089345e+01 5.843299e+01 5.997913e+01 6.369444e+01
#>   [51] 5.717938e+01 5.889528e+01 6.481862e+01 7.547452e+01 5.566296e+01
#>   [56] 7.235215e+01 8.252575e+01 5.715690e+01 6.800102e+01 5.460824e+01
#>   [61] 6.048246e+01 3.254944e+02 1.080689e+02 7.006735e+01 5.833193e+01
#>   [66] 6.510149e+01 5.910959e+01 5.458084e+01 6.001631e+01 5.372415e+01
#>   [71] 7.669459e+01 5.730236e+01 5.998005e+01 6.515910e+01 6.321166e+01
#>   [76] 5.542210e+01 5.609069e+01 7.206864e+01 5.651915e+01 9.898328e+01
#>   [81] 2.678702e+02 7.104808e+01 5.923881e+01 1.215798e+02 8.043171e+01
#>   [86] 6.362412e+01 5.730830e+01 6.660193e+01 6.169627e+01 6.563009e+01
#>   [91] 6.015601e+01 6.574323e+01 6.848951e+01 6.219421e+01 1.907148e+02
#>   [96] 1.215236e+02 5.304642e+01 6.345627e+01 6.690362e+01 5.691593e+01
#>  [101] 5.597888e+01 5.962328e+01 7.106104e+01 6.894804e+01 6.839423e+01
#>  [106] 5.605908e+01 5.838884e+01 6.302565e+01 8.524223e+01 7.297544e+01
#>  [111] 6.770838e+01 5.953249e+01 5.532977e+01 7.593108e+01 1.063027e+02
#>  [116] 6.200128e+01 6.206614e+01 5.572877e+01 4.957177e+02 6.175077e+01
#>  [121] 8.074649e+01 2.387371e+02 7.205206e+01 6.595829e+01 6.528628e+01
#>  [126] 6.124011e+01 6.016528e+01 8.467512e+01 6.536433e+01 9.029237e+01
#>  [131] 1.090071e+02 6.237019e+01 6.213342e+01 7.300101e+01 7.460329e+01
#>  [136] 5.834871e+01 6.619529e+01 5.835478e+01 6.182364e+01 5.911716e+01
#>  [141] 5.806826e+01 6.915487e+01 6.048224e+01 5.883423e+01 6.162753e+01
#>  [146] 7.030823e+01 7.670673e+01 9.308816e+01 6.966735e+01 6.033336e+01
#>  [151] 5.364653e+01 7.634976e+01 6.329805e+01 1.028658e+02 5.788617e+01
#>  [156] 6.398308e+01 5.808349e+01 6.285747e+01 7.976650e+01 1.369535e+02
#>  [161] 7.106887e+01 1.223237e+02 1.983141e+02 8.497192e+01 6.529936e+01
#>  [166] 5.844553e+01 5.940171e+01 5.763710e+01 6.180071e+01 6.931098e+01
#>  [171] 6.163424e+01 5.584851e+01 5.971909e+01 6.557452e+01 8.454273e+01
#>  [176] 6.127632e+01 5.975576e+01 6.207382e+01 6.737880e+01 7.344321e+01
#>  [181] 7.173919e+01 5.287506e+01 6.737759e+01 6.182481e+01 2.062958e+02
#>  [186] 5.425163e+01 5.619637e+01 7.610225e+01 6.669823e+01 5.798640e+01
#>  [191] 6.147291e+01 5.753067e+01 6.445499e+01 7.125365e+01 5.305271e+01
#>  [196] 6.715022e+01 5.449302e+01 1.003384e+02 1.009885e+02 5.755932e+01
#>  [201] 8.849709e+01 8.465855e+01 7.460136e+01 1.027190e+02 6.780921e+01
#>  [206] 6.263528e+01 5.693929e+01 7.059477e+01 8.720318e+01 6.054251e+01
#>  [211] 6.055004e+01 8.622955e+01 7.324410e+01 4.973823e+02 7.855903e+01
#>  [216] 6.091989e+01 1.959597e+02 1.276435e+03 6.654560e+01 1.693092e+02
#>  [221] 5.877768e+01 5.299172e+01 6.416627e+01 7.877732e+01 2.570012e+02
#>  [226] 5.230018e+01 8.250953e+01 1.143407e+02 5.137769e+01 1.075825e+02
#>  [231] 1.165482e+02 5.733191e+01 7.432810e+01 5.211854e+01 6.227969e+02
#>  [236] 1.694743e+02 5.979510e+01 6.231021e+01 1.327486e+02 2.039437e+02
#>  [241] 1.130437e+02 2.721566e+02 1.052626e+02 5.864321e+01 5.627949e+01
#>  [246] 6.073469e+01 7.645575e+01 5.522182e+01 5.788933e+01 6.001596e+01
#>  [251] 7.605539e+01 5.734944e+01 5.604568e+01 6.669510e+01 7.347636e+01
#>  [256] 6.031562e+01 5.407243e+01 6.054875e+01 7.046459e+01 8.732473e+01
#>  [261] 6.889908e+01 9.416193e+01 6.209319e+01 7.276360e+01 7.626374e+01
#>  [266] 1.133630e+02 1.441263e+02 5.353333e+01 1.184049e+03 6.860647e+01
#>  [271] 6.886828e+01 7.063714e+01 8.115575e+01 5.952302e+01 6.599190e+01
#>  [276] 6.360805e+01 1.068147e+02 6.373712e+01 6.494933e+01 5.892391e+01
#>  [281] 5.417746e+03 5.880010e+01 9.221864e+01 5.997683e+01 6.512635e+01
#>  [286] 6.376998e+01 6.879615e+01 6.078574e+01 6.846699e+01 5.832223e+01
#>  [291] 7.255465e+01 1.076594e+02 5.908218e+01 1.179502e+02 6.519744e+01
#>  [296] 7.378404e+01 6.128242e+01 6.914090e+01 5.756842e+01 5.782035e+01
#>  [301] 6.564240e+01 6.568705e+01 7.365145e+01 6.027383e+01 7.356877e+01
#>  [306] 5.462079e+01 5.765864e+01 6.805984e+01 6.079973e+01 8.295677e+01
#>  [311] 7.501289e+01 6.087998e+01 6.307782e+01 5.523168e+01 7.563441e+01
#>  [316] 9.178719e+01 6.327455e+01 5.774748e+01 5.965074e+01 1.095113e+02
#>  [321] 1.184848e+02 6.448582e+01 5.699429e+01 6.441785e+01 6.612116e+01
#>  [326] 7.485842e+01 6.002417e+01 6.836087e+01 6.564039e+01 5.821517e+01
#>  [331] 1.208413e+02 5.459228e+01 6.132404e+01 5.399635e+01 8.550887e+01
#>  [336] 4.509742e+03 6.250418e+01 6.567885e+01 9.267244e+01 6.309068e+01
#>  [341] 8.170109e+01 1.028728e+02 9.583485e+01 5.829973e+01 7.772622e+01
#>  [346] 6.776236e+01 6.373216e+01 5.483078e+01 7.788712e+01 1.338905e+02
#>  [351] 9.217418e+01 7.783695e+01 5.698597e+01 6.655347e+01 8.032904e+01
#>  [356] 5.906138e+01 2.059675e+02 2.165295e+02 1.606805e+02 1.851549e+02
#>  [361] 2.380690e+02 7.469068e+01 1.216587e+02 6.524564e+01 6.860830e+01
#>  [366] 6.536707e+01 7.040074e+01 7.406496e+01 7.046835e+01 5.801360e+01
#>  [371] 1.331617e+02 4.422464e+01 5.363285e+01 1.343196e+02 6.465958e+01
#>  [376] 1.771238e+02 8.199093e+01 5.724672e+01 5.722058e+01 8.025855e+01
#>  [381] 5.985204e+01 8.673711e+02 6.560707e+01 5.903801e+01 6.506855e+01
#>  [386] 5.653455e+01 5.851593e+01 6.323110e+01 6.739782e+01 3.791506e+01
#>  [391] 6.398686e+01 6.645688e+01 5.830305e+01 7.862243e+01 8.430266e+01
#>  [396] 4.599216e+02 4.100780e+01 5.629385e+01 6.706486e+01 5.501143e+01
#>  [401] 6.804091e+01 6.293102e+01 6.947223e+01 6.583817e+01 4.543142e+01
#>  [406] 4.626781e+01 6.111205e+01 7.130154e+01 6.083520e+01 5.845476e+01
#>  [411] 9.437541e+01 7.602731e+01 6.584483e+01 5.585749e+01 6.370641e+01
#>  [416] 6.012862e+01 5.725763e+01 5.900620e+01 5.476947e+01 7.832811e+01
#>  [421] 5.621778e+01 5.893471e+01 7.225347e+01 6.048319e+01 7.837692e+01
#>  [426] 6.674149e+01 6.482552e+01 5.723271e+01 7.909235e+01 6.636057e+01
#>  [431] 6.407065e+01 5.828290e+01 8.879126e+01 5.338843e+01 6.233818e+01
#>  [436] 7.719949e+01 7.288997e+01 6.652129e+01 7.224105e+01 5.720633e+01
#>  [441] 1.173641e+02 8.126473e+01 2.321267e+02 9.782518e+01 1.084661e+02
#>  [446] 8.501736e+01 9.938865e+01 6.571747e+01 6.851008e+01 8.719855e+01
#>  [451] 5.924348e+01 6.852027e+01 5.955307e+01 7.120323e+01 5.948387e+01
#>  [456] 5.737786e+01 7.304223e+01 4.932326e+01 1.840198e+02 1.007476e+02
#>  [461] 6.138217e+01 7.081550e+01 5.740018e+01 2.531125e+02 2.891770e+02
#>  [466] 5.787901e+01 1.196534e+02 7.954056e+01 5.987960e+01 5.050936e+01
#>  [471] 4.742201e+01 5.271226e+02 8.896035e+01 9.685226e+01 6.687007e+01
#>  [476] 5.893628e+01 4.521484e+02 5.672728e+01 7.450257e+01 5.693927e+01
#>  [481] 6.127848e+01 6.716523e+01 6.560072e+01 5.438993e+01 6.041346e+01
#>  [486] 1.234186e+02 6.631943e+01 9.515202e+01 6.470874e+01 1.078363e+02
#>  [491] 9.813800e+01 1.347947e+02 6.935986e+01 5.588405e+01 6.830415e+01
#>  [496] 8.454885e+01 5.417252e+01 6.220726e+01 6.025848e+01 7.508397e+01
#>  [501] 5.578644e+01 5.648364e+01 7.838507e+01 6.903648e+01 7.210550e+01
#>  [506] 7.348342e+01 7.680156e+01 6.284744e+01 6.657499e+01 5.707112e+01
#>  [511] 6.283633e+01 2.037122e+02 9.236507e+01 1.014946e+02 9.850952e+01
#>  [516] 4.585519e+01 6.758795e+01 5.936487e+01 6.550279e+01 6.524674e+01
#>  [521] 5.060696e+01 5.206430e+01 1.084502e+02 6.041868e+01 1.260789e+02
#>  [526] 6.298386e+01 7.532370e+01 5.875142e+01 5.921166e+01 7.287224e+01
#>  [531] 1.041186e+02 6.801707e+01 6.462380e+01 7.917345e+01 7.951809e+01
#>  [536] 5.682912e+01 6.874668e+01 1.085534e+02 8.376930e+01 6.570861e+01
#>  [541] 6.144718e+01 5.998177e+01 5.633156e+01 6.392589e+01 7.269638e+01
#>  [546] 6.227448e+01 7.526607e+01 7.298641e+01 8.220947e+01 1.346639e+02
#>  [551] 5.950333e+01 6.534041e+01 7.215092e+01 6.863538e+01 7.983473e+01
#>  [556] 8.329722e+01 6.253911e+01 5.441299e+01 7.048359e+01 7.619150e+01
#>  [561] 9.040546e+01 5.989284e+01 4.944359e+01 5.814871e+01 6.676949e+01
#>  [566] 1.138516e+02 5.891439e+01 6.868949e+01 6.331740e+01 8.299125e+01
#>  [571] 1.012208e+02 6.454573e+01 6.753654e+01 5.566916e+01 6.131168e+01
#>  [576] 6.798650e+01 5.766510e+01 9.053861e+01 6.020326e+01 7.733688e+01
#>  [581] 7.028443e+01 6.022084e+01 6.062940e+01 6.826556e+01 8.754182e+01
#>  [586] 2.206935e+02 5.668949e+01 6.970193e+01 6.705680e+01 9.108115e+01
#>  [591] 6.688877e+01 6.317719e+01 8.192984e+01 6.014201e+01 1.563945e+02
#>  [596] 9.982423e+01 6.398075e+01 6.020524e+01 6.752169e+01 7.239501e+01
#>  [601] 5.561912e+01 6.738679e+01 7.233044e+01 7.341542e+01 6.892921e+01
#>  [606] 6.701218e+01 1.129868e+02 9.230536e+01 6.015910e+01 6.445643e+01
#>  [611] 5.661874e+01 6.048674e+01 6.285068e+01 6.229203e+01 1.111276e+02
#>  [616] 9.004268e+01 6.242225e+01 6.475674e+01 5.911045e+01 6.989785e+01
#>  [621] 6.405538e+01 6.318212e+01 6.927434e+01 6.221549e+01 2.513993e+02
#>  [626] 7.057124e+01 6.341467e+01 5.707457e+01 7.245665e+01 7.719905e+01
#>  [631] 6.900630e+01 7.893508e+01 6.131417e+01 5.461695e+01 9.099557e+01
#>  [636] 1.330776e+02 6.885628e+01 8.591150e+01 1.161071e+02 7.093185e+01
#>  [641] 6.153671e+01 6.169220e+01 6.742361e+01 1.085107e+02 5.345080e+01
#>  [646] 7.366561e+01 5.741580e+01 8.809687e+01 7.112502e+01 5.923080e+01
#>  [651] 6.464682e+01 9.965051e+01 9.014963e+01 5.847857e+01 8.006823e+01
#>  [656] 5.796685e+01 6.011209e+01 5.978361e+01 5.947462e+01 6.868313e+01
#>  [661] 6.808131e+01 5.670368e+01 4.029570e+02 5.494455e+01 6.291694e+01
#>  [666] 5.727808e+01 5.839897e+01 7.412401e+01 5.689099e+01 6.452371e+01
#>  [671] 1.984750e+02 1.077686e+02 6.626749e+01 4.911222e+01 7.435296e+01
#>  [676] 6.481529e+01 6.007248e+01 6.224359e+01 5.741492e+01 6.194581e+01
#>  [681] 6.253901e+01 6.685087e+01 6.539159e+01 5.968084e+01 6.242301e+01
#>  [686] 7.934410e+01 1.479297e+02 8.344561e+01 6.480434e+01 1.323509e+02
#>  [691] 8.998493e+01 6.433360e+01 6.157235e+01 6.590451e+01 5.350831e+01
#>  [696] 6.093403e+01 6.030834e+01 2.586075e+02 1.075872e+03 7.973908e+01
#>  [701] 5.857005e+01 5.722390e+01 6.769595e+01 6.495645e+01 6.252343e+01
#>  [706] 5.348874e+01 5.814165e+01 6.112977e+01 6.112430e+01 5.871705e+01
#>  [711] 5.749215e+01 6.123382e+01 8.890879e+01 7.726442e+01 5.783193e+01
#>  [716] 6.576780e+01 9.078537e+01 6.979258e+01 6.017113e+01 9.772597e+01
#>  [721] 7.452659e+01 1.438308e+02 6.600709e+01 6.651613e+01 1.210728e+02
#>  [726] 5.731534e+01 5.824557e+01 7.037341e+01 6.574017e+01 7.958579e+01
#>  [731] 6.239705e+01 5.780883e+01 5.493025e+01 7.155386e+01 5.749114e+01
#>  [736] 5.696528e+01 1.263483e+03 2.217621e+02 1.982609e+02 8.033885e+01
#>  [741] 5.718272e+01 7.572166e+01 1.853554e+02 5.903432e+01 6.903089e+01
#>  [746] 6.495602e+01 6.833827e+01 7.375969e+01 7.067313e+02 7.743747e+03
#>  [751] 7.093665e+03 3.059024e+06 1.464699e+02 1.158619e+02 6.009906e+01
#>  [756] 6.358137e+01 6.580628e+01 6.193848e+01 7.446309e+01 5.858580e+01
#>  [761] 6.089016e+01 5.685066e+01 8.462177e+01 5.966839e+02 6.667301e+01
#>  [766] 6.415309e+01 5.601506e+01 1.011010e+02 6.296003e+01 4.514893e+02
#>  [771] 1.861625e+02 7.236055e+01 7.152588e+01 5.773120e+01 9.989230e+01
#>  [776] 8.744703e+01 6.106152e+01 5.677724e+01 6.252481e+01 5.834998e+01
#>  [781] 5.463316e+01 6.713467e+01 1.000963e+02 5.694005e+01 9.155247e+01
#>  [786] 6.163284e+01 7.196030e+01 9.710410e+01 6.100806e+01 6.458600e+01
#>  [791] 7.725377e+01 6.360545e+01 5.817065e+01 6.134054e+01 6.088061e+01
#>  [796] 1.147051e+03 1.612737e+02 5.426761e+01 5.489443e+01 6.471067e+01
#>  [801] 2.797199e+02 1.809478e+02 5.726931e+01 1.280018e+02 6.856461e+01
#>  [806] 7.842118e+01 6.848668e+01 6.046865e+01 6.124385e+01 5.908256e+01
#>  [811] 5.584176e+01 7.866796e+01 5.631236e+01 6.272152e+01 5.624869e+01
#>  [816] 7.231652e+01 6.497828e+01 8.010220e+01 1.402861e+02 5.221728e+01
#>  [821] 7.074004e+01 9.277372e+01 5.729411e+01 5.897571e+01 5.841198e+01
#>  [826] 6.102023e+01 7.102251e+01 5.836739e+01 6.045080e+01 1.602592e+02
#>  [831] 9.542026e+01 1.687948e+03 1.200082e+02 6.031859e+01 6.378382e+01
#>  [836] 6.081247e+01 6.900157e+01 8.534318e+01 7.610978e+01 5.490736e+01
#>  [841] 7.322505e+01 9.040862e+01 5.673676e+01 5.608838e+01 5.961854e+01
#>  [846] 5.633151e+01 6.275846e+01 9.065839e+01 5.516886e+01 6.561704e+01
#>  [851] 1.859177e+02 1.030540e+02 8.388114e+01 6.334638e+01 5.820284e+01
#>  [856] 6.767388e+01 7.781063e+01 9.793486e+01 6.232842e+01 6.873800e+01
#>  [861] 5.453565e+01 6.974780e+01 6.191360e+01 5.765194e+01 2.398068e+02
#>  [866] 8.037736e+01 6.185146e+01 5.592078e+01 7.985488e+01 1.566807e+02
#>  [871] 6.919499e+01 1.687632e+02 4.399026e+01 5.479651e+01 6.075314e+01
#>  [876] 1.004541e+02 7.929565e+01 7.324984e+01 8.867527e+01 6.948047e+01
#>  [881] 6.013576e+01 6.252739e+01 6.414558e+01 1.338632e+02 6.535533e+01
#>  [886] 7.456280e+01 5.946455e+01 5.436045e+01 5.930734e+01 7.855200e+01
#>  [891] 8.438911e+01 4.611794e+02 1.093423e+02 6.995966e+01 5.961917e+01
#>  [896] 1.085360e+02 6.350063e+01 5.871042e+01 6.694003e+01 9.770877e+01
#>  [901] 1.369155e+02 2.323598e+02 7.077958e+01 9.100881e+01 1.015778e+02
#>  [906] 2.755342e+04 6.260523e+01 8.236512e+01 5.165701e+01 5.587583e+01
#>  [911] 6.242748e+01 5.188052e+01 1.084570e+02 6.022327e+01 6.508125e+01
#>  [916] 6.677333e+01 8.889976e+01 1.100853e+02 6.635987e+01 5.757948e+01
#>  [921] 5.642381e+01 5.687072e+01 1.430859e+02 4.730362e+02 6.710145e+01
#>  [926] 4.893876e+01 6.165587e+01 7.294708e+01 5.393825e+01 6.188068e+01
#>  [931] 6.065994e+01 6.225145e+01 5.874189e+01 1.173751e+02 5.117103e+01
#>  [936] 6.710007e+01 6.018843e+01 5.830722e+01 6.809251e+01 1.112178e+02
#>  [941] 1.243931e+02 1.004471e+02 7.174082e+01 7.589907e+01 6.203368e+01
#>  [946] 6.180229e+01 5.616288e+01 5.798357e+01 2.080639e+03 1.637205e+02
#>  [951] 5.531884e+01 9.480877e+01 7.624135e+01 7.909018e+01 5.328399e+01
#>  [956] 5.432616e+01 7.096181e+01 1.351015e+02 1.311245e+02 5.939024e+01
#>  [961] 5.758841e+01 7.187800e+01 1.176115e+02 8.965476e+01 6.942174e+01
#>  [966] 6.162050e+01 6.039050e+01 5.617921e+01 1.864724e+02 7.273343e+01
#>  [971] 4.938516e+01 6.310524e+01 8.024457e+01 5.580297e+01 8.797789e+01
#>  [976] 5.703819e+01 5.965407e+01 7.930214e+01 6.132446e+01 6.990984e+01
#>  [981] 5.549498e+01 6.014417e+01 6.027218e+01 5.701620e+01 6.246391e+01
#>  [986] 6.521921e+01 6.564713e+01 7.939417e+01 1.041624e+02 7.590602e+01
#>  [991] 5.671688e+01 6.279571e+01 6.641358e+01 6.109838e+01 7.629221e+01
#>  [996] 7.043006e+01 6.393641e+01 1.014110e+02 8.693925e+01 5.628867e+01
```
