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
#>  [1]  76.01477  76.01477  76.01477  60.75526  60.75526  63.09113  95.57338
#>  [8]  95.57338  95.57338  95.57338  40.05994  14.75573  82.35738  82.35738
#> [15]  82.35738  82.35738 169.78211  27.56070  27.56070  26.80050

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
#>    [1] 5.902102e+01 2.951577e+02 8.055047e+01 1.958449e+02 7.134037e+01
#>    [6] 5.196551e+01 6.317372e+01 8.597143e+01 6.102083e+01 8.056166e+01
#>   [11] 5.423210e+01 1.743544e+02 8.107576e+01 6.364794e+01 8.459254e+01
#>   [16] 6.656678e+01 7.895516e+01 5.615940e+01 6.658075e+01 1.286869e+02
#>   [21] 7.780056e+01 5.168642e+01 5.588926e+01 9.266194e+01 6.199442e+01
#>   [26] 6.450711e+01 8.782509e+01 1.755367e+02 7.159670e+01 7.074601e+01
#>   [31] 1.061472e+02 6.018282e+01 5.829683e+01 9.165081e+01 6.047128e+01
#>   [36] 6.584113e+01 7.828440e+01 7.794073e+01 1.106761e+02 7.429747e+01
#>   [41] 6.415370e+01 6.110455e+01 6.895700e+01 6.852656e+01 1.199876e+02
#>   [46] 6.588314e+01 1.524091e+02 6.104943e+01 6.672398e+01 1.983739e+02
#>   [51] 6.455077e+01 5.758487e+01 1.483955e+02 1.556701e+02 7.363508e+01
#>   [56] 8.393636e+01 5.389160e+01 5.204390e+01 7.907430e+01 8.272919e+01
#>   [61] 5.421324e+01 6.521388e+01 9.685282e+01 7.073329e+01 6.576878e+01
#>   [66] 6.447559e+01 1.341441e+03 5.692957e+01 6.958365e+01 6.270714e+01
#>   [71] 1.375483e+02 6.536688e+01 5.666871e+01 6.182362e+01 6.302983e+01
#>   [76] 7.224367e+01 8.783346e+01 7.009995e+01 5.544363e+01 6.276057e+01
#>   [81] 7.123779e+01 9.568766e+01 5.930772e+01 5.280758e+01 5.534743e+01
#>   [86] 6.295658e+01 6.548945e+01 7.279649e+01 2.909334e+02 6.925916e+01
#>   [91] 6.569851e+01 6.099581e+01 5.558530e+01 6.037315e+01 1.965805e+02
#>   [96] 5.488345e+01 5.323264e+01 1.262683e+02 1.796337e+04 6.826628e+10
#>  [101] 5.952119e+01 6.258122e+01 6.066130e+01 6.260334e+01 5.450207e+01
#>  [106] 1.196974e+02 7.129373e+01 5.771241e+01 6.244178e+01 5.471623e+01
#>  [111] 1.244853e+02 1.608021e+02 7.662094e+01 5.432914e+01 5.447443e+01
#>  [116] 6.145539e+01 7.006443e+01 6.646874e+01 7.207651e+01 8.483016e+01
#>  [121] 7.828515e+01 6.623820e+01 1.035065e+02 7.045429e+01 5.453862e+01
#>  [126] 5.260311e+01 6.880392e+02 5.741883e+01 6.418645e+01 5.793220e+01
#>  [131] 8.232470e+01 6.654737e+01 5.984665e+01 6.299967e+01 6.152958e+01
#>  [136] 6.882003e+01 6.692559e+01 5.572481e+01 6.449971e+01 6.397961e+01
#>  [141] 6.530848e+01 5.849052e+01 5.686583e+01 1.002263e+02 5.875687e+01
#>  [146] 6.317821e+01 5.891858e+01 6.226440e+01 6.014200e+01 9.280788e+01
#>  [151] 6.995899e+01 5.403673e+01 6.082634e+01 5.570189e+01 5.870522e+01
#>  [156] 6.863050e+01 1.242611e+02 6.082223e+01 6.364603e+01 6.555162e+01
#>  [161] 6.630552e+01 8.978012e+01 6.198411e+01 1.251325e+02 5.746991e+01
#>  [166] 6.204799e+01 6.264991e+01 6.166400e+01 7.798244e+01 3.671138e+02
#>  [171] 6.087250e+01 6.369945e+01 8.473486e+01 9.366924e+01 5.339777e+02
#>  [176] 1.124988e+02 6.910114e+01 5.550572e+02 1.129555e+02 7.079211e+01
#>  [181] 6.663409e+01 6.861502e+01 7.692238e+01 7.087516e+01 6.408578e+01
#>  [186] 6.539821e+01 5.708295e+01 9.364407e+01 5.996899e+01 7.427849e+01
#>  [191] 5.749774e+01 5.509784e+01 6.263707e+01 6.108544e+01 7.316881e+01
#>  [196] 7.661872e+01 7.609941e+01 6.073211e+01 6.236310e+01 8.673552e+01
#>  [201] 5.558392e+01 9.518182e+01 5.649423e+01 6.151354e+01 5.942177e+01
#>  [206] 6.222162e+01 3.951387e+01 5.788095e+01 6.173247e+01 5.485187e+01
#>  [211] 5.707483e+01 7.615348e+01 8.623185e+01 7.747835e+01 5.410778e+01
#>  [216] 6.395191e+01 8.162377e+01 7.606593e+01 2.411694e+02 5.511006e+01
#>  [221] 2.119937e+02 5.709159e+01 6.109899e+01 9.430776e+01 7.842022e+01
#>  [226] 5.617524e+02 5.122328e+01 8.011935e+01 7.314485e+01 2.014175e+02
#>  [231] 8.042118e+01 6.744489e+01 5.837916e+01 5.613929e+01 6.444066e+01
#>  [236] 9.612936e+01 6.961295e+01 8.526776e+01 5.975702e+01 8.793186e+01
#>  [241] 8.682372e+01 8.732404e+01 7.221549e+01 7.055377e+01 9.563146e+01
#>  [246] 1.024283e+02 6.271507e+01 1.258263e+02 5.424164e+01 1.100441e+02
#>  [251] 1.582327e+02 5.932563e+01 5.972311e+01 6.514886e+01 5.145251e+01
#>  [256] 6.316940e+01 6.774252e+01 8.934240e+01 6.027281e+01 2.046759e+02
#>  [261] 1.133160e+02 5.803407e+01 1.584513e+02 8.598345e+01 7.598001e+01
#>  [266] 5.998374e+01 5.703982e+01 5.522566e+01 6.243960e+01 6.056501e+01
#>  [271] 6.266110e+01 6.624292e+01 7.463574e+01 6.487425e+01 5.366515e+01
#>  [276] 6.092690e+01 6.078439e+01 5.999943e+01 6.981181e+01 7.183478e+01
#>  [281] 6.344393e+01 5.333630e+01 6.269351e+01 6.991970e+01 5.868793e+01
#>  [286] 6.693759e+01 4.586356e+02 6.196449e+01 1.093037e+02 5.643916e+01
#>  [291] 5.843161e+01 1.266668e+02 5.536557e+01 6.894592e+01 6.343186e+01
#>  [296] 5.887825e+01 1.668223e+02 7.225122e+01 6.037244e+01 7.188746e+01
#>  [301] 5.653933e+01 9.142356e+01 7.055087e+01 5.837303e+01 6.059446e+01
#>  [306] 6.304034e+01 6.347574e+01 6.293872e+01 5.387686e+01 6.368610e+01
#>  [311] 7.532781e+01 6.368965e+01 6.010097e+01 1.086050e+02 7.737876e+01
#>  [316] 6.774755e+01 6.344484e+01 6.307748e+01 6.595134e+01 5.974591e+01
#>  [321] 5.704192e+01 5.577988e+01 7.155462e+01 6.857581e+01 6.744630e+01
#>  [326] 6.505742e+01 6.229128e+01 5.654898e+01 8.007668e+01 8.079232e+01
#>  [331] 5.826653e+01 6.767466e+01 5.833319e+01 6.922271e+01 6.839713e+01
#>  [336] 1.892522e+02 5.812445e+01 7.066581e+01 8.942310e+01 5.727651e+01
#>  [341] 9.145199e+01 6.193793e+01 6.376451e+01 6.116778e+01 5.818390e+01
#>  [346] 5.900776e+01 5.465958e+01 6.153680e+01 5.992627e+01 5.946665e+01
#>  [351] 1.194017e+02 9.776349e+01 4.849702e+01 6.000879e+01 8.037109e+01
#>  [356] 4.637360e+03 1.594032e+02 6.013666e+01 6.820099e+01 1.054042e+03
#>  [361] 7.497604e+01 6.166199e+01 9.106264e+01 5.652396e+01 6.294741e+01
#>  [366] 5.777487e+01 6.976204e+01 6.898126e+01 1.010460e+02 1.125439e+02
#>  [371] 7.659958e+01 9.766142e+01 2.030936e+02 1.295467e+02 5.773259e+01
#>  [376] 9.989260e+01 8.682351e+01 6.239263e+01 1.069436e+02 1.328552e+02
#>  [381] 6.150250e+01 7.245990e+01 1.102572e+02 7.325557e+01 8.072239e+01
#>  [386] 5.759646e+01 5.952687e+01 6.467822e+01 5.910780e+01 5.850849e+01
#>  [391] 6.827079e+01 6.956941e+01 5.898195e+01 8.880057e+01 6.215781e+01
#>  [396] 6.446463e+01 8.747660e+01 7.422282e+01 6.384186e+01 6.557514e+01
#>  [401] 5.887651e+01 7.373868e+01 6.099242e+01 5.643452e+01 6.102922e+01
#>  [406] 6.971202e+01 5.054298e+01 6.213190e+01 5.667104e+01 7.901797e+01
#>  [411] 5.498036e+01 6.291842e+01 5.817022e+01 8.415702e+01 8.169736e+01
#>  [416] 8.423967e+01 5.864726e+01 6.435585e+01 7.448832e+01 5.595378e+01
#>  [421] 1.331919e+02 5.072647e+01 5.696797e+01 7.963660e+01 5.945502e+01
#>  [426] 6.310929e+01 5.646580e+01 1.160411e+02 7.708846e+01 1.368976e+02
#>  [431] 6.926328e+01 7.127406e+01 9.566531e+01 6.520102e+01 5.705701e+01
#>  [436] 7.741405e+01 5.967205e+01 6.330032e+01 6.012231e+01 6.804698e+01
#>  [441] 6.739790e+01 6.010200e+01 6.728740e+01 6.517185e+01 6.067588e+01
#>  [446] 8.201014e+01 7.551181e+01 7.011073e+01 7.187403e+01 1.729282e+02
#>  [451] 5.914388e+01 6.229458e+01 9.081839e+01 6.349568e+01 6.204422e+01
#>  [456] 7.897225e+01 1.116832e+02 2.095223e+02 5.828448e+01 8.374878e+01
#>  [461] 1.450962e+02 5.534994e+01 5.621945e+01 1.949199e+02 2.556412e+02
#>  [466] 5.375018e+01 6.647798e+01 6.205176e+01 6.434805e+01 5.581687e+01
#>  [471] 6.841987e+01 5.830880e+01 5.446979e+01 5.438637e+01 7.374388e+01
#>  [476] 7.133409e+01 1.121136e+02 8.338584e+01 8.708831e+01 5.436556e+01
#>  [481] 3.493526e+02 6.489626e+01 5.407418e+01 8.532824e+01 8.228334e+01
#>  [486] 7.267252e+01 6.608154e+01 5.823238e+01 5.841892e+01 8.249332e+01
#>  [491] 6.981691e+01 8.671425e+01 1.486584e+02 1.065823e+02 1.050789e+02
#>  [496] 1.305107e+02 7.689034e+01 7.160640e+01 5.654063e+01 7.187286e+01
#>  [501] 8.823769e+01 7.147388e+01 6.388227e+01 5.927504e+01 7.005442e+01
#>  [506] 6.617496e+01 5.290377e+01 7.226025e+01 9.261684e+01 7.564165e+01
#>  [511] 1.073067e+02 5.218218e+01 5.669425e+01 5.480529e+01 6.197332e+01
#>  [516] 5.171457e+01 6.568727e+01 7.078488e+01 6.388094e+01 1.214832e+02
#>  [521] 6.466987e+01 1.206717e+02 8.103699e+01 6.292190e+01 5.887468e+01
#>  [526] 1.456751e+02 9.230961e+01 1.034148e+02 7.350174e+01 6.091839e+01
#>  [531] 9.098142e+01 1.231931e+02 7.215946e+01 7.969962e+01 9.198861e+01
#>  [536] 6.056204e+01 7.119115e+01 1.274674e+02 5.689325e+01 1.257750e+02
#>  [541] 5.508308e+01 5.652743e+01 6.058541e+01 6.214650e+01 9.118978e+01
#>  [546] 3.695077e+02 5.375612e+01 5.872574e+01 7.737947e+01 5.886721e+01
#>  [551] 6.667290e+01 5.709018e+01 7.157891e+01 5.419442e+01 6.508274e+01
#>  [556] 7.756620e+01 6.959260e+01 5.747725e+01 1.424405e+02 1.156511e+02
#>  [561] 5.927005e+01 7.503243e+01 9.612221e+01 8.610882e+01 9.120745e+01
#>  [566] 8.598284e+01 5.889657e+01 6.424000e+01 5.990315e+01 1.700455e+02
#>  [571] 1.294447e+02 1.114789e+02 3.606533e+02 2.763166e+03 4.646805e+02
#>  [576] 1.410047e+03 9.814470e+01 5.939977e+01 7.293890e+01 5.997526e+01
#>  [581] 6.634870e+01 5.574775e+01 1.010303e+02 5.724542e+01 9.518386e+01
#>  [586] 8.024427e+01 5.923910e+01 9.786007e+01 6.353193e+01 5.403637e+01
#>  [591] 8.501763e+01 6.923991e+01 6.034235e+01 5.826838e+01 7.242170e+01
#>  [596] 6.685699e+01 5.938475e+01 5.814415e+01 6.220568e+01 5.576804e+01
#>  [601] 5.993927e+01 7.484428e+01 2.605424e+02 4.072972e+03 4.997635e+01
#>  [606] 6.460762e+01 8.508989e+01 5.497381e+01 6.185019e+01 5.876589e+01
#>  [611] 8.985579e+01 5.889129e+01 1.405767e+02 1.184587e+02 7.251871e+01
#>  [616] 5.921391e+01 7.059317e+01 6.118320e+01 5.704535e+01 6.175218e+01
#>  [621] 5.994763e+01 6.702326e+01 7.575542e+01 6.152463e+01 1.376803e+02
#>  [626] 7.481144e+01 6.281012e+01 6.880818e+01 5.787689e+01 6.063328e+01
#>  [631] 7.224668e+01 1.022227e+02 9.869770e+01 6.086673e+01 7.988220e+01
#>  [636] 6.237711e+01 6.249169e+01 7.304422e+01 7.105723e+01 2.300560e+02
#>  [641] 5.855050e+01 9.954229e+01 8.303451e+01 5.523767e+01 8.825190e+01
#>  [646] 7.637287e+01 5.798096e+01 6.592768e+01 5.537002e+01 6.241698e+01
#>  [651] 1.068817e+02 5.712770e+01 5.621391e+01 6.022106e+01 6.659000e+01
#>  [656] 6.565970e+01 5.267707e+01 6.036090e+01 6.516250e+01 6.346270e+01
#>  [661] 5.777880e+01 7.592028e+01 7.194303e+01 5.482368e+01 2.041480e+02
#>  [666] 5.443412e+01 5.087890e+01 5.619199e+01 6.172272e+01 5.768171e+01
#>  [671] 7.744009e+01 6.034968e+01 5.626960e+01 5.993680e+01 7.025377e+01
#>  [676] 6.605274e+01 5.528994e+01 5.660264e+01 5.548381e+01 1.119491e+02
#>  [681] 6.123672e+01 8.362186e+01 1.077624e+02 6.052098e+01 5.724622e+01
#>  [686] 5.188197e+01 6.766593e+01 5.911120e+01 7.081030e+01 6.130392e+01
#>  [691] 9.465883e+01 1.018858e+02 5.857543e+01 6.488137e+01 6.574731e+01
#>  [696] 7.232058e+01 6.440064e+01 6.434349e+01 6.030134e+01 6.456556e+01
#>  [701] 7.134314e+01 1.991215e+02 2.446224e+02 6.500147e+01 5.873377e+01
#>  [706] 2.734331e+02 6.370071e+01 7.645268e+01 6.230338e+01 5.939086e+01
#>  [711] 6.453233e+01 2.148274e+02 8.348771e+01 1.627506e+02 4.796989e+01
#>  [716] 6.968933e+01 6.809392e+01 8.701259e+01 5.183447e+01 1.453796e+02
#>  [721] 3.824621e+02 5.815841e+01 7.172688e+01 9.032499e+01 5.114627e+01
#>  [726] 7.302019e+01 5.064599e+01 5.255891e+01 1.447317e+02 9.732122e+01
#>  [731] 9.514877e+01 8.449491e+01 5.791786e+01 6.766249e+01 5.677780e+01
#>  [736] 6.414814e+01 5.588593e+01 7.595851e+01 1.740266e+02 1.327593e+02
#>  [741] 1.828011e+02 5.651169e+01 6.079660e+01 1.053694e+02 6.296810e+01
#>  [746] 7.216916e+01 6.175337e+01 5.953844e+01 5.460892e+01 7.962515e+01
#>  [751] 4.987379e+01 6.186407e+01 4.130683e+02 5.503445e+01 6.601447e+01
#>  [756] 5.687558e+01 9.702312e+01 5.731000e+01 5.865820e+01 6.662811e+01
#>  [761] 6.788171e+01 6.811197e+01 6.885236e+01 5.701911e+01 6.281147e+01
#>  [766] 7.115920e+01 7.127117e+01 5.851845e+01 6.591814e+01 8.865249e+01
#>  [771] 5.964090e+01 7.579425e+01 8.506922e+01 2.251367e+02 1.117796e+02
#>  [776] 1.116647e+02 6.561556e+01 5.862086e+01 5.813599e+01 8.287711e+01
#>  [781] 5.461031e+01 1.080802e+02 6.495387e+01 6.404105e+01 7.466656e+01
#>  [786] 1.185548e+02 6.389739e+01 9.108534e+01 7.920838e+01 9.173920e+01
#>  [791] 6.050923e+01 5.743659e+01 5.383089e+01 1.928792e+02 7.855222e+01
#>  [796] 6.049222e+01 7.027111e+01 6.599168e+01 5.715101e+01 5.950129e+01
#>  [801] 7.911501e+01 7.881793e+01 7.611225e+01 6.085320e+01 7.441214e+01
#>  [806] 5.378112e+01 1.057451e+02 6.591019e+01 6.659601e+01 6.135329e+01
#>  [811] 6.428701e+01 7.750363e+01 6.073694e+01 7.521013e+01 8.872214e+01
#>  [816] 6.166960e+01 8.991896e+01 9.106044e+01 7.783241e+01 6.591188e+01
#>  [821] 6.540473e+01 5.673144e+01 5.960629e+01 9.542070e+01 5.740385e+01
#>  [826] 5.957045e+01 9.198031e+01 6.248343e+01 6.514879e+01 1.141950e+02
#>  [831] 5.489244e+01 5.583180e+01 6.704236e+01 1.492242e+02 8.386288e+01
#>  [836] 7.491787e+01 6.200827e+01 6.481960e+01 9.474211e+01 6.080419e+01
#>  [841] 6.561571e+01 5.408299e+01 7.439185e+01 5.656481e+01 6.592417e+01
#>  [846] 9.893748e+01 6.137783e+01 2.281819e+02 5.522600e+01 7.859100e+01
#>  [851] 7.083950e+01 6.464339e+01 5.573968e+01 5.797308e+02 8.923252e+01
#>  [856] 6.573846e+01 6.338487e+01 7.083145e+01 4.671113e+01 1.424221e+02
#>  [861] 5.741109e+01 5.712007e+01 6.298223e+01 6.580242e+01 5.985909e+01
#>  [866] 7.705749e+01 6.352147e+01 6.847105e+01 6.103165e+01 5.633305e+01
#>  [871] 6.465416e+01 5.521288e+01 1.320692e+02 6.370605e+01 6.235391e+01
#>  [876] 1.042042e+02 6.193367e+01 7.360572e+01 5.822029e+01 7.588659e+01
#>  [881] 6.772808e+01 6.231535e+01 1.075881e+02 5.986927e+01 6.469889e+01
#>  [886] 7.201994e+01 8.181369e+01 1.040877e+02 6.077331e+01 7.243421e+01
#>  [891] 5.902872e+01 6.464438e+01 8.168967e+01 6.220111e+01 6.189325e+01
#>  [896] 1.001595e+02 5.295454e+01 1.962717e+02 8.805983e+01 6.447297e+01
#>  [901] 8.735087e+01 5.602116e+01 5.134864e+01 6.199883e+01 5.649488e+01
#>  [906] 6.046446e+01 3.150041e+05 8.160217e+01 5.397671e+01 5.804246e+01
#>  [911] 5.397702e+01 6.731284e+01 5.811775e+01 6.546966e+01 5.233879e+02
#>  [916] 5.704019e+01 5.795668e+01 6.938247e+01 6.468650e+01 1.107979e+02
#>  [921] 5.924663e+01 6.010870e+01 6.738140e+01 5.646480e+01 6.607411e+01
#>  [926] 7.185816e+01 5.843681e+01 7.924676e+01 5.056139e+02 4.282506e+02
#>  [931] 6.051115e+01 6.420597e+01 8.262130e+01 6.601768e+01 5.987781e+01
#>  [936] 5.475501e+01 6.401046e+01 6.037492e+01 7.406678e+01 1.539014e+02
#>  [941] 7.138766e+01 7.102802e+01 6.841522e+01 2.424014e+02 9.677838e+01
#>  [946] 6.010440e+01 6.270233e+01 7.366633e+01 5.623268e+01 5.421653e+01
#>  [951] 7.876993e+01 6.527977e+01 5.893505e+01 6.256032e+01 5.625358e+01
#>  [956] 6.222714e+01 5.877981e+01 6.516326e+01 5.549500e+01 6.146831e+01
#>  [961] 5.864600e+01 6.015534e+01 6.212290e+01 1.915558e+02 6.356155e+01
#>  [966] 6.540715e+01 6.160943e+01 6.558355e+01 5.873916e+01 2.999878e+02
#>  [971] 2.190605e+02 6.634653e+01 1.988980e+02 1.131761e+02 6.159363e+01
#>  [976] 5.960651e+01 6.973939e+01 6.105027e+01 4.263959e+02 6.561787e+01
#>  [981] 6.788927e+01 1.239379e+02 6.448191e+01 6.034891e+01 7.217296e+01
#>  [986] 6.710763e+01 7.729751e+01 5.338206e+01 7.503016e+01 8.140555e+01
#>  [991] 8.053953e+01 6.420211e+01 5.667674e+01 7.805639e+01 6.030609e+01
#>  [996] 6.862538e+01 6.589159e+01 6.596945e+01 7.077028e+01 6.610675e+01
```
