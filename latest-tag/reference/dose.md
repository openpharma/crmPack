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
#>  [1] 71.82688 38.90897 38.90897 38.90897 37.05625 21.44498 21.44498 21.44498
#>  [9] 26.79166 28.83331 28.83331 28.83331 28.83331 28.83331 28.83331 28.83331
#> [17] 28.83331 28.83331 28.83331 28.83331

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
#>    [1] 1.390391e+02 2.902709e+02 5.657694e+01 6.777122e+01 6.581905e+01
#>    [6] 1.095513e+02 5.669328e+01 6.302305e+01 8.341464e+01 6.576791e+01
#>   [11] 5.581761e+01 6.341028e+01 5.870039e+01 6.480722e+01 5.656104e+01
#>   [16] 5.351528e+01 7.774558e+01 5.956751e+01 5.767060e+01 7.808372e+01
#>   [21] 4.425468e+01 5.759071e+01 6.014923e+01 6.274416e+01 8.848108e+01
#>   [26] 6.310618e+01 6.611029e+01 6.441486e+01 5.685015e+01 5.555444e+01
#>   [31] 6.424345e+01 6.311228e+01 6.120223e+01 6.837955e+01 5.862775e+01
#>   [36] 6.265254e+01 8.395250e+01 8.476980e+01 6.210890e+01 6.719100e+01
#>   [41] 6.574466e+01 5.311166e+01 7.659202e+01 9.926908e+01 1.340804e+02
#>   [46] 5.488919e+01 6.030595e+01 7.272707e+01 1.216776e+02 7.487662e+01
#>   [51] 2.148848e+02 7.111961e+01 8.723269e+01 6.696034e+01 6.810367e+01
#>   [56] 9.406191e+01 7.227510e+01 5.908625e+01 6.582985e+01 5.534705e+01
#>   [61] 7.172499e+01 6.208608e+01 2.207390e+02 7.360950e+01 1.390041e+02
#>   [66] 9.469856e+01 2.722575e+02 1.594240e+02 5.817734e+01 6.084647e+01
#>   [71] 6.773412e+01 6.712981e+01 5.751362e+01 6.882933e+01 6.504448e+01
#>   [76] 7.925484e+01 5.131590e+04 2.144766e+03 7.652720e+01 8.503836e+01
#>   [81] 6.556445e+01 5.868456e+01 5.633593e+01 1.791023e+02 9.386619e+01
#>   [86] 2.548232e+02 1.161712e+02 1.099950e+02 5.656999e+01 6.840212e+01
#>   [91] 6.166724e+01 6.738301e+01 6.601716e+01 6.039819e+01 5.767139e+01
#>   [96] 9.627283e+01 6.127162e+01 7.808518e+01 1.491836e+02 6.553622e+01
#>  [101] 6.970443e+01 6.051189e+01 6.206829e+01 6.415546e+01 6.825320e+01
#>  [106] 6.273558e+01 1.208518e+02 1.527322e+02 9.206485e+01 7.414018e+01
#>  [111] 8.235426e+01 7.527047e+01 5.713239e+01 9.435810e+01 8.136405e+01
#>  [116] 7.461297e+01 6.021731e+01 5.986926e+01 5.605840e+01 5.822597e+01
#>  [121] 7.629807e+01 8.266337e+01 6.092174e+01 5.568834e+01 1.033482e+02
#>  [126] 6.162257e+01 5.731770e+01 8.268697e+01 7.505189e+01 6.525260e+01
#>  [131] 6.672163e+01 7.845361e+01 7.477774e+01 7.248932e+01 5.899733e+01
#>  [136] 5.801377e+01 8.806723e+01 1.097631e+02 6.909940e+01 5.749171e+01
#>  [141] 6.712469e+01 6.117295e+01 6.219736e+01 1.421313e+02 5.567853e+01
#>  [146] 6.628418e+01 5.726885e+01 1.314072e+02 5.667141e+01 7.447907e+01
#>  [151] 2.683656e+02 9.960425e+01 6.842634e+01 5.558586e+01 5.447035e+01
#>  [156] 8.791146e+01 5.619348e+01 7.248400e+01 7.655012e+01 1.295366e+02
#>  [161] 6.228687e+01 6.281410e+01 7.799551e+01 6.608779e+01 9.075191e+01
#>  [166] 6.914139e+01 9.083690e+01 6.715557e+01 6.266704e+01 6.147886e+01
#>  [171] 2.834276e+02 2.073919e+02 6.456791e+01 5.835657e+01 5.789004e+01
#>  [176] 6.596349e+01 5.883993e+01 5.165852e+01 8.398151e+01 1.062619e+02
#>  [181] 6.311757e+01 5.503020e+01 6.557627e+01 7.002435e+01 5.769658e+01
#>  [186] 6.791214e+01 6.667573e+01 6.710836e+01 5.884902e+01 6.801060e+01
#>  [191] 1.121312e+02 7.001129e+01 8.384338e+01 7.893813e+01 5.887538e+01
#>  [196] 5.746177e+01 9.047013e+01 6.262185e+01 1.236483e+03 6.368240e+01
#>  [201] 5.991397e+01 6.178582e+01 6.925072e+01 7.233000e+01 5.777866e+01
#>  [206] 6.548758e+01 7.112260e+01 7.569740e+01 5.772493e+01 7.496762e+01
#>  [211] 1.403384e+02 5.946828e+01 6.258232e+01 5.895606e+01 6.264064e+01
#>  [216] 6.078044e+01 5.809568e+01 8.673045e+01 6.457664e+01 7.774442e+01
#>  [221] 6.629312e+01 6.892294e+01 6.797941e+01 7.238158e+01 6.680628e+01
#>  [226] 6.170056e+01 1.140112e+02 6.756696e+01 6.164890e+01 7.880268e+01
#>  [231] 5.784175e+01 7.475573e+01 7.305681e+01 6.012621e+01 1.198355e+02
#>  [236] 8.990854e+01 7.914788e+01 5.930610e+01 6.122030e+01 5.570498e+01
#>  [241] 9.444031e+01 7.365360e+01 5.735564e+01 7.640399e+01 6.043604e+01
#>  [246] 6.275324e+01 9.271992e+01 1.381130e+02 1.017166e+02 4.998028e+02
#>  [251] 6.581803e+01 1.841083e+02 1.096944e+02 6.438164e+01 6.470626e+01
#>  [256] 6.309933e+01 6.400647e+01 8.357600e+01 1.009508e+02 6.541517e+01
#>  [261] 6.551507e+01 6.893222e+01 7.838370e+01 7.579103e+01 5.753857e+01
#>  [266] 8.134318e+01 7.309895e+01 8.071520e+01 6.253814e+01 5.947628e+01
#>  [271] 6.450415e+01 7.607229e+01 6.640491e+01 6.656418e+01 2.109451e+02
#>  [276] 1.341581e+02 6.836081e+01 6.222504e+01 1.194428e+02 5.721209e+01
#>  [281] 4.785883e+01 6.178899e+01 3.183634e+02 1.989800e+02 6.604585e+01
#>  [286] 9.302817e+01 1.409938e+02 1.257795e+02 2.091235e+02 1.347136e+02
#>  [291] 1.873503e+02 6.567282e+01 9.261272e+01 6.426732e+01 1.213122e+02
#>  [296] 3.488078e+01 5.875696e+01 6.247072e+01 5.940676e+01 5.862888e+01
#>  [301] 7.236878e+01 6.193331e+01 5.158956e+01 5.983201e+01 5.588630e+01
#>  [306] 7.400046e+01 6.449249e+01 6.723395e+01 6.166043e+01 5.814570e+01
#>  [311] 6.438982e+01 7.264853e+01 6.693487e+01 5.330221e+01 6.962615e+01
#>  [316] 6.518124e+01 6.236876e+01 5.739119e+01 5.557523e+01 5.620232e+01
#>  [321] 6.014363e+01 6.218081e+01 6.645260e+01 6.561972e+01 5.951684e+01
#>  [326] 6.385404e+01 6.435954e+01 5.837023e+01 1.017746e+02 5.503297e+01
#>  [331] 5.738574e+01 5.858764e+01 6.916796e+01 6.251948e+01 6.507072e+01
#>  [336] 6.692198e+01 1.059666e+02 1.476059e+02 5.242262e+01 8.968763e+01
#>  [341] 7.853538e+01 6.452185e+01 5.445599e+01 6.140206e+01 6.415832e+01
#>  [346] 8.285013e+01 6.538121e+01 5.589092e+01 6.829175e+01 1.211791e+02
#>  [351] 1.210405e+02 6.284345e+01 7.395999e+01 7.392186e+01 7.125479e+01
#>  [356] 7.910057e+01 7.706894e+01 9.125840e+01 6.560453e+01 6.581269e+01
#>  [361] 5.990381e+01 6.157055e+02 5.367772e+01 6.427159e+01 5.750080e+01
#>  [366] 5.984822e+01 5.678852e+01 8.303607e+01 5.580957e+01 3.058669e+02
#>  [371] 1.653915e+02 6.443484e+01 9.846647e+01 5.952121e+01 6.658151e+01
#>  [376] 6.690107e+01 6.235677e+01 1.351942e+02 6.365181e+01 6.613420e+01
#>  [381] 7.104570e+01 6.366691e+01 5.796123e+01 1.155781e+02 6.499904e+01
#>  [386] 6.678294e+01 5.728187e+01 6.629776e+01 7.086007e+01 6.631901e+01
#>  [391] 6.309774e+01 6.248591e+01 6.209364e+01 6.070345e+01 6.223047e+01
#>  [396] 6.379564e+01 8.994089e+01 6.980528e+01 6.744951e+01 4.517027e+01
#>  [401] 6.520841e+01 6.492377e+01 5.891932e+01 6.187905e+01 6.337984e+01
#>  [406] 1.424679e+03 3.236764e+02 5.756528e+01 5.193752e+01 6.028048e+01
#>  [411] 1.068559e+02 5.766177e+01 1.564897e+02 5.471876e+01 5.315393e+01
#>  [416] 6.315799e+01 7.222752e+01 8.343847e+01 1.708256e+02 5.316775e+01
#>  [421] 7.449898e+01 6.969374e+01 5.800707e+01 7.779654e+01 9.247080e+01
#>  [426] 5.922715e+01 8.156151e+01 8.005001e+01 7.966912e+01 8.033460e+01
#>  [431] 7.487970e+01 5.236736e+01 9.019767e+01 9.904099e+01 8.413733e+01
#>  [436] 6.351070e+01 6.785156e+01 5.663111e+01 7.054930e+01 6.995076e+01
#>  [441] 1.361053e+02 7.018104e+01 5.998337e+01 6.189504e+01 7.905619e+01
#>  [446] 2.959426e+02 6.041370e+01 6.191849e+01 4.853192e+01 6.973431e+01
#>  [451] 6.567004e+01 6.342985e+01 8.338030e+01 7.386766e+01 6.402453e+01
#>  [456] 6.114066e+01 9.930707e+01 5.765879e+01 8.130180e+01 5.566668e+01
#>  [461] 6.386461e+01 7.438700e+01 1.112333e+02 5.405900e+01 7.210613e+01
#>  [466] 6.352796e+01 6.628690e+01 6.104319e+01 5.359288e+01 6.272121e+01
#>  [471] 1.142449e+02 6.956725e+01 3.156865e+02 1.105781e+02 5.370374e+01
#>  [476] 7.834018e+01 6.186594e+01 6.104470e+01 5.960590e+01 5.834644e+01
#>  [481] 6.031322e+01 1.344604e+02 6.684953e+01 6.213114e+01 5.957199e+01
#>  [486] 1.471555e+02 8.536042e+01 5.996035e+01 5.798200e+01 2.189201e+02
#>  [491] 7.016007e+01 1.062516e+02 6.398885e+01 5.902802e+01 9.737240e+01
#>  [496] 5.803246e+01 5.983962e+01 9.247468e+01 6.232661e+01 7.497242e+01
#>  [501] 1.060951e+02 2.903315e+02 6.100786e+01 6.210973e+01 6.842389e+01
#>  [506] 5.998454e+01 5.343174e+01 8.547343e+01 6.507531e+01 9.061500e+01
#>  [511] 5.396544e+01 5.574189e+01 1.002801e+02 6.369063e+01 6.084024e+01
#>  [516] 5.939093e+01 7.323034e+01 5.945776e+01 6.939233e+01 5.729029e+01
#>  [521] 6.291072e+01 7.139158e+01 5.264852e+01 7.179065e+01 1.368066e+02
#>  [526] 5.521704e+01 6.190903e+01 6.083969e+01 8.435770e+01 6.374023e+01
#>  [531] 1.194538e+02 4.534259e+01 5.562438e+01 1.205006e+02 6.891605e+01
#>  [536] 1.766993e+02 7.065318e+01 8.804823e+01 4.903273e+01 1.545974e+02
#>  [541] 6.035749e+01 5.876374e+01 7.167969e+01 5.999694e+01 6.232066e+01
#>  [546] 5.589572e+01 8.236574e+01 7.197494e+01 7.149838e+01 8.330787e+01
#>  [551] 5.670589e+01 5.769181e+01 8.147878e+01 5.881362e+01 9.843242e+01
#>  [556] 5.581096e+01 7.039642e+01 9.512240e+01 7.067926e+01 6.518550e+01
#>  [561] 5.120010e+01 7.439567e+01 5.910971e+01 6.348477e+01 6.672602e+01
#>  [566] 5.384472e+01 5.953531e+01 7.377149e+01 9.400573e+01 5.207728e+01
#>  [571] 1.124578e+02 6.019711e+01 5.913318e+01 5.488389e+01 6.100221e+01
#>  [576] 6.089821e+01 6.002154e+01 6.938373e+01 7.044322e+01 8.990381e+01
#>  [581] 8.584932e+01 6.547180e+01 8.124050e+01 6.277696e+01 1.017135e+02
#>  [586] 1.019706e+02 6.390683e+01 6.065853e+01 5.784851e+01 2.340390e+03
#>  [591] 5.984396e+01 1.035388e+02 5.887918e+01 6.656255e+01 6.858048e+01
#>  [596] 5.993616e+01 6.024674e+01 6.915968e+01 5.962108e+01 7.473283e+01
#>  [601] 1.872306e+02 2.930386e+02 3.234476e+02 8.588900e+01 5.790483e+01
#>  [606] 5.363559e+01 6.163998e+01 5.848835e+01 5.567657e+01 6.267051e+01
#>  [611] 9.249002e+01 6.238676e+01 6.605942e+01 4.807072e+01 5.641771e+01
#>  [616] 5.918595e+01 1.367380e+03 8.009498e+02 4.591079e+03 4.537928e+02
#>  [621] 6.547118e+01 7.307686e+01 3.448180e+02 2.021784e+02 8.856372e+01
#>  [626] 5.346319e+01 6.124748e+01 7.041963e+01 6.051057e+01 1.335562e+02
#>  [631] 8.641722e+01 6.487687e+01 5.958412e+01 6.191866e+01 6.064083e+01
#>  [636] 6.161633e+01 6.114000e+01 5.955255e+01 5.525979e+01 6.889567e+01
#>  [641] 6.018868e+01 6.250529e+01 6.023306e+01 6.833224e+01 8.099239e+01
#>  [646] 6.727017e+01 8.478285e+01 6.535068e+01 8.099339e+01 6.068854e+01
#>  [651] 7.368684e+01 6.668916e+01 6.428591e+01 6.333253e+01 6.048931e+01
#>  [656] 5.696339e+01 6.173622e+02 5.902477e+01 5.930713e+01 5.781191e+01
#>  [661] 1.041579e+02 1.105493e+02 1.026612e+02 6.593307e+01 5.917283e+01
#>  [666] 5.885056e+01 6.180270e+01 2.078130e+05 1.090964e+02 6.782488e+01
#>  [671] 6.011776e+01 6.393672e+01 7.042497e+01 9.383473e+01 7.918734e+01
#>  [676] 6.526596e+01 6.975286e+01 6.627130e+01 6.222888e+01 8.845873e+01
#>  [681] 6.009323e+01 5.599585e+01 1.523911e+02 6.943898e+01 6.859497e+01
#>  [686] 6.073200e+01 6.704140e+01 1.117257e+02 5.394753e+01 8.379501e+01
#>  [691] 6.389938e+01 5.951962e+01 7.197849e+01 6.765589e+01 6.939530e+01
#>  [696] 7.288226e+01 7.542906e+01 6.124679e+01 5.522420e+01 6.321072e+01
#>  [701] 3.518622e+02 7.040585e+01 5.863792e+01 7.651079e+01 6.461859e+01
#>  [706] 2.549393e+02 9.893494e+01 1.036767e+02 5.564917e+01 9.621012e+01
#>  [711] 6.429852e+01 7.315753e+01 5.699461e+01 5.503365e+01 7.004235e+01
#>  [716] 1.116422e+02 8.970490e+01 5.947682e+01 6.232490e+01 6.388152e+01
#>  [721] 6.606130e+01 5.687097e+01 9.532460e+01 5.320409e+01 7.079082e+01
#>  [726] 6.995277e+01 6.972478e+01 5.805887e+01 6.295054e+01 5.735410e+01
#>  [731] 5.597976e+01 5.807823e+01 7.297805e+01 6.573425e+01 6.852083e+01
#>  [736] 6.002133e+01 6.570345e+01 6.785367e+01 6.278980e+01 6.342552e+01
#>  [741] 5.777253e+01 5.880991e+01 1.537703e+02 7.314697e+01 8.251424e+01
#>  [746] 1.382417e+02 1.373362e+02 9.063683e+01 5.842376e+01 6.217953e+01
#>  [751] 6.713660e+01 5.822559e+01 5.678921e+01 6.098197e+01 1.263020e+02
#>  [756] 6.549416e+01 1.183474e+02 1.054322e+02 6.282016e+01 6.228896e+01
#>  [761] 6.862429e+01 1.768812e+02 9.093837e+01 2.004554e+02 5.886831e+01
#>  [766] 6.218844e+01 2.447186e+02 8.782287e+01 1.332336e+02 2.456365e+02
#>  [771] 6.957722e+01 7.736287e+01 7.675564e+01 1.989716e+02 1.866672e+02
#>  [776] 5.437371e+01 5.645539e+01 5.507042e+01 7.418927e+01 7.468517e+01
#>  [781] 5.998193e+01 6.277125e+01 5.849343e+01 6.047911e+01 5.962882e+01
#>  [786] 5.538419e+01 8.075708e+01 1.116217e+02 4.327923e+02 6.338882e+01
#>  [791] 5.934313e+01 8.335775e+01 5.971362e+01 7.327891e+01 6.054608e+01
#>  [796] 6.079135e+01 2.196962e+03 2.232918e+02 5.986626e+01 7.757382e+01
#>  [801] 5.695695e+01 5.907050e+01 8.186246e+01 6.093349e+01 5.569132e+01
#>  [806] 6.222751e+01 6.525531e+01 6.058335e+01 1.316546e+02 6.939216e+01
#>  [811] 6.107330e+01 1.188953e+02 6.492978e+01 5.797319e+01 5.915007e+01
#>  [816] 7.444295e+01 6.565846e+01 1.683579e+02 1.185023e+02 5.468379e+01
#>  [821] 7.697298e+01 6.647432e+01 6.041322e+01 6.143110e+01 5.588342e+01
#>  [826] 6.653161e+01 5.895440e+01 6.033080e+01 6.999259e+01 7.965961e+01
#>  [831] 1.727895e+02 5.652383e+01 6.312373e+01 5.862704e+01 7.534045e+01
#>  [836] 5.544715e+01 1.906433e+02 1.829305e+02 6.147988e+01 5.907346e+01
#>  [841] 5.880142e+01 5.854923e+01 1.955445e+02 5.464116e+01 6.898252e+01
#>  [846] 6.069410e+01 6.471600e+01 1.255699e+03 1.688854e+02 8.053700e+02
#>  [851] 7.092916e+01 9.550161e+01 6.175733e+01 6.577634e+01 8.826208e+02
#>  [856] 1.105863e+02 6.024346e+01 8.343099e+01 5.601841e+01 6.410943e+01
#>  [861] 5.514488e+01 5.938934e+01 6.010621e+01 6.446030e+01 5.880918e+01
#>  [866] 8.279674e+01 6.977836e+01 7.677956e+01 6.498371e+01 7.713148e+01
#>  [871] 6.675661e+01 7.713082e+01 5.631486e+01 6.346762e+01 8.753269e+01
#>  [876] 5.911415e+01 6.717887e+01 5.471331e+01 5.561887e+01 5.706739e+01
#>  [881] 5.719559e+01 5.590682e+01 5.445050e+01 1.448166e+02 7.414206e+01
#>  [886] 6.426778e+01 6.480332e+01 5.883915e+01 5.865887e+01 6.248916e+01
#>  [891] 7.685807e+01 6.279603e+01 5.868552e+01 5.876789e+01 7.417583e+01
#>  [896] 5.910882e+01 7.711875e+01 5.630471e+01 5.934522e+01 5.933007e+01
#>  [901] 5.485768e+01 6.208700e+01 5.915861e+01 6.092617e+01 7.025653e+01
#>  [906] 6.209109e+01 5.690757e+01 3.151325e+02 2.326521e+02 6.081140e+01
#>  [911] 5.770881e+01 5.296315e+01 1.971573e+02 6.463449e+02 8.226523e+01
#>  [916] 5.432552e+01 6.122073e+01 1.514947e+02 7.880638e+01 7.197497e+01
#>  [921] 7.815578e+01 7.945547e+01 1.198838e+02 6.896849e+01 9.194934e+01
#>  [926] 1.514926e+02 5.937119e+01 5.180903e+01 6.565744e+01 5.146370e+01
#>  [931] 1.461003e+02 6.008550e+01 6.246496e+01 7.363280e+01 9.819303e+01
#>  [936] 7.670730e+01 9.351653e+01 8.141407e+01 5.403391e+01 7.059091e+01
#>  [941] 6.710870e+01 5.850140e+01 5.771157e+01 7.231574e+01 7.346997e+01
#>  [946] 6.762244e+01 6.906746e+01 5.616282e+01 9.687980e+01 1.281362e+02
#>  [951] 5.935507e+01 8.844433e+01 6.833965e+01 5.949656e+01 7.792746e+01
#>  [956] 6.455015e+01 7.690517e+01 6.200620e+01 6.419861e+01 6.238530e+01
#>  [961] 5.923685e+01 7.395730e+01 9.911716e+01 7.640536e+01 6.353919e+01
#>  [966] 5.926013e+01 7.875832e+01 6.081666e+01 7.639293e+01 6.139584e+01
#>  [971] 9.175822e+01 5.965924e+01 6.198126e+01 7.996002e+01 5.550716e+01
#>  [976] 6.528787e+01 6.069633e+01 5.778290e+01 5.648311e+01 6.079754e+01
#>  [981] 5.580398e+01 1.578491e+06 6.488371e+01 6.769614e+01 5.564455e+01
#>  [986] 7.131612e+01 7.260361e+01 5.810541e+01 5.835202e+01 6.265242e+01
#>  [991] 6.232850e+01 1.495447e+02 4.317202e+02 5.209487e+01 5.356705e+01
#>  [996] 6.396367e+01 6.710959e+01 5.140326e+01 5.084808e+01 7.102064e+01
```
