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
#>  [1] 40.16379 68.68869 68.68869 58.87801 58.87801 58.87801 58.87801 69.69956
#>  [9] 69.69956 69.69956 36.33106 31.74014 31.74014 68.80855 68.80855 68.80855
#> [17] 68.80855 68.80855 23.56527 16.02212

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
#>    [1] 5.704405e+01 6.104419e+01 6.387859e+01 8.092531e+01 5.191781e+01
#>    [6] 4.991896e+01 6.259169e+01 5.251833e+01 6.962837e+01 5.716571e+01
#>   [11] 5.830217e+01 7.636094e+01 9.362766e+01 5.307888e+01 1.246532e+02
#>   [16] 6.247528e+01 5.857617e+01 8.592076e+01 7.315292e+01 7.593237e+01
#>   [21] 7.277734e+01 7.185080e+01 5.521229e+01 5.800975e+01 6.312843e+01
#>   [26] 6.363861e+01 5.970519e+01 6.560222e+01 6.432689e+01 6.005694e+01
#>   [31] 5.757399e+01 6.093256e+01 5.801195e+01 6.516654e+01 5.946039e+01
#>   [36] 6.381687e+01 5.902138e+01 6.006718e+01 7.101357e+01 8.216096e+01
#>   [41] 6.721986e+01 5.251059e+01 6.552943e+01 5.723735e+01 9.571924e+01
#>   [46] 6.173873e+01 6.474475e+01 5.704315e+01 6.239828e+01 6.491967e+01
#>   [51] 6.229466e+01 8.932741e+01 8.936603e+01 9.499729e+01 1.491622e+02
#>   [56] 1.473127e+02 4.952099e+04 1.159920e+02 5.293061e+01 7.407739e+01
#>   [61] 7.880177e+01 5.951048e+01 5.945354e+01 6.990963e+01 5.720603e+01
#>   [66] 5.756810e+01 8.199366e+01 6.297466e+01 7.912225e+01 1.014615e+02
#>   [71] 7.750550e+01 6.009913e+01 5.895866e+01 5.739353e+01 6.558805e+01
#>   [76] 5.715699e+01 6.212968e+01 7.901482e+01 6.560827e+01 5.928716e+01
#>   [81] 6.019685e+01 6.178196e+01 5.908948e+01 6.221475e+01 5.798310e+01
#>   [86] 6.242212e+01 6.212685e+01 6.300355e+01 8.752594e+01 5.699880e+01
#>   [91] 6.242882e+01 5.315375e+01 7.407273e+01 5.365039e+01 5.292615e+01
#>   [96] 8.091876e+01 6.082236e+01 6.032498e+01 5.620531e+01 5.780926e+01
#>  [101] 5.791731e+01 1.044640e+02 5.811572e+01 6.036391e+01 5.792244e+01
#>  [106] 5.863522e+01 6.586943e+01 1.075400e+02 5.731529e+01 5.484042e+01
#>  [111] 5.899711e+01 2.685653e+02 6.424913e+01 7.699387e+01 6.074147e+01
#>  [116] 7.076087e+01 1.459473e+02 6.187547e+01 1.526573e+02 1.276521e+02
#>  [121] 3.335120e+02 9.457851e+01 1.260050e+02 8.095974e+01 1.489334e+02
#>  [126] 6.913799e+01 8.800588e+01 6.967149e+01 6.554569e+01 6.002208e+01
#>  [131] 7.980078e+01 6.632666e+01 5.522323e+03 1.216364e+02 4.867610e+01
#>  [136] 1.057558e+02 3.545483e+05 1.615178e+08 1.580973e+02 2.061598e+02
#>  [141] 6.149600e+01 8.869012e+01 9.530938e+01 1.992473e+02 1.410642e+04
#>  [146] 1.878241e+02 6.940205e+01 7.756473e+01 7.236006e+01 5.831978e+01
#>  [151] 5.276359e+01 7.329246e+01 6.729465e+01 6.046371e+01 5.626981e+01
#>  [156] 5.493935e+01 8.547847e+01 1.670560e+02 6.508486e+01 6.824959e+01
#>  [161] 7.945190e+01 7.766240e+01 8.820218e+01 1.069307e+02 8.103543e+01
#>  [166] 4.851185e+01 5.698727e+01 7.112019e+01 9.250374e+01 8.061181e+01
#>  [171] 7.801545e+01 1.104731e+02 5.085943e+01 5.475839e+01 5.873911e+01
#>  [176] 6.520589e+01 5.934733e+01 7.848318e+01 6.904226e+01 5.885071e+01
#>  [181] 5.745820e+01 6.203833e+01 7.161171e+01 2.300998e+02 1.102205e+02
#>  [186] 9.986194e+01 5.705750e+01 6.663084e+01 5.982072e+01 7.099069e+01
#>  [191] 1.610175e+02 1.581267e+02 1.471311e+02 8.181205e+01 5.670425e+01
#>  [196] 6.840803e+01 5.945309e+01 6.369139e+01 1.328776e+02 6.095392e+01
#>  [201] 6.369534e+01 6.170862e+01 6.262126e+01 6.262111e+01 6.525157e+01
#>  [206] 5.845693e+01 7.037554e+01 5.602537e+01 5.130587e+01 1.069060e+02
#>  [211] 6.917605e+01 6.882354e+01 9.885757e+01 9.536626e+01 6.359500e+01
#>  [216] 6.361098e+01 5.459208e+01 6.117303e+01 6.663872e+01 8.537177e+01
#>  [221] 8.725334e+01 7.694566e+01 9.420625e+01 7.208442e+01 7.240921e+01
#>  [226] 9.719887e+01 5.608417e+01 6.141887e+01 8.410722e+01 7.883039e+01
#>  [231] 6.136479e+01 5.920245e+01 5.458430e+01 6.914816e+01 6.289901e+01
#>  [236] 1.252188e+02 1.601581e+02 5.680043e+01 4.887088e+01 2.685137e+02
#>  [241] 5.420727e+01 5.393487e+01 8.266143e+01 7.332556e+01 6.498703e+01
#>  [246] 1.248944e+02 5.546507e+01 5.852261e+01 5.785815e+01 9.972354e+01
#>  [251] 5.707516e+01 7.604439e+01 7.183335e+01 8.009156e+01 6.717564e+01
#>  [256] 8.264793e+01 5.377337e+01 6.280994e+01 5.876287e+01 5.624504e+01
#>  [261] 5.774535e+01 6.563197e+01 5.608432e+01 6.255795e+01 6.336515e+01
#>  [266] 6.214227e+01 6.313718e+01 5.539338e+01 9.347844e+01 5.974041e+01
#>  [271] 6.068950e+01 7.486805e+01 6.244589e+01 1.382316e+02 5.571087e+01
#>  [276] 1.174061e+02 6.068436e+01 5.189973e+01 5.952926e+01 6.726248e+01
#>  [281] 5.714505e+01 1.021790e+02 6.956680e+01 7.773394e+01 1.067795e+02
#>  [286] 6.869565e+01 8.646434e+01 5.751181e+01 7.739145e+01 9.158071e+01
#>  [291] 7.447276e+01 6.387485e+01 6.386387e+01 1.542858e+02 6.678267e+01
#>  [296] 6.262240e+01 1.223083e+02 6.361591e+01 6.333193e+01 6.119581e+01
#>  [301] 5.908119e+01 6.708740e+01 6.285030e+01 5.606467e+01 5.806851e+01
#>  [306] 6.458999e+01 6.546859e+01 6.247069e+01 7.938431e+01 6.283012e+01
#>  [311] 5.985946e+01 6.888571e+01 6.100980e+01 1.189705e+02 7.807423e+01
#>  [316] 6.247003e+01 6.610728e+01 8.500006e+01 9.555552e+01 6.852362e+01
#>  [321] 5.873646e+01 6.232826e+01 6.573880e+01 7.814457e+01 8.864238e+01
#>  [326] 5.455267e+01 8.264879e+01 6.263329e+01 6.306075e+01 5.916618e+01
#>  [331] 4.043797e+02 7.241985e+01 7.015238e+01 6.518817e+01 5.924943e+01
#>  [336] 7.430314e+01 8.264887e+01 5.498476e+01 5.722382e+01 6.335882e+01
#>  [341] 5.566224e+01 7.025398e+01 5.329338e+01 1.559378e+02 5.754486e+01
#>  [346] 5.590480e+01 8.095134e+01 8.979347e+01 6.133655e+01 6.971887e+01
#>  [351] 6.509942e+01 6.338203e+01 7.475230e+01 6.158607e+01 6.339857e+01
#>  [356] 5.446072e+01 5.973871e+01 6.936813e+01 5.286846e+01 6.853739e+01
#>  [361] 8.050639e+01 6.857234e+01 8.919445e+01 9.754627e+01 5.669842e+01
#>  [366] 6.034204e+01 6.933109e+01 6.687350e+01 5.654913e+01 6.782551e+01
#>  [371] 6.130736e+01 1.783569e+02 6.002545e+01 6.834231e+01 6.863003e+01
#>  [376] 5.771333e+01 7.365407e+01 6.592268e+01 6.835067e+01 6.819945e+01
#>  [381] 5.950566e+01 7.036084e+01 5.731602e+01 8.137214e+01 1.129120e+02
#>  [386] 1.278687e+02 6.318505e+01 6.216417e+01 6.476296e+01 6.944845e+01
#>  [391] 5.718886e+01 2.330662e+02 5.896390e+01 6.069005e+01 5.892892e+01
#>  [396] 6.062151e+01 1.104194e+02 6.093941e+01 6.397859e+01 5.724546e+01
#>  [401] 6.531548e+01 6.270230e+01 6.524312e+01 7.052987e+01 5.789876e+01
#>  [406] 6.146045e+01 6.864503e+01 6.545497e+01 6.254189e+01 6.782798e+01
#>  [411] 5.986572e+01 5.911923e+01 6.825648e+01 6.654295e+01 6.957874e+01
#>  [416] 8.840345e+01 6.798681e+01 7.613958e+01 9.204898e+01 5.450012e+01
#>  [421] 7.728905e+01 6.230951e+01 7.511767e+01 7.092505e+01 6.629903e+01
#>  [426] 6.083234e+01 8.012475e+01 7.737815e+01 1.473712e+02 1.040726e+02
#>  [431] 5.739969e+01 6.689943e+01 8.127228e+01 5.916438e+01 7.964956e+01
#>  [436] 5.911721e+01 6.357792e+01 9.328499e+01 5.511770e+01 6.792396e+01
#>  [441] 8.267402e+01 8.019636e+01 7.045827e+01 5.876444e+01 5.877970e+01
#>  [446] 5.957955e+01 1.804834e+02 8.195731e+01 5.741200e+01 5.684405e+01
#>  [451] 6.786759e+01 5.503289e+01 6.088273e+01 1.201249e+02 5.945906e+01
#>  [456] 6.390321e+01 5.832803e+01 6.668673e+01 1.012359e+02 1.010554e+02
#>  [461] 6.829354e+01 6.961887e+01 6.463499e+01 6.706451e+01 1.087568e+02
#>  [466] 6.255675e+01 5.394420e+01 5.884319e+01 5.896595e+01 6.409255e+01
#>  [471] 7.139965e+01 5.746326e+01 1.662171e+02 9.414052e+01 7.918902e+01
#>  [476] 1.528077e+02 4.434422e+02 6.593057e+01 5.310118e+01 5.345312e+01
#>  [481] 6.988331e+01 7.166839e+01 6.268161e+01 2.089492e+02 3.950263e+02
#>  [486] 6.161232e+01 7.511191e+01 7.025775e+01 5.494859e+01 9.729453e+01
#>  [491] 6.243620e+01 5.627427e+01 6.701498e+01 6.815957e+01 6.555666e+01
#>  [496] 1.461417e+02 6.578554e+01 6.903051e+01 6.762775e+01 6.176873e+01
#>  [501] 6.338223e+01 8.255724e+01 5.424014e+01 5.129171e+01 6.645149e+01
#>  [506] 4.217412e+02 2.246340e+04 2.876478e+02 9.573136e+01 1.037807e+02
#>  [511] 6.467091e+01 6.307857e+01 6.481207e+01 8.937540e+01 5.750215e+01
#>  [516] 6.971122e+01 5.648268e+01 6.124893e+01 6.665435e+01 6.609918e+01
#>  [521] 8.942430e+01 9.281304e+01 6.311965e+01 1.819173e+02 5.535366e+01
#>  [526] 6.425982e+01 5.920770e+01 6.181381e+01 1.036229e+02 6.402564e+01
#>  [531] 7.563506e+01 7.709423e+01 5.576884e+01 7.694345e+01 7.959168e+01
#>  [536] 6.255697e+01 6.751125e+01 5.920928e+01 6.143442e+01 6.123676e+01
#>  [541] 1.634103e+02 9.669016e+01 8.062793e+01 5.862238e+01 9.694736e+01
#>  [546] 7.177152e+01 2.571959e+04 1.662129e+03 2.724375e+03 6.750359e+01
#>  [551] 5.997394e+01 5.761416e+01 5.775967e+01 6.714925e+01 6.694974e+01
#>  [556] 6.335468e+01 5.502290e+01 5.626282e+01 7.013068e+01 6.456893e+01
#>  [561] 6.293397e+01 1.103139e+02 6.104612e+01 7.058067e+01 6.252054e+01
#>  [566] 6.629083e+01 5.605435e+01 5.585330e+01 1.520588e+02 7.467749e+01
#>  [571] 8.684841e+01 5.392765e+01 1.193598e+02 5.797138e+01 6.449423e+01
#>  [576] 7.341225e+01 6.186199e+01 7.841099e+01 5.577059e+01 6.269895e+01
#>  [581] 6.243839e+01 5.977756e+01 7.124345e+01 5.790603e+01 6.400254e+01
#>  [586] 5.704375e+01 7.283996e+01 6.496207e+01 6.989082e+01 7.601093e+01
#>  [591] 1.141288e+02 5.567997e+01 4.847246e+02 5.752005e+02 1.088475e+02
#>  [596] 6.240421e+01 1.535867e+02 1.561911e+02 6.642406e+01 3.726164e+02
#>  [601] 5.295258e+01 6.846615e+01 6.582139e+01 8.932643e+01 1.521916e+02
#>  [606] 2.135640e+02 1.792136e+02 6.574170e+01 6.053750e+01 7.442498e+01
#>  [611] 5.952081e+01 7.920764e+01 7.833820e+01 6.285766e+01 5.927208e+01
#>  [616] 5.802420e+01 9.047469e+01 6.555314e+01 6.109176e+01 5.756478e+01
#>  [621] 5.802656e+01 6.286331e+01 5.957169e+01 8.265610e+01 1.272876e+02
#>  [626] 8.820942e+01 7.205167e+01 6.024870e+01 7.171901e+01 9.740451e+01
#>  [631] 6.184926e+01 8.476450e+01 5.442322e+01 1.061036e+02 5.485982e+01
#>  [636] 6.961170e+01 5.994226e+01 5.852415e+01 5.941496e+01 8.193666e+01
#>  [641] 3.655375e+03 9.927080e+01 6.120980e+01 5.568750e+01 5.854953e+01
#>  [646] 7.582889e+01 5.855291e+01 5.001041e+01 7.265025e+01 6.351767e+01
#>  [651] 5.781188e+01 6.721114e+01 5.865997e+01 1.773836e+02 4.909247e+02
#>  [656] 6.072989e+01 1.435285e+02 6.065920e+01 6.092791e+01 6.608024e+01
#>  [661] 5.610141e+01 6.839874e+01 7.027840e+01 7.331600e+01 5.523533e+01
#>  [666] 4.548672e+01 5.370588e+01 1.844118e+02 5.848332e+01 1.121775e+02
#>  [671] 2.575262e+02 6.798278e+01 5.958271e+01 8.912355e+01 7.203443e+01
#>  [676] 7.218693e+01 7.072918e+01 5.552337e+01 5.615907e+01 5.241321e+01
#>  [681] 5.382567e+01 6.127684e+01 6.645632e+01 6.452897e+01 8.600181e+01
#>  [686] 1.164015e+02 1.326062e+02 1.025520e+02 6.208148e+01 6.390063e+01
#>  [691] 1.174489e+02 1.633123e+02 7.230600e+01 7.521462e+01 7.285906e+01
#>  [696] 5.918718e+01 7.786829e+01 7.760869e+01 5.413508e+01 7.790623e+01
#>  [701] 8.208006e+01 6.990216e+01 6.098944e+01 6.322301e+01 6.179917e+01
#>  [706] 6.211865e+01 8.946385e+01 6.096051e+01 8.245306e+01 1.491953e+02
#>  [711] 2.235168e+02 7.779723e+01 7.591733e+01 5.918911e+01 5.896825e+01
#>  [716] 6.125166e+01 5.942743e+01 6.304487e+01 7.117752e+01 7.570137e+01
#>  [721] 5.106114e+01 6.040246e+01 7.694736e+01 3.100290e+02 3.704185e+02
#>  [726] 5.351964e+01 5.991668e+01 7.628206e+01 8.357745e+01 7.344765e+01
#>  [731] 8.478641e+01 6.229753e+01 6.374754e+01 6.023746e+01 5.997919e+01
#>  [736] 1.321548e+02 7.250424e+01 6.715867e+01 6.287808e+01 6.044982e+01
#>  [741] 6.578564e+01 8.324439e+01 5.878011e+01 5.871666e+01 7.318502e+01
#>  [746] 6.034007e+01 6.652887e+01 6.119095e+01 7.718745e+01 6.041634e+01
#>  [751] 8.769650e+01 7.804884e+01 5.933505e+01 5.441523e+01 9.225181e+01
#>  [756] 1.363541e+02 7.821632e+01 6.309138e+01 8.744180e+01 6.812972e+01
#>  [761] 9.500772e+01 6.086307e+01 6.865781e+01 6.425718e+01 7.812548e+01
#>  [766] 5.747124e+01 6.619323e+01 9.781402e+01 7.148868e+01 5.752612e+01
#>  [771] 6.062714e+01 5.545670e+01 6.527209e+01 8.991931e+01 5.709354e+01
#>  [776] 6.232948e+01 5.588718e+01 5.717683e+01 6.672772e+01 6.391096e+01
#>  [781] 7.520270e+01 8.441034e+01 6.907548e+01 6.622279e+01 6.049867e+01
#>  [786] 6.025525e+01 5.257035e+01 1.152722e+02 6.476765e+01 6.350788e+01
#>  [791] 8.284185e+01 5.363843e+01 6.425344e+01 7.896494e+01 5.910696e+01
#>  [796] 6.407491e+01 5.813470e+01 6.248171e+01 5.801444e+01 6.052540e+01
#>  [801] 1.008325e+02 5.309072e+02 6.095712e+01 5.195926e+01 6.325241e+01
#>  [806] 2.128506e+02 7.839422e+01 1.463669e+02 7.778113e+01 6.421784e+01
#>  [811] 6.971301e+01 5.880499e+01 8.333099e+01 6.530161e+01 6.380250e+01
#>  [816] 1.989802e+02 4.882331e+02 7.061636e+01 8.674465e+01 7.768663e+01
#>  [821] 6.557614e+01 6.389484e+01 5.708842e+01 6.130502e+01 7.906564e+01
#>  [826] 9.114831e+01 5.742443e+01 7.430189e+01 6.464183e+01 5.846220e+01
#>  [831] 7.515767e+01 8.229466e+01 5.565593e+01 6.148412e+01 5.568742e+01
#>  [836] 6.459873e+01 6.196645e+01 5.897818e+01 5.619584e+01 7.152891e+01
#>  [841] 7.389387e+01 7.349739e+01 5.538727e+01 6.666779e+01 5.053741e+01
#>  [846] 7.220755e+01 6.845736e+01 6.128783e+01 6.192172e+01 6.011496e+01
#>  [851] 6.450834e+01 6.682423e+01 5.648650e+01 5.757876e+01 1.072374e+02
#>  [856] 8.788749e+01 5.658863e+01 6.221733e+01 6.814174e+01 6.316231e+01
#>  [861] 5.530694e+01 5.569076e+01 5.575213e+01 7.232324e+01 2.209255e+02
#>  [866] 6.413093e+01 6.431985e+01 6.747391e+01 8.195280e+01 5.530798e+01
#>  [871] 1.906340e+02 9.998094e+01 7.759406e+01 1.195551e+02 7.401372e+01
#>  [876] 6.512173e+01 6.445834e+01 6.265065e+01 7.617222e+01 6.128199e+01
#>  [881] 5.716800e+01 3.301165e+02 7.170695e+01 6.235592e+01 9.379780e+01
#>  [886] 6.090077e+01 6.033722e+01 5.823630e+01 8.588637e+01 7.351608e+01
#>  [891] 2.701161e+02 6.293921e+01 1.164370e+02 5.502959e+01 5.990662e+01
#>  [896] 5.716808e+01 7.963099e+01 1.369057e+02 2.208862e+02 5.969872e+01
#>  [901] 7.991644e+01 1.326444e+02 7.882142e+01 5.374448e+01 5.195449e+01
#>  [906] 5.724621e+01 1.386386e+02 2.214940e+02 4.666183e+01 7.038135e+01
#>  [911] 5.589839e+01 2.200998e+02 7.928631e+01 9.334191e+01 8.559635e+01
#>  [916] 5.855455e+01 5.672755e+01 8.364900e+01 6.312230e+01 6.317851e+01
#>  [921] 1.131167e+02 9.031251e+01 7.138330e+01 7.286240e+01 5.816283e+01
#>  [926] 6.258126e+01 6.405187e+01 6.189429e+01 6.940343e+01 5.888957e+01
#>  [931] 7.081014e+01 5.661061e+01 6.441246e+01 5.881282e+01 9.752295e+01
#>  [936] 6.331992e+01 7.312387e+01 6.346576e+01 5.375885e+01 1.009394e+02
#>  [941] 5.506615e+01 8.347884e+01 6.378497e+01 6.323950e+01 7.221079e+01
#>  [946] 8.660009e+01 8.460461e+01 6.128986e+01 6.597541e+01 5.708916e+01
#>  [951] 5.639759e+01 6.983173e+01 9.595862e+01 8.256128e+01 9.288520e+01
#>  [956] 5.807114e+01 5.435913e+01 6.150770e+01 6.041019e+01 1.180987e+02
#>  [961] 9.653217e+01 1.212532e+02 8.544960e+01 6.013898e+01 7.638836e+01
#>  [966] 3.001601e+02 7.352147e+01 7.079652e+01 5.787246e+01 6.207042e+01
#>  [971] 6.634556e+01 8.467649e+01 2.416195e+02 1.358075e+02 9.429868e+01
#>  [976] 6.024547e+01 5.896364e+01 5.337404e+01 8.673095e+01 3.397703e+02
#>  [981] 1.594718e+02 6.019215e+01 6.171464e+01 5.162368e+01 1.073377e+02
#>  [986] 5.642456e+01 1.921299e+02 5.681672e+01 6.258653e+01 5.800868e+01
#>  [991] 6.246546e+01 6.134131e+01 6.733550e+01 5.897915e+01 5.375560e+01
#>  [996] 7.332373e+01 1.478356e+02 6.561417e+01 6.464691e+01 6.108029e+01
```
