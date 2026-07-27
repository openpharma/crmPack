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
#>  [1]  43.76416 140.80817  20.72080  32.95490  32.95490  32.95490  60.92490
#>  [8]  60.92490  60.92490  60.92490  42.50433  42.50433  42.50433  42.50433
#> [15] 330.48014  17.15921  48.18321  48.18321  21.91102  21.91102

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
#>    [1] 1.055037e+02 5.925296e+01 6.172603e+01 1.964294e+02 8.661884e+01
#>    [6] 7.675853e+01 6.239001e+02 4.940781e+03 2.975673e+03 1.005052e+02
#>   [11] 6.674755e+01 6.289753e+01 5.321075e+01 5.747487e+01 1.031186e+02
#>   [16] 7.500286e+01 7.588850e+01 1.003924e+02 7.825653e+01 5.711722e+01
#>   [21] 4.979366e+01 6.668553e+01 6.982260e+01 4.154629e+02 6.935521e+01
#>   [26] 7.261453e+01 7.067166e+01 8.098489e+01 5.885386e+01 2.139306e+02
#>   [31] 3.880556e+02 1.238804e+02 3.579230e+02 1.351585e+02 5.491839e+01
#>   [36] 6.438261e+01 7.897362e+01 5.237329e+01 5.466871e+01 7.259877e+01
#>   [41] 1.204746e+02 5.969541e+01 7.221202e+01 5.317401e+01 5.144180e+01
#>   [46] 6.823377e+02 6.157563e+01 6.444870e+01 6.436009e+01 5.614184e+01
#>   [51] 6.428182e+01 6.112433e+01 6.617352e+01 8.240875e+01 6.444492e+01
#>   [56] 6.547115e+01 7.068630e+01 5.967930e+01 6.992212e+01 5.851547e+01
#>   [61] 6.416425e+01 1.030452e+02 6.764963e+01 7.136390e+01 6.060079e+01
#>   [66] 8.458579e+01 1.515962e+02 5.712217e+01 9.158433e+01 9.395478e+01
#>   [71] 5.975748e+01 6.493662e+01 5.895322e+01 7.395984e+01 1.396804e+02
#>   [76] 1.060198e+02 7.698559e+01 5.647368e+01 4.972399e+04 1.618185e+02
#>   [81] 6.613609e+01 6.131340e+01 5.741851e+01 5.820685e+01 1.222001e+02
#>   [86] 5.569500e+01 6.192339e+01 6.339476e+01 5.654128e+01 6.426773e+01
#>   [91] 5.963876e+01 5.865369e+01 7.575112e+01 1.296761e+03 5.951169e+01
#>   [96] 5.839719e+01 8.990280e+01 6.193318e+01 8.099084e+01 6.485227e+01
#>  [101] 1.085659e+02 5.764338e+01 7.762449e+01 6.031154e+01 8.070528e+01
#>  [106] 5.946145e+01 6.023254e+01 6.611135e+01 5.639984e+01 8.882563e+01
#>  [111] 5.942853e+01 6.085271e+01 9.044213e+01 5.913335e+01 7.973378e+01
#>  [116] 5.600143e+01 5.956730e+01 5.947993e+01 7.280874e+01 1.084974e+02
#>  [121] 6.310630e+01 6.294492e+01 6.037169e+01 6.880891e+01 5.839161e+01
#>  [126] 7.779255e+01 6.732096e+01 1.205571e+02 1.462866e+02 1.075258e+02
#>  [131] 9.515575e+01 6.502044e+01 5.668130e+01 7.209589e+01 7.881103e+01
#>  [136] 6.965500e+01 1.774421e+02 6.125633e+01 6.087767e+01 6.492131e+01
#>  [141] 6.204039e+01 4.954118e+01 1.312687e+02 8.620558e+01 1.739008e+02
#>  [146] 5.533255e+01 8.422113e+01 6.973681e+01 1.795969e+02 5.763767e+01
#>  [151] 5.343770e+01 1.037481e+02 5.910171e+01 6.987368e+01 7.268307e+01
#>  [156] 6.248848e+01 7.636436e+01 1.059675e+02 6.999361e+01 5.432990e+01
#>  [161] 6.935317e+01 5.653758e+01 6.737300e+01 5.805842e+01 5.640761e+01
#>  [166] 6.224352e+01 1.157493e+02 6.685059e+01 5.330351e+01 7.281653e+01
#>  [171] 1.839068e+02 6.753906e+01 5.780256e+01 7.589567e+01 5.228064e+01
#>  [176] 5.816092e+01 5.861208e+01 6.107959e+01 9.228016e+01 5.769752e+01
#>  [181] 5.416487e+01 1.363332e+02 6.871537e+01 7.289334e+01 5.884993e+01
#>  [186] 7.605209e+01 8.907865e+01 1.082166e+02 5.980914e+01 5.656016e+01
#>  [191] 5.600161e+01 6.709611e+01 5.966188e+01 8.606374e+01 6.059386e+01
#>  [196] 5.486845e+01 1.895158e+02 6.818722e+01 4.361898e+02 5.485213e+01
#>  [201] 6.217086e+01 6.262532e+01 6.362849e+01 1.372591e+02 7.217796e+01
#>  [206] 8.369953e+01 5.294510e+01 4.956652e+01 5.186316e+01 9.117189e+01
#>  [211] 8.349164e+01 6.211803e+01 7.726943e+01 5.575847e+01 9.345783e+01
#>  [216] 6.399414e+01 6.798361e+01 5.608194e+01 5.550976e+01 8.427328e+01
#>  [221] 6.475569e+01 6.502332e+01 9.998048e+01 5.850988e+01 6.423719e+01
#>  [226] 5.471842e+01 6.469392e+01 6.037562e+01 8.233588e+01 7.659762e+01
#>  [231] 7.381710e+01 6.422725e+01 7.849170e+01 8.904244e+01 6.335487e+01
#>  [236] 5.846656e+01 7.182070e+01 5.692141e+01 6.389651e+01 1.415844e+03
#>  [241] 5.765965e+01 5.533220e+01 6.560586e+01 5.506428e+01 6.270053e+01
#>  [246] 6.037048e+01 1.723546e+02 5.701548e+01 6.370405e+01 6.048840e+01
#>  [251] 6.905888e+01 7.142409e+01 6.076878e+01 9.152174e+01 3.938510e+02
#>  [256] 2.504730e+03 7.760080e+01 5.865806e+01 6.542053e+01 5.985291e+01
#>  [261] 1.080737e+02 7.261545e+01 8.152350e+01 6.878066e+01 6.213343e+01
#>  [266] 5.346338e+01 5.837382e+01 9.026129e+01 5.833280e+01 6.677598e+01
#>  [271] 6.478277e+01 9.092300e+01 7.379990e+01 8.945628e+01 5.763270e+01
#>  [276] 6.245969e+01 9.640423e+01 6.771692e+01 1.025519e+02 6.387760e+01
#>  [281] 2.259443e+02 6.311709e+01 7.569385e+01 7.266339e+01 6.034226e+01
#>  [286] 1.115583e+02 5.703914e+01 6.720721e+01 5.414530e+01 6.695831e+01
#>  [291] 6.181428e+01 5.856808e+01 6.461903e+01 5.491923e+01 4.121191e+02
#>  [296] 7.145840e+01 9.022388e+01 2.293970e+02 6.052870e+01 6.015299e+01
#>  [301] 7.608571e+01 6.184828e+01 1.198167e+02 9.137752e+01 5.785848e+01
#>  [306] 5.862259e+01 7.934549e+01 1.143301e+02 7.293278e+01 5.274557e+01
#>  [311] 2.477776e+02 8.828003e+01 1.589342e+02 6.417938e+01 6.461935e+01
#>  [316] 5.700826e+01 9.563470e+01 8.150234e+01 6.664288e+01 1.439191e+02
#>  [321] 6.490091e+01 6.821337e+01 7.700101e+02 4.264923e+02 2.920860e+02
#>  [326] 7.887871e+01 5.826040e+01 1.394577e+02 1.211455e+02 5.657044e+01
#>  [331] 1.037473e+02 8.048856e+01 7.064939e+01 7.166901e+01 6.744349e+01
#>  [336] 7.118638e+01 6.987122e+01 5.957794e+01 1.092092e+02 5.430688e+01
#>  [341] 6.355066e+01 5.753298e+01 7.204966e+01 6.172048e+01 7.270881e+01
#>  [346] 8.276030e+01 7.749268e+01 8.605930e+01 5.559262e+01 5.529605e+01
#>  [351] 6.871742e+01 9.660718e+01 6.379449e+01 5.873722e+01 5.455308e+01
#>  [356] 5.954443e+01 6.102570e+01 6.017557e+01 1.409574e+02 6.206959e+02
#>  [361] 5.555223e+01 7.081251e+01 1.910310e+02 1.105130e+02 5.705996e+01
#>  [366] 6.822971e+01 9.751977e+01 6.471862e+01 6.627214e+01 5.456995e+01
#>  [371] 6.932212e+01 6.413659e+01 6.524542e+01 7.169762e+01 6.590713e+01
#>  [376] 1.449078e+02 3.646655e+02 8.736914e+01 5.719515e+01 9.491190e+01
#>  [381] 6.825572e+01 6.378652e+01 1.026194e+02 1.093622e+02 9.810321e+01
#>  [386] 1.002609e+02 7.551289e+01 6.348067e+01 5.770452e+01 6.059007e+01
#>  [391] 5.886066e+01 6.669693e+01 6.387219e+01 8.136058e+01 1.077613e+02
#>  [396] 5.659673e+01 1.626793e+02 6.199521e+01 9.824013e+01 7.455414e+03
#>  [401] 5.302343e+01 6.608901e+01 5.521098e+01 9.297176e+01 7.832935e+01
#>  [406] 5.725494e+01 5.695254e+01 5.625551e+01 6.880207e+01 6.156335e+01
#>  [411] 5.838763e+01 5.887782e+01 6.233788e+01 7.288902e+01 5.668514e+01
#>  [416] 4.781473e+04 1.083716e+03 5.595001e+01 2.454735e+02 8.302644e+01
#>  [421] 7.285513e+01 5.635309e+01 7.283941e+01 1.250532e+02 6.994499e+01
#>  [426] 5.648230e+01 5.568548e+01 6.923467e+01 6.520141e+01 5.466001e+01
#>  [431] 5.976525e+01 7.192940e+01 1.589573e+02 7.110348e+01 7.351256e+01
#>  [436] 7.688722e+01 5.627559e+01 6.445613e+01 5.846456e+01 5.915769e+01
#>  [441] 6.430683e+01 6.820393e+01 5.838534e+01 9.609833e+01 7.796324e+01
#>  [446] 9.363870e+01 5.495673e+01 5.374303e+01 6.536672e+01 6.767086e+01
#>  [451] 9.492062e+01 6.395476e+01 9.392294e+01 5.323289e+01 6.138781e+01
#>  [456] 6.604457e+01 6.756542e+01 5.389922e+01 7.283917e+01 5.839724e+01
#>  [461] 6.450481e+01 4.986867e+01 6.040824e+01 5.823430e+01 6.229661e+01
#>  [466] 6.195505e+01 5.689319e+01 7.955736e+01 6.943053e+01 7.169344e+01
#>  [471] 6.783603e+01 6.287380e+01 9.548212e+01 5.504528e+01 7.076892e+01
#>  [476] 5.916891e+01 6.099781e+01 6.222579e+01 6.173379e+01 7.583603e+01
#>  [481] 6.669728e+01 5.195100e+01 5.339844e+01 1.029390e+02 5.450526e+01
#>  [486] 5.532123e+01 7.647131e+01 6.926352e+01 7.199964e+01 6.675055e+01
#>  [491] 6.211415e+01 6.549831e+01 6.294698e+01 6.135521e+01 8.620586e+01
#>  [496] 8.347591e+01 8.162655e+01 1.010985e+02 6.302701e+01 6.123217e+01
#>  [501] 5.465506e+01 8.432709e+01 6.967608e+01 9.093346e+01 5.916351e+01
#>  [506] 6.823580e+01 6.426893e+01 8.058856e+01 7.295030e+01 6.498202e+01
#>  [511] 5.857883e+01 7.783847e+01 6.602158e+01 8.818349e+01 9.833126e+01
#>  [516] 5.785869e+01 6.050235e+01 7.094761e+01 5.617529e+01 7.329053e+01
#>  [521] 6.011803e+01 5.946379e+01 6.285270e+01 6.647769e+01 8.220858e+01
#>  [526] 5.803023e+01 6.239897e+01 9.364400e+01 5.745195e+01 5.252985e+01
#>  [531] 2.771011e+02 6.655692e+01 6.062548e+01 7.365573e+01 1.096647e+02
#>  [536] 2.115643e+02 6.064774e+01 1.001460e+02 8.360529e+01 6.416205e+01
#>  [541] 9.926296e+01 5.761705e+01 6.136153e+01 5.476347e+01 8.851556e+01
#>  [546] 6.226491e+01 6.849142e+01 6.583143e+01 9.957489e+01 5.934732e+01
#>  [551] 6.324984e+01 5.488131e+01 6.329059e+01 5.944829e+01 1.142375e+02
#>  [556] 2.167025e+02 9.604549e+01 8.440806e+01 6.139810e+01 6.720906e+01
#>  [561] 9.858395e+01 7.148521e+01 5.583085e+01 5.606303e+01 6.436893e+01
#>  [566] 5.877622e+01 1.690794e+02 5.553468e+01 6.618633e+01 5.660323e+01
#>  [571] 6.327334e+01 5.696095e+01 5.817230e+01 6.790187e+01 5.570078e+01
#>  [576] 7.985561e+01 7.698617e+01 4.997546e+01 5.973998e+01 8.603328e+01
#>  [581] 7.082716e+01 6.396538e+01 5.390453e+01 6.379699e+01 8.076787e+01
#>  [586] 6.387727e+01 7.704986e+01 6.105102e+01 5.623834e+01 5.524486e+01
#>  [591] 7.030814e+01 5.673776e+01 5.830716e+01 8.889778e+01 6.597960e+01
#>  [596] 7.681038e+01 5.950365e+01 1.898075e+02 5.075408e+01 6.484855e+01
#>  [601] 5.782654e+01 5.544757e+01 6.266252e+01 7.942627e+01 5.775328e+01
#>  [606] 5.683227e+01 5.659025e+01 6.870797e+01 5.949202e+01 6.813425e+01
#>  [611] 5.650584e+01 7.205647e+01 6.330137e+01 6.174383e+01 5.231996e+01
#>  [616] 8.642913e+01 7.803900e+01 7.832231e+01 1.054092e+02 6.023922e+01
#>  [621] 5.887647e+01 6.448653e+01 6.796535e+01 6.159211e+01 6.123915e+01
#>  [626] 8.174810e+01 6.573920e+01 5.601710e+01 6.090051e+01 5.734282e+01
#>  [631] 4.314683e+02 1.795985e+02 7.564897e+01 2.001284e+02 5.565843e+01
#>  [636] 5.730583e+01 6.966345e+01 7.478842e+01 8.731125e+01 6.438418e+01
#>  [641] 7.183065e+01 6.489911e+01 6.649391e+01 5.870075e+01 5.206191e+01
#>  [646] 1.455369e+03 1.348504e+02 9.479990e+01 8.540882e+01 1.152644e+02
#>  [651] 6.201953e+01 6.270370e+01 7.474980e+01 8.249722e+01 5.443484e+01
#>  [656] 5.536493e+01 7.707428e+01 6.635468e+01 9.868881e+01 5.968181e+01
#>  [661] 5.665595e+01 7.613391e+01 1.068548e+02 5.432715e+01 6.030760e+01
#>  [666] 6.131847e+01 5.912462e+01 7.139515e+01 5.636838e+01 6.664379e+01
#>  [671] 7.150447e+01 5.951321e+01 6.213699e+01 6.820396e+01 5.829526e+01
#>  [676] 6.050388e+01 7.897248e+01 5.388536e+01 6.825798e+01 6.545237e+01
#>  [681] 5.856456e+01 6.373585e+01 6.644484e+01 7.165585e+01 5.809728e+01
#>  [686] 7.648285e+01 2.854856e+06 7.448487e+01 3.115089e+02 4.195960e+02
#>  [691] 5.836677e+01 6.604393e+01 5.981773e+01 6.136171e+01 7.308940e+01
#>  [696] 5.697638e+01 1.629690e+02 5.410371e+01 5.067042e+01 7.431611e+01
#>  [701] 5.939230e+01 6.681520e+01 6.293507e+01 6.513578e+01 4.837664e+01
#>  [706] 1.747733e+02 9.082048e+01 6.131625e+01 8.179343e+01 7.089036e+01
#>  [711] 7.153879e+01 5.753403e+01 1.284369e+02 7.940470e+01 9.450755e+01
#>  [716] 2.945833e+02 6.700780e+01 1.173956e+02 5.092494e+01 7.073764e+01
#>  [721] 5.563400e+01 6.168828e+01 7.276601e+01 7.804607e+01 6.240557e+01
#>  [726] 6.076130e+01 6.725977e+01 6.001427e+01 6.421968e+01 6.717164e+01
#>  [731] 6.122192e+01 6.634470e+01 5.603376e+01 6.978609e+01 1.290607e+02
#>  [736] 6.719520e+01 8.610668e+01 5.660720e+01 5.816334e+01 6.119361e+01
#>  [741] 7.263312e+01 1.520491e+02 5.439601e+01 6.440124e+01 2.306075e+02
#>  [746] 1.168734e+02 1.018389e+02 6.321324e+01 1.313817e+02 9.975358e+01
#>  [751] 5.390108e+01 5.555142e+02 5.504139e+01 7.069696e+01 7.321090e+01
#>  [756] 8.358204e+01 7.029374e+01 7.455095e+01 5.192429e+02 9.366042e+02
#>  [761] 7.968377e+01 5.860699e+01 6.919228e+01 6.046766e+01 6.594579e+01
#>  [766] 8.991967e+01 6.822753e+01 7.282085e+01 5.843521e+01 5.841616e+01
#>  [771] 6.136143e+01 6.199489e+01 5.583838e+01 5.956912e+01 5.459640e+01
#>  [776] 6.146456e+01 4.959114e+02 5.145047e+01 5.271760e+01 8.752103e+01
#>  [781] 6.124940e+01 5.808905e+01 5.910231e+01 9.121988e+01 1.092807e+02
#>  [786] 1.355951e+02 6.290927e+01 6.070756e+01 5.917976e+01 6.403134e+01
#>  [791] 8.466022e+01 5.398228e+01 5.482339e+01 7.143530e+01 6.436681e+01
#>  [796] 5.448780e+01 9.328027e+02 5.755311e+01 6.007158e+01 7.273230e+01
#>  [801] 6.827623e+01 4.422087e+02 6.490407e+01 1.343506e+02 8.686520e+01
#>  [806] 6.173968e+01 4.564012e+02 6.788122e+01 7.183213e+01 6.252080e+01
#>  [811] 6.459361e+01 1.524876e+02 5.458046e+01 7.053137e+01 5.656232e+02
#>  [816] 5.807227e+01 1.140586e+02 5.565040e+01 1.061132e+02 2.333994e+02
#>  [821] 1.181297e+02 6.691666e+01 1.335532e+02 9.624272e+01 7.604579e+01
#>  [826] 1.057902e+02 6.315033e+01 7.534280e+01 7.434360e+01 8.577022e+01
#>  [831] 6.183732e+01 9.717209e+01 6.437096e+01 6.200364e+01 6.681548e+01
#>  [836] 1.860645e+02 9.456892e+01 6.477429e+01 6.855076e+01 6.080547e+01
#>  [841] 7.324579e+01 6.098514e+01 6.054872e+01 4.571935e+02 1.080985e+02
#>  [846] 4.912851e+03 1.680912e+02 6.576171e+01 6.396361e+01 1.069416e+02
#>  [851] 5.382307e+01 5.857525e+01 6.598056e+01 6.743587e+01 1.201038e+02
#>  [856] 1.551497e+02 3.389301e+02 7.014477e+01 2.107802e+02 3.491116e+02
#>  [861] 9.115114e+01 1.038798e+02 1.008127e+02 7.802380e+01 5.631785e+01
#>  [866] 6.496939e+01 9.842008e+01 6.045270e+01 5.571980e+01 2.206129e+02
#>  [871] 5.924994e+01 5.934324e+01 5.979919e+01 6.174139e+01 8.026627e+01
#>  [876] 5.748408e+01 8.026753e+01 6.768239e+01 5.830838e+01 5.653651e+01
#>  [881] 8.863421e+01 1.701109e+02 1.618119e+02 9.373795e+01 6.034569e+01
#>  [886] 6.161057e+01 5.042455e+01 7.342128e+01 8.477924e+01 6.541030e+01
#>  [891] 5.280546e+01 5.195602e+01 6.448208e+01 7.311366e+01 6.368226e+01
#>  [896] 5.919208e+01 5.776612e+01 6.128033e+01 5.742013e+01 6.098029e+01
#>  [901] 5.994418e+01 6.023023e+01 7.298881e+01 9.488917e+01 6.101894e+01
#>  [906] 6.176262e+01 6.928388e+01 5.263267e+01 1.257481e+02 1.070031e+02
#>  [911] 6.864607e+01 9.444713e+01 6.114851e+01 6.384908e+01 5.475669e+01
#>  [916] 6.552232e+01 1.440345e+02 8.521830e+01 1.000676e+02 1.048583e+02
#>  [921] 5.068197e+01 2.801981e+02 7.868882e+01 6.222615e+01 6.128330e+01
#>  [926] 5.751952e+01 9.822456e+01 7.537128e+01 6.920702e+01 1.247250e+02
#>  [931] 6.200572e+01 1.230181e+02 8.052461e+01 6.410222e+01 6.229744e+01
#>  [936] 6.533012e+01 9.932956e+01 5.885735e+01 5.855450e+01 5.739124e+01
#>  [941] 5.883156e+01 5.897490e+01 5.139127e+01 5.310968e+01 6.201150e+01
#>  [946] 6.402210e+01 5.737321e+01 6.182998e+01 6.419960e+01 7.548724e+01
#>  [951] 6.527492e+01 6.197242e+01 6.131213e+01 6.439115e+01 8.147953e+01
#>  [956] 5.547391e+01 6.360448e+01 6.412397e+01 6.414544e+01 8.282530e+01
#>  [961] 5.799756e+01 6.047675e+01 6.078625e+01 5.678798e+01 9.246783e+01
#>  [966] 1.006398e+02 6.007983e+01 6.460105e+01 1.020542e+02 1.052681e+02
#>  [971] 6.093803e+01 1.010581e+02 2.358424e+02 6.070565e+01 6.800343e+01
#>  [976] 5.751261e+01 8.753419e+01 6.190445e+01 6.149985e+01 7.281896e+02
#>  [981] 6.193093e+01 6.475620e+01 5.848441e+01 6.088797e+01 5.889121e+01
#>  [986] 5.704040e+01 6.885314e+01 8.052320e+01 7.180057e+01 6.000230e+01
#>  [991] 5.583003e+01 1.652449e+02 2.221877e+02 5.942769e+01 7.965372e+01
#>  [996] 7.465964e+01 2.422419e+02 4.870539e+01 6.169461e+01 5.719832e+01
```
