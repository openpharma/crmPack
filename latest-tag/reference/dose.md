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
#>  [1] 90.372482 90.372482 90.372482 90.372482 52.852876 19.482431 19.482431
#>  [8] 11.525898  5.287683  7.114025 49.753485 49.753485 31.295434 31.295434
#> [15] 24.353747 24.353747 24.353747 24.353747 49.944882 16.941156

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
#>    [1]   216.37443    72.15655    63.64450   144.81364    61.77757   164.36026
#>    [7]    96.02799    55.76365    56.67414    62.37971    60.93296    62.76240
#>   [13]    78.70692    64.52076    61.93546    83.63428    54.49505    62.89330
#>   [19]    68.15661   165.92975    58.57773    57.29640   139.96122   125.09678
#>   [25]    53.26760    60.33601    63.52858    61.62235   370.74440    92.52080
#>   [31]   100.83913    58.87474    56.78885    56.87372    50.23877   130.34063
#>   [37]    68.64431 58613.91487  2057.84566    57.84105    74.53708    55.04390
#>   [43]    57.12568    84.13178    58.35862    62.09252    66.62312    62.08303
#>   [49]   161.25662   142.47377    84.82131   100.06075    49.15499    67.92593
#>   [55]    83.26944    65.68700    62.22077    84.52687    65.67482    57.33380
#>   [61]    64.39926    58.44936   104.22621    67.69243   121.89099    68.91396
#>   [67]    59.49455    60.59018    65.18300    60.25119    77.48616    56.06434
#>   [73]    67.26508    60.32942    55.45962    54.64076    54.94227    79.97647
#>   [79]    60.83393    54.77430    92.43261    77.79226    59.06201    56.88319
#>   [85]    74.09752    55.27347    86.07760    71.95190   142.37708    65.40906
#>   [91]    63.38942    52.62085    56.33955    63.12126    61.64229    63.51599
#>   [97]    89.52142    61.93801    69.91380    67.73299    60.26888    68.79005
#>  [103]   103.35516    59.48160    54.24856    54.75742    59.83227    60.32355
#>  [109]    83.58565    60.85823    67.91585    56.11138    56.51252    62.46625
#>  [115]    85.32223    75.99193    60.13063    54.75018    71.21336    55.82756
#>  [121]    54.28859    66.71069    61.26299    84.80558   132.52820   191.59053
#>  [127]  4001.67876    68.81399    57.41342    61.41726   127.37742    71.40443
#>  [133]    57.71594    66.65144    72.93939    82.05189    68.18609    65.79971
#>  [139]    79.32325    84.61605    87.78101    69.65090    62.60768    62.48027
#>  [145]    74.85418   116.14127    59.26264    76.70255    67.99232    72.64695
#>  [151]    58.21780    68.34797    99.24448    69.69555    57.37419    64.45215
#>  [157]    55.33002    56.99040    54.32574    53.61263    58.73158    65.70601
#>  [163]    68.65830    75.55149    61.07047    61.45048    59.82794    87.96064
#>  [169]    90.95529   108.79508    90.64962    64.53728    65.76104    55.91938
#>  [175]    68.52806    67.48663    66.38034    62.83620    64.87761    68.05365
#>  [181]    56.24801    60.05325   353.87204    85.23854    59.49119    58.98948
#>  [187]    55.12164    91.39387    94.30447    91.93288    81.38391   140.44892
#>  [193]   151.68314    58.46003    62.91725   105.47149    61.85678    59.13914
#>  [199]   324.68262   101.37360    60.52924    63.76351    59.09546   728.97786
#>  [205]    89.98520    83.93899    72.69357    58.41624  1391.14096    64.47450
#>  [211]    58.63790    57.39705    55.84405    61.83591   130.95329   677.28059
#>  [217]   196.61680    62.77444    68.99686    66.13276   126.66785    57.82756
#>  [223]    58.25062    63.11425    67.02798    84.04595    54.65841    64.17297
#>  [229]    59.33227    64.01957   286.71277    66.03798    61.24783    67.29095
#>  [235]    65.82692    57.85463    75.84514    60.96484   424.14330   382.41906
#>  [241]    56.06628    65.25255    60.85613  4396.04481    67.54901    61.67287
#>  [247]    87.20465    92.98967    81.76409    93.85713    61.00672    59.06422
#>  [253]    60.15920    66.79949    81.28302    69.09270    52.15795    54.00073
#>  [259]    61.63430    53.94581    65.04256    67.81066    60.28162    66.57798
#>  [265]    61.07547  1826.07687    74.75885    55.10003    64.64793    61.55297
#>  [271]    69.17947    78.17914    89.10606   112.32673    82.02175   109.55816
#>  [277]    74.70903   104.47378    53.19682    57.46735    61.24754    54.11999
#>  [283]    60.45535    77.45372   100.06390    70.09521    85.82435    88.22012
#>  [289]    76.36525    62.03867    62.66531    60.45669    60.89197    92.83567
#>  [295]    63.96982    57.48025    71.61847    64.94073    62.73429    66.92845
#>  [301]    61.65562    62.03131    60.10332    63.19146    61.69337    69.59275
#>  [307]    59.15564    87.32789    58.28900    59.43218    60.56271    59.15027
#>  [313]    66.17197    69.50816    66.82324    90.87201    84.97740    60.21144
#>  [319]    67.25036   113.92872    81.09823    64.88138    72.49013    57.97342
#>  [325]    56.48492    76.49764    62.33751    63.86677    91.27996   126.73243
#>  [331]    73.45670    59.70630    59.89880    59.92691    65.17209    53.73219
#>  [337]    60.79907    67.46576    54.01328    61.12763    63.59857   323.26256
#>  [343]    58.78986    60.67113    85.44843    71.78179    57.99546    56.00031
#>  [349]    56.86615    59.95704    78.33170    69.23454    66.42160    61.60244
#>  [355]    52.58498    49.02956    57.94204    75.60228   133.19894    63.55522
#>  [361]    62.55063    92.60616    52.82462    58.98710    60.18069    82.10949
#>  [367]    64.29137    68.56884    68.59204   212.89011    60.27985   135.46643
#>  [373]    83.19791    83.37542    63.55297    60.84545    54.38841    59.43632
#>  [379]    69.93118    66.76090    65.47412    69.57221    55.18485    62.80657
#>  [385]   109.19734    70.40967   946.56528   112.43809    68.90195    68.53288
#>  [391]    78.09082    73.21674    68.79141    68.43523    73.55478   111.13523
#>  [397]   111.08815    86.11888    64.13762  1550.56321    72.07407    57.42681
#>  [403]    71.23915    59.37012    61.62266    68.07113    68.37761    84.23732
#>  [409]    55.50774   249.32717   389.25152  1002.81616    59.75391    60.30573
#>  [415]    62.87245    76.44259    54.83342   181.58386    57.89575    73.19245
#>  [421]    55.16697    86.12287   156.30927    83.58616    59.13833   162.02075
#>  [427]    45.06332    91.50565   101.04986    56.95374    61.85590    62.51132
#>  [433]    65.55347    88.35752    73.20773    69.78488   168.58857    93.22444
#>  [439]    58.12055    63.21353    68.65946   108.68069    61.93145    66.22590
#>  [445]   264.34800    52.01352    50.23491    51.85774    61.60440    63.21447
#>  [451]    76.86413    58.58335    53.77125    56.97751    56.83703    57.50898
#>  [457]   110.71933    57.91473    64.73676    62.13619    57.88283    62.30104
#>  [463]    59.91658    78.02851    94.13810   173.76645    56.14996    60.09052
#>  [469]    59.39991    60.81910    67.56733   157.94943   198.70049    54.61228
#>  [475]    57.36018   101.18331    82.98430    56.51267    57.03318    58.06965
#>  [481]    67.71417    67.41716    70.91695    58.36657    63.50331    68.42074
#>  [487]    53.87910    65.30001   266.19734  7273.34101    61.61236   125.82055
#>  [493]    59.42766   100.94840    61.63435   119.05704   196.29555    63.26104
#>  [499]    67.45065    59.84978    67.68003    62.19530    61.04368    71.46076
#>  [505]   101.65112    61.84555   344.55496   252.56744    92.80836   158.96958
#>  [511]    51.55131    63.45205    88.11275    67.05848    54.37786    64.54388
#>  [517]   104.80731    75.88692   114.27321    61.50769    99.98378    54.54711
#>  [523]    60.32227    65.41363    71.39863    82.38326    64.54579    71.58739
#>  [529]    53.59831   110.27138    52.44351    61.87582    59.11574    60.23449
#>  [535]    48.42584    60.36330    79.51990    79.49309   519.97702   209.30348
#>  [541]    63.72469    69.32680    62.22083   101.65889    76.55475   119.62445
#>  [547]   244.57417  3853.22360    59.40056    67.93272   105.43271    59.41503
#>  [553]    71.91349   175.98937   249.49495    58.92947    63.75267    71.79448
#>  [559]    65.23409    94.28522    55.59745    56.36752    57.80812    81.62206
#>  [565]    65.66780    63.64612    62.35853    65.98810    72.23345    62.09190
#>  [571]    78.86320    98.14824   125.50577    62.19239    91.21808    64.28037
#>  [577]    72.76665    77.50286    60.86074    67.25584    85.36233   243.33584
#>  [583]    85.38582    60.30692    61.82056    65.15125   102.78018    57.93805
#>  [589]    63.76785    68.50743    60.33548    60.58621   128.84186    61.02456
#>  [595]    58.52401    53.67844    93.14604    69.08071    82.48372    81.82444
#>  [601]    55.51840   116.73945    68.24380    65.95102    72.27221    87.45144
#>  [607]   705.56702    71.55401    62.16997    74.66371    51.23385    61.74576
#>  [613]    65.76116    64.15763    64.03787    58.83209    58.48951    74.65923
#>  [619]    97.52991   255.76896  1342.43374  2338.23014    62.77851   682.48462
#>  [625]    65.98607    58.46898    66.78701    59.37476   647.62996   101.50163
#>  [631]    56.21789   103.02363    56.27578    71.79533   153.78954    72.75064
#>  [637]   100.61951   335.07803    62.62348    85.68598    91.65705    91.59550
#>  [643]    57.04283    61.41084   148.60844    56.31536    72.25571   146.36241
#>  [649]   135.80981    63.75724    59.97012    52.35221    97.80031    85.26702
#>  [655]   123.30985   169.25366    76.74502    65.09887    68.95979    61.66089
#>  [661]    55.35145    77.75982    69.22441    63.80037    75.48903    69.29737
#>  [667]   127.65022   281.21720    55.61891    61.25838    56.13006    70.02284
#>  [673]    72.91079    61.71965    76.13403    58.41886    57.65660    54.28320
#>  [679]    47.08503    55.33834    55.87348    60.30037    83.11458    54.86718
#>  [685]    77.63005    84.59236    60.84294    51.04606   100.11802    63.71884
#>  [691]    56.13950    59.73553    65.85482   612.32596    84.72487    84.21961
#>  [697]    60.16411    97.70567    58.59950    87.10994    69.34504    63.07570
#>  [703]    75.32903    76.03549    62.59861    68.47036    69.82998    68.93405
#>  [709]    56.52283    65.06018    68.54224    55.29990    91.66578    90.99291
#>  [715]    61.52829    58.63340    57.65969    69.01776    60.72340    63.40936
#>  [721]    64.25540    63.04677    61.36943    62.58362    78.54775   128.75079
#>  [727]    56.02727   276.71062    70.13956    64.23156   112.92928    65.77519
#>  [733]    78.59623    91.71155    63.15398    58.63785    63.42404    62.12219
#>  [739]    64.04957    62.20392   102.77373   201.56276    57.99516    62.65110
#>  [745]    67.25383    71.56690    77.57473    64.57852    67.77628    88.74607
#>  [751]    57.59075    61.81737    76.15049    89.24179   183.99680   138.89978
#>  [757]    79.17074    67.02267    61.35829    61.67466  2689.23977   152.81266
#>  [763]   161.64000    60.60044    65.96286    58.09781    55.43332   103.92796
#>  [769]   158.82010    93.52283   169.54526    78.27389    56.41188    57.12536
#>  [775]   106.24840    67.33288    76.62211    62.75184   125.67879    87.02995
#>  [781]    66.22745    63.83170    55.87019   118.60290    63.50635    73.11782
#>  [787]    58.98583    64.35408   135.91724    89.84635    59.95107   368.60556
#>  [793]    61.84092    61.49114    61.35058   258.09974    75.91177    76.68407
#>  [799]    66.72984   100.71595   560.39732    56.24485    57.19549    64.47997
#>  [805]   392.37434    56.59934    66.09399    64.21813    57.63154    60.06120
#>  [811]    59.74955    57.80829    60.37345    72.29071    63.26381    56.54941
#>  [817]    98.69420    44.97756    57.39710   185.21790   103.80887    56.37701
#>  [823]    99.81462    65.03248    55.05090    61.22630    79.77670    64.57291
#>  [829]    83.51538    61.91555    71.97630    76.43770    81.66506   399.14196
#>  [835]    83.86735    77.04687   105.11210    69.04608    71.54813  1637.20848
#>  [841]   316.49072    68.49988    56.40592    72.13488    67.23239    60.59919
#>  [847]    67.14822    95.34987    54.50939    63.24601    65.60977    57.80789
#>  [853]    57.94460    58.35414    63.47968    67.21995    79.53150    66.36645
#>  [859]   103.54314    55.84030    62.32528    60.59895    69.43856    70.49030
#>  [865]   123.40820   293.29765    55.58291    62.00027    61.54862    69.06636
#>  [871]    61.35226    68.50110    72.41136    53.94750    59.42707    61.37437
#>  [877]    56.09371    78.66583    61.34457    76.64407    60.85154  3375.60099
#>  [883]    61.06661    67.51716   198.88663    60.90042    57.52596    71.46502
#>  [889]    91.46764    72.74509    74.76166    62.01146    71.30365    60.35791
#>  [895]    57.32936    59.22725    47.79608    81.84815    53.96843    69.16826
#>  [901]   176.96812    61.61222    60.13473    60.86142    60.12087    59.27730
#>  [907]    58.67647    65.21041    78.57751    56.49749    62.76465    75.52538
#>  [913]    58.45092    59.60748    56.32267   201.29122    69.51016    67.78259
#>  [919]    56.55081    68.32207    62.83027    71.54976    66.67146   183.17335
#>  [925]    69.45029    59.76088 31776.08179    91.91230    70.28997   103.42887
#>  [931]    66.43444    57.92848    58.71318    61.09297    53.84048    59.45755
#>  [937]    97.01455    71.61886    62.64994    61.59006    82.51093    54.36530
#>  [943]    62.95460    77.84609   261.11818    96.88352    61.28431    63.12100
#>  [949]    56.88535    69.78875    65.00046   374.16227    56.52765    69.56773
#>  [955]    73.36513    61.25065    51.82462    74.99969    89.09924    70.41979
#>  [961]    60.14440    71.40460    66.87896    70.16016   144.06882   179.10964
#>  [967]    64.20868    63.57886   168.98231    60.36896    61.06439    83.34759
#>  [973]    60.02809    57.77193    66.03764   102.15270   105.33007   198.28899
#>  [979]   130.55808    53.89596    79.34238    61.75270    60.10103    59.51907
#>  [985]    66.40021   130.52981    51.29707    50.68866    68.06139    89.43247
#>  [991]   107.32239    91.09901    60.06747    58.51641    61.58261    82.85964
#>  [997]    53.27229    65.58290    69.95529    72.67631
```
