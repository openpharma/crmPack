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
#>  [1]  36.91561  36.91561  36.91561  83.06868  81.74463  81.74463  81.74463
#>  [8] 292.77530 292.77530  29.95102  13.52425  13.52425  13.52425  19.34449
#> [15]  12.66570  12.66570  12.66570  12.66570  27.51774  21.48722

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
#>    [1]    72.47746    65.17403    54.27231    77.42856    63.76975   142.67795
#>    [7]    71.14320    69.89031    61.92357    57.49384    96.13066    58.57596
#>   [13]    60.69700    61.37585  1041.90485    54.94550    78.76719    47.76051
#>   [19]    56.01666    90.90026    61.23543    52.90334   160.54427    95.94700
#>   [25]    58.50428    65.08488    58.86201   557.92565    64.21315    81.76832
#>   [31]    69.56500    50.26312    79.78290    61.84144    60.79889    56.95084
#>   [37]    80.87188    63.18636    64.56878    61.63364    92.64552    57.40045
#>   [43]    55.81177    68.86447    56.74283    61.34920    58.85388    58.54395
#>   [49]    65.42377   303.58155    80.40427    64.75968    56.13431    97.88851
#>   [55]    68.77163    63.94454    72.08930    65.27471    74.15749    59.55670
#>   [61]    64.12533    56.45803   179.01489    90.91265    61.38797    73.35031
#>   [67]   310.79118    69.05570    78.58979    98.59081    62.66914    91.48976
#>   [73]   197.41627    70.82797    68.49423    66.98428   131.22610    73.57773
#>   [79]    60.02673    66.80932    59.16430    55.18270    62.12290    72.92188
#>   [85]    68.74191    72.84113    66.29995    63.56323    72.69410    77.41839
#>   [91]    56.88294    60.41878    70.85992   111.88877    80.73845    59.78275
#>   [97]    79.30307    61.21656    56.44386    82.03401    59.89526    64.07053
#>  [103]    50.30517    70.52626    82.70590    64.82193    58.19662    60.83389
#>  [109]    58.38708    70.58685   133.84493    57.28425    69.15694   128.50454
#>  [115]    69.24796    60.24991    60.55437    55.32461    62.68127    74.25014
#>  [121]    68.27326    66.76783    79.05004   133.81587    75.87381    75.27274
#>  [127]    76.97916   270.99449    52.98816    58.05892    63.10647    80.04102
#>  [133]    57.99900    60.74214    60.77519    61.37916    66.40360    66.22140
#>  [139]    64.28327    55.87684    57.22461    61.08190    54.63922  2562.15257
#>  [145]    57.85656    65.30345    53.42308    67.36040    70.19767    55.40576
#>  [151]    59.52350    70.00156    68.57674   103.25521    59.65985   150.58431
#>  [157]    85.78704  3037.05804   152.88741    72.91020    54.63452   101.29887
#>  [163]    76.84381    89.11886    54.71055    77.43162    69.78703    64.25686
#>  [169]    64.36876    82.72691    81.11847    55.64070    61.40204    87.24369
#>  [175]   219.49182    25.66019    59.89425    52.81841    63.58887    67.62620
#>  [181]   154.00414    68.76974    65.99804    69.62877    63.70402    96.68744
#>  [187]    56.64088    66.65735   319.57608    55.01036    69.67437    77.23182
#>  [193]    77.25169    55.65600    67.18370    69.24390    54.30804    73.17705
#>  [199]   120.07341    63.46445    87.96693    84.97247    73.02449    92.97745
#>  [205]    58.96867    53.11622    52.75754    63.83534    67.75007   432.29692
#>  [211]    63.28913    58.69849    63.80015   107.97874    75.54880   189.28702
#>  [217]   294.03936    62.45236    58.46256   334.13273    65.96781    63.53641
#>  [223]    57.26073    90.51519    62.60834   157.22269    76.07241   136.16364
#>  [229]   102.11310    86.84516    51.84056   116.33875    73.23845    56.36604
#>  [235]    82.26947    58.45390    96.42409    65.56695    56.89048    64.69570
#>  [241]    77.70183    60.00241    89.97844    59.69616    71.09190    53.85908
#>  [247]    66.92672    72.14202   127.85264   173.21065    63.76974   132.24510
#>  [253]    52.41502    55.50147    55.41775    84.59835    63.10140    60.47545
#>  [259]    59.07970    59.82287    64.30599    68.37885    58.55752    78.90631
#>  [265]    69.66001    62.17231    73.21222    60.86761    89.51922    65.09227
#>  [271]   148.35757    52.56185    63.16260    69.80006    62.56231    61.36129
#>  [277]    82.48648    61.87929    84.78903    57.66517    61.81411    56.45673
#>  [283]    57.51447    64.09783    66.08321    55.35148    61.30998    79.37164
#>  [289]    65.31163   101.48257    50.41208    98.71869    61.86429    63.27163
#>  [295]    61.87066    83.08845    61.36678    62.12486   105.25191    59.30089
#>  [301]   315.96941   317.14954    64.05701    66.24221    60.65661    61.83946
#>  [307]    54.80195    66.21158    62.93323    67.54353    68.00466    68.08516
#>  [313]    94.05830    57.32752   168.61515    59.90303   149.45102    98.29392
#>  [319]    65.55362    61.04322    82.23338    65.18793    55.88713    55.97975
#>  [325]    58.90256    61.45418    66.93188    72.30769    68.50040    55.90294
#>  [331]    71.84110    86.09133   298.77599    75.76745    55.25970    63.13197
#>  [337]    76.02615    61.03122    57.29976    74.69561    64.08120  1783.89820
#>  [343]    73.15027    64.62861    53.52700    55.72504    63.69880    57.52579
#>  [349]    70.87719    60.03783    57.83862    65.29078    56.27762    60.22328
#>  [355]    61.24531    66.72724    72.06635    60.90200    63.97798    62.08609
#>  [361]    59.13002    83.87802    76.19450   104.23076    84.07776    56.91650
#>  [367]    57.17495    53.47664    47.16755    79.68183   210.19951    82.28137
#>  [373]    68.89088    76.84827    63.48353    57.28319    58.86062    52.28731
#>  [379]    52.53996    77.53562    80.95418    84.26145    60.33078    75.22153
#>  [385]    91.22479    59.91351   101.68007   112.66846    62.48794    60.81564
#>  [391]    57.16150    71.11305    64.44640    58.14866    87.49670    57.35677
#>  [397]    76.39962    63.90447    83.19386    59.44430    57.93208    57.68506
#>  [403]    64.74626    57.40666    59.56894   214.04268    66.59969    58.02624
#>  [409]    63.01951    64.13942    58.17284    56.82393    74.80843    67.30135
#>  [415]    61.39526    90.84092    57.85172   127.84880    54.07995    62.78352
#>  [421]    70.20323    76.45416    64.33751    83.22221   114.82964    55.17234
#>  [427]    60.12557    60.92367    56.91524    60.78539    89.64844   489.02375
#>  [433]   778.95646   480.22481    61.60709    63.50879    97.10167    89.06848
#>  [439]    60.10830    55.53835    57.99579    55.09034    61.84919    55.24523
#>  [445]    54.58668    57.99840    79.04813    64.37349    71.87755    66.02513
#>  [451]    62.39320    73.30701    87.38129   132.36239    63.50459    61.02606
#>  [457]   158.20569    56.69928    92.30656   210.38325    55.19411    56.91705
#>  [463]    51.69089    59.63136    61.14405   116.04583    63.70227    61.52539
#>  [469]    68.49983    69.41883    62.10026    72.02868    74.90263    60.00982
#>  [475]    80.04744    67.91641    55.81460    53.27009    60.26851    54.36453
#>  [481]    69.96666    94.01740    66.98748    64.61913   152.26079    72.93100
#>  [487]   131.86553   288.87783   105.74974    86.97719    77.86408    59.45620
#>  [493]    55.67929    64.07600   126.91942    62.45026    63.69523    58.98870
#>  [499]    66.35133    62.65966   195.89056    56.98399    63.36591    57.14892
#>  [505]    74.37743    62.31551    55.79124    55.28378    74.04527   109.43586
#>  [511]   121.34508    76.42839    96.34600    55.00079    53.99352    55.05392
#>  [517]    63.19541    55.84946    59.81126    60.97699   135.18515    82.58759
#>  [523]    64.32438    64.39135    97.04390   149.14249    65.84545    59.91750
#>  [529]    62.13170    57.85187    65.40683    76.68599    60.78817    63.88713
#>  [535]    63.56030    79.60889   104.62378    53.18109    60.16862    58.86502
#>  [541]    58.46220    59.82993    69.41451   100.69088    54.52510    63.05584
#>  [547]    89.21555    62.01153   230.87508    46.56338    89.44366    61.84937
#>  [553]    92.15142    58.70062   109.26189    70.99542    66.63975   110.79010
#>  [559]    56.71468    57.64338    66.01795   124.72958    79.77632    63.15269
#>  [565]    61.16777    57.19446    56.79694    58.44610    67.34329    69.54786
#>  [571] 52364.83794    82.25752    61.17826    75.69031    75.26617    58.51771
#>  [577]    58.15236    66.74044    76.62120    85.41755    53.43237    55.74542
#>  [583]    66.30000    56.46177    76.28796    69.78887    59.84070    70.49540
#>  [589]    64.59286   204.70172    60.94164    61.47299    59.81856    74.72879
#>  [595]    68.12934    60.44035    58.90909    74.59055   188.51998    55.53343
#>  [601]    89.19669    85.27778    84.46176    87.63530    65.22773    62.70180
#>  [607]    80.08091   109.98870   399.94354   435.39350    60.96300    58.77998
#>  [613]    60.24314    63.04105    62.65398    62.78057    77.64279    56.49718
#>  [619]    56.85881    62.14617    56.56812    60.46962    61.07966    63.04813
#>  [625]    72.10005    64.14363    64.38936    68.82844   249.21312  7416.58151
#>  [631]    58.36427    61.06209   102.27198    63.93846   112.43365    57.80001
#>  [637]    63.18814    58.82842    59.59133   136.41878    58.16017    78.44026
#>  [643]    57.28774    62.76170   101.25537    73.63165    82.63500   298.09888
#>  [649]   288.76201    59.41153    65.04712   152.21904    91.46410    61.59577
#>  [655]    95.03222    58.34084   213.02168    73.47733    65.36127    73.51421
#>  [661]    62.85026    60.45691    68.38619    70.93936    59.21126    70.56260
#>  [667]    92.94995    53.25452    85.25268    43.25278    75.71672   304.07294
#>  [673]    53.64884    55.54770    73.70919    79.65202    61.83957    62.17561
#>  [679]    65.79119    63.51405    59.98343    72.71919    52.64327    68.34919
#>  [685]    57.21087    66.11008    65.07074    70.13095    61.70998    74.41656
#>  [691]    71.15305    65.08786    87.55508    61.57940    57.47126    59.21748
#>  [697]    62.93341    65.18160    70.62096    88.34287    65.36152    77.06478
#>  [703]    60.46092    64.38563    64.08733   208.54655    57.87223    62.52863
#>  [709]   275.01814    35.40349    43.61151    73.31663    71.90424   133.60490
#>  [715]    53.33076    59.32313   105.26899    57.56666    54.91297    60.49107
#>  [721]    67.36168    62.60703    87.09867    57.67583    70.80833    66.20506
#>  [727]    46.69329    60.20980    64.70313    54.17607    85.48409    61.34168
#>  [733]    59.19077    56.03376    63.28361    59.68801    81.27601    54.84665
#>  [739]    63.92480   267.89164    58.98336    57.94133    67.38500    60.15652
#>  [745]    89.91295    57.37143    63.94399   115.17497    57.32844    79.71163
#>  [751]    72.65442    58.85730    59.53654    63.71520    60.57425    79.82173
#>  [757]    60.34567    58.40566   106.75403    57.34852    61.13865    61.00939
#>  [763]   233.94245   168.56804   211.34195    58.22756    54.35426    62.86928
#>  [769]    61.03493    59.02373    71.72041    62.26296   344.80530   160.15391
#>  [775]    58.40619    67.19291    63.98154    70.73797    61.56614    59.02322
#>  [781]    61.49345    61.45264    55.26963    74.82794    97.60053    63.92591
#>  [787]    57.70032    62.39988    58.74591    68.54569    89.67726    67.95100
#>  [793]    60.97306    66.95835    65.60247    73.38682   101.77342    56.18120
#>  [799]    71.87958    81.48767    58.09980    66.39223    67.94919   177.20573
#>  [805]    64.22334    58.28844    64.05211    57.49498    81.13313   116.54610
#>  [811]    56.83220    59.28011    58.30953    56.32513    58.90756   591.82081
#>  [817]    56.16306    59.83971    64.55094    66.40700    54.89392   146.24803
#>  [823]    59.09332    70.27256    63.41424    58.19752    58.17467   117.89871
#>  [829]    77.90299   148.26677   157.41109    61.24350   126.87431    75.15153
#>  [835]    56.72160    67.92497    80.48197    71.89652    71.59985    60.63827
#>  [841]    68.40715    55.92620    69.50500    56.88471   102.84712    60.33992
#>  [847]    92.00918    70.30740    67.08452   425.67152   158.32007    55.21472
#>  [853]   110.42737    62.17097    79.05518    81.60784   202.04721    67.17935
#>  [859]   109.95577    55.58085    57.07482    62.39225    59.00816    57.58876
#>  [865]    63.06522    76.22922    67.91011    85.23868    59.51374    70.97540
#>  [871]    60.16346    56.54539    55.16518    57.27252    66.43744    67.26465
#>  [877]    79.41475    53.91513    77.71533    73.85845    73.47545    66.32753
#>  [883]    77.16034    70.96472   114.18587    70.60801    64.74184    76.07700
#>  [889]    63.78135    69.37633    75.83579   332.61987    59.45247    64.86964
#>  [895]    58.88873    71.63832    67.06841    54.70824    59.24875    61.32379
#>  [901]    66.47885    64.29638    56.12279    63.95390    58.23461    58.72955
#>  [907]    65.96896    76.59404    91.17241    70.46634    70.58724    69.80445
#>  [913]    67.59147    59.65607    66.82101    63.09175    60.52452    58.57492
#>  [919]    60.24913    81.68737    57.53502    54.46614    72.87159   101.88432
#>  [925]    73.33981    60.91934    57.44801    61.89693    92.62149    70.86598
#>  [931]    58.22340    59.08515    61.91151    65.30457    72.41474    79.34893
#>  [937]    80.41686    61.59357    59.36574   150.18328    59.42585    64.78514
#>  [943]    59.32869    84.31632    61.85145   197.11635    52.37588   105.79926
#>  [949]    65.37708    66.81400    69.58975    68.55866    76.97007    78.92570
#>  [955]    63.47514    58.50557   128.18260    86.02547    89.75399    61.54890
#>  [961]    64.31046    54.21539    58.79107    73.65771    59.56362    61.49225
#>  [967]    57.19386    91.04455    61.43665    56.42349    58.66009   452.27072
#>  [973]    65.25078    69.14946    63.16558   310.69259   111.71563    71.02682
#>  [979]    73.05996    57.78368    93.07819    63.19112    67.65063   289.14706
#>  [985]    62.84201    63.15687    67.80693    58.71267    51.21988    77.82833
#>  [991]    59.72167    73.71505    60.11947    59.55909    62.56227   113.87678
#>  [997]    57.70198   103.90671    62.22289    67.75824
```
