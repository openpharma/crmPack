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
#>  [1]  71.20067  71.20067  71.20067  71.20067  15.36328 241.15746 328.11504
#>  [8] 203.15748 203.15748  43.56910  43.56910  43.56910  43.56910  43.56910
#> [15]  43.56910  43.56910 168.35141 168.35141 168.35141  48.83736

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
#>    [1]    79.33752    56.62494    61.10547    61.81409    62.54052    55.34972
#>    [7]   424.65432    68.16561    67.57592    50.02428    58.39275    59.76731
#>   [13]    96.96273    55.50492   143.78219    75.73199    60.50399    61.14297
#>   [19]    67.99281    55.34646    63.41685   844.26111    52.30691    56.88262
#>   [25]    72.13657    48.98884    71.93748   116.57542   151.17245    73.04450
#>   [31]    61.28152    64.91346    58.29006    81.88333    53.23648    61.15782
#>   [37]    65.35412    64.09678    54.53296    60.26807    69.31336    81.68999
#>   [43]    77.28023    48.68449    70.66428    67.39880    73.56113    56.86257
#>   [49]   105.64185    53.06935    63.07123    55.92331    57.48799    72.27806
#>   [55]    57.91947    60.35019    74.53267    61.66612   134.14526    63.27100
#>   [61]    74.42212    65.46572    70.71070    87.66072    64.72204    50.76740
#>   [67]   103.62208    76.49086   180.08603    66.61270    63.91467    61.23070
#>   [73]    65.27036    69.74478    70.96551    58.28903   146.77535    70.32187
#>   [79]    58.75705    70.67556    60.42087    58.21698    56.12032    62.12149
#>   [85]    61.55701    61.48774    72.54311    54.75254    80.10578   752.88636
#>   [91]    46.64086    83.92308   123.19260    63.06133    68.01312    94.72267
#>   [97]    74.15193    52.05856    68.89980    61.67601    56.22533    66.90581
#>  [103]    71.94836    88.56656   680.21627   101.43068    75.07859   139.56462
#>  [109]    64.40764    60.83860   105.36936    51.50408    59.79538  4836.17520
#>  [115]    84.80621   148.83599    71.76482    66.02738    57.10330    55.31635
#>  [121]    68.81970    71.39098    61.21723    58.30982   119.10100    76.01411
#>  [127]    77.12734    71.69213    72.72946    54.34387    67.67069    76.50792
#>  [133]    63.26124    57.89552    63.59601    61.72891    57.38672    65.99668
#>  [139]    51.02283    77.18940    57.53975    60.60387    69.45048   105.78165
#>  [145]   127.96045    61.45407    56.95999    58.53807    78.60962    60.13759
#>  [151]    57.23880    59.43378    83.08182    52.99188    58.10119   217.67879
#>  [157]   148.75475    60.93896    60.26184   100.10750    74.52025    55.85244
#>  [163]    85.50562    54.04184    54.16189    73.91082    62.75742    94.40134
#>  [169]   623.82285   130.66595    99.24918    58.67692    57.12559    73.11777
#>  [175]    56.75110    56.85746   115.60780  2141.91004    68.96194    78.40933
#>  [181]    77.79134    80.58976    58.49292    62.04020    89.27950    76.87315
#>  [187]    61.76881    58.40932    63.24296    60.50776    73.15807    57.65932
#>  [193]    68.03965    58.59131    54.84392    78.69382    54.55042    76.68180
#>  [199]   157.34599    87.30974    69.80383    65.30932    87.28918   149.28077
#>  [205]    57.37161    64.86364    59.39258    60.66586    61.48552    84.97546
#>  [211]    58.23909   111.88906   107.37130   169.67219    56.79543    71.15601
#>  [217]    53.12030    89.19477    57.74209    98.05957   114.81765    63.18601
#>  [223]    67.41326    63.45653    77.58559    63.83173    57.88238    70.07448
#>  [229]    75.71466    65.31887    49.95502    65.59702    57.69283   117.61817
#>  [235]   100.42511    62.62881    58.93782    95.54144   106.00123  1100.83663
#>  [241]    65.73203    60.67344    56.48286    60.10793    58.53488    63.26406
#>  [247]    78.68824    61.00223   218.26543   125.87924    53.81080    51.13664
#>  [253]    58.53054    73.11597    63.96403   102.47110    57.12185    61.79717
#>  [259]    62.85735    56.65974    60.49752    64.24442    59.48775    60.56123
#>  [265]    62.60010    66.32082    77.80851    53.81894   132.46985   153.94248
#>  [271]    69.28538    69.35418  1031.47184    56.70198   149.66200    56.32947
#>  [277]   311.81125    67.00308    69.79666   150.23278    52.25596    58.12197
#>  [283]   103.19636    63.96488    61.46444    68.45033    57.47707    96.54294
#>  [289]   758.67896    82.32642    66.92700   113.12154    81.49863   201.83780
#>  [295]    73.22352    63.25893    57.77882    60.17371    83.66205    61.54060
#>  [301]    54.42333    61.22323    58.54289    65.05043    58.42328    58.70904
#>  [307]   404.60822    59.56288    65.62370    79.79628    69.35593    86.34714
#>  [313]    65.53009    56.89966   102.52637    60.51747    90.29550   183.12353
#>  [319]    56.55315    78.78781   100.48680    60.79002   125.77965    54.50119
#>  [325]    67.91360    61.83682    64.78237    57.84494    58.79939    57.57215
#>  [331]    61.32779    65.49796    57.43726    69.96205   114.34053    60.49859
#>  [337]    64.73388   406.08408    54.66906    54.05629    56.99477    62.23568
#>  [343]    86.85317    56.12511    70.40364   119.88402    69.06466    55.24367
#>  [349]   104.90455    62.04276    61.27178    88.09046    77.09625    59.15701
#>  [355]   139.11403    53.42196    60.49163    72.11613    60.25010    63.75901
#>  [361]    68.01339    58.72432    72.19918    54.21880    62.27037    65.51380
#>  [367]    70.87943    71.67763    70.33281    69.95300    55.22563    61.76059
#>  [373]    62.16445    64.24163    71.09189    60.75090    57.37245    57.30245
#>  [379]    69.85261    74.99027    59.74096    61.36516    59.36784    86.25132
#>  [385]   145.70033    69.25087    58.57457    62.18862    49.95318    64.55896
#>  [391]    65.99715    77.81084    60.12135    63.09497    55.21080    50.30728
#>  [397]    59.45457   683.33037    63.79011   110.85705   134.46217    62.56934
#>  [403]    61.25851    72.49991    54.93232    62.58731    58.34833    61.63733
#>  [409]    59.63704    61.51313    56.51673    59.22699   631.58444  3723.22833
#>  [415]    98.77172    55.25361    76.05775    84.45371    65.61622    56.77520
#>  [421]    69.40394    64.89398   129.46912    80.70112    58.35866    72.73002
#>  [427]    53.67373    88.26525    75.65012    63.93865    93.39658    58.30027
#>  [433]    57.86405    56.89290    76.46015    57.43740    55.78364    70.34751
#>  [439]    89.98160   120.09915    73.78583    80.25519    57.09617    56.74085
#>  [445]    58.14251 11027.86135  1639.55284   533.91317    84.69979    79.10214
#>  [451]    78.17931   148.32371    64.01628    86.27226    59.87005    71.98226
#>  [457]    99.36146   141.56791    54.61025    59.40633    66.66160    66.48514
#>  [463]   293.44128    80.89636    64.48184    55.77379    56.40417    94.51829
#>  [469]    78.71455    67.67419    73.38439    70.12447    69.76418    83.47118
#>  [475]    98.10536    61.94208    72.48010   125.57203    64.28396   372.57175
#>  [481]    64.00508    70.00461    85.31233    59.49069    62.04612    56.73310
#>  [487]    68.99558    62.50551    75.49698    67.02167   141.51029    96.73576
#>  [493]    79.07245    68.53781    36.05128  9207.47869 21226.28350    56.86371
#>  [499]    97.21556    87.33389    59.70813    59.13571    65.75544    70.04958
#>  [505]    72.35291    53.07934    71.10713    64.74832    88.49509   284.58865
#>  [511]    69.44231    64.06149    65.22767    56.54898    56.09505   185.18122
#>  [517]    70.83133   193.39404    66.64338    67.30670   274.87334   116.63280
#>  [523]    55.61649   121.56120    57.38914    63.20573    59.79067    73.66487
#>  [529]    68.22000    78.06774    57.73216   101.11445   157.41987   204.14033
#>  [535]    65.40124    61.45542    59.48444    93.17499    70.25184    63.89482
#>  [541]    64.87018    58.50646    79.64181    58.61307    84.90359    60.22954
#>  [547]    64.97358    66.14478    60.87714    70.54445   282.29840  1002.62688
#>  [553]    60.37996    73.81289    61.22033    61.54694    68.91908    74.17412
#>  [559]    67.85237    62.75012    67.42759    71.52129    72.97811    74.78063
#>  [565]    76.24743    58.69407   136.07813    53.69058    61.07276    62.27359
#>  [571]    68.82008    59.15328    58.34479  1180.40765    54.53333    86.96582
#>  [577]    58.10526    61.00349    53.39155   120.45420    74.17657    58.15503
#>  [583]    61.18418    62.47894    67.88977    64.29104    56.98397   107.27198
#>  [589]    92.10150    87.38238    59.85451    71.80059    52.57221    56.92551
#>  [595]    61.96073    67.60405   152.74522    65.21467   169.82084    73.03309
#>  [601]    64.25591    72.13033    57.09288    60.55454   121.76742   108.24476
#>  [607]    91.08787    68.29017   205.06370   224.74625    80.88864    61.82753
#>  [613]    53.83737    61.74530   198.08464    67.40906    55.66255   107.64229
#>  [619]    58.79115    80.55157    68.67378    93.55585    72.82504   112.87784
#>  [625]    55.70181    72.59313    61.64155    95.74911    60.26272    65.57464
#>  [631]    65.55334    80.60477    81.71237    70.72936    55.44443    66.60873
#>  [637]    89.77293    56.65267    62.44959    63.41829    56.54630   118.27824
#>  [643]    55.18143    54.62016    54.54316    75.55050    68.92780    64.00178
#>  [649]    55.76152   107.13079    55.45946   148.63982    60.91479    69.51706
#>  [655]    62.38235    67.06583    69.81771    53.82799    57.36173    61.23103
#>  [661]    73.64967    63.14340    58.26150    71.22889    74.58352    62.35033
#>  [667]    52.19054    64.68581    60.11006    63.65497    59.99969    69.23958
#>  [673]   121.14398 11379.60376    86.27792    80.62804    76.25623   214.13380
#>  [679]    75.06413    64.82911    59.48843    59.09512    67.89849    66.99282
#>  [685]    61.60538    71.37439    73.85940   114.17523    54.99318    55.39030
#>  [691]    90.18214    57.46217    80.62039    99.67508    82.10306    58.90169
#>  [697]    74.67244    81.40291    63.20060    62.62242    78.09103    63.87632
#>  [703]    63.26836    66.32960    91.34931    77.46561    56.41953    60.74477
#>  [709]    61.38661    68.11891    65.01692    73.24243    60.06584    74.21642
#>  [715]    92.28315    57.68334    58.44779    72.29661    60.17257    59.28045
#>  [721]    56.43513    53.07190   177.89426    53.03125    62.19257    54.91781
#>  [727]    71.40905  2429.68411   475.83773    63.77465    76.56709    59.17415
#>  [733]    68.67899    55.14034    57.66103    66.76545   101.48706   222.97650
#>  [739]    59.62970    68.81792    70.63123    62.07767   106.18130   133.42277
#>  [745]   156.08481   114.55569    63.26845   206.93775    88.29062    69.90542
#>  [751]    67.35960    57.21104    68.19290    61.43106   158.92574    57.55027
#>  [757]    77.03589    51.27320    84.12925    58.33068    55.60951    55.16630
#>  [763]    57.69871   132.55588    80.38652    59.39753    61.40125   158.59820
#>  [769]    63.20084    61.70526   129.81414   107.42159   165.28331    70.56133
#>  [775]    63.62186    60.55011    74.83131    70.08555    60.31923    61.81648
#>  [781]   103.74281    55.65415    59.34022   127.46512    56.68302   101.85804
#>  [787]   176.54382    56.94041    56.65426    57.07728    79.75951   128.60277
#>  [793]   128.57106   216.43171   103.74974    70.23563    57.11409    69.99795
#>  [799]    56.87003    63.48988    73.51639    59.93325    89.10165   188.41779
#>  [805]    71.64213   183.01691    50.20741   110.81550    74.88733    61.87736
#>  [811]    75.76262    68.78138    67.36179    58.93336    59.85592    73.56228
#>  [817]    60.62149    66.39731    59.35859   122.36026   114.19545    59.03576
#>  [823]    57.86527    75.25504    54.16980    61.08314   135.95505    62.71563
#>  [829]    72.59266    56.02656    51.87185   483.80971   122.58435   166.80647
#>  [835]    63.80879    71.19667    62.88875    52.58234    53.48075    98.34441
#>  [841]    62.62549    99.18873    87.64906    62.75789    63.15544    52.07347
#>  [847]   114.03328    79.10983    93.83366   115.04738    64.94390    65.82207
#>  [853]    64.91272    57.09951    58.52548    75.41222    68.08365   115.23471
#>  [859]    58.63801    78.17715    73.23616    50.94208    66.54292    95.43813
#>  [865]   332.80093   462.73228   879.43411 83774.92065   732.99179    58.87930
#>  [871]    77.77781    61.46582    70.31599    86.53721    78.81322   114.18683
#>  [877]    62.14550   159.68959  1314.32551  3385.60839   222.61478   188.91729
#>  [883]    70.92244    69.87201    72.84349    56.93382    60.32640  1101.21159
#>  [889]   152.69797    67.32002   106.07735    55.26460    56.81931    67.76296
#>  [895]    61.79215    68.24442    57.38705    65.97425    70.03833   112.09613
#>  [901]    71.86784    61.60170    60.75877    70.12008    62.64653    55.94257
#>  [907]    63.09551    88.54416    56.49417    53.97848    70.22704    66.85268
#>  [913]    61.45269    70.83381    83.79066    66.46756    67.53519    73.85075
#>  [919]    61.81725    71.46465    76.99589    66.60251    72.65450    79.39299
#>  [925]    61.03249    64.87641    56.80221    85.35883    63.73862   166.95708
#>  [931]   142.52321   130.10572    58.72914    56.74799    66.11703    56.30015
#>  [937]    62.07971   353.90451    55.00183   104.91030    86.60716   101.31870
#>  [943]   109.23433    49.64896    59.10401    62.37477    75.00850    64.27458
#>  [949]    57.37009   123.40830    58.69167   125.91195    61.73259   110.04124
#>  [955]    74.94291    58.77243    59.25394    67.75857    65.85970    72.37561
#>  [961]   125.15375    79.07864    55.05951    91.62625    58.31076    79.60323
#>  [967]    61.11271    66.47013    89.13884    62.48915    66.06281    82.77520
#>  [973]    57.95812   102.44202    57.20059    75.65976    58.73912    57.07562
#>  [979]    57.11871   144.01889    79.52306    53.21940    72.75435   167.08594
#>  [985]    56.63127    58.49812    68.57937   209.62477  7571.66080    65.73630
#>  [991]    61.88098   183.05899    60.45434    59.62530    57.09177   257.19057
#>  [997]    60.07388    71.46187    58.61698   138.10749
```
