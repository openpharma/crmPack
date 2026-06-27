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
#>  [1]  13.02957 108.48662 108.48662  60.28365  60.28365  15.76974  15.76974
#>  [8]  15.76974  15.44086  15.44086  15.44086  15.44086  15.44086  15.44086
#> [15]  10.37336  10.37336 186.13753  22.82277  74.28359  74.28359

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
#>    [1]    56.80719    72.70216    82.87831    84.82032    57.42981    59.40728
#>    [7]    65.13345    58.72171    57.44857    59.64651    63.21587    56.59299
#>   [13]    60.12880    66.83419   114.28270    62.66793    48.99678    55.00794
#>   [19]    68.10215    63.86818    70.00712    56.76133    66.84450    62.07900
#>   [25]    64.98331    59.21501    56.89943    65.89247    64.70309    69.41563
#>   [31]    69.00134    55.75648    59.92618    58.81729    59.11300    56.58014
#>   [37]    61.83947    59.34347    57.38720    76.71070    73.61488    98.33120
#>   [43]    57.04791    49.18657    84.08542    90.36096    72.18981    61.60532
#>   [49]    71.64705   246.22134    66.36479   559.09750    62.01829    58.24462
#>   [55]    61.49186    58.00769    59.16068    58.60950    59.23507    71.24341
#>   [61]   964.60043    60.90440    68.25418    76.58634    63.43499   106.98289
#>   [67]    69.67162    70.78242    70.45457    65.89519    57.63975    68.36231
#>   [73]    75.37069    60.74979    66.18972    66.17277    57.92621    67.73288
#>   [79]    68.85513   127.83570   107.23247   125.68921    61.42069    51.21410
#>   [85]    57.40212    68.90804    85.72284    61.63189    64.16654    84.45499
#>   [91]    63.72853    57.50610    58.48226   193.50739   116.44834   109.73209
#>   [97]    63.85952    82.72169    72.86218    60.38873    76.22787   100.86997
#>  [103]    66.44504    84.47167    61.67048    55.98019    63.70806   115.96631
#>  [109]    63.56410  1309.67944 10715.34258    58.94172    58.46923    58.08493
#>  [115]    89.63439    60.00000   349.61001    69.95828    75.47155    70.47359
#>  [121]    68.09432    64.01733    66.47649    63.48639    73.21034    71.35436
#>  [127]    82.52190    60.61018    62.17200    71.24487    75.10313    66.81471
#>  [133]   121.36646    71.64644    68.45360    73.94349    62.31179    81.66858
#>  [139]    77.75579    58.11549    63.91296   308.85587    88.20852    67.19468
#>  [145]   273.33924    94.58028    69.82731    57.38020    49.84694    82.23739
#>  [151]    65.63860   254.52757    75.13996    60.04533    62.18275    66.81036
#>  [157]   130.89563    71.09474   112.55694   169.66723   149.14957    91.78463
#>  [163]    54.52907    59.08489    56.86133    56.73301    74.27194    81.14504
#>  [169]    58.33978    54.57066    53.07706   110.21812    66.85367    59.12978
#>  [175]    55.18472    55.77949    62.48754    97.55163    85.10838    65.89495
#>  [181]    85.07532    62.13688   273.10063    80.29344   109.97557    64.67137
#>  [187]    76.43382    74.54252   180.66351    60.54339   100.13264    86.47111
#>  [193]    71.73251    60.04085    91.41027    58.86851    61.01888    70.07265
#>  [199]    70.74069    61.99172   158.92307    65.49624    62.00087    66.45611
#>  [205]    66.62185    76.84556    54.95509   117.88889    65.86590    60.02642
#>  [211]    59.99574    90.01439    61.56982   123.16975    57.89131    61.60826
#>  [217]    57.33332   242.30495    66.13423    77.82456    89.45801    82.46809
#>  [223]   512.72748    72.83177    63.05445   242.77660    71.20074    73.09610
#>  [229]    58.34954    63.80208    62.09921    71.24277    53.01310    88.89863
#>  [235]   274.52132    58.35495    71.86801   172.15317    92.21754    63.36889
#>  [241]    72.38834    55.63778    61.30622    80.95800    74.94118    95.09447
#>  [247]    62.98958    66.60197    69.69388    56.88703    58.96659    58.19473
#>  [253]    72.41761    67.88148    62.28538    64.00093    70.71682    61.87072
#>  [259]    62.15231    81.78381    59.61118    66.25184    77.19494    68.60911
#>  [265]    57.69653    53.01513    71.81301    61.54931    58.11372    66.60419
#>  [271]    68.70363    56.79819    61.08180    65.71173    58.15959    54.63000
#>  [277]    60.61166    65.94097    58.74840    69.21825    65.25779    74.36032
#>  [283]    97.95058    45.77832    81.86627    85.76063   422.80238    56.09016
#>  [289]    62.18068   132.94976    64.03468    60.72785    69.68135    59.10901
#>  [295]    67.08003   260.63839    58.01585    72.38067    78.04741    58.39839
#>  [301]    68.91869    74.55253    56.53592    57.87112   105.27169    58.37916
#>  [307]    61.64643    66.44522    62.94792    58.03262    81.71263    75.39649
#>  [313]    62.51583    59.57580    86.16135    60.88148    62.66202    63.88781
#>  [319]    60.40577    97.07048    66.27655    60.52573    98.42965    56.78048
#>  [325]    63.78844    66.06202    67.40815    55.97369    58.67262    60.74069
#>  [331]    68.81885    60.29078    69.40758    68.21182   119.10279    68.24317
#>  [337]    58.73906    62.51688    58.46044    66.58907    56.29982   111.94267
#>  [343]    82.99531    58.79668    60.99587    68.21413    65.47846    59.30608
#>  [349]    86.75593    66.68338   101.65309    52.69051    58.97188    56.24248
#>  [355]   278.20180    86.84229    78.45544    50.92566    61.73489    61.22051
#>  [361]    60.78069    56.88314    82.47834    46.87712    52.89346    55.54398
#>  [367]    57.40739    62.31339    67.21523    54.93480   132.39709   227.82930
#>  [373]    54.28551    66.69761    62.10136    65.80059    57.85960    60.23934
#>  [379]    59.29684    62.30702    54.53524    57.26755    56.94892    63.20416
#>  [385]    75.69906    55.16788    70.98694    61.66207    63.24103   101.88252
#>  [391]    59.85268    57.25777   288.60478    67.09465    83.97100    76.38189
#>  [397]    89.34780    56.48464    53.07034    67.83163    59.62072    80.26100
#>  [403]    62.26787    93.04979   108.10852    96.34387   111.54264    82.78196
#>  [409]    55.39574    84.74120    47.85289    60.69192    54.89314    80.42823
#>  [415]    62.29225    92.08193    61.82280    68.37775    69.94594   137.09687
#>  [421]    60.13894   101.59501    60.07137    75.97047    70.76432   123.43570
#>  [427]    98.29439   265.68792    92.48542   980.87013    58.72450   150.43887
#>  [433]   326.44236   514.00859   116.49644    65.52208    72.83300    72.26611
#>  [439]    67.30207   153.64617    62.71057    60.06619    69.44257    53.44425
#>  [445]    52.55184    53.40843   211.00804    65.66046    66.17497    79.93676
#>  [451]   109.54285    75.01951   179.55528    55.69799    89.99582    71.78657
#>  [457]    78.91574   283.85500    63.96429    60.61241    67.10519    85.71519
#>  [463]    60.69059    59.58047    79.14390    54.23483    65.84400    91.80829
#>  [469]    57.01281    84.64219    70.90771    59.47427    75.64948    62.06677
#>  [475]   114.83082   103.47238    65.74899    74.48320    63.94176    64.99995
#>  [481]    56.17673    64.21385   103.58367    93.64715    68.73050    92.42319
#>  [487]    55.17753    62.94784    64.56462   104.38496    65.09148    80.80684
#>  [493]    71.05792    73.14186    64.69309   205.62250    55.52483    92.52470
#>  [499]    89.84611   123.84254    63.28882    55.04061    64.62757    74.23005
#>  [505]   160.36657    92.01807    57.92341    75.29877    61.75172    56.35998
#>  [511]    61.18206    60.84822   100.63757   155.63003    62.90240    55.76175
#>  [517]    58.07088    82.58839    59.43314    65.89731    87.15160    68.73421
#>  [523]    65.14981   219.60986    62.78032    64.53181    54.95561    84.10771
#>  [529]   174.60136    51.52974    69.74469    67.16799   108.27160    55.23771
#>  [535]    89.41120    55.80681    96.15554    86.05491    84.30790    63.79577
#>  [541]    67.53181   111.97670    64.57124    58.27984    59.57271    60.03228
#>  [547]    67.86634    58.84688    58.61256    58.03266    58.34013    70.62890
#>  [553]    67.27102    97.55533    52.58384    60.81644    64.27139   197.69088
#>  [559]   111.13458    60.58096    61.24328    59.85222    64.44540    61.52718
#>  [565]    77.57889    56.98521    70.28969    64.64350    90.18788   111.50209
#>  [571]   113.89206    81.58196    77.08001    73.30167    60.55872    61.45952
#>  [577]    55.47646    57.08185    66.75753    84.68606    72.77290   497.36209
#>  [583]    69.45560    61.13635    92.55853    67.72919    68.69425    56.95823
#>  [589]    66.02286    57.76986   162.95840    58.04895    69.85744    65.35463
#>  [595]    63.56147    84.03674    65.47581   175.51502    50.48524   107.03845
#>  [601]    64.09978    59.34492    58.57275    61.48196    58.15382    78.52106
#>  [607]    57.38176    77.67361   168.18505    67.07605    84.70851    66.24561
#>  [613]    76.31563    56.38539    87.34833    61.01729    56.54304    94.90642
#>  [619]    67.18804    64.62216    61.54248    55.44205    73.17165    57.75649
#>  [625]    66.61581    65.42627    68.22255    66.52191    70.72958   156.04119
#>  [631]    56.81285    67.35744    54.84824    67.39553    65.02417    60.80119
#>  [637]    63.79499    82.48700   347.62867    36.66513    81.55947    89.82345
#>  [643]    60.07573    64.32794    58.97915    58.65194    88.80893    57.72524
#>  [649]    55.38562    85.56117   116.87662    70.91896    51.42796    58.03597
#>  [655]    59.69439    67.02865    68.18731   103.95163    61.70644    54.37842
#>  [661]    53.16927    63.65273    86.06653    59.71213   106.49081    71.27676
#>  [667]    87.42636    98.65130    57.47777    57.49084    62.92232    69.69026
#>  [673]   161.58673    51.45613    84.91077    87.52447    96.91155    56.43601
#>  [679]    59.54663    96.90359    62.43120    66.56607    63.78516    63.28712
#>  [685]    65.50589    72.07661   121.15472    56.37726    70.22008    58.74383
#>  [691]   355.94002   324.69337    63.71352   108.67048    84.26918    77.69630
#>  [697]    59.80944    63.08958    61.40587    63.91762    84.29807    61.73646
#>  [703]    61.93302    61.45050    58.47417    65.54346    56.63291    57.87971
#>  [709]    61.33401    69.10657    59.17047    57.29773    61.13557    64.93003
#>  [715]    56.30587    80.52004    57.98447    59.63276    58.27544    57.18109
#>  [721]    68.89145   143.53309    64.24521   102.98805    50.54531    42.91010
#>  [727]    71.60071   122.83460    59.99706   200.22637    78.14467    61.97135
#>  [733]    62.14309    63.06987    65.46763    61.98015    72.36730    73.41039
#>  [739]    55.81810    52.00099    53.38822    89.87640    58.30377    61.19038
#>  [745]    88.80417    59.40260   118.87304   103.94544  2835.76127    96.85317
#>  [751]    56.19298    60.40500    57.24660    69.06569    61.50641    69.67004
#>  [757]    76.07450    57.07731    62.52759    68.64239    57.91151    88.20972
#>  [763]    84.91498    64.82948    69.27385    96.42903    59.59541    61.25277
#>  [769]    75.54711    59.59253    56.99635    55.02573    69.97143    70.05919
#>  [775]    61.06296    77.20616    55.66483   108.66648    61.27537   166.95967
#>  [781]    59.28928    56.80623   334.37680   134.03398    60.30624    59.77617
#>  [787]   100.24376    70.98333    62.84299   107.27090    65.26279    65.18446
#>  [793]    84.94702  2033.59796  1024.69353    72.67157    62.02860    61.18328
#>  [799]    57.59577    85.48268    83.57455   142.43154    59.36229    75.34229
#>  [805]    57.91228    66.86814    69.35789    68.21548    71.97338    55.77098
#>  [811]    54.04017    68.27242   111.87049    67.44890   164.28254    66.55324
#>  [817]    64.17983    98.80719   200.41933    78.50641    57.82786    56.03639
#>  [823]    57.46727    65.54176   123.83137    73.58659    83.70900    62.20562
#>  [829]    54.62494    59.36843    64.54682    70.99908    93.38238    60.78522
#>  [835]    54.37333    55.54576    73.07318    59.27065    79.75389    52.88235
#>  [841]    62.76501    60.29987    56.09393    65.86020    61.31725    64.61972
#>  [847]    80.01826   102.10279    61.53201    60.93616    58.46570    65.73186
#>  [853]    66.99335   103.09216    61.20769    60.36308    61.98877   108.23715
#>  [859]    79.95570    63.37385    90.35002    69.05963    75.48254    59.49161
#>  [865]    92.58479    57.32038    90.62008    90.84113    60.56938    75.37862
#>  [871]    62.05710    58.10888    79.66843    63.08807    70.37955    74.62855
#>  [877]    66.44502    57.74967    87.52861    69.55889    72.88611    56.24398
#>  [883]    67.89020    57.45560    57.15370    90.73700    60.20787    78.49293
#>  [889]    85.08618    73.36229    63.36862    49.38092    53.66200    62.52322
#>  [895]    55.20726    68.32908    63.88317    55.56136    65.89336    76.13520
#>  [901]    85.67088    59.95553   382.25352    43.46476    66.02823    56.43698
#>  [907]   559.11691   260.15405    69.35313    54.62495   114.49738    58.81494
#>  [913]    61.03363    60.42680    61.78694    89.98907    80.21729    94.97427
#>  [919]    84.65302    80.75177    56.97536    72.27658    57.41664    69.88019
#>  [925]    63.88471    51.79658    61.16271    64.07765    86.61603    63.71193
#>  [931]   108.62576    60.11472    98.13565    64.36533    62.86297   107.85536
#>  [937]   123.52221    63.23798    71.60015    74.73989    81.68483    63.03241
#>  [943]    63.19147    85.92059    57.59936    60.33079    60.73301    59.65692
#>  [949]    78.06833    72.73255    57.70432   108.91581    66.99395    51.51805
#>  [955]    54.22923    62.37909    78.87763    65.27997    53.53930    65.45713
#>  [961]    62.06983    60.94812    58.53328    61.70795    57.99817    58.87855
#>  [967]    81.28440    63.64267    55.70293    64.01880    70.59242    59.09806
#>  [973]    57.65037    69.97217    87.47050    59.87867    71.90213    73.46701
#>  [979]    53.27526    74.16044    56.59185   432.19759    94.98115    58.51541
#>  [985]    57.45593    71.08198    67.51293    72.74512    80.79544    59.22904
#>  [991]    61.36288    56.69096  2958.16121    58.52453    85.08056    64.48193
#>  [997]    68.64974    43.82671    61.84663    61.73766
```
