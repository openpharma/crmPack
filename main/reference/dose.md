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
#>  [1] 91.76879 91.76879 48.77461 48.77461 66.72184 69.41276 69.41276 69.41276
#>  [9] 69.41276 69.41276 69.41276 69.41276 69.41276 69.41276 69.41276 69.41276
#> [17] 69.41276 69.41276 69.41276 69.41276

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
#>    [1]   80.08748   80.12032   64.07947  107.11249  131.58905   62.47723
#>    [7]  101.48088   62.50752   56.80680   49.69543   87.02113   69.55022
#>   [13]   68.16435   56.30357  105.19873   57.87780   72.90791   57.59205
#>   [19]   82.02611   71.20836   57.49112   62.63185   59.50505   67.10708
#>   [25]   79.06459   67.37422  174.52885  231.30831  215.54978   62.57347
#>   [31]   57.48749   58.70095   64.71893   53.54973   71.41066   63.40009
#>   [37]   62.02767   60.01397  132.14741   56.31963   53.80647  102.27635
#>   [43]   98.97804   61.54999   85.74488   60.24449   58.52009   61.87631
#>   [49]   78.98867   64.54759  100.36483   68.87960   73.79818   74.00382
#>   [55]  135.15585   66.35830   63.91751   68.23537   79.83496   57.41890
#>   [61]   59.25923   74.10632   76.09970   58.68029  210.50577   51.86951
#>   [67]   58.56559   67.22098   57.30007  103.82129   67.77707   56.10313
#>   [73]  126.62774   88.51564  124.86728   95.74945   64.17086   62.37596
#>   [79]   81.31085   62.35004   95.02720   55.71858   69.16388   77.54900
#>   [85]   59.63132   60.45769   66.39475   94.05132   53.58365  256.38294
#>   [91]   59.88510   63.81323   64.34517   64.69307   64.40535  632.67174
#>   [97] 2883.44953 1389.72719 2314.56259   56.81779   70.05215   57.10579
#>  [103]   56.92571   77.18460   59.30694   58.95138   59.86675   65.36334
#>  [109]   90.02487  108.91099   64.78241   70.80518   85.31101   74.84429
#>  [115]   62.11609   82.90572   65.93504   64.08914   68.45697   64.03946
#>  [121]   58.17544   57.58439   62.39841   55.37059 4937.12494  110.84461
#>  [127]   58.56444   63.75280   62.13432   57.93161   89.25048   61.55812
#>  [133]   73.36580   56.19762  102.99193   74.18652  106.84346   72.84781
#>  [139]   57.14314  197.28349   66.57766   68.43157   58.69645  138.28217
#>  [145]  258.13965   80.80267   60.81499   62.19129   83.79839   95.52695
#>  [151]  509.21511  278.97971  121.48075   74.86669   57.67913   94.03281
#>  [157]   59.87583   74.56322   53.35018   64.50005   66.47462   65.30972
#>  [163]   60.68430  103.46506   55.71294   67.89295   54.96229   55.24083
#>  [169]   61.07523   61.77690   59.06471   74.58187   63.74110   75.91965
#>  [175]   58.18961   61.68792   59.15588   78.96379   60.51601   76.47886
#>  [181]  194.13279   62.78190   71.08677   60.03254   58.75770   60.38869
#>  [187]   77.00328   79.85500   58.80500   68.91167   63.47370  110.62090
#>  [193]  351.51339  232.54669   76.85248   60.01973  116.99055   66.98154
#>  [199]   59.51922  169.12739   67.57797   51.79777   89.80800   50.22919
#>  [205]   60.08920   61.95306   89.22825   83.33024   58.47002   73.43836
#>  [211]   73.60675   85.54727   70.97120   57.84779   54.97010  102.29803
#>  [217]   87.52887   58.77310   61.34160   58.53698   55.54700  113.02976
#>  [223]   60.77955   58.43590   57.78506   62.68428   61.04451   66.18730
#>  [229]   68.26872   55.25898   70.84534   77.76579   56.15929   56.73346
#>  [235]   73.89567  101.59004  317.83772  284.69810   52.75461   53.03715
#>  [241]   83.36729   67.60769  174.67426   63.39410   98.27218   61.51824
#>  [247]   62.77382   57.09784  423.89364   56.01952   70.08827   59.92110
#>  [253]   77.38379   81.36169   64.26735   89.62286  105.40921   66.63032
#>  [259]   61.47401   78.37008   63.32812   86.42627  144.35095  858.87901
#>  [265]  759.53762   99.81888  129.57544   62.19686   56.85921  118.47133
#>  [271]   59.18756   74.89460   68.34450   81.06419   69.59270   77.83527
#>  [277]   64.26073   58.93458   62.85386   60.71689   53.26979   87.50155
#>  [283]   83.92481   62.86259   85.27723   78.28219   59.58772   59.16256
#>  [289]   54.87916   69.02479   83.25686   82.06885   80.84294   68.18781
#>  [295]   63.46345   56.32656   56.23032   62.66221   75.74890   60.57241
#>  [301]   58.42910   57.82205   69.49448   57.37346   61.06560   60.20637
#>  [307]   88.73981   63.26892   78.62487   63.81431  109.02380   67.52139
#>  [313]   53.86612   61.42472   53.38814   64.29315   88.75112   81.61096
#>  [319]   67.12137  108.15708   69.26876   60.78144   61.31748   59.67253
#>  [325]   65.25362   56.92255   61.65317   58.29554  162.07186   65.54783
#>  [331]   65.31471   63.56802   82.30574   56.14407   59.61125   73.10873
#>  [337]   50.01530   87.01679   69.98195   63.34186   77.03087   66.08876
#>  [343]  112.43498   51.97354   93.66044   68.09054   62.72059  110.16177
#>  [349]   88.47433   65.83360   59.09361 4655.54035  434.79531 2810.92459
#>  [355]  214.16655  261.45072   63.21450   81.63261   57.33804   57.04842
#>  [361]  116.04813  233.86154   72.91832  111.07199   72.58309   59.27532
#>  [367]   59.74031   65.04477   62.35795   62.99175   62.58033   73.61403
#>  [373]   59.53573   58.67558   81.58058   82.35335  113.63060   63.52414
#>  [379]   83.99660   58.44579   58.50415   71.79021   56.26768   68.44255
#>  [385]   60.21125   69.73444  127.83529   79.80307   59.35872   55.62067
#>  [391]   79.59989   94.69951   59.73982   81.08779  898.24870 2603.66991
#>  [397]   62.19162   58.53637   58.33195   61.44968   64.54293   83.81312
#>  [403]   52.76609   91.40268   60.07236   73.04585   60.81360   62.98468
#>  [409]   78.99420   66.41896   70.08958   65.01075   76.40423   55.20079
#>  [415]   60.54899   62.48421   69.62242  245.38786   82.62917  113.82827
#>  [421]   59.80811   64.40556   55.72441   61.33629  110.30440   57.13102
#>  [427]   59.94150   53.73367  193.44962  154.28002   56.51287   62.66876
#>  [433]   62.34799   60.01769   65.37216   58.63651   58.54681   58.76230
#>  [439]  169.19837  141.83680   63.33037   58.43049   67.53807   57.94284
#>  [445]  266.00617   65.91718   68.12672   54.18902   70.54502   61.85847
#>  [451]  135.33068   56.31355 1462.84764  147.78762   54.65862  199.52989
#>  [457]   56.61171   59.18825   68.49691   77.04468  170.87533   78.72628
#>  [463]   67.30521  153.74435   70.07563   59.51368   77.51147   71.27993
#>  [469]   66.56222   62.52937   56.30786   55.91629   64.04911   65.25238
#>  [475]  113.01132   86.70036   89.85988   58.17983   66.12568   64.60161
#>  [481]   67.30810   65.85017   76.33406   79.65756  100.33667   52.94922
#>  [487]   63.70153  104.27856  173.77622  142.45347   62.96419   64.01178
#>  [493]   56.71216   67.70645   60.86056   69.43999   64.48915   59.73647
#>  [499]   56.77521   55.30729  119.49123   59.06949   76.28774   57.08824
#>  [505]   66.67178   63.09079   81.09274   60.08269   55.05996  112.96260
#>  [511]   59.13537   65.21209   84.98342  288.00902   70.64985   61.35650
#>  [517]   60.49064   59.33593   59.95698   64.41319   60.11563   56.46943
#>  [523]   60.93556   92.81194   80.85593  102.59208   56.82471  105.71470
#>  [529]   72.81753   78.10191   70.31195   62.20815   72.75642   60.46148
#>  [535]   64.86852   89.72863  121.51532  106.14054   89.32728   62.43778
#>  [541]   74.93837   57.79621   63.52440   80.37725   60.81922   67.05427
#>  [547]   62.67374   87.73816   73.47375  129.06459   80.27605   61.56811
#>  [553]   64.62118  152.90508   69.68446   87.61189  435.89278   72.98352
#>  [559]   59.63695   59.17764   57.71013   61.87354   54.81166   82.13568
#>  [565]   69.58132   55.41973   67.64557   92.20932   87.81131   73.76986
#>  [571]   72.58057  134.59823   90.01550   54.39454   58.52526   57.55436
#>  [577]   60.20950  176.82212   67.86836   57.11574   56.10711  222.66658
#>  [583]  104.83443  275.72714   63.06409   61.72279   91.95168   60.20157
#>  [589]   57.04968   57.37197   62.31591   64.31578  104.53980   59.70957
#>  [595]   64.22183   59.78025   97.28379   91.34945   80.44636   49.36827
#>  [601] 1260.70949   60.35413   64.69711  229.33945   56.94027   62.25087
#>  [607]   64.73816   57.86964   59.69539   56.97146   64.45445   70.38155
#>  [613]   73.10556   64.35477  133.76813   64.04875   60.28771   60.32198
#>  [619]   58.89784   57.99024   64.07725   60.08823   57.85797   58.06176
#>  [625]   60.82523   59.82918   81.89434   60.10640 2181.19276   70.84187
#>  [631]   62.88803   59.94026  186.46110  107.60048  122.01366   72.72837
#>  [637]   87.88609   44.73863   75.05717   64.92758   67.66371  444.97690
#>  [643] 3771.70105  115.41323   55.56564   66.24975  107.94473   73.82672
#>  [649]   86.14921   62.03048   61.27226  262.13386   71.33243   60.12969
#>  [655]   64.59918   93.89805  117.11645   67.93900  105.44238   50.59296
#>  [661]   57.20727   58.23349   79.03948   65.53559   70.46992   72.96059
#>  [667]   55.45866  343.65358   63.78596   63.06219   67.20573   86.51236
#>  [673]  113.81541  127.83443   58.01410   61.30858   66.08771   55.19325
#>  [679]   58.78948   72.54603   76.67751   60.31870  115.64046   63.28937
#>  [685]   59.24760   62.53713   65.34091   59.00799   60.18048   56.74011
#>  [691]   62.81061   68.86054   50.53175   90.95381   61.54347   57.88107
#>  [697]   59.31464   73.81826   77.20670  234.74136   70.71626   56.74844
#>  [703]   64.20526   59.65323   64.82662   63.72647  954.91456   85.93610
#>  [709]   87.36925  158.56542   91.90589   60.46195  105.53102   94.07866
#>  [715]   58.90436  176.90751   77.85991  125.44013   74.98535  473.03091
#>  [721]   63.95922   78.85787   94.01666   64.73796   97.24779  102.67186
#>  [727]   70.91911   59.47833  379.03037  103.99687   75.39644   96.38780
#>  [733]   90.96653   56.62580   61.79898  126.59500  132.86040  132.05478
#>  [739]   59.06594   58.54657   58.25338  126.11857   57.21392   52.14608
#>  [745]   59.75956   61.77933   67.46232   60.15038   59.14270  108.20710
#>  [751]  174.86577   88.93594   65.21910   67.66968  153.91817   52.23751
#>  [757]   63.37461   60.72866  292.80614   57.05659   65.68539   61.53212
#>  [763]   67.41636   57.60459   84.25223   55.25603   57.51675  245.82979
#>  [769]   61.34600   61.10796   63.45211   59.47624   78.92739  708.22212
#>  [775]   71.37299   71.98804  121.27360   59.13931   56.43743   57.08015
#>  [781]   77.64664  108.67436   71.17738   57.36728   66.67133   64.39136
#>  [787]   79.89250   60.37101   83.94842   51.57402   40.97275   58.77400
#>  [793]   55.27104   58.97760   75.99439   62.76326   62.86679  116.34561
#>  [799]   73.28922   70.65290   56.25341  147.89939   62.74738   75.94946
#>  [805]   60.46890   58.10190  210.93703   58.49477   58.77961   56.97611
#>  [811]   66.21857   54.65612   56.07556   66.82241   65.59626   68.72290
#>  [817]   58.99547   75.25469  105.95443   77.11943   53.81064  107.87027
#>  [823]   53.92065   86.75706   66.13664   66.37704   73.84131   61.50654
#>  [829]   61.36716   56.66466   55.58827   62.60444   79.97991   77.29708
#>  [835]   63.71761   58.95730   82.98578  126.45433   57.61291   66.44571
#>  [841]   89.33349   64.80212   68.17137   76.23360   71.71068   64.82375
#>  [847]   87.17280   58.80042   72.73886   85.82854   72.67171   60.37326
#>  [853]   64.18588   56.57726   64.73673   60.32225   76.42645   53.67181
#>  [859]   65.77576   71.31612   59.42498   91.90689   64.27371   68.30901
#>  [865]   52.34916   66.21246   52.75175   67.80981   55.99181  191.88253
#>  [871]   55.73620   97.62510   81.64644   59.09631   82.95183  122.46291
#>  [877]  107.39744   76.08713   61.63708   59.33869   70.20185   67.88346
#>  [883]   59.77970   66.48320   50.86481   93.73075  118.20416  129.20375
#>  [889]  186.75096  241.95543  111.41235   82.29431  138.79764   55.78764
#>  [895]   55.83468   52.15860   64.74193   74.94477   56.74514   67.60864
#>  [901]   58.94538   62.87160   63.71727   70.56489   70.74761   70.34320
#>  [907]   56.23570   58.35505   67.86639   61.57404   65.72257   60.43468
#>  [913]   60.41321   69.37175   57.38960   59.60813   78.32778   80.63147
#>  [919]   54.26942   58.19989   67.54615   66.45224   56.85860   56.97393
#>  [925]   78.95760   64.06676  190.69382   89.65617   58.64362   81.74998
#>  [931]  105.18047   61.53869   47.72154   63.06229   54.64173   61.80980
#>  [937]   74.00097   61.56130   58.92807 1500.24160   86.22851  600.86935
#>  [943]  268.37716   62.56172   58.95595   80.95953   63.10016   61.16422
#>  [949]   96.24487  295.58265   59.25682   62.35292   61.25506   71.69958
#>  [955]   60.47847   59.53021   58.00999   74.07865  129.45735   57.51467
#>  [961]   66.17973   67.97714   59.27274   67.05695   73.24248   57.99699
#>  [967]   81.47033  142.91197   50.84351   72.24279   58.51653   64.74411
#>  [973]   58.88175   69.35292  114.53462   82.57580   57.76673  175.98903
#>  [979]   60.71483   58.94061   67.27213  101.62496   59.36397  114.64415
#>  [985]   63.40491   75.97074  172.29112  102.28201   58.42902  107.67303
#>  [991]   85.73506   62.42225   52.99868   63.76572   82.14297   57.53605
#>  [997]   64.37534   63.99306   71.23856   71.35962
```
