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
#>  [1]  19.96673  66.55164  66.55164  31.83190  31.83190  77.80634 467.99027
#>  [8] 153.14195 153.14195 153.14195  73.05879  73.05879  77.75142  77.75142
#> [15]  85.08635  85.08635 107.48843 107.48843  85.22925  85.22925

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
#>    [1]   57.68457   55.45603  125.54360   71.43838   55.30368   81.60233
#>    [7]   56.74484   59.06363   57.77388   76.26046   90.82716   65.33874
#>   [13]   58.97693   49.07443   71.53774   75.21552   65.32380  101.90393
#>   [19]  143.55323   57.72982  149.66329   59.02643   74.37007   92.24348
#>   [25]   58.95102   61.85026   73.33857   65.68651   54.66403   59.84633
#>   [31]   99.65523   87.71938  116.41253   78.35944   68.29860   69.67357
#>   [37]   60.86870   59.06664   64.00052   57.19897   93.47278   77.83240
#>   [43]   67.18287   93.63012   55.75919   67.57050   85.02641  161.02944
#>   [49]   57.97613   76.42575   89.99473   69.14737   62.71238   58.83145
#>   [55]  315.06870   61.85419   66.07156   60.49058   57.15445   60.91520
#>   [61]   60.86785   86.91284  188.98838   56.50447   68.18316   72.10867
#>   [67]   66.75565   65.14403   72.65664   54.27690   59.07683   58.76647
#>   [73]   60.32213  269.51070  112.00657   54.04080   56.63218   60.87506
#>   [79]   64.80531   73.37208   84.25805   56.77647   40.77363   47.45916
#>   [85]   64.50246   72.88426   63.09249   53.21745   54.25471   73.90878
#>   [91]   62.88167   95.35904  100.14760   75.78240   84.52552   69.40302
#>   [97]   56.87336   61.94393   95.71919   63.11969  114.93379   57.88003
#>  [103]   59.80675  195.41448   68.54676   85.15861   87.05721   94.94893
#>  [109]   97.24285   65.81492   62.19754   60.05239  207.90581   77.03755
#>  [115]   70.29168   74.75475   58.87435   75.28856   76.11460   80.03972
#>  [121]  162.28655   54.73100   83.48795   68.13413   53.77255  132.69064
#>  [127]   70.24193   53.99012   58.04401  282.94984  188.09945  353.52595
#>  [133] 1656.95280  129.42879  109.75822   56.08762   53.07764  131.64010
#>  [139]   70.82501   56.49678   60.31068   56.24055   56.43305   58.96307
#>  [145]   83.61773   66.43396   56.97417   66.77640   57.79441   57.99991
#>  [151]   79.70677   68.07880   61.17877   68.61082   54.75817   94.22400
#>  [157]   56.54789   65.90094   69.29215  132.58482 2988.32550  106.84103
#>  [163]   56.81753   60.17307   63.47073   65.93838  121.70912  130.68053
#>  [169]   61.22257   52.76835   68.02011   65.65774   59.35607   61.58013
#>  [175]   68.98461   55.25410   65.73815   68.37521   41.73461  108.10362
#>  [181]   57.90204   58.99076   60.36148   55.12991   62.79872   64.03080
#>  [187]   56.45260   59.95585   76.04154  110.33389   82.52100   67.36508
#>  [193]  120.17009   61.38299   66.16804   60.10597   64.67245   56.27816
#>  [199]  289.42849  276.97675   69.72355   59.30325   93.71457  109.13742
#>  [205]   57.94418   56.26563   60.25490   60.33106   92.18354   87.62436
#>  [211]  163.43633  239.32598  165.82252   57.93170   56.10375   55.57215
#>  [217]   70.08231   53.36895   68.11023   59.89253   76.37054   47.34574
#>  [223]   58.36481   57.75680   57.79241   63.82827  123.80214  106.13272
#>  [229]   53.19899   80.68266   75.35124   99.93843  152.99386   68.12236
#>  [235]   76.76168   62.54378   59.40459   55.86272  140.94183  320.08901
#>  [241]  133.21033  403.55685   68.92241  616.57715   69.60171   92.98786
#>  [247]   61.96555   60.71728   86.97037   73.40331   82.57145   60.34587
#>  [253]   69.65664   61.02802   81.85179   62.45742   87.17777   79.86127
#>  [259]   83.61307   90.62649  119.08510   92.67551   80.22181   55.70468
#>  [265]   74.02506   67.79798   68.30371   62.75804   66.11879   57.40331
#>  [271]   62.09202   51.44253   72.88899   72.93977   60.64565   82.97709
#>  [277]   69.71847   60.97127   59.47991   57.75683   58.72504  213.08505
#>  [283]   77.32210   57.33457   64.89258   65.88257   71.53613   57.07450
#>  [289]  254.51280   77.14672   63.28315   57.99219   74.23002   77.81430
#>  [295]   59.11761  124.57983   59.90642   69.11230   91.87829  111.08880
#>  [301]   64.33029   67.81774   61.38613   60.01353   89.95437   59.00523
#>  [307]   61.00676   57.14779   93.51224   57.33380   96.80443   68.19559
#>  [313]   69.04931   50.72066   63.75403  307.19331   91.69811   64.95860
#>  [319]   69.96817  102.98787   72.44279   87.49314   70.77839   56.09506
#>  [325]   59.16534   60.08530   61.65197  105.40119   60.36737   74.74627
#>  [331]   60.82677   65.91794   60.17201  146.70096   53.28292   51.05962
#>  [337]   60.02645   79.79616   81.80113   58.55215  339.97638   55.68064
#>  [343]   60.61272   59.86340  131.88324   55.84350   59.39160   97.01326
#>  [349]  126.07957   68.60824   60.66760   59.39078   57.54917   61.64309
#>  [355]   66.36574   65.71688   59.03415   65.73482   64.74105   66.50630
#>  [361]   71.68587  106.30256   74.81824   60.87367   93.72528   72.04085
#>  [367]   59.77967   68.59759  103.71379   63.20460   56.80236   70.84826
#>  [373]   58.38550   72.74702   83.12913   86.63605   53.91955   72.65629
#>  [379]   78.85180   58.54281   75.39851   79.01815   98.01985   88.21813
#>  [385]   89.86753   85.42260   99.45662   50.89926   54.70657   55.01720
#>  [391]   72.28879   69.11157   88.18975   60.46496   59.79914   62.47998
#>  [397]   56.70335   71.52096   76.19592   63.44720   51.00246   60.98509
#>  [403]   69.28249   61.21428   69.19882  119.27754   61.33027   80.43206
#>  [409]   66.36578   54.23255   57.56015   58.51852   66.58455   65.49228
#>  [415]   91.19997   89.37265   67.77459   67.46923   84.90643  104.08439
#>  [421]  111.33368   96.46253  210.98553   58.18921   59.63330   74.34681
#>  [427]   95.67520  283.92474  124.34956   64.75917   72.05350   60.72068
#>  [433]   57.20493   79.92323   59.70272   59.02744   59.34319  109.66025
#>  [439]   62.10563   71.73416   66.06422   57.63567   63.70990   64.81321
#>  [445]   94.51382   60.26211   68.73289   55.27769   76.05164   60.71770
#>  [451]   68.15093  109.83262   80.12342   60.02760   66.53031   58.13851
#>  [457]   67.38633   63.31566   64.83446   95.85076   65.17601   72.62306
#>  [463]   64.63683   87.84051  145.31173   79.52273   54.97435   72.57819
#>  [469]  120.06988 2691.60155   66.99341   59.84702   58.08778   61.22460
#>  [475]   55.77246   52.62958   59.87767   59.99410   79.07548   52.91387
#>  [481]   53.98219   58.25660   57.84896   58.88532   61.71861   84.74897
#>  [487]   55.50818  800.59395  132.02175   60.08012   59.62375   61.59757
#>  [493]   66.35877   67.66945   64.34664   55.07046   62.36078   68.80338
#>  [499]   61.60042   57.68222   60.16421   66.22486   57.57210   81.01742
#>  [505]   59.08195   66.10738   55.85697  117.11837   52.85088   66.03232
#>  [511]   66.85768   71.18679   67.57188  585.22642  712.03159   81.56452
#>  [517]  131.48965   69.69063   54.67694  198.96060   81.29785   64.91193
#>  [523]   57.11116  103.22251   70.72617   66.09177   56.91270   60.05673
#>  [529]  151.44265   63.72146   89.73188   58.92583   52.64061   99.71090
#>  [535]   49.52477   60.53055  126.99380   56.78951   59.35487   60.55110
#>  [541]   64.82266   79.69129   65.26627   94.66689   62.16155   63.08283
#>  [547]   63.22872  134.87410   69.62696   59.65947   58.83610   85.87598
#>  [553]   70.50202   94.45239   77.21625  110.96119   75.18159   55.84726
#>  [559]   59.34749   60.54773   60.09310   71.56827  832.44628   56.91365
#>  [565]   95.13414   59.17213  152.76035   76.74243   56.44871   58.38827
#>  [571]   66.34311   62.66738  437.24142   66.07100  113.72488   61.96901
#>  [577]  163.57480   59.92037   59.46307   57.30305  118.74820  189.88187
#>  [583]   56.18837   67.17781   45.26905   55.92880  126.26543   61.40680
#>  [589]   70.79912  154.12689   88.64515   79.25549   97.35240   86.85467
#>  [595]   67.65075   54.06393   59.52104  180.56012   60.81320   71.57866
#>  [601]   55.67852   65.00114   61.75650   85.70979   63.03838   62.38741
#>  [607]   64.68701   63.88436   57.34934   65.10587  320.66225  122.18564
#>  [613]   63.29090   61.55674   80.49340   76.09632   89.70082   83.78377
#>  [619]  109.02153  108.75766   59.83747   72.70332   61.96786   61.11609
#>  [625]   60.56043   61.24287   68.47668   88.37205   55.99550  155.25405
#>  [631]   78.88593   72.50006   73.91366   44.73988   53.89770   57.86066
#>  [637]   61.42735   62.91060   67.48828  110.92334   65.23338   58.52785
#>  [643]  112.56525   61.12946  135.99482   57.10640   89.28687   95.08588
#>  [649]  139.20912   63.94363   62.78908   68.31879   80.35214  210.09265
#>  [655]  101.61093   76.53857   99.38047   58.82966   88.82733   54.81840
#>  [661]  203.52066   82.08188 1306.61297   71.59222   84.52668  109.00825
#>  [667]   56.30687   97.03866   76.69599   69.04323  151.39843   97.71453
#>  [673]   58.61771   62.62531   70.34013  117.89686   54.25460   55.69898
#>  [679]   85.55120   61.36005   60.63765   65.49697   55.31445   58.79497
#>  [685]   82.05563   58.27273   74.40095   56.77178   58.89662   85.99606
#>  [691]   65.08096   69.37162   59.03869   58.58433   97.74586   66.40521
#>  [697]   58.76869   67.61426   58.71591   56.52037   55.57509   55.36988
#>  [703]   67.96429   71.73686   61.37571   56.41617   55.28281   65.69821
#>  [709]   82.47719   58.71260   63.59797   62.55567   79.54667  128.40784
#>  [715]   66.49287   63.22101   63.17511   68.39032   65.07845   68.59245
#>  [721]   84.15287   53.71820   65.49304  188.43322   57.58065   61.90101
#>  [727]   60.29996  166.88215  104.11271   74.00763   54.45566   66.28352
#>  [733]   65.36893   68.65268   73.57118   65.75444   75.66075   76.36140
#>  [739]   64.70269   57.10950   60.98361  148.91563  145.31844   56.05854
#>  [745]   67.23171   66.40521   71.14467   67.51601   68.51898   57.44309
#>  [751]   63.37848   63.63910   57.45546   62.57018   62.01114   66.60658
#>  [757]   65.51890  108.04805   67.09011   57.08556   55.39001   54.38754
#>  [763]   57.21964   65.94753   55.67526   57.30921   83.31017   88.10761
#>  [769]   65.15510   55.52160   69.27465   67.84888   58.33381   61.84991
#>  [775]   64.97805   64.47538   64.97799   68.05411   52.43504  112.69398
#>  [781]  104.71538  114.27927   55.28951   52.76012   90.40826   54.00875
#>  [787]   62.79150   77.40324   82.27561  103.45901   57.75618   59.70495
#>  [793]  327.91081   36.82933   52.09364   77.60680   59.91874   56.23533
#>  [799]   55.97248  111.26702   53.30266   60.78560   62.15322   78.87245
#>  [805]   74.80639   62.87366   77.43994  113.56451  131.07550   61.71454
#>  [811]   68.49945   73.85303   66.27865   64.51857   65.71032   69.71625
#>  [817]  115.76971   80.72806   61.89861   65.28251   88.18469   56.32319
#>  [823]  401.13881   65.52105  305.96374   88.29135   63.90031   69.71184
#>  [829]   60.39203   61.80847   83.31759  114.64867   65.04328   56.85672
#>  [835]  112.56284   59.14981  111.26135   65.46517   62.22718   88.07125
#>  [841]   67.96832   81.47599   56.19716  101.91403   58.14816   62.39810
#>  [847]   56.11063   83.96552   89.49467   73.98747   73.02088   67.45142
#>  [853]   74.33797   59.63933  195.52995  122.44134   59.95396   89.85994
#>  [859]   55.23380   94.01827   57.94963   71.48774   66.27290   56.57577
#>  [865]   58.35086   59.67747  155.40317   58.67379   60.51204   59.36617
#>  [871]   64.91990   59.19060   60.75191   61.64968   61.52035   55.32394
#>  [877]   72.67465   56.26760   66.50685  169.48852   69.34729   84.71343
#>  [883]   85.94259   59.71377  213.28701  163.21744   59.08025   65.83368
#>  [889]   56.76618  467.73856   58.78736  104.83073  349.33011   63.00770
#>  [895]   85.96854   59.63893   59.63935   54.59018   63.90224   98.23684
#>  [901]  103.01139   65.99723   68.32968   79.24286   65.83009   56.56521
#>  [907]   67.28155   62.38269   56.32933   59.99779   62.58077   78.62244
#>  [913]   59.48545   80.73956  125.73562   88.97922   66.21691  154.88811
#>  [919]  463.51578  213.17564   52.49227   74.16321  326.20922   90.67636
#>  [925]   67.79795   68.59601   58.65441   73.75993   55.68624   68.92015
#>  [931]   67.62524   56.25557   55.17457   60.98508   58.88677  359.25931
#>  [937]   85.02865  177.17321   52.55566   62.91266   70.61173   66.19435
#>  [943]   54.93295   63.12629   70.14728   52.67598   64.26793   62.31325
#>  [949]   67.73725   66.95190  104.04972   93.49442   64.67384   65.03547
#>  [955]   61.61745   67.83467   75.23411   68.99349   70.40872   79.19300
#>  [961]   61.54720   57.48797   59.42935  201.72961   70.61706   53.84135
#>  [967]  125.36652   56.51476  117.83556   55.14889  220.06288   55.93665
#>  [973]   55.16399   56.85446   55.76254   71.36824   92.95399   63.92275
#>  [979]   57.32815   56.90521   56.23334   62.18521  110.49184  156.92581
#>  [985]  215.33731   70.77484   59.00832   55.87621   69.53996   88.12856
#>  [991]  487.26484  129.91183  105.64743   67.45792   70.48980   55.77480
#>  [997]   61.52980  935.20947   63.59697   75.75663
```
