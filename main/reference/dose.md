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
#>  [1]  98.59642  65.81517 120.94418 120.94418  75.68118  75.68118  22.94640
#>  [8]  22.94640  22.94640  28.97547  28.97547  17.80466  17.80466  17.80466
#> [15] 108.65733 108.65733 108.65733  82.53730 211.90535 211.90535

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
#>    [1]   83.53155  113.43762   54.72524   75.63692   70.02902   53.90533
#>    [7]   60.44461   90.21551   71.91876   82.05023   61.20122   61.25866
#>   [13]   76.18009   53.55550   67.22990   58.72742   68.34037   58.93887
#>   [19]   57.87785   55.94009  112.21273   56.12347  139.97962   85.62333
#>   [25]   59.66638   81.51293   74.06229   62.34234   60.75068   62.53179
#>   [31]   86.60282   62.16492   55.81521  181.16128   62.62331   75.78447
#>   [37]   82.69358   59.85781  113.86178   56.68726   69.39804   64.77502
#>   [43]   93.24062   60.91772   59.50182  520.22573  127.30042   66.52282
#>   [49]   67.93789   57.45694   62.82395   77.49191  147.27537   59.31181
#>   [55]   83.27273   59.15064   54.90902   61.00884   87.48844  141.19285
#>   [61]  309.86290   80.19849  389.64906   57.23182   59.88134   74.30302
#>   [67]   67.29316   88.07152   58.87179   49.81115  217.15369   66.03903
#>   [73]  229.10753  121.20967  678.33189  372.29076   68.02213   58.51760
#>   [79]   65.06239   64.74103   62.09012   57.91297   87.56167  104.09713
#>   [85]   67.63835   56.74220   60.42882   57.83293   65.72177   63.80877
#>   [91]   86.63379   54.30211  640.34858   54.05956   61.15709  168.82856
#>   [97]   66.27465   64.69318   50.89651   58.40688   86.50178   54.31977
#>  [103]   55.53966   72.04647   66.55680   55.38663   92.15714   68.33638
#>  [109]   56.24444   59.84435   56.81046   60.32765  292.19926 2903.21579
#>  [115]  248.65944   72.38678   80.99779   65.48879   68.89581   58.50330
#>  [121]   73.61994  330.84816   92.42054   57.41917   53.04894   87.97346
#>  [127]   65.86657  136.05267   66.75641   68.89758   73.94984  148.28575
#>  [133]   56.40454   62.85481   61.10142   58.14102  121.28773   59.57928
#>  [139]   59.48492   54.44726   58.16165   62.29268  117.20098  121.87956
#>  [145]   68.81981   59.26191   72.70684   60.70448   57.41056   61.50412
#>  [151]   67.70684   59.24141  128.07708   63.93633  177.86147  114.41918
#>  [157] 2004.85982  180.17306   88.66482   75.08461  157.01878  155.46268
#>  [163]   66.54499   64.18124   69.08871   64.70628   54.81177   65.95046
#>  [169]   58.54631   53.84749   82.15194   61.14955   61.18038   60.10112
#>  [175]   75.20025   60.29979   57.52782 3129.27191   79.33776  105.98520
#>  [181]   55.86270   58.95147   72.59401 4433.98540   66.37401   57.06741
#>  [187]   76.67713   66.06069   58.28470   69.39525   54.71299   62.45322
#>  [193]   59.84889   65.35969   75.87304   58.14004   61.45253   63.93045
#>  [199]   63.47527   59.78824   70.07006  136.92319   57.78086   74.33407
#>  [205]   56.22641   56.79226   64.54800   63.22813   55.93914   57.50854
#>  [211]   67.88800   57.67881   65.67153   61.81197   55.69849   63.99806
#>  [217]   65.82619   68.85719   62.79463   75.27339   72.05486   70.71457
#>  [223]   61.08376   80.91735   56.97978   95.56265   52.41806   86.44095
#>  [229]   65.09081   96.06738  112.43076   63.74844  956.45292   57.22596
#>  [235]   76.04132   85.54657  427.13367  240.91215  148.57051   57.55819
#>  [241]   58.96528   60.79315   56.31804   66.73869   57.42906   54.64926
#>  [247]   68.70173   82.08418  128.62885   63.07867   64.21705   58.17232
#>  [253]   58.72224  101.77883   57.46551   71.57958   50.28064  121.07760
#>  [259]   61.29407  267.32443  192.51020   56.37403   62.40605   70.32290
#>  [265]   72.55122   65.72004   75.37658  104.56275   59.47740   57.22178
#>  [271]  119.15544   61.72996   57.91453   53.85728  404.53676   67.94900
#>  [277]   57.51944   57.83945  111.59397   63.62819   57.78841   61.39667
#>  [283]   60.68192   79.61207   91.20222   62.45239   59.65035   60.01187
#>  [289]   59.25162   72.26926   67.83331   59.20384   67.72270  126.57263
#>  [295]   73.88402  105.70201  361.78086   60.82683   66.14419   65.07148
#>  [301]   69.02066  115.36962   55.99155   58.05936   57.27581   81.08104
#>  [307]   85.82604   60.16663   63.97846   84.69784   62.38236   62.27401
#>  [313]   61.47090   70.22135   63.54095   62.24692   72.20929   85.18696
#>  [319] 1209.86433  103.88761   62.75460  101.40509   53.15131   59.01959
#>  [325]   57.37295   59.77969   70.30633   74.44700   56.96772   61.80911
#>  [331]  115.20782   83.76336   74.74005   58.81045  169.24503   78.89996
#>  [337]   55.50430   67.56569   59.07283   85.63149   67.55907   60.46645
#>  [343]   50.78900   74.26617   59.96454   82.11352  188.12664  255.61696
#>  [349]   63.11907   72.28645   55.60806   77.63725   63.85397   59.15624
#>  [355]  114.71176   63.62839   53.53417   60.38939  162.12561  113.60261
#>  [361]  171.63754  123.43267   53.12066   72.36717   63.78629   90.63179
#>  [367]  121.26436  129.21559   60.10238  134.38547   78.05847   96.35838
#>  [373]   72.36873   55.57437   60.63766   60.91448  243.31279   57.20673
#>  [379]   65.75683   89.94119  101.50691   84.94639  119.68326   63.82558
#>  [385]   71.71160   60.74262   63.08941   67.97069   64.37430  131.50309
#>  [391]   57.81340   59.22698   73.39935   54.68338   87.48148   62.55918
#>  [397]   71.09554   90.50547   81.23811   58.76476   56.29457   61.73421
#>  [403]   58.38274   59.62144   63.49008   61.73876  106.39298   58.82558
#>  [409]   59.80031  256.30553   62.99832   59.21680   54.22384   98.44011
#>  [415]   90.34139   60.56469   57.88270   64.89970  117.37036 1029.08519
#>  [421]   54.82504   57.53537   74.86641   59.00791   97.44843   73.13701
#>  [427]  124.32050  640.92004   69.57087   57.92182   60.50616  105.90677
#>  [433]   62.30803   62.00548   80.24419   63.23237   67.93360   54.55582
#>  [439]   70.47466   93.68630   76.37216   58.89374   69.74629   65.01564
#>  [445]   72.02911   80.23932   67.23997   86.62974   73.20440   67.63451
#>  [451]   61.77518   76.04563   77.80482   64.87959   82.78763   91.67842
#>  [457]   66.48851   60.51558   65.14547   67.81347   59.65694   75.34394
#>  [463]   56.21784   56.48378   62.55668   68.63266   87.59698  212.01143
#>  [469]   57.75901   63.28253   64.59845   53.27001   57.63487  113.01843
#>  [475]   83.31318   61.66848   83.52993   68.41806   62.78686   62.62386
#>  [481]   57.78193   64.74490   71.69855   74.09274   58.90621  231.04419
#>  [487]   69.43866  147.81312   52.38337   81.70390   60.80969   79.54035
#>  [493]   73.70818   55.13339  111.56545   58.49763   62.16530  105.59199
#>  [499]   78.82910   61.88963   60.75627   62.18507   58.34203   66.13916
#>  [505]   64.24440   57.67430   59.50714  109.74695   90.09582   82.70929
#>  [511]   61.51025   63.06590   71.89807  106.86994  137.36396   66.03515
#>  [517]   60.66902   67.30175   76.69334   69.82242   66.32935   60.46286
#>  [523]   68.72202   65.13984  236.86819  600.06022   61.59891   59.54995
#>  [529]   63.18562   57.24682   62.82649   58.53938   62.98906   60.83965
#>  [535]   85.79646   84.62285   60.94749   65.37269   90.55820   55.87029
#>  [541]   63.51070   59.87080   66.36781   77.23794   72.38918   56.77155
#>  [547]   80.71305   57.50814   59.22219   68.68851   59.97469   58.50500
#>  [553]   53.84954   62.76862   68.25478   53.79550   65.78280   56.76743
#>  [559]  157.26234   84.16331   74.00586   61.28035  114.86444  144.40780
#>  [565]  107.77114   62.11961  153.90404   61.36769   54.88615   71.45763
#>  [571]   56.05418   60.53159   75.29333   67.73605   65.94752   78.97573
#>  [577]   77.62177   59.16884   63.01325   73.05876   66.53042   61.96067
#>  [583]   70.13613   68.23372   53.63908   58.89357   67.34178   51.15951
#>  [589]   57.85888   75.72412   77.43039   68.22162   62.11623   66.55932
#>  [595]   58.28914   61.67208   61.53759   61.22079   58.18619   66.08842
#>  [601]   62.51257   57.39105   62.10824   63.39827   54.63988   60.87655
#>  [607]   54.29842   79.90372   58.98661  117.70830   83.25594   66.09625
#>  [613]   63.68956   58.20588   55.42129   64.04403   64.69170   59.49948
#>  [619]   75.69406   56.90434   63.16695   88.37720   79.06164   65.15282
#>  [625]   60.54868   86.58038   94.72381   80.95560   83.79656   54.91309
#>  [631]  153.84368   97.21202   65.54656   56.88443   60.36657   63.82941
#>  [637]   69.62519   59.96142  142.89101  152.44419   87.14246   60.62918
#>  [643]   60.59031   58.81942   71.60377   63.10773   82.17094   56.20133
#>  [649]   71.50434   80.86043   64.81127   61.62338   59.98901   85.52710
#>  [655]   57.44009   68.22896   59.41479  210.27645   53.71108   54.31400
#>  [661]   57.28761   85.19031   76.48578  122.36064   70.58520  105.40128
#>  [667]   63.85529   93.60270   71.04765   56.26708  104.22442   61.71491
#>  [673]   83.93734   61.81056  114.71444   61.86061   57.45423   63.15580
#>  [679]   63.14686  149.17505   88.66102   66.64611   60.28113   62.20792
#>  [685]   56.58894   56.36516   61.83276   77.52297   91.04969   74.17459
#>  [691]   68.04007  116.75139  131.61858  103.49620   63.35649   64.21220
#>  [697]   71.50870   65.19005   62.33935   69.00091   60.62252   65.97374
#>  [703]   53.78756   70.58342   66.55056   68.75215   64.63280   59.54112
#>  [709]   71.93787  109.25708   97.96540  409.33913   99.16558   62.28792
#>  [715]  109.93186   54.09734  104.90533   61.96988   78.19222   52.90892
#>  [721]   58.70726   67.31593   77.64416   58.32087  128.28469   61.26154
#>  [727]   70.14787   76.55911   68.25062   86.96994  225.98547   95.49716
#>  [733]   61.08664   85.12276  101.41763  152.90627  118.42675  411.53797
#>  [739]   56.63652   65.88261   64.09478   66.91938   53.07895   59.82596
#>  [745]   58.92608   61.29607   59.15694   67.48892   76.82106  103.54341
#>  [751]   66.76876   56.88943   58.95309   61.76368  748.40055   59.31169
#>  [757]  384.77452   83.54530   65.10952   62.71729   58.18760   56.51095
#>  [763]  123.18254   88.50317  114.91201   75.20183   80.96458   64.95446
#>  [769]  104.14150   69.90624   80.43403  212.41657   55.23109   78.88568
#>  [775]   68.11746   65.34002   55.24958   58.00230   62.87449   81.96885
#>  [781]   60.93597   60.02145  139.55060  616.07302   61.16325   91.03839
#>  [787]   68.81684   69.93521   64.35780   67.28693   59.37635   61.54328
#>  [793]   63.34322   78.52303   58.37707   57.36916  234.02025   60.37342
#>  [799]   56.01938   55.30468   94.69803   62.17663   55.19868   71.64366
#>  [805]   57.28701   53.64494  144.62532   61.28096   56.25677   63.02377
#>  [811]   60.92822   97.20714   55.95303   65.87407   58.84672   61.20141
#>  [817]   58.98541   65.28757   62.38086   65.77873   64.13272   60.15315
#>  [823]   56.20635   71.24692  102.10231   58.07941   55.93471   62.71652
#>  [829]   60.15758   59.35437   71.01792   79.69309   74.83920   93.06081
#>  [835]   72.91845   62.61997   58.63169   73.15318   76.37429   71.78807
#>  [841]   56.06927   60.65476   58.27297   57.83378   80.92029   57.70480
#>  [847]  105.84642   65.27226   63.23294   89.78785   69.90791   71.49115
#>  [853]   60.27582   71.48258   71.84850   60.03551   57.58781   56.73015
#>  [859]  100.95577   60.81057   56.06917   58.55623   67.85511  142.11920
#>  [865]   60.47221   62.89642   60.08703   59.56753   69.62042   60.96378
#>  [871]   57.45662   61.03831   75.41171   56.05489   67.17871   68.90627
#>  [877]   84.28824   64.96474   99.52980  160.02333   56.32771   54.16056
#>  [883]   71.22302  127.74280   82.81646   63.74959   64.81283   60.78086
#>  [889]   60.52219   56.93199   53.20755   95.99875   55.91863   58.42229
#>  [895]   75.98139  111.96893 9032.45660   63.06316   80.89630   60.73071
#>  [901]   57.83466  108.73377   70.40126   65.71156   60.79724   63.20554
#>  [907]   59.15193  114.46356   66.02112   57.77853   64.90850   66.14329
#>  [913]   58.20119   58.70471   59.73053   62.20522   82.86951 2128.51447
#>  [919]  107.36188   77.62262   70.51417  302.32822   59.82042   58.99231
#>  [925]   63.92448   54.44413   60.53144  121.84629   55.87240  402.08327
#>  [931]  105.58281   59.87281   55.57668   72.40619   56.89913   58.07823
#>  [937]   52.36748   84.59276   67.64612   53.87380   51.18771   57.14292
#>  [943]   56.45998   65.82278  174.00217   70.13176   74.07045   59.01458
#>  [949]   91.19008   76.60379   87.37043   74.02222   55.71268   58.63284
#>  [955]   61.79770  126.79860   60.93596   57.25733   75.14769   63.44396
#>  [961]   80.32444   62.10867   56.98188   65.22890   67.09554   65.27144
#>  [967]   59.54037   65.56143   98.41042   73.62979   72.55086   56.54561
#>  [973]   69.83787  293.72678   62.26858   84.67127  105.56154  205.50677
#>  [979]   51.22718   62.45156  118.61240   56.79235   66.53608   70.06564
#>  [985]   56.56012   78.54139   64.37594   61.07486   64.57768   62.04843
#>  [991]   60.94326   84.74193   72.29795   64.00086   55.50129   62.38128
#>  [997]   72.99596   61.47704   57.60618  102.48031
```
