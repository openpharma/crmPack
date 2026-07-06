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
#>  [1]  52.19352  52.19352  52.19352  52.19352  52.19352  52.19352  52.19352
#>  [8]  32.10539  32.10539  20.50703  21.75186 262.17763  41.77354  35.35429
#> [15]  35.35429  35.35429  35.35429  35.35429  35.35429  88.08545

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
#>    [1]   61.57143   63.60992   61.05911   58.34662   60.96081   66.51320
#>    [7]   57.14634  104.27861  113.20325  173.97787  193.06199  131.84628
#>   [13]   55.40658   97.98041   94.26642  161.85913   63.99573   79.74986
#>   [19]   74.53476   57.78074   52.16903   75.08287  108.84305   51.74980
#>   [25]   76.09700  656.00852   60.99944   58.52141   87.20131   76.49347
#>   [31]   63.15518   65.29738   86.52097   60.28777  179.33711   49.29309
#>   [37]  127.78527   70.00066   95.47960   54.27236  104.10472   65.72476
#>   [43]   62.37132   60.78902   84.24867  140.74828   74.30429   69.01124
#>   [49]   78.27205   66.06028   60.75122   55.78574  102.45983   60.38997
#>   [55]  180.32535  391.11246   70.41865   71.36262   82.77930   77.58674
#>   [61]   66.08308   56.13617   67.95914   61.40226   60.62762   55.42737
#>   [67]   55.26951   60.78385   97.39819   66.44253   65.53926   76.36955
#>   [73]   55.17363   95.42199   63.18233   78.26900   82.75191   61.85828
#>   [79]   62.26654   98.14964   53.06034   61.19151   65.77818   55.34677
#>   [85]  993.44112 3731.40489   73.87850   56.31330   62.61868   56.31264
#>   [91]   65.18062   58.94660   53.81255   59.69230   63.49003   56.11890
#>   [97]   61.14001   85.31617   55.93651   89.28607   49.79682   53.70059
#>  [103]   87.36414   65.38418   61.54244   85.68033   79.06554   72.48000
#>  [109]   60.76326   55.09091   64.09650   57.78417   59.98118   57.96149
#>  [115]   65.41032  225.95943   78.86070   67.77450   57.57619  107.73911
#>  [121]   62.33482   72.57635   96.83296   56.72418   70.58653 4057.01002
#>  [127]   60.64948   51.29413   68.40082  100.22771   45.20338   89.20290
#>  [133]   58.95640 1595.76218  282.96695   71.76148   61.02702   58.00089
#>  [139]  101.61804   89.93536   50.88104   58.15017   62.81100   76.18054
#>  [145]   67.48945   93.60739   98.27766  122.67001  119.19216   63.11354
#>  [151]   61.14137   71.26185  102.74591  102.79013  156.11219   76.69864
#>  [157]   78.91023   64.54741   56.57906   67.94561   86.32590  286.42323
#>  [163]   49.35471   53.50725   73.51201   54.64021   59.88203   61.91628
#>  [169]   61.22920  742.38302  141.16333   66.16604   57.12181   58.23843
#>  [175]   55.55611   58.84949   54.49502   81.83822   52.51759   58.44176
#>  [181]   60.15266   88.18227   62.23351  118.00528  101.98016  188.72979
#>  [187]   51.98697   55.24463  126.31755   60.03676   68.18932   55.58836
#>  [193]   67.72665   64.73484   60.12139  885.90743   56.44183  101.26163
#>  [199]   40.78811   52.42403   67.02069  162.53248   62.87506   58.95961
#>  [205]   74.83037   62.45759   60.34669   65.97798   96.61943   54.57035
#>  [211]  299.57050   48.49758   62.84274   63.66388   63.82106   72.48988
#>  [217]  139.72291 3169.30046  172.35885   58.30661   73.89831   82.55187
#>  [223]   62.94124   58.71340   55.08103   65.91088   62.90875  273.77295
#>  [229]   64.44939  187.10545   59.26684   64.72622  100.89711   82.55349
#>  [235]  175.99456   59.40528   62.03422   66.68753   96.60748   59.52278
#>  [241]   65.38405   88.43945   56.00797  237.30160  162.27938  647.53056
#>  [247]   52.02555   64.50652   69.57076   65.08526  112.06976   76.32861
#>  [253]   53.90043   72.97482   53.64979   68.30519   67.50450   57.39190
#>  [259]   68.66017   75.17634   56.73384   59.76647   63.57364   91.85598
#>  [265]   57.43796   60.08344   55.04875  206.31613  121.76548   71.81125
#>  [271]  110.25139   75.55764   77.47438   52.21634   69.30396   56.97236
#>  [277]  126.03945   56.83279   57.46467   63.58141   56.15218   87.86441
#>  [283]   56.53237  418.72149   80.72769  197.60357  149.10214   59.51632
#>  [289]   69.39001   74.39864  144.80722   91.13273   60.48319   60.25218
#>  [295]   68.09073  703.97337  162.71892   91.39086   57.01687   56.57996
#>  [301]   54.13164  102.04517   51.93886   57.12638   79.68658   64.52546
#>  [307]   55.57836  149.55043   54.41403   63.57984   57.64070   83.05941
#>  [313]   66.00767   59.97713   72.85715   80.75098   66.02041   73.76633
#>  [319]   58.33802   56.65474   87.96914  114.91095   58.45621   77.55568
#>  [325]   49.71070   52.36817   54.51842   62.95598   88.41798   61.26494
#>  [331]   56.93152   68.99288   54.54854   79.55235   59.29410  192.59469
#>  [337]   59.62450   57.03937   49.45451   65.40072   63.53822   82.27817
#>  [343]   61.56438   68.26149   64.47242   64.29655   61.33789   84.66183
#>  [349]   84.94360   66.65197   74.74021   67.40770   79.29671  205.05348
#>  [355]   58.05551   80.27441   52.98051   67.86486   65.86887   70.69328
#>  [361]   52.84710  124.79141   64.12426   74.40870   56.77869   76.81662
#>  [367]   79.35625   73.02963   64.04228   61.40481   57.53750   76.90807
#>  [373]   60.47824   49.86193   85.14938   63.46864  102.76628   69.27773
#>  [379]   76.28665   64.81405   61.07192  937.62748   88.64423   66.43775
#>  [385]   82.05934   57.19796   60.65547   57.24668   67.39544   53.91161
#>  [391]   63.90661   55.43458   69.63973   57.92238   60.07296   59.11578
#>  [397]   75.23856  166.74872   56.51317   61.65828  335.11806   87.58359
#>  [403]   81.16843  663.23361   55.38238   64.71429   66.70910  139.24942
#>  [409]  148.96288 1206.52493   83.80089   60.07658   56.38475   76.12324
#>  [415]   68.18173   94.64319   63.19895   57.65246   60.89540   66.40729
#>  [421]   77.80986 1229.15262   72.84619   52.42474   58.28086   59.92593
#>  [427]   66.02849   68.47613   50.91976   70.12881   59.88267   84.96637
#>  [433]   72.50840   71.55194   95.38273   57.42196   66.07512   83.28930
#>  [439]   40.39644   39.78716   56.38044  138.38519  134.59873   60.39879
#>  [445]   61.36852   66.83969   61.43955   60.83151   59.40643  548.72561
#>  [451]   55.18102  110.65799   87.39093   66.82170  118.52528  451.46334
#>  [457]   57.13515   82.77853   60.62823   88.16002   94.47084  156.83729
#>  [463]   64.68309   71.66998   67.82066   91.32674   59.99521   56.23014
#>  [469]   49.54213   66.93086   90.00733   61.23716   69.14563   55.41396
#>  [475]   56.90478  123.69056   77.00499   58.86246  166.10369   59.03185
#>  [481]   77.52668   56.45170   62.16482   53.23576   72.78459   76.19264
#>  [487]   57.14045   62.79205   64.47813   59.00561   69.17593   76.21516
#>  [493]   64.57810  681.15808   76.30722   88.71890   53.07969   74.25928
#>  [499]   56.80773   75.37107   57.75027   79.38062   60.23741   77.44795
#>  [505]   65.13825   78.58337   63.63173   51.84285   70.73556   58.25526
#>  [511]   60.69175   71.73563   56.71903   63.00978   64.17321   79.40685
#>  [517]   58.18919   60.64574   68.45081  123.43675   75.05984   84.50029
#>  [523]   69.53946   65.75017   59.15871   61.33051   66.33076   71.19227
#>  [529]   59.31673  308.62656  101.64126   72.64818   61.13878   62.99885
#>  [535]   61.42934   61.43070   58.08564   76.85304   59.82139   66.30361
#>  [541]  104.02335   63.90497   59.08096   60.13177   59.92393   91.26131
#>  [547]   56.86348   92.42645   59.20525   64.22272   70.72422   76.13897
#>  [553]   53.70316   61.02253   63.06251   65.70500   75.73122   88.11773
#>  [559]   72.27142  482.72467   64.03569   48.13905   67.12391  170.55712
#>  [565]   94.43513   85.63097   70.87005   96.75969   79.44541   73.84226
#>  [571]   65.11934   65.99272  155.59158   62.25209   59.90522   60.03713
#>  [577]   58.55399   67.01993   73.08313   59.69138   59.38902   79.18880
#>  [583]  104.03112   65.22488   66.93283   83.46695   67.89964   78.81013
#>  [589]   66.35102   75.39504   58.27968   64.01492   57.21692   64.83585
#>  [595]   55.52121   62.34047   59.27840   65.82904   66.26297   84.85127
#>  [601]  287.83442  152.69318   59.06910   63.98595   58.58542   64.76209
#>  [607]   50.43249   59.11754   61.00075  111.66259   77.74514   56.55772
#>  [613]   58.65254   56.14014   42.78815  105.04785   61.30664   61.42720
#>  [619]   87.31352   59.22656   74.34223  189.43885   65.97103  116.34940
#>  [625]   70.17894   70.61504   64.80398   51.98930   66.19977   56.96325
#>  [631]   62.85015   60.42993   57.24411   58.62961   69.72425   58.78214
#>  [637]   85.18290   80.29656   67.89501   70.79765  217.43633   68.39948
#>  [643]   74.77435   58.36750   62.88273  253.05790   61.48201   64.23930
#>  [649]   55.86807   55.86561   53.90034   63.83542  135.15155  503.17916
#>  [655]   81.83208   46.73279   73.83244   84.05820   82.33140   59.18865
#>  [661]   62.24166 2300.23037   68.10869   58.01378   57.08762   78.68776
#>  [667]   55.15358   63.47482   63.17554   61.45322   69.41500  100.38971
#>  [673]   55.32587   59.58751   61.50702   58.05671   60.26814   89.14799
#>  [679]   65.46219   65.56538   65.64443   59.13259   61.19320   62.79941
#>  [685]   65.08836   64.61077   60.92005   58.78774   59.83225   74.00208
#>  [691]   64.01058   56.53468   93.07540   57.82447   67.39670   60.28366
#>  [697]   63.00403  119.14945   75.91720   76.68117   91.14909   83.70234
#>  [703]   53.77560   68.53358  114.51721   83.51555   83.57190   59.37865
#>  [709]   59.70151   60.85275   80.96746   94.32447   72.01036   58.28751
#>  [715]   55.79989  188.07270   62.80487   68.77030   58.87069   93.25229
#>  [721]   72.08641   71.24488   61.03364   55.71095   66.91619   66.11182
#>  [727]   68.63341   52.49101   62.71340   59.19395   45.85198   74.86230
#>  [733]   87.37705   64.03501   62.70131   75.06405   79.15846   49.90810
#>  [739]   62.39148   56.86551   63.42117   82.72456   85.59333   96.94709
#>  [745]  133.46670   72.76381   77.48001   72.85109   67.23951   74.76726
#>  [751]   74.82878   57.34916   56.89678   95.50331   62.74351   70.95499
#>  [757]   90.95828  630.67215   60.11762   63.18487   78.26553   73.96082
#>  [763]   76.15020   66.43078   62.81071   65.59756   57.77609   75.38369
#>  [769]  147.72415   87.52012   61.48414   58.19415   62.89538   75.37248
#>  [775]  460.80514   55.93833   72.51415   67.99331   61.36622  140.30466
#>  [781]   61.79042   56.90917   67.41738   68.70897 2977.66669   66.50054
#>  [787]   80.54602   60.25800   69.36839   70.79281   87.32904   66.37034
#>  [793]   70.41598   81.79342   62.51373   56.50746   69.16159   84.21784
#>  [799]  106.07325   69.15700   59.00344   66.24406   61.47322   68.89387
#>  [805]  120.98010  112.65286  115.72147   55.27067  108.54389   52.96496
#>  [811]   54.36302   53.01723   62.85898   62.36501   57.83395   73.37918
#>  [817] 2055.20423   55.78378   60.08348   55.67115  143.61577  387.71184
#>  [823]   60.98344   78.59918   78.32645  153.40142 5615.33995   54.73118
#>  [829]   61.23855   81.17155   61.72942   74.77105   87.03952   89.29900
#>  [835]   66.54634   53.83694   76.39067   87.66948   98.48411   58.39223
#>  [841]   79.53793   58.58909   63.80753   54.42962  105.31629   60.44506
#>  [847]   60.52872   60.52030   84.26190  137.08424   74.09523   60.08713
#>  [853]   64.48905   56.52076   57.74039   76.42964   59.31918   59.65258
#>  [859]   74.43141  114.89688   78.26963   61.41428  161.26025   61.06553
#>  [865]  110.28225   55.88904   68.27727   64.30689   55.55131   74.14793
#>  [871]   52.35977   73.71486   58.49313   58.92614   58.98718   72.81600
#>  [877]   65.03851   59.46795  218.31977  246.52702   62.50615  115.34251
#>  [883]   56.11417   78.74171   58.33048  165.06626  114.51555  102.84120
#>  [889]  102.08893  136.54693   82.87364   81.03334   69.06041   61.88488
#>  [895]   63.76349   57.08136   69.26570   55.38169   86.35407   68.75306
#>  [901]   69.98495   60.44401   87.05101   63.27155   72.53180   61.14924
#>  [907]   68.48753   75.13384   73.11972   52.85906   60.98157  124.36979
#>  [913]   63.73924   62.16946   65.07495   61.95446   64.45899   55.01964
#>  [919]   54.11343   56.73479   61.35498   79.20842   66.21036   66.10700
#>  [925]   53.25406   62.06696   63.79532   61.89370   66.44528  148.33421
#>  [931]   63.92465   62.34470   69.13055   73.82701   89.74461   76.09627
#>  [937]   63.36871   61.90256   61.72808   57.12428   63.60130   89.88054
#>  [943]   56.27335   67.14137   55.45569   68.92978   59.57976   59.65113
#>  [949]   73.60773   56.93161   56.72841 1852.00234 3173.93167  111.16986
#>  [955]   73.74862   74.95221   52.83314   44.38537   64.53837   87.20655
#>  [961]   79.04155   83.03387   83.92833   99.45890  103.14432   88.49051
#>  [967]   88.80849   56.78001   69.67289   86.23691   64.12974   58.87439
#>  [973]   76.80411   57.97924   56.39416   71.22593   58.72437   64.06289
#>  [979]   67.28718   53.68736   72.29421   79.74608   62.77373   59.16170
#>  [985]   63.89771   68.51347   63.41805   65.37443   57.67524   69.94838
#>  [991]   65.76245   84.35143   75.92042   61.92874  110.24600   56.12204
#>  [997]   62.19596   64.18401   67.54904   62.09550
```
