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
#>  [1]  71.02750  85.44924  85.44924  19.81421  16.61326  59.46016  35.08763
#>  [8]  29.26914  21.90868  21.90868  45.05739  88.10499 106.13048  41.00378
#> [15]  41.00378  19.37875  32.82093  13.39925  39.79529  39.79529

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
#>    [1]   67.34107  987.17686  245.66601   77.60265   57.70303   66.93954
#>    [7]  283.97500   69.02513   66.55479   78.15661   75.93039   76.95261
#>   [13]   54.87799   58.54254   93.98077   75.46104   65.34027   72.60764
#>   [19]   54.94718   57.66657   55.10521   58.08737   70.74865   59.79828
#>   [25]  138.17154   56.74587   60.58154   68.77710   63.03738   88.15504
#>   [31]  100.91537   54.85562   50.72069   43.16386   84.70040   80.81240
#>   [37]   65.67281   59.01528   61.29105   51.18783   65.28918   91.67517
#>   [43]   56.53320   67.17750  153.19214   57.37601   78.03442   79.47860
#>   [49]  103.05070   62.87861  134.11167   61.70660   86.05619   69.92572
#>   [55]   49.03504  106.86904   76.58803   70.37012   81.19280  120.44581
#>   [61]   61.57121   77.58983   54.79907   71.88949   60.82183  134.52973
#>   [67]   60.61522   60.95967   64.02789   58.38113   70.32193   58.53316
#>   [73]   65.11212   57.27443   61.34983   58.42054   57.25419   62.95415
#>   [79]   58.04059   57.59096   92.10713   95.99436  112.96545  104.02998
#>   [85]   58.32712   61.58804   57.95632  135.53402   56.76305   77.28958
#>   [91]   96.11293   60.23079   65.34786   82.21181   62.65712   66.35013
#>   [97]   60.19035   67.64680   56.79156  425.52014   54.62953   53.65060
#>  [103]   65.96521   63.90346   58.95423   95.41991  133.24911   61.62537
#>  [109]   56.14677  177.49080   60.36075   73.84967   82.57266   62.91587
#>  [115]   55.54835   61.13614   64.13353   80.71677   62.70876   69.34778
#>  [121]   59.20454   58.77382  145.05638   73.29937   67.93679   80.05028
#>  [127]   59.19547   62.62761   83.07507   58.00680   60.87008   64.38675
#>  [133]   65.17898   65.06231   60.93181   58.33427   60.86114   58.81707
#>  [139]   57.59231   76.81716   54.65780   62.31545   53.38344   67.80983
#>  [145]   82.57220   71.85553   69.24468   57.10136   60.16745   72.48876
#>  [151]   95.89606  255.18255   62.05698   68.79238   81.12091   55.87914
#>  [157]  131.85592   74.97503   65.98118   88.10471   63.13873   60.74762
#>  [163]   65.21339   72.48858   63.44689   80.43735   59.84295   61.39925
#>  [169]   98.23694   66.17118   60.27844   58.59612   60.72777   80.37927
#>  [175]   99.74304   86.36370   67.79017   57.71251   60.52261   55.54404
#>  [181]  183.73613   89.17675  189.47644   57.02614   70.69369   56.76396
#>  [187]   75.03487   64.70446   84.32715   64.28129   56.67873   59.78164
#>  [193]   99.83044   71.92278   54.90080   69.93274   96.18364  378.74069
#>  [199]   54.94770   60.41761   55.43039   80.69630   57.93869   57.58887
#>  [205]   56.80874   67.51863   65.55877  202.11971   79.20505   63.23690
#>  [211]   57.64332  101.74785   57.29517   58.99239   63.17872   59.48006
#>  [217]   66.98550   65.06972   56.83652   71.27746  111.34452   53.97216
#>  [223]   64.25269   54.17444  107.24466   90.66467   70.18191   59.21290
#>  [229]   74.33730   60.74818   59.90252   65.98990   63.60701   58.86789
#>  [235]   63.74188   65.96817   60.21274   58.40311   83.87655   58.71371
#>  [241]   60.79573   66.15641   57.41361  231.60757   78.78932  132.27952
#>  [247]   83.39755   65.78541   79.86058   55.35983   63.85676   59.12734
#>  [253]   64.76667   57.67669   71.27901   57.53610   81.13368   67.61134
#>  [259]  299.41802   58.60760   60.68784   60.00967   70.32370   77.68522
#>  [265]   67.49487   74.89501   57.28013   70.84221   65.19762   61.58692
#>  [271]   81.24734   65.17544   63.33245   59.88328   55.14225   70.56486
#>  [277]   67.48248  243.47764   63.79114   64.03284   63.30895   64.68769
#>  [283]   92.58728   61.66593   65.09696   57.97439   60.25611   90.63083
#>  [289]   53.51819   57.43830   76.60253  108.58847   70.36828   68.05765
#>  [295]  126.39658  157.88200   61.21208   63.08963   55.89644  124.58515
#>  [301]   59.64630   57.96574   59.04005   94.42767   62.57965  133.36461
#>  [307]   55.03472   58.71856   56.17644   71.22666   64.92984   72.58210
#>  [313]   64.66060   62.65845   56.61773   72.43443   65.25271   71.09557
#>  [319]   69.68985   64.91420   55.66927   70.77085   62.69946   63.07357
#>  [325]   54.95099   62.84161   54.56426   72.95526  109.50196   62.60199
#>  [331]   55.19425   58.05896   69.74079   56.07058   89.49605   61.32343
#>  [337]   70.78227   94.74190   66.76490  107.75192   57.77946  142.77339
#>  [343]   58.75703   54.50300   77.36972   54.93565  150.26877  191.22551
#>  [349]   57.78573   56.07945   57.57641   80.43770   58.50275   56.39932
#>  [355]   82.78345   94.08222   67.38591   89.00723   52.03713  105.20250
#>  [361]   65.33960   71.25606   57.57338   63.65891   65.67717   89.51061
#>  [367]  218.48175   64.74683   63.09399   57.68133   73.21020   73.89342
#>  [373]   78.13681   96.00277   81.39845   59.21603   64.33330   67.33922
#>  [379]   85.14791  120.56893   56.12979   94.51266  436.18092   69.07078
#>  [385]   75.71592   74.59993   73.96980   56.65178   71.46360   99.87077
#>  [391]   73.74155   54.58112   53.73995   61.96432   66.78556   72.67424
#>  [397]   56.98610   67.66191   70.04333   61.94872   61.95953  130.08089
#>  [403]   59.09119   66.84054   61.82507  140.09036   64.04206   70.38305
#>  [409]   55.73677  182.75478   80.96831   89.21001  102.66698   63.50678
#>  [415]  112.18003  107.09262   63.81717  172.74544  252.66946   58.03864
#>  [421]  179.83326   62.35680   71.11603   63.58899  102.99863   61.33711
#>  [427]   80.56020   57.23297   72.52319   70.04916   66.20743   66.89713
#>  [433]   93.57109   69.94814   67.36717   64.14179   57.16625   56.90230
#>  [439]   55.79733   77.66657   57.56807   59.70784   73.77737   55.79036
#>  [445]  114.97251  120.23057   57.35892   65.88403   58.22499   60.34993
#>  [451]   57.63193   55.33681   49.61450   52.84902   65.99885  106.29019
#>  [457]   72.32247   60.43015   60.24704   96.19755   57.76331   63.83041
#>  [463]   60.00817   70.78273   71.49000   66.33052   72.77637   82.62116
#>  [469]   61.88006   70.30208   60.84567   61.30555   59.09166   61.03405
#>  [475]  122.82636   82.44515  369.46731   64.63426   72.13731   69.47658
#>  [481]   69.55078   93.86650   61.63345   83.65017   72.18734   63.98661
#>  [487]   67.94175   72.29872   58.07526   74.96504   52.68413   60.92159
#>  [493]   62.48975   82.78478   74.30755   76.52680   65.24967   65.59778
#>  [499]   71.45041   57.97905   76.15253   59.19411   65.33045   55.54824
#>  [505]   86.45150  111.16598  164.11687  179.71351   62.83388   69.49843
#>  [511]   66.55644   58.62553   73.10024  124.69949   80.41182  430.13935
#>  [517]  128.71825   61.43744   58.13336   59.50301   59.16099   61.35010
#>  [523]   64.97435  101.46583  407.28028 2128.33435   57.64153  143.61118
#>  [529]   58.75699 5646.99862   59.63520   58.41178   86.25203   88.89955
#>  [535]   70.29773   66.20493  102.21202   79.40962  213.92055   59.96190
#>  [541]   69.25138   58.94243   55.12197   68.25014  204.03126   99.83340
#>  [547]   38.74379   36.24943   77.94836   63.47206   62.04383   61.24911
#>  [553]  146.42072   58.82409   83.44854   71.06224  102.15022   52.55940
#>  [559]   56.75598  101.82802   57.36348  146.41969   66.41858   59.71913
#>  [565]   98.34739   55.86959   64.31502   68.72818   48.86046   79.33139
#>  [571]   70.76615   98.24748   56.67748   64.40643   91.80334   62.61423
#>  [577]   69.71841   75.23992   58.37254  108.06344  251.32074   72.70674
#>  [583]   59.68933   73.43481   60.30856  105.79008   53.25645   74.58881
#>  [589]   61.16680   88.53608   83.31717   69.84000 2992.38913   54.40835
#>  [595]   56.77514   53.34321   50.56979   93.30715  162.71675   70.25598
#>  [601]   54.53738   55.75244   53.97329   62.27361  116.32795   59.58640
#>  [607]   65.34125   83.16628   37.62834   59.91469   55.94346   89.98132
#>  [613] 1061.94160   91.81614   60.10637   68.24991   66.58067  127.06169
#>  [619]   68.24733   59.08789   65.61007  143.95763   60.67514   54.54789
#>  [625]   54.45250   56.76666   58.82238  118.51176  101.88402   56.06350
#>  [631]   60.17224   73.65236   56.50790   78.44022   54.13244  113.02018
#>  [637]   84.80797   76.21601   74.14101   61.26483   78.24443   74.57565
#>  [643]   60.01192   57.25355   81.18772   84.41545   51.07074  116.05179
#>  [649]   60.22279   58.45698   93.80145   62.85130  307.91241   56.04604
#>  [655]   64.84171   64.77631   61.67939   58.66331   94.08175   58.88376
#>  [661]   82.20769   60.33556  124.22993   61.93989   95.44720  108.38708
#>  [667]  190.14580   79.60091   69.22986   79.52743   86.21335   58.85229
#>  [673]   63.06280   61.86041   56.08051   57.11119   65.92163   57.77711
#>  [679]  564.91064  106.31892   64.83137  120.30376   72.77973   59.47940
#>  [685]   80.87946   97.06855   71.41619   70.77097   92.77223  103.49942
#>  [691]   71.25628   56.64638   62.73713   57.33929   74.96817  216.26087
#>  [697]   56.66275   90.78301   54.04160   60.55938   88.87131   75.35343
#>  [703]   48.74760  102.25148   82.05572   75.43035   77.09469   91.71252
#>  [709]   60.21210   54.10575  154.42545  111.24450   55.81174   61.99254
#>  [715]   59.03895   77.98335   73.05051   62.53876  105.40364   56.74850
#>  [721]   60.23331   86.24149   75.75481   62.03973   61.32261   60.83083
#>  [727]   58.18491   69.84044   66.76531   58.73054   86.94937   56.50280
#>  [733]   88.79385   85.57604   62.05033   62.21376  180.73330   97.87715
#>  [739]   48.33697   55.18014   94.84928  142.55189  519.55158   48.94052
#>  [745]   68.50160   52.06731   59.32388   85.97864   56.35207   68.83455
#>  [751]   65.33145   62.34590   59.59245   49.50838   57.94468 1680.04145
#>  [757]   56.83632   56.96779   99.27811   62.75292   66.81360   62.68041
#>  [763]   65.17386   75.83727   76.99337   66.38249   68.07892   56.04554
#>  [769]   67.50572   91.47203   70.71869   58.44825   55.20206   76.86897
#>  [775]   60.35111   66.43643   58.46786   57.11838   54.60359   67.74463
#>  [781]   61.94291  108.79574  109.34366  184.39479   54.46344  205.57037
#>  [787]  315.37202  543.15354   72.31903   57.97277   58.52488   89.14596
#>  [793]   66.25128   73.54495   74.37050   59.03541   83.71007   73.42634
#>  [799]   61.59138   66.74039   58.39493  110.19506   53.57494   59.07578
#>  [805]   64.22273   84.03229   61.17787   74.01076  224.47427  102.18362
#>  [811]   60.51816   84.58269   73.42336   63.17745   62.19082   62.62384
#>  [817]   61.99636   58.73633   80.73303   66.93217   58.67481   79.08691
#>  [823]   57.26424   72.46757   59.33754   86.67068   63.10049   59.92839
#>  [829]   61.50227   59.65818   66.18649   59.15020   62.77151   58.56014
#>  [835]   63.59919   62.64450   53.61750   84.61506   62.31661   56.62561
#>  [841]   59.31434   64.29155   67.25094   62.05499   60.12209   58.52134
#>  [847]   66.86609   62.22147   64.63140   62.53631   59.02503   62.16533
#>  [853]   56.25839   73.80621   59.44312   50.28029  109.34411   83.71026
#>  [859]   64.17392   59.80869  101.85540   65.63988  320.35580   94.93178
#>  [865]   63.94258   64.74608   59.51235   60.43546   72.08947  114.97896
#>  [871]   68.81650   70.08835   59.87709   63.56954   71.35486   67.66835
#>  [877]   64.41653   75.43629   66.55998   97.25780   62.36542   68.12460
#>  [883]   53.27703  155.67900  118.84432  189.01051   73.49231   60.38261
#>  [889]   97.29668   69.57539   61.93705  138.74489   64.10840   62.35856
#>  [895]   78.91345   95.42185   66.84134  103.76441   68.61380  111.38560
#>  [901]   65.06269   57.09256   62.94792   58.41114  202.18408   76.59399
#>  [907]   59.86578  161.06876   59.64978   59.46876   55.96505   59.19414
#>  [913]   59.30671   61.00914   60.70905   59.28479   67.65195   66.85949
#>  [919]   81.33635   57.01550   66.30654   73.59336   86.69292   84.06697
#>  [925]   59.01503   54.35466   59.26550   57.21584   73.19202   93.22318
#>  [931]   56.76529   53.20500   60.90067   55.61393   73.13591   62.60122
#>  [937]   57.79487   70.69168   62.30269   61.61386   73.77226  177.44587
#>  [943]   63.32565   54.78521   69.16019   70.42481   74.27480   96.17490
#>  [949]   57.55855   57.43977   56.60467   54.50780   57.42980   58.46332
#>  [955]  141.91703   65.54130   63.42375   55.36997   58.53131   72.81860
#>  [961]  212.37459   57.86589   58.08096   57.55741   74.14744   66.92911
#>  [967]  121.44458   58.38921   55.80167   71.82340   80.92596   53.66630
#>  [973]   70.91330  107.19275   68.93395   60.58530   78.85262   66.05372
#>  [979]  142.00392   94.67703   62.40892   78.40579   89.44044   64.84794
#>  [985]   65.41754   62.97231  167.45090   60.97390   69.29695   65.58674
#>  [991]   84.78210   67.34128   56.61223   62.85160   73.27365   72.11462
#>  [997]   66.12612   68.98527   62.12860   56.74488
```
