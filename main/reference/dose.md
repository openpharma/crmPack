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
#>  [1] 339.14280 339.14280  39.64234  39.64234  55.48145  55.48145  55.48145
#>  [8]  50.50270  50.50270  50.50270  51.75863  51.75863  51.75863  51.75863
#> [15] 856.86178 856.86178 367.07767 367.07767 367.07767 278.19214

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
#>    [1]   71.35354   58.55901   67.42123   64.46549   66.89479   64.40811
#>    [7]   58.94050   60.48576   58.41772   77.17001   74.15257  104.37121
#>   [13]   63.43494   55.33737   62.24137  314.80315   73.30166   64.43510
#>   [19]   59.74035   72.48803   90.80196   78.71827   53.51053  148.70644
#>   [25]   57.62044   98.66270   62.43903   93.07009   55.27017   65.52057
#>   [31]   54.88234  150.64583  120.51366  203.28190   64.33233   54.91873
#>   [37]   63.79394   59.18331   61.52183  135.89912   57.46660   62.14050
#>   [43]   99.37529  144.11827   87.32269   71.13921   59.70869   63.59625
#>   [49]   65.31591   78.85778   68.44160   63.13702   68.05552   63.85177
#>   [55]   61.46791   73.00163   73.71080   61.99104   55.30410  196.53521
#>   [61]   67.31636   66.84951   66.55277   62.34528   56.78148   69.93089
#>   [67]   55.31799  108.18644   60.54082   69.15073   54.58923   55.33917
#>   [73]   64.92277   57.88035   55.92509   71.68282   78.57769   67.77301
#>   [79]   66.14283   58.77429   73.15558   79.78873   56.34089   57.18326
#>   [85]  519.70745   59.65780   91.14825   71.65140   94.05040   59.04319
#>   [91]   57.43926   72.86330   62.49480   74.62200  238.39190   57.97378
#>   [97]   78.75195   68.87407   62.87643   57.69758   59.43263   90.89017
#>  [103]   60.83274   85.15866  510.17141   69.15559  104.76329   60.01496
#>  [109]  126.20639 2335.78593   67.75852   54.83259   60.52951   63.36273
#>  [115]   80.04813  142.33078   60.33879  136.18213  133.38828   42.87650
#>  [121]   82.18382  155.44744  117.04165   54.78739   61.48770   61.98158
#>  [127]   62.00173   71.09143   65.31214   54.41550   98.46962  296.76041
#>  [133]   64.61539   68.38148 6971.62958   79.69192   65.99761  191.88750
#>  [139]   88.99966   47.75915   59.12623   49.90382   96.69945   52.62632
#>  [145]   56.57001   64.10274   61.51510   65.54938   55.18091   60.39348
#>  [151]   60.93125   66.60012  160.34867  142.85080   58.30490   56.75700
#>  [157]   68.27043   54.76024   60.19010   67.98229   99.80866   57.99036
#>  [163]  114.97995   70.22445   59.40713   88.74757   57.12694   67.42425
#>  [169]  181.96601  105.91280   52.34863   63.77091  120.20646   76.28530
#>  [175]   76.26382   84.76968   57.20706   57.44970   66.81888   60.93699
#>  [181]   99.47129   63.52353  142.94896   58.31844  104.90579   68.05011
#>  [187]   69.70425 1171.97652   79.45883   60.52043   59.67372   84.08216
#>  [193]   61.04528   55.94713   64.49067   68.40655   62.94179   60.25325
#>  [199]   63.93597   72.74511   86.89877  119.46312  146.97252   55.90315
#>  [205]   52.60999   73.24667   78.53464   58.54404 9587.64468  755.49955
#>  [211]   63.77091 4687.95344  104.02511   66.40953   62.27012   75.60491
#>  [217]   65.17760   77.75162  313.13793   74.54273   59.62341   55.72880
#>  [223]   62.20031   69.98355   98.91438  150.30700   85.80959   71.20018
#>  [229]   79.40048 1349.27931   88.85031   60.23465   77.05849   65.31250
#>  [235]   68.24103   54.81886   53.67294   68.92735   60.53797   64.65256
#>  [241]   60.19224  316.78314   56.27086  316.60766   60.94055   72.92605
#>  [247]   67.88509   63.67348   73.29813   56.91729  182.48350   67.41271
#>  [253]   72.77392   65.04080   57.49799  116.12831   53.12031   64.97589
#>  [259]   88.27875   71.20160  260.66220   56.07814   60.85059   61.64395
#>  [265]   85.66859   61.83036   79.55799   66.69093   61.30186  105.86880
#>  [271]   40.55500  123.12310   48.81025  104.25898   52.10951   57.82808
#>  [277]   57.77581   59.11077   56.17306   57.16898   82.30159   61.45665
#>  [283]   67.54645   54.33219  175.69097   58.50892   60.30063  553.53582
#>  [289]   62.32018   59.84471   81.67183   98.87928   61.32290   65.32354
#>  [295]   66.36138   81.08137   63.63598   65.08924   63.75005   55.78560
#>  [301]   60.24414  161.29930  206.55629   67.24307   86.40093   59.30335
#>  [307]   80.70496   76.16434   99.73645   66.78564   50.15358   54.08386
#>  [313]   90.04957   56.01893   59.54778   73.18586   67.16793   71.18794
#>  [319]   69.75775   54.94914   73.98550   83.37811   65.44796   66.20849
#>  [325]   66.83511   64.34111   73.45087  155.75674   62.41186   71.55863
#>  [331]   62.83609   65.39780   60.08205   47.78761   58.96558   55.75974
#>  [337]   60.38293   67.68622  139.90871   52.71951   61.20144   62.19770
#>  [343]   60.81046   73.22616   76.24487   53.97015   63.01244   82.66826
#>  [349]  331.97512   80.48676  259.02770   69.83247   63.39998   54.85494
#>  [355]   64.46275   67.07840   67.67784   55.12027   60.45864   60.71423
#>  [361]   59.80085   62.83702   72.62115   93.28019   58.02951   64.77729
#>  [367]   74.18333   65.61288   62.01112   55.58361   55.05480   67.00860
#>  [373]   60.01668   63.38660   58.05010   82.49686   69.83821   58.71703
#>  [379]   79.55165  145.28657   58.61555   58.92219   64.74661   59.59318
#>  [385]   61.05303   60.95207   59.42583   62.99900  140.48394   56.91568
#>  [391]   64.85004   72.51107   98.54612   85.52733   59.61727   56.07720
#>  [397]   63.32500   77.19919   59.39433  119.39062   69.32074   71.46217
#>  [403]   65.20413   57.93528  114.08278   69.15474   58.29715   58.23520
#>  [409]   79.79562   62.15097   61.01853   55.01251   70.44510   54.71091
#>  [415]   64.22909   57.92687   54.78735   85.51872   62.50124  784.11151
#>  [421]   68.97950  464.16890   75.98387   62.51203   77.53894   66.22887
#>  [427]   59.40376   87.32327   72.92750   53.07903   59.52113   54.13325
#>  [433]   59.11987   59.61747   56.39706   61.70654   64.77782   61.27226
#>  [439]   74.13246   94.97177   54.78473  109.06377   55.21014   72.87886
#>  [445]   74.45844   62.27051   84.05244   79.50273   69.52921   56.94370
#>  [451]   76.59489   70.56396   99.73373   76.11594   75.21383   68.59886
#>  [457]  271.64060  121.35716   71.11987   71.71015   69.62095   98.20415
#>  [463]   66.19276   81.34664   63.44576   62.35588  335.08693  246.65596
#>  [469]   57.25902   59.84006   75.36598   70.34471   78.13770   59.94342
#>  [475]   62.28448   70.58058   70.65193   60.71772   64.51011   59.58919
#>  [481]   54.74928   56.76245   58.26914   68.89728   65.02608   59.67341
#>  [487]   63.65132   61.64281   65.08214   69.01433   71.62597  137.94832
#>  [493]   66.29029   67.99668  154.36713   65.64244   65.10182  142.44229
#>  [499]   68.99983   68.53228  267.60856   54.23934   72.23286   71.32245
#>  [505]   82.78480   63.30096   70.14531   96.22087   96.91189   81.74625
#>  [511]   57.62557   56.66488   72.59715   62.10022   57.51701   57.15973
#>  [517]   74.47908   67.03248   57.97645   74.73419   67.34044   64.63279
#>  [523]  155.01711  145.82496   91.64501   58.75654   59.04754   64.44755
#>  [529]   64.13902   74.58098   60.43213   59.25115   68.11313   80.11816
#>  [535]   50.25091  217.12389   60.95774   73.31433  210.43530   66.65793
#>  [541]   62.96105   68.34423   57.55003   67.94066  429.06795  886.81060
#>  [547]  548.92961  116.01758   70.99439   79.98272   68.56562   58.99200
#>  [553]   58.03230   58.69361   62.11642  120.72096   48.88061  120.04925
#>  [559]   82.30450   61.90473  118.97071   57.11929   58.09147   58.38535
#>  [565]   64.68481   60.94953   61.89512   71.14993   70.13301  154.93247
#>  [571]   56.47636   55.64017  350.22441   63.57826   81.46987  233.15652
#>  [577]   68.26937   93.56769   68.39459   71.03818   66.46843   62.87347
#>  [583]  126.50138   58.00970   62.94743   55.93796  275.51639   75.37510
#>  [589]   70.78722   70.05901   67.83276   75.81536   89.77954   59.56484
#>  [595]  172.27711  124.58003   63.55052   69.49388   58.76285   59.66079
#>  [601]   55.58969  139.77944   59.39473   58.09399   68.86923   96.53961
#>  [607]   91.56581   75.49413   64.89420  109.38348   72.51767  214.07717
#>  [613]   59.34039   76.70791   66.02691  121.18242   56.11339   60.28742
#>  [619]  112.96453   92.46041   60.48283   73.62675  107.36152   52.09518
#>  [625]   67.79614   65.97836   66.04499   55.65757   59.14716   61.47462
#>  [631]   63.17753   85.48670  120.61182   67.02142   62.95849   62.33410
#>  [637]   94.29230   74.97676   57.24831   57.06991   75.08754   71.34031
#>  [643]   62.78305   56.41963   56.58757   55.40467   66.02737   70.79098
#>  [649]   71.13335   75.41048   66.98708   82.11521   71.26236   72.53192
#>  [655]   62.09017   60.76411   66.13170   62.35427  438.20448   56.21763
#>  [661]  285.08063   87.05158   56.92789   95.19326   55.61215   59.69172
#>  [667]   62.27005   59.68909  448.40768   67.48552  115.96338   60.92294
#>  [673]   50.03493   73.00783   94.32787   61.70997   59.94457   64.96911
#>  [679]   52.47303   59.18267   71.13892   86.79769   66.14190   59.20992
#>  [685]   96.11115   58.69649   60.11031  318.13422   61.60255   58.79808
#>  [691]   63.42754   63.82992   60.32411   73.14067   59.19667   79.15577
#>  [697]   58.93398   77.52488   67.63747   62.89653   59.56995   75.51498
#>  [703]   58.49167   64.87426   58.26903   80.62349   58.90638   62.28569
#>  [709]   77.30541   59.93283   64.70675   61.40125   79.03857   61.97547
#>  [715]   73.84679   63.68826   78.49938  102.83649   60.11695   60.39988
#>  [721]   66.21538   75.34516   82.99308   68.78099   57.28732  118.22702
#>  [727]  108.29221   68.42682   58.91904   57.77597   66.19995   60.59857
#>  [733]   74.88839   61.95699   59.81567   63.31233   69.27144   56.65535
#>  [739]   65.93478   66.07336   97.00827   70.48849   65.13635   58.59024
#>  [745]  104.80389   63.19358   84.99325   58.74971   71.85832   59.45618
#>  [751]  121.62504   72.47628   61.27597  203.04709   61.38907   65.37013
#>  [757]   59.10627  103.48026   77.78512   59.58931   55.52263  108.57979
#>  [763]   59.85069   60.10231   95.76712   83.31350   62.86851   67.92590
#>  [769]   64.08910   63.42490   83.30556   62.18436   81.33988   76.58029
#>  [775]   53.20440   69.81757   70.87915   53.47073   52.20762   56.87285
#>  [781]   75.78052   55.10359   64.05959   74.35818   60.72771   59.31399
#>  [787]   59.19222   67.08928   65.04003   68.46468   54.30073   65.12924
#>  [793]   73.63425   64.37260   60.38730   67.19109   69.76916   62.95445
#>  [799]   66.59060   64.21911  118.30826  294.75789   70.30869   76.26016
#>  [805]   88.55796   83.26405  170.52389   65.78329  868.67850 2841.34146
#>  [811]   86.43668   83.85451   59.90748   56.09325   76.73619   76.06406
#>  [817]   81.12830   66.47047   55.73398   72.52757   63.31997   56.72217
#>  [823]   56.67571   58.36374   60.69557   65.16097   74.64354   69.46864
#>  [829]   78.14699   76.08332   52.81229   65.43970   55.09661   56.43409
#>  [835]   82.23876   72.80744  130.81196   63.29754   91.38357   76.39475
#>  [841]   56.88854  109.74146   76.31910   65.55373   57.81485   92.50406
#>  [847]   76.00115  114.96058   55.09594   60.38719  112.49095   57.05752
#>  [853]   65.68919  105.44804   75.94723   58.87340   55.34373   60.80152
#>  [859]   64.59975   79.91720   60.79399   59.48704   74.42288   66.34803
#>  [865]   62.52077   65.00030   61.14767   61.43677   56.04903   62.19323
#>  [871]   67.83374   59.89514  144.01581  135.20620   61.26156   74.13714
#>  [877]   59.39756   71.17686   71.27286   58.43210   57.78274   62.09999
#>  [883]  100.52506   55.97325   57.75951   70.03094   65.07380   77.81598
#>  [889] 1043.05697  168.67248   59.85869   88.83881   58.50993   66.92262
#>  [895]   57.46184   62.12624  112.89462   65.63225   60.50396  101.88308
#>  [901]  162.94297   59.85865   61.41680   54.66950   56.27271  150.86942
#>  [907]   61.38176   91.24107  211.93272   83.18307   52.67522   67.37547
#>  [913]   87.01348   57.80611   70.02008  137.61472  102.06425   87.14240
#>  [919]  916.66699   63.02173  167.71171   63.86736   57.41589   55.38442
#>  [925]   55.67339   55.43338   63.41987   64.87908   62.92724   63.19390
#>  [931]   58.94323   71.59538   86.72236   93.18025   53.20666   77.57364
#>  [937]   67.60458   56.01873   56.31026   80.81331   76.57167   53.75506
#>  [943]  439.80044   52.70092   78.75774   98.55240   85.47830   68.54291
#>  [949]   89.47307   95.94279   58.29906   66.56845   64.42266   77.72774
#>  [955]   57.50128   80.84045   65.48433   92.74474   66.35385   58.71036
#>  [961]   75.16064   63.97881   59.12354   60.87722   49.16742  169.88076
#>  [967]  103.75660  112.21891   59.02392   61.28625  154.45872  165.17181
#>  [973]  122.23253   63.59287   68.61594   54.60293   94.20999   54.78260
#>  [979]   76.06601   58.88857   64.30253  131.80024   66.59523  983.53460
#>  [985]   97.34978  160.67034  109.67778   80.18172   63.34666   59.09118
#>  [991]   62.79414   61.63283  121.39665   59.75778   61.50864  121.91684
#>  [997]   58.26140   71.55113   59.81837  103.16267
```
