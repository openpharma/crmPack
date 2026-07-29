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
#>  [1] 108.78427 108.78427  82.37674  82.37674  82.37674  82.37674  54.91513
#>  [8]  54.91513  54.91513 117.26341 117.26341 117.26341 117.26341  24.64881
#> [15]  56.10756 100.71843 100.71843 100.71843  54.71423  54.71423

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
#>    [1]    88.11415    56.19430    60.06670   483.78693   239.75441    56.44799
#>    [7]    64.24386    63.10435    62.08223    61.34616    75.41547    74.52532
#>   [13]    61.69216    84.63620    60.65165    80.15059    56.79612    62.76375
#>   [19]    64.89420    74.13193    59.73246    58.21702    53.70854   113.50746
#>   [25]    59.50322    73.05973    67.68900    89.84768   970.35460    96.28304
#>   [31]    76.77560    63.18584   146.37826    69.27134    65.21065    60.06831
#>   [37]    73.00928    65.05121    86.01642    58.60126    56.43716    63.47322
#>   [43]    59.23942    68.28593    73.31214    89.46148    60.13789    73.50018
#>   [49]  3674.27796  3707.50985   345.22289    54.20161    67.07712    73.49244
#>   [55]    57.85908    78.95095    61.09744    59.77888    56.40620    65.27989
#>   [61]    64.91042    66.02968   112.38325    56.91228   254.68091    77.47352
#>   [67]    53.72805    64.07159    67.68168   165.63563  9170.11928    77.85272
#>   [73]    68.44183    64.09202    53.80591    67.33564    63.47270    61.05702
#>   [79]    66.78786    56.68867    54.92434    62.51366    53.50153   119.27534
#>   [85]    64.37195   123.93943    67.74682    69.18484    58.10324    71.11537
#>   [91]   102.44499    59.71315    51.17565    60.26834    72.92650    73.23435
#>   [97]   141.51992   116.82535    60.57024    67.05888    72.52810    65.40495
#>  [103]   101.27145    81.79957    58.22049    68.33539    58.51423    59.14072
#>  [109]    91.01513    61.02520    73.22781    60.48680    59.78385    56.02750
#>  [115]    63.41249    64.67841   259.18038    46.42550    52.77955    54.93840
#>  [121]    52.45823    74.28209    78.25149    50.06158    60.79006    60.64190
#>  [127]    90.84483    68.99784    65.54767   110.35430    63.59828    64.18383
#>  [133]    51.42050    71.46924    56.16624    60.72766    84.00786    66.37233
#>  [139]    66.04685    57.36793    54.60704    58.19985    57.96104    71.94693
#>  [145]    57.23780    83.38320    59.68022    59.57284    60.17854   108.61485
#>  [151]    54.77892    62.58972    64.66583    60.48831    96.17695    73.63880
#>  [157]    56.45001    59.93354    69.01532    53.27821    73.38524    59.96485
#>  [163]    58.20666    57.02354    62.24221    62.14163   110.95528    57.53285
#>  [169]    83.74879    64.62659    94.59544   262.92739    63.05640    59.74953
#>  [175]    56.73091    56.29248    90.35203  1504.20354    57.27470   102.32716
#>  [181]    56.64410    72.25068    48.04372    54.00263    69.22820    59.66489
#>  [187]    59.19280    54.32203    76.21485   108.41338    88.55360    60.91333
#>  [193]    71.48536   133.51996    70.88549    82.89959    67.27275    62.95804
#>  [199]    78.56702    79.03243   165.29631    58.34798    57.05949    62.34968
#>  [205]    56.06480    58.89755    58.98318    79.88464   139.16174    58.77248
#>  [211]  2792.03411    59.71927    75.83904   128.61675    92.51972   108.60042
#>  [217] 19752.65760    89.01302    55.41302    60.83115    62.42980    63.76526
#>  [223]    78.96586    64.26734    76.11216    96.72448    57.72215   109.12657
#>  [229]    65.15053    60.46793    55.98733    60.85387    57.89450    58.57310
#>  [235]    63.06680    73.37183   174.62986   168.07208    64.82923    52.54177
#>  [241]   203.23265    81.15495    52.43596    59.75873    57.35181    60.00098
#>  [247]    67.78849    91.38686   107.69355    67.21341    58.90390    64.38889
#>  [253]    61.35817    73.48406   107.15643    65.58083    61.01202    57.43500
#>  [259]    63.17655    64.22670    60.57762    90.24848    67.39196    58.20759
#>  [265]    74.12051    64.34916    65.09445    95.50947    61.29831    63.26945
#>  [271]    79.46540    57.47227   202.54347   127.53530    66.04236    60.17460
#>  [277]   101.53392    80.96804   135.85569    79.26702    55.23463    64.42963
#>  [283]    62.40387    65.60957   133.96114    65.12448    61.08586    58.05471
#>  [289]    62.28500    61.52828    60.62898    78.05195   240.50619    73.31374
#>  [295]    63.69141    64.57895    60.49954    78.06371   141.52200    55.28910
#>  [301]    64.93729    75.80424    62.08564    63.48366    61.47276    72.54102
#>  [307]   102.55038    60.03311    71.05133    63.45259    55.79475    60.92798
#>  [313]    85.14106    62.78558    77.01352   171.08698    81.11624    80.48504
#>  [319]    81.11790    65.78357    64.46655    55.56418    53.22542    59.18665
#>  [325]    76.86745    67.85533    54.36233    78.09075   210.94716    58.17353
#>  [331]    49.29830    71.33548    76.36685    62.63849    56.53085    90.44642
#>  [337]    65.64372    70.70462    72.09984  3377.01201    63.60940    56.67696
#>  [343]    65.41359    60.85977    57.70473   102.44801    91.92183    59.29904
#>  [349]   237.70334    69.49241    58.18873    76.84108    58.16708    54.76861
#>  [355]    79.40473    62.04628    77.38934   953.91293    81.42621   118.95897
#>  [361]    61.89808    62.41001    57.98764    84.51774    75.35740    60.07317
#>  [367]   156.40128    62.27202    56.39660    70.56835    64.14215    61.43807
#>  [373]    65.76531    55.61602   135.56786    60.76795    69.60137    80.51916
#>  [379]    96.44172    79.00717    59.99647    64.44931    89.34509    56.52463
#>  [385]    66.37012    58.20008   142.39341    48.30319    74.43843    58.24128
#>  [391]    82.84429   549.20471    57.10986    60.83928    92.90666    53.77934
#>  [397]    70.96117    57.13528    90.13114   111.40652    66.24984    55.45414
#>  [403]    56.35556    58.94004    58.64530   102.65766    82.76581    67.85775
#>  [409]    56.40537    72.37606    54.98912    64.24943    83.59576    62.09431
#>  [415]    63.42873   107.51282    60.20149    56.62848    63.18003    81.72392
#>  [421]    62.37977    72.31775   174.83865   115.67752   165.54872    88.07023
#>  [427]    62.17170    63.35730    66.80062    68.30338    89.77311    61.14462
#>  [433]    85.52664    66.70851    65.37069    58.87843    81.60547    71.40803
#>  [439]    95.00137    57.67062    76.55983    87.43505    53.39864    74.53283
#>  [445]   135.96120   243.32553    58.70466    60.45448    88.31058    85.25103
#>  [451]    56.30317    57.03113    77.27767    56.53000    57.55924    71.29823
#>  [457]    78.67242    72.43357   164.07255    53.99492    89.87914    60.11969
#>  [463]    58.47534    71.90743    60.56641    95.76682    60.06952    60.59098
#>  [469]    66.48211    62.88108    58.63994    64.49988    66.27793    64.07637
#>  [475]   156.18391    62.81628    67.32965    65.05475    54.75977    67.58528
#>  [481]    53.45603    62.35629    52.43926   111.67694    59.58625    56.15635
#>  [487]    58.90450    64.04196    72.00572    86.88550    63.03505    63.44107
#>  [493]    62.02805    61.86346    57.23755    56.22227    68.39221    55.88352
#>  [499]    71.16357    58.11554    96.86224    59.23189    57.33503    64.54792
#>  [505]    65.35719    62.89883    52.25935   134.55472    95.25785    92.33061
#>  [511]    60.44243    59.81548    68.54330    60.61008    64.51702    82.57894
#>  [517]    58.69644    64.57644    71.00516    59.97675    90.39978    60.53340
#>  [523]    66.13842    65.87060   130.04722    79.06969   111.98533    59.53041
#>  [529]    54.29445    93.92053    73.17599    89.91196    61.13920    59.87113
#>  [535]    62.11482    59.98698    54.33301    67.52580    57.94010    65.59786
#>  [541]    62.51894    56.97212    56.60860    92.85668    72.18418    89.54790
#>  [547]    67.05052    54.55821   196.44609   107.40971    71.84312    55.41789
#>  [553]    66.69322   172.97183   259.96903   110.70629   183.85318   813.58019
#>  [559]    64.75684    68.91646    86.37764    59.90468    88.28872    80.25480
#>  [565]    65.62933    66.17429    58.40200    57.89789    58.09906    52.58250
#>  [571]    75.27608    68.24071    64.40190    57.87697    62.14030    72.49969
#>  [577]    63.70646    53.70058    77.57462    55.33633    56.85122    82.82528
#>  [583]   302.16793    60.60914    59.96952    58.24699   118.84859    75.66502
#>  [589]    61.62889    61.95449    75.66292    67.11873    64.22157    53.48053
#>  [595]    69.75952    66.54412    59.70684    58.67432    71.52111    58.15178
#>  [601]    69.86418    79.14835    56.11722    65.11298    71.81270   326.80268
#>  [607]   137.30707   296.09474    52.38647   303.94786   114.22603    56.64975
#>  [613]    59.55604    52.45554    58.91262    58.72347    56.74145    69.09825
#>  [619]   175.40221    54.34999    60.99775    59.49766   111.37256    69.18155
#>  [625]   153.68180   435.90597   308.43622    56.14002    67.01644    59.75105
#>  [631]    71.88858    57.15294    55.22991    72.00221    72.29840   102.09666
#>  [637]    78.16138   114.48192   195.68883   129.14919    53.17527   229.13471
#>  [643]   114.25663   147.27330   103.17354    57.69458    66.75362    64.91899
#>  [649]    86.22300    86.37054    64.14139    81.71696   388.15966    74.06556
#>  [655]    62.80233    61.70230    65.59075    58.61635    57.76468    61.85658
#>  [661]    67.15488    54.22835    65.07023    66.90700    61.92045    63.05673
#>  [667]    66.37939    69.75469    63.44374    91.45659    57.70271    65.21305
#>  [673]    65.16420    76.38523    60.28230    60.69560    70.15846    69.77289
#>  [679]   210.24899    65.75321    63.95323    60.94286    61.52504    63.02286
#>  [685]    56.82152    57.74638    65.23118    95.74207    76.29801    85.16753
#>  [691]    60.92260    58.95056    63.27942    64.43151    62.43106    66.45604
#>  [697]    66.30137    71.10361    59.94131    73.17022    58.54146   129.04174
#>  [703]    81.74647   113.25365    54.75132   105.20469   834.01349   206.54119
#>  [709]    77.94697    58.31781    67.18894    64.39033    83.15168    69.07410
#>  [715]    57.01479    54.17479    67.80320    71.99300    78.08828    74.66753
#>  [721]    68.62667    78.79880    99.51458    68.84662    80.76496    99.97661
#>  [727]   101.64620    59.13482    97.60068   579.71019    59.20496    66.61496
#>  [733]    74.52706    59.14248    84.08867    51.60285    57.81132    55.06983
#>  [739]    79.12615    63.73977   177.16299    54.81432    83.35668    65.60624
#>  [745]    52.99898    59.57463    68.16247   174.87103    65.39209    62.93953
#>  [751]   138.22242    62.62386    59.82606    59.59766    74.35584    88.04334
#>  [757]   129.73420    64.98856   120.56029    58.44054    61.21271   106.97959
#>  [763]    53.62075    55.28540    56.77671    66.07070    57.76705    68.87079
#>  [769]    64.77563    51.04111    85.10955    69.03613    95.70258    68.04343
#>  [775]    78.17780    50.90189    50.76922    54.85778    66.07603    74.52571
#>  [781]    66.35395    60.14046    66.01756    58.80512    74.64925    95.45648
#>  [787]   150.34559    60.37632    61.32807    70.11529    68.39900    63.98426
#>  [793]    62.32766    61.20496   245.26237   147.94035    74.57767    61.03171
#>  [799]    66.65411    63.19460    74.45326    53.92349    57.95148    59.38859
#>  [805]    67.89711    66.44214   334.82776    90.23722    71.30697    57.73118
#>  [811]    80.68451    75.74776    64.30248    66.86069    55.24241    53.70955
#>  [817]    60.41212   144.38659    66.71815    72.04680   122.12140    70.94957
#>  [823]    63.04752    63.98606    65.22365    83.81807    59.74577    65.11475
#>  [829]    61.41821    69.82299    65.62397    57.40844    95.13782    72.80454
#>  [835]    57.19011    68.40176    68.36588    64.37752    56.77771    60.54614
#>  [841]    64.83352    65.07927    78.57059    58.68714    55.75937   195.38497
#>  [847]    57.93285    66.28494    82.79992    60.62613    54.70875    52.01796
#>  [853]    67.16647   661.50365    64.11053    89.21787    71.77057   190.86367
#>  [859]   520.34020    85.15793   213.29876   131.48522    88.50111    56.22995
#>  [865]    57.85962    66.65535    58.77098    60.97097    60.61497    58.59218
#>  [871]    60.41493    58.38479    68.45960    73.49602   106.07181    88.41479
#>  [877]    57.38674    59.17137    53.79402    67.79978    60.95904    58.27917
#>  [883]    55.03195    56.17587    57.86260   113.29373   185.24347    64.74549
#>  [889]    59.37901   213.90854    54.58698   103.97132    88.99907    75.45763
#>  [895]   102.96297    61.62811    58.74528    65.74749    68.20967   105.66822
#>  [901]    62.70457    68.33363    81.75006    54.55016    67.69547    71.81395
#>  [907]   249.92475    53.02218   203.34389    47.77276    88.16809    65.24024
#>  [913]   162.09375    58.36333    58.26771    58.26427    72.66716    61.97503
#>  [919]    60.73511    54.79355  1341.33808    92.43000    64.05167    65.29121
#>  [925]    60.47459    67.73660    82.63375    53.42109    95.46395    59.48670
#>  [931]   137.79001    59.97197    59.02265    74.82732    67.01016    55.52493
#>  [937]    62.87147    64.45114    64.16280    58.16905    79.44137    64.59572
#>  [943]    55.26099    51.97912    56.82702    59.27276    78.07755    63.24572
#>  [949]    66.71111    69.73984    56.42306    60.16258    69.22887   112.47379
#>  [955]   434.28448    58.53507    58.51512    60.63496    58.12332    72.77516
#>  [961]    60.08152    89.31538   134.18828    60.17673   196.94766    56.80197
#>  [967]    58.70585    60.17103    57.20197    61.75700    59.26979    59.50463
#>  [973]    65.94123    87.78635    62.23790    64.46989   107.11479   237.80828
#>  [979]   320.06070    54.70754    57.34235    64.80670    78.89209   158.92090
#>  [985]    68.81178    86.26637   222.30983    61.86168    59.53149    72.89512
#>  [991]    58.89467    73.34706    58.86488    63.22392    77.98530    85.33789
#>  [997]    61.77280    85.90971    65.94959    60.14159
```
