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
#>  [1] 193.01055  68.06380  84.63628  46.64759  47.38883  47.38883  47.38883
#>  [8]  47.38883  47.38883  47.38883  47.38883  47.38883  47.38883  47.38883
#> [15]  47.38883  47.38883  47.38883  83.17066  74.24831  74.24831

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
#>    [1]    62.93657    57.53390    85.23925    60.13939    59.81231    60.49623
#>    [7]    72.97285    61.96255    59.02319    58.77779    64.06992    66.88743
#>   [13]    73.38527    58.04713   129.32083    57.97293   379.39589    66.53681
#>   [19]    92.03655    51.12468    56.64143    62.99280    66.39622    66.79335
#>   [25]    76.53557   143.43384    53.88428    56.44341    83.51588    57.17690
#>   [31]    64.38944    64.96071   114.22905    56.25917    95.06140    90.52521
#>   [37]    84.78233    62.53126    56.72515   101.34228    53.47175    57.53073
#>   [43]    58.11491    53.48429    53.90304    91.63530    81.51877    81.14796
#>   [49]    76.86877    75.42486   144.10535  1718.33762    67.82996    62.51606
#>   [55]    60.09871    64.77320    80.91268    67.88002    73.08705    57.18001
#>   [61]    55.24425    80.00699    56.58718    65.90510    95.92451    70.22674
#>   [67]    58.01165    74.88973    61.21144    59.15530   111.67962    65.50981
#>   [73]    68.11995   102.88958    91.04753    66.47414    62.44703    64.66025
#>   [79]    76.27908    86.46082   269.49763 10335.89741    63.15644    71.60748
#>   [85]    65.77817   203.29915    55.99374    69.77338    62.17349    63.08122
#>   [91]    59.57591    82.18326    55.32194   108.40419    58.27200   128.13467
#>   [97]    84.44887    59.50579    68.61570    56.32031    62.44366    61.81774
#>  [103]    56.72201    72.02515    57.63906    69.21879    68.14948    59.79727
#>  [109]   241.09352    63.16541    63.37391    54.78704    53.29187    68.39352
#>  [115]    54.42576    72.58107    65.34856    77.14349    67.06194    57.86461
#>  [121]    70.10314    63.72127   142.80821    56.94249    82.92413    66.90795
#>  [127]    58.78209    58.21591    91.27931    63.19082   287.48423   150.89850
#>  [133]  2137.87452   762.60174    61.68157    71.99878    68.09352    84.76117
#>  [139]    77.59996    77.54834    57.14739    66.95070    60.78882   111.80803
#>  [145]   135.64391    61.02783    61.65332    54.15763    65.24058    60.04001
#>  [151]   234.38463   103.45223    84.85714   209.87465    68.14222    64.02643
#>  [157]    69.70353    65.78315    89.61930    72.96937    58.79114    56.23148
#>  [163]  1530.50609    77.09458    58.58102    81.97807    62.72137    97.55728
#>  [169]   179.13578   114.20853    59.56225    75.04746    64.68286   138.88533
#>  [175]    48.97933    65.60103    60.81195    53.45755    65.28560    62.60095
#>  [181]    61.14840    54.24127    63.75688    61.79279    87.91826    60.03765
#>  [187]    79.24843   196.25092    81.43242    63.54604    52.63884    85.52578
#>  [193]    68.39783    73.40432    52.96947    94.83098    53.84288    97.24081
#>  [199]    57.57981    99.50609    96.31419   105.91782   100.49164    62.07287
#>  [205]    58.10418    69.03398   129.08700    58.25598    58.00984    82.95544
#>  [211]    61.10180    79.62349    56.44573    68.24080    66.03446    58.13847
#>  [217]    59.72587    61.69785    77.14693    65.35997    59.87092   223.56511
#>  [223]    58.80025    61.21559    89.90739    60.77234    87.42649    55.55423
#>  [229]    60.95072    74.78557    63.44369   427.01783    61.65788    55.34183
#>  [235]    59.52290    55.80890    63.76104    58.51586   160.06212   636.73890
#>  [241]    59.74134    68.12131    59.87118    75.69479    51.93906    93.44246
#>  [247]    59.07333   118.40163   113.67700    56.23318    68.88863    82.83956
#>  [253]    59.32094    66.96410    62.67180    56.58338   102.25148   104.48587
#>  [259]    57.39103    85.28217    60.16932    56.52272   151.88803    63.30004
#>  [265]    69.39961    63.33897    62.38237   104.34958    90.72063    62.11293
#>  [271]    66.98405    53.17577    63.65162    66.14249 29711.25266   130.25772
#>  [277]    67.29615    69.80392    67.32636   152.21580    71.48679    52.45100
#>  [283]   189.69157    67.85938    72.31305    80.87558    64.05735    55.20107
#>  [289]    95.38399    59.71945    70.03113    62.49138    77.98867    55.75887
#>  [295]   365.59763   322.40363    55.76955    62.47654    79.46991    65.72561
#>  [301]    70.73619    71.37289    79.18334    84.33023   132.09097   173.80316
#>  [307]    88.57372    59.43106    67.57437    65.28386    62.50423   128.41981
#>  [313]   101.10082    55.09517    91.77985   503.43148    58.93708    59.50061
#>  [319]    59.88289    94.85994   112.09463    72.37454    56.96511    63.09669
#>  [325]    66.66293    58.97672    59.76193    69.67860    49.32534   541.52176
#>  [331]   250.96386    58.96220    66.38691    59.42989    73.66517    57.93721
#>  [337]    70.29564   104.56463    70.89446    61.44578    66.70108    59.97474
#>  [343]    58.64749    62.09847    67.42550    59.70522    67.49007    56.70166
#>  [349]    60.54623    89.72879    91.28375    55.59397    58.54821    58.78726
#>  [355]    57.22870    55.51264    67.75182    63.99220    99.08596    76.30635
#>  [361]    58.67933    74.41339    66.08383    81.82902   774.65026   153.31053
#>  [367]    71.74551    56.98552   105.01649    58.78405    93.64877   110.55494
#>  [373]    58.82731    70.53008    57.16775   107.99877   166.92232    57.22768
#>  [379]    58.42325    69.78041    61.72184    87.61892   162.09548    58.31512
#>  [385]    75.73759    50.29069   200.97908   100.41039    61.54897    58.92344
#>  [391]    58.24261    63.24611   118.47181    57.46266    58.91886    59.78453
#>  [397]    75.09396    81.44713    63.38982    61.46222    59.74104    79.87649
#>  [403]    63.22397    56.10082    83.05831    60.84341    56.37981    55.08079
#>  [409]    81.11748    71.58747    59.27617    61.93929    55.31625    61.75387
#>  [415]    52.56606    57.37192    70.14321    59.23604    70.23147    89.34694
#>  [421]    63.34062   149.45223    60.65278    50.38349   121.42750    70.94621
#>  [427]    80.70343    59.23489    59.13204    55.76825    72.30985    84.70804
#>  [433]    56.45458    57.42587    60.01727    62.61743    63.67317    68.12489
#>  [439]    55.40123    59.49465    58.46049    57.39125   289.87913    61.74705
#>  [445]    58.56800   127.42905    84.90187    72.78951    54.23280    66.86798
#>  [451]    79.22462    78.60498    61.41799    60.93441   213.63242    70.30924
#>  [457]    80.18982   105.74120   137.39374  1228.34799    59.17190    58.41438
#>  [463]    64.11074    76.69375    49.91957    56.58825    54.03836    62.40201
#>  [469]    67.44669    99.35067    63.59413    65.80259   459.38489   181.24378
#>  [475]    66.88573    61.58232    83.45461    69.57617   220.34837    81.76572
#>  [481]    62.48823    78.00411    68.74653    61.77685    59.67505    80.18044
#>  [487]    53.39872    80.32401    84.12240    62.45187    99.01801    67.19745
#>  [493]    77.45930    58.87683    62.69964    61.25705    66.26839    56.55604
#>  [499]    68.84235    59.23208    60.81220    60.75112    79.59553    54.72684
#>  [505]    58.30692    64.68416    71.45869   116.23016   120.91730    61.23149
#>  [511]    52.56817    59.37666    67.22107    58.42078   229.73900   212.53508
#>  [517]    61.31579    60.07193    58.68844    58.12527    64.60607    77.16186
#>  [523]   292.12617    56.07232    60.14580    53.33068    65.31717    60.05419
#>  [529]    78.08394    54.37646    56.66416   124.70422    60.45205   119.19557
#>  [535]    58.52736    64.24346   120.35042    68.23964    88.36970    68.79212
#>  [541]    61.74668    66.67410   588.24068   547.22687    53.53827    78.53604
#>  [547]    67.11342   215.08554   101.53454    50.03484   191.03389    82.66603
#>  [553]    57.14510    57.79572    72.50762    63.16050    52.13571    72.42011
#>  [559]    64.32299   121.58479    82.88879    63.02835    64.76272    62.55445
#>  [565]    51.44037    55.29107    61.85234    65.09763    86.26294    60.88004
#>  [571]    75.14163    55.70889    76.30607    55.99145    77.75939    44.81023
#>  [577]    55.55050    58.90032    67.10931   109.72883   110.68932    69.55957
#>  [583]    89.95018    71.97429    57.75657    62.58772    86.99621    70.59288
#>  [589]    63.76852   224.64658    72.41556    56.82355    59.70717    67.12438
#>  [595]    68.85638    60.96291    65.65462    59.18945    56.24640    87.49015
#>  [601]    84.03195    69.22226    56.71947    56.51156    59.77071    57.42924
#>  [607]    70.00027   123.06953   104.69746    86.36579    51.94729   120.73349
#>  [613]    62.07661   137.38602    61.22111    53.84037   111.27513   364.30806
#>  [619]  9023.46543   334.83839    56.99925    68.00653    60.25196    67.65656
#>  [625]    84.35111    69.20771    63.13104    62.30417    61.23110    57.05661
#>  [631]   222.39351  1419.96863   100.25838    63.71279    62.64808    59.04498
#>  [637]    73.32246    74.11632    70.60254    55.17857    58.16778    64.37403
#>  [643]    58.42623    60.64216    58.68879    57.64568    76.90758  1758.41412
#>  [649]   106.60073    73.90457   182.20604   118.60762    59.68836   112.86957
#>  [655]    68.78027    54.92962    59.64363    70.59559    59.96665    84.91729
#>  [661]    74.21200    48.85961    64.21750    78.73224    75.72974    97.85291
#>  [667]   268.28304    66.40955    61.07560    55.38533    53.05862    53.35525
#>  [673]    93.65795    59.18910    58.70615    62.79916    86.47686    81.81038
#>  [679]    86.96755   312.62678    90.49223  1296.68220    77.40553    66.02766
#>  [685]    61.81907    61.10581    49.86654   104.89094    56.52108    94.84848
#>  [691]    97.93481    73.19436    82.88927   315.24985    61.17985    62.66109
#>  [697]   114.60732    69.54753   200.03851    97.77289    70.27330    65.01082
#>  [703]    59.11955    65.77167    79.85957    56.99078    62.62658    69.07718
#>  [709]   127.69236    58.21741    99.11761    64.09832    57.34503    65.12144
#>  [715]    61.93320    72.31470    57.24824    97.42783    55.33806    75.59078
#>  [721]    55.51768   123.80543   109.94132    79.77102    59.15476   106.78318
#>  [727]    64.49542    87.20424    55.36620    65.39829    59.24409    77.66318
#>  [733]    66.64051    77.78288    61.94779    57.49296    69.81201    76.95049
#>  [739]    63.21823    54.55762    53.55013    65.34949    70.93542    79.49058
#>  [745]    64.87924    71.83719    50.36079    57.14914    70.05555    75.09644
#>  [751]    81.59935    64.30772    75.06027   121.99485    62.57438    85.08348
#>  [757]    55.68188    80.08985    65.31538    59.67720    58.82491    62.83530
#>  [763]    73.74520    67.82364    58.65360    90.66504    55.98281    66.22908
#>  [769]    63.50263    56.06671    59.12465    99.07428    66.83531    61.30431
#>  [775]    65.85515    86.19749    57.61442    66.45538    56.46718   103.11475
#>  [781]    70.49494    98.20932    58.44208    59.74439    97.00071    54.99011
#>  [787]    58.59236    57.50832    60.83173    66.55012    61.02279    54.66947
#>  [793]   136.75970   184.14828    42.36032    61.37483    61.88473    55.87553
#>  [799]    75.62507   110.73976    65.28353   108.34212    60.53905    74.52054
#>  [805]    69.03270    62.73928    64.80509    57.22307    69.77578    66.38943
#>  [811]    81.08316    53.61377    53.70683    67.84257    57.72388    64.03643
#>  [817]    67.35580    59.46217    61.51587   372.72311   199.93127    87.85424
#>  [823]   166.30639   190.27880    60.65084    58.20943    72.29284    70.30255
#>  [829]    77.50618    57.50754    67.21935    52.85914    52.15755    97.45460
#>  [835]   106.42949    67.82627    64.34167    68.30255    66.67209    61.82843
#>  [841]    54.22927    57.52101    79.95964    70.64435   105.98907    64.26889
#>  [847]    58.91981    58.46713    67.54504    86.05582   538.71634    64.98784
#>  [853]    59.64879    56.94186    59.99369    69.05242    75.65567   108.02978
#>  [859]    67.31731    64.53338   182.98076    93.67365    65.96983    55.01625
#>  [865]    69.08107    57.25609    72.82189    60.75826    59.74100    60.81348
#>  [871]    79.02481    89.48458   181.40332    66.27062    69.50363    75.91469
#>  [877]    86.40876    68.68017   133.25588    68.84860    80.94426    86.37942
#>  [883]    60.77624    87.12274    65.43212    60.12547    57.11487    71.96445
#>  [889]    62.63902    63.99477    58.48932   232.51903    71.45741    97.65946
#>  [895]    59.28846    61.60572   152.58454    59.84949    68.61794    72.13519
#>  [901]    87.49253    57.23658    60.33578    56.71500    62.85750    58.82110
#>  [907]   185.48675   117.85916    67.75229    77.18295    59.13588    63.55713
#>  [913]    68.09522    56.46428    67.12948    77.04340    60.18677   133.94435
#>  [919]    70.67324    75.08338    70.96536   959.56513    69.56748    57.80630
#>  [925]    70.85669    48.77475   110.29869    60.60076    58.85796    60.03399
#>  [931]    57.77499    75.99205    53.59308    71.56366   194.35786   111.80546
#>  [937]    58.64309    67.93903    72.71979    67.60379    92.81343   124.42968
#>  [943]    52.09267   185.99227    73.19209    60.92596    63.54907    53.82915
#>  [949]    94.47623    76.48684    62.56855    57.98998    60.34149    64.44350
#>  [955]    65.69443    59.63190    56.31932   230.68766    73.62812    53.61942
#>  [961]    56.64462    58.02013    56.46287    60.14289    74.47749   127.87320
#>  [967]    59.27437   102.24713    81.63798    62.06175    89.92786    58.93620
#>  [973]    56.57008    96.09008    73.65011    73.13258    93.38474   111.65248
#>  [979]    59.86224    70.05286    58.17505    68.72614    63.16861    60.59245
#>  [985]    64.31644    58.70707    57.43940    58.40536    78.60239   101.92151
#>  [991]    79.83003    87.78701    69.15422   107.78389    68.49921    56.19261
#>  [997]    95.07297    48.94130    56.13470    72.49983
```
