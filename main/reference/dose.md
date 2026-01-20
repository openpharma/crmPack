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
#>  [1]  36.32564  36.32564  57.58049  57.58049  57.58049  57.58049  57.58049
#>  [8]  70.97010 100.28598  41.03216  40.42898  40.42898 152.69842  11.67794
#> [15]  43.59518  43.59518  43.59518  42.75238  53.79434  46.15210

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
#>    [1]    286.82257     88.49029     63.95274     58.38845     55.61789
#>    [6]     96.57760    181.88953     96.76791     78.08399     71.82397
#>   [11]     59.85294     79.32139     57.41438     57.33077     51.38024
#>   [16]     59.70547     61.24545     58.67672     67.04989     62.48203
#>   [21]     92.84690     69.03145     60.81335     62.43724     58.70755
#>   [26]     64.13819     85.03263     57.52158    147.95605  26422.23294
#>   [31]    137.52974     93.82916     52.59929     63.19258     56.35329
#>   [36]     56.91066     62.29996    109.00765     76.69788     65.14978
#>   [41]     96.82334    140.98207     62.89609     91.21465     65.74951
#>   [46]    134.07435     58.37981     75.69385     72.43914     62.30655
#>   [51]     63.68383     62.24694    584.49637    143.73041    126.14846
#>   [56]     85.80673     73.06346     56.58000     75.78208     65.36582
#>   [61]    337.88431    165.08661     81.22688     58.17870     64.26788
#>   [66]     79.56718     63.36591     57.17644     71.01381     66.51972
#>   [71]     62.68345    213.00803     62.51874     57.63273     67.39075
#>   [76]     59.17558     76.64940     78.89109    116.80619     57.27545
#>   [81]     68.52449     60.60669    283.02515    107.36648     87.95963
#>   [86]     70.79853     60.97450     47.81990     56.55420     59.42187
#>   [91]     62.30566    181.72552     80.66669     58.77368     59.97025
#>   [96]    163.53989     75.69860     59.45228     75.31582     65.70093
#>  [101]     60.59122     59.14270     59.92440     79.41255     72.53317
#>  [106]     63.34994     59.02874     50.61361     72.15567     52.52634
#>  [111]     62.59695     60.05100    132.65795     61.54061    392.73544
#>  [116]     93.10256     59.85613     68.21671     83.49674     57.51130
#>  [121]     58.47590     59.51619     60.24135   2005.11649     66.79399
#>  [126]     74.40905     85.45344     62.66567    104.79881     69.82642
#>  [131]     76.50145     74.43381     56.35479     63.63139     56.64364
#>  [136]     61.92037     61.53614     73.92593     57.23976     58.88731
#>  [141]     61.73967    132.32984     57.79481     69.42857     73.99328
#>  [146]     58.82534     86.99716     59.84729     64.35038     67.63647
#>  [151]     62.96403     63.98369     58.87837    105.63810   1985.33105
#>  [156]     64.00106     68.98691     62.39240     57.46817    135.16252
#>  [161]    107.04290     94.96001     99.65092     59.34857     59.31798
#>  [166]     80.97136     77.28506     54.13652   3224.31641   2334.89806
#>  [171]     90.65274     55.02933     62.08340     67.43072    113.66552
#>  [176]    197.46694     58.68636     56.63257     71.62793     58.48319
#>  [181]     81.03949     65.97797    108.04766    413.51560     53.85263
#>  [186]     89.37129     77.54744     66.28095     56.01023    105.34641
#>  [191]     86.70922     68.96382     56.09639     76.09642     66.45944
#>  [196]     62.21114     59.58141     95.15512     62.61757     59.20663
#>  [201]     87.12342     57.94126     59.84369     79.23137     61.15483
#>  [206]     55.98442     79.65192     62.90891     78.84781     54.24291
#>  [211]     54.26247     66.60992     61.90398     58.05488     75.63889
#>  [216]     71.57870     54.92193     62.97990     95.38554     41.03408
#>  [221]     61.54698     78.10125    231.68927    471.73996     86.53813
#>  [226]     72.55554     61.51552    129.48654     87.30122     74.19524
#>  [231]     82.19434    111.34900     66.95461    286.25047     76.08853
#>  [236]     44.57982     64.32898     98.86480     62.00651     65.51870
#>  [241]     62.61317     57.04334     61.13300     79.52218     61.34289
#>  [246]     59.97988     73.37926     67.18320     75.83684     65.06598
#>  [251]     61.56707     67.21960     60.46953     54.60584     61.22956
#>  [256]     87.37483     72.39197     55.96115     68.97886    100.77559
#>  [261]     67.22780     60.43848     59.10597     66.97573     60.10155
#>  [266]     83.91255     75.59811     78.67095     59.53476     56.49338
#>  [271]    196.43042     58.52237     71.53158    172.68451     96.92927
#>  [276]     55.06560     59.22177     61.29795     63.01927     60.44405
#>  [281]     60.05482     57.66415     97.17653     75.38256     65.54922
#>  [286]     56.74450     61.17753    939.01985     54.23435     60.79469
#>  [291]     67.19181     58.91322     52.29405     61.37787    227.80474
#>  [296]     56.88644     59.82730    117.19212    139.05253    149.32679
#>  [301]    126.54738     61.35408     62.53145     72.96402     74.20566
#>  [306]     67.84824     51.37237     71.42963     60.54342     55.30237
#>  [311]     61.84591     67.57029    176.51610     79.12698     57.89747
#>  [316]     58.22620     92.17648     47.27647     61.29195     93.85189
#>  [321]    109.62602     54.40748     59.96634     64.15606     86.27635
#>  [326]     63.22654    979.76233     66.51170     59.50886     57.16741
#>  [331]    136.69912    105.80123    125.23266     60.36765     65.05441
#>  [336]     67.71820     50.79196     59.42799     54.74443    114.74140
#>  [341]     60.37026    302.02160     54.18149     63.73225     63.66862
#>  [346]     76.15782     55.15453     58.32420     57.90252     69.17845
#>  [351]     60.89016     63.68095    110.60448    101.08733     99.01151
#>  [356]     60.26216     67.48281     70.25757     66.29369     84.17439
#>  [361]     54.91253     58.69122     59.54650    116.83453     65.53772
#>  [366]     55.33090     58.89462     84.47264     75.98058     61.99614
#>  [371]    267.39349     52.50460     59.86914     60.17115     58.93362
#>  [376]     61.35265     57.96113     55.49470     72.54484     61.00443
#>  [381]     88.41572     61.94743     57.52598     59.51892     56.99216
#>  [386]     58.05231     65.91974     61.55433    129.01928     66.79834
#>  [391]     60.28309     63.48330     87.85674     58.78136     57.09607
#>  [396]     60.31901     61.46371     56.80340     59.18856     63.70133
#>  [401]     66.84646     72.02095     85.32043     65.54523     57.68985
#>  [406]     62.49966     66.51316     64.58693     69.09536     59.46396
#>  [411]     58.22910     58.92634     62.80820     61.17674     61.41336
#>  [416]     58.64065     57.98264     64.06306     65.38741     62.85270
#>  [421]     82.49158     69.66714    226.62863     56.27100     58.15527
#>  [426]     59.99354    208.19105     62.21238     75.94057    108.66043
#>  [431]     77.59980     65.08385     59.10301   1234.36511     94.26767
#>  [436]    102.19881     94.49529     57.15338     55.16435     56.50438
#>  [441]     59.60971    105.30224    373.33654     64.75219     58.04302
#>  [446]     74.19389     73.03120     60.41485     89.36214     96.92381
#>  [451]     57.87583    105.65996    102.71385     93.29130     62.07449
#>  [456]     56.52264    279.30476    303.90143     58.85541     77.46669
#>  [461]     61.56251     59.16234     75.92805    112.83744     68.83879
#>  [466]     59.95637     62.53376     80.34308     59.81284     89.33018
#>  [471]    122.33796    395.59275     53.51572     58.83377     70.97007
#>  [476]     55.37773     58.67550     59.47206     84.02824     85.20775
#>  [481]     85.91792     60.92422     57.54859     59.89693     63.40413
#>  [486]     58.62320     72.40018     49.15990     56.42972     77.15888
#>  [491]    129.61681     62.15944    185.97102    113.02305     57.05612
#>  [496]     64.58183     61.59052     94.27059     60.21085     70.56233
#>  [501]    158.18419     55.42358     50.20797     55.74591     83.88558
#>  [506]     70.08214     70.17401     62.65579     63.51406     71.32589
#>  [511]     49.84248     82.27959    100.06127     61.67010     61.42968
#>  [516]     66.72174    203.32525     61.13389     56.04578     60.35962
#>  [521]     57.04590     56.49352    113.43574     59.58621     57.39693
#>  [526]     56.45869     69.48455    128.35563     75.74907     91.56900
#>  [531]    970.53514     62.65035     63.19050     74.98097     68.41655
#>  [536]     66.67955     73.16394     58.13307     93.42834     50.47982
#>  [541]     60.39324   3069.60283     69.56695     68.49249     60.29051
#>  [546]     66.04294     56.06081     68.44696     91.72182     56.74372
#>  [551]     66.48105     68.10479     66.22734     56.34251     60.94830
#>  [556]     72.72672     67.26237     64.47457     68.37795    112.62727
#>  [561]     68.71214     73.62574    104.13390     79.00625    133.25608
#>  [566]     53.98312     70.95917     60.06474     60.18910     74.00202
#>  [571]     72.40074     52.62112     57.09137     65.30189     65.21660
#>  [576]     72.82574    226.59930    130.78961     61.58764     63.12456
#>  [581]     58.85540     64.74084    133.20130     83.93920     68.31498
#>  [586]     97.98878     61.28563     61.60908     58.68496     57.87811
#>  [591]     59.26196     85.04193     63.75057     69.33428     68.95762
#>  [596]     61.87754     66.91203     47.81208    106.13243    181.41055
#>  [601]    197.78630     88.52839     69.21521     66.37810    126.05464
#>  [606]     69.21368     53.48704     62.39849     62.60910    105.18998
#>  [611]     61.32282     55.47788     59.72376     63.38178     65.63929
#>  [616]     88.73825     59.91871     55.11583     64.42696     84.60699
#>  [621]     47.98940    139.21094     79.67374     62.69995     64.34749
#>  [626]     54.40650     62.55429     66.48208     61.93379     61.31399
#>  [631]     59.08360     58.51827     57.30965     53.35248    534.50568
#>  [636]     60.78247    176.57893     95.76159     60.91878    132.06004
#>  [641]     68.04064     59.23414     69.83794     57.31424    112.55440
#>  [646]     61.74532     59.59681     76.32620     89.21850     56.26890
#>  [651]     61.33355    200.20196     61.46304     61.66119     65.45311
#>  [656]     61.56591    599.63144     54.68650     68.04575     87.26696
#>  [661]     82.17396     63.60904    442.33135     74.15090     67.37726
#>  [666]     71.17976     66.18162     50.17221     60.67818     62.59694
#>  [671]     74.14099     68.43249     72.00446    157.72378     74.71926
#>  [676]     77.43407     78.65734     70.08544     46.78929     68.40765
#>  [681]     67.84604     57.67612    700.92692     98.75388     56.24666
#>  [686]   1465.87685     53.03185     57.92188     71.33206     65.00440
#>  [691]     61.83049     58.28709     57.75688     59.04518    105.72923
#>  [696]     59.81208     62.83204     59.31811     61.79677     54.14932
#>  [701]     58.24643    650.36664     69.21529     57.62938     82.66407
#>  [706]     67.81287     64.78482     72.38850     57.73617     57.76236
#>  [711]     70.34543     56.87859     54.78715     63.06105    150.38416
#>  [716]     77.55697     65.74193     62.68232     55.98804     59.55004
#>  [721]     61.31817     75.81689     68.48730     80.42079     61.61930
#>  [726]     64.57734     53.17470     70.99026     56.45552     63.86501
#>  [731]     85.15041     59.69682     57.72371     77.98876     55.80565
#>  [736]    104.31015     79.39440     62.53544     56.60785     59.01815
#>  [741]     58.84341     57.14723     96.22343     54.31231     58.10066
#>  [746]     66.65751     57.57656     91.76495     60.31138     74.48949
#>  [751]     52.86516     72.24088     84.11068     55.74277    163.08303
#>  [756]     72.66346     64.82541     63.31277     73.26849     58.82741
#>  [761]     60.77505     68.34009     61.67511     57.19090     71.59542
#>  [766]     66.46208     62.38670     59.07021     70.87393     54.61922
#>  [771]     68.06679     57.73469     55.24184     65.43672     62.65310
#>  [776]     90.66256    690.62078    245.05500     64.55181     71.58039
#>  [781]     71.46244     66.42898     62.73817    138.31765    152.69898
#>  [786]     57.01016    101.82507     59.78038     96.32584     66.88838
#>  [791]     69.70031     78.13456     83.09384     74.78966    115.28662
#>  [796]     73.10515     84.83191     47.15727     65.02610    172.01597
#>  [801]   2359.15497     62.27664    200.40151     63.24923     56.32812
#>  [806]     62.48959     56.54976     55.52684     57.56209     56.25115
#>  [811]     55.67291     73.40775     56.34878     56.93666     60.96186
#>  [816]     75.17154     76.41500     67.49920     71.83261     63.18989
#>  [821]    118.59645     57.40559     56.73311     69.96654     56.77368
#>  [826]     71.35729    103.78537     53.54490     65.27481     60.47553
#>  [831]     73.73314     64.38902     58.23227     66.12944     59.36549
#>  [836]    125.43921    214.37949     85.94608     53.03786    104.95765
#>  [841]     67.38105    104.82888     65.05255     67.04016     58.48332
#>  [846]     57.16868   1826.46128    487.32840     54.64307     64.79730
#>  [851]     82.96837     75.67240     61.26863     79.49718     59.07921
#>  [856]     59.47854     63.33601     63.24915     50.79171    438.87499
#>  [861]     86.36926    383.81554    208.38792    179.45759     62.41078
#>  [866]     79.49195     92.27653     77.45025     63.39108     70.51335
#>  [871]     63.11660     78.88149     61.78472     64.85365     53.84346
#>  [876]     88.90481     64.81393     68.50077     55.18454     60.40012
#>  [881]     54.31655     62.06851     66.46906     59.29122     63.39661
#>  [886]    284.68921     56.62514     59.66219     68.71336     76.63089
#>  [891]     72.07251     66.46553     92.39146    390.51027    100.30738
#>  [896]   3077.21698     53.46881    137.04076     60.52447    259.32407
#>  [901]     55.59144     74.16080     67.82862     67.49048     65.86809
#>  [906]     57.26232     75.69032     57.88949    138.31417     51.60520
#>  [911]    208.45475     60.71456     62.73650     69.66729     94.29597
#>  [916]     60.52894    109.30968     59.30839     63.33089     56.40670
#>  [921]     51.38106     78.15082     61.69008     61.39128     70.31148
#>  [926]     77.46187     63.11088     72.89264     68.57410     71.03088
#>  [931]     58.38766     87.96058     60.64210     53.16436     78.95228
#>  [936]     63.96189     77.74331     59.06517     80.26563     60.76638
#>  [941]     66.55959     71.30130     61.87151     59.59571     88.20348
#>  [946]     69.66688     81.29273    527.28059     60.50173     90.22705
#>  [951]     56.81654     77.18975    109.37918    169.15609 114899.93066
#>  [956]    884.62110     92.81217     76.98254     73.31515     59.81158
#>  [961]     64.86385     63.63607     63.89365     65.72348     61.14345
#>  [966]     65.48497     61.44591     57.18292     64.04938     59.70251
#>  [971]     61.40159    343.68449     66.50117     75.97656    116.78887
#>  [976]     61.74899     81.24083    120.81564     62.75387     60.52074
#>  [981]     71.25940     70.59751     55.25571     75.50942     59.11971
#>  [986]    147.88983     79.31426     65.79767     66.31874     65.31623
#>  [991]     63.14550     66.79710     67.73959     65.28696     82.43356
#>  [996]     89.59558     68.92128    106.48170     57.15906    124.36103
```
