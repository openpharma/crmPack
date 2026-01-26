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
#>  [1] 130.28941 497.40524 497.40524 497.40524 103.89796 110.24276 110.24276
#>  [8]  47.87734  47.87734  47.87734  47.87734  47.87734  47.87734  47.87734
#> [15]  47.87734  47.87734  47.87734  70.63621  70.63621  70.63621

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
#>    [1]     62.37185     60.86711    115.97312     71.87723    102.52343
#>    [6]     54.36669     96.31952     58.73471     56.04861    104.77025
#>   [11]     89.35008     61.39752     74.85489    239.14571    177.25660
#>   [16]     65.53413     60.10398     78.45686     62.72825     59.20465
#>   [21]     63.66692     53.28775     95.56191     98.08282    101.66776
#>   [26]     85.31169     95.50910     76.93590     92.06033     57.24402
#>   [31]     90.13428     61.27117     59.47183     72.09942     52.48290
#>   [36]    129.58274    156.04836    172.33584     98.98620     99.12632
#>   [41]     49.10966    229.81984     64.89396     59.33119     85.90018
#>   [46]     77.45827   2824.52493    318.20472     62.35328     58.72647
#>   [51]     59.98838     57.44428     58.37277    100.78094     69.76454
#>   [56]     68.58774     69.99542     64.88779     53.75024     59.08316
#>   [61]     54.81047     66.97080     53.84958     81.55926     61.29783
#>   [66]     58.95424     58.54123     64.98434     68.57722     58.23195
#>   [71]    170.74091     66.50443     58.43327    105.12438     67.70913
#>   [76]    120.30340     65.19469     60.40229     67.36042     70.95245
#>   [81]     57.72082     96.53076     59.55379     59.21156     56.60379
#>   [86]     69.23992     79.34532    134.33133     70.41854     44.49413
#>   [91]     48.57235     91.64865     70.97043    163.21471     66.14930
#>   [96]     49.36176     69.58992     90.79419     69.40391     55.36546
#>  [101]     72.44724     71.39722     58.19711    124.31835     91.63328
#>  [106]     57.73681     77.65212     61.45120     60.23944     53.29384
#>  [111]  32777.74307    111.02256     62.35595     85.60701    127.99963
#>  [116]     80.27387     73.97595     70.09695     57.98172    131.00566
#>  [121]     50.43730     59.39962     57.51572     78.69525    124.45614
#>  [126]    386.67837     51.25184     58.62735     53.53133    225.23899
#>  [131]     78.11325     81.23008     55.15653     64.55487    105.91732
#>  [136]     63.94677    140.65109    523.14111    392.13658     56.84171
#>  [141]     63.26336     55.43913     60.64879     56.68634     68.81638
#>  [146]     54.86361     58.29463     69.91541  31484.75179    361.26842
#>  [151]     68.74275     54.63861     52.79390     63.74466    169.89030
#>  [156]     60.27519     64.40766    105.64937     90.66228     75.21600
#>  [161]     59.71864     58.56602     65.11479     86.16235     80.81611
#>  [166]     88.24892     53.09173    369.36076     68.31684     57.14755
#>  [171]    952.77646     80.00816     65.54348     67.31223     65.78833
#>  [176]    150.75798 222080.64652     67.57401     80.62263     57.75819
#>  [181]     74.02279     64.30479     97.29029     57.02414     55.81334
#>  [186]     68.09641     65.15573     75.93645     56.34681     68.42047
#>  [191]     60.26040    116.48894     78.16986     70.62995     66.94101
#>  [196]     56.83116     54.66758     55.40758     59.84242    110.57325
#>  [201]     58.17483     63.55586     77.57685     83.04590     70.34900
#>  [206]     66.76752     61.55809     58.81358     59.73941     66.97799
#>  [211]     79.20083     86.55956     90.93001     87.58795     56.79843
#>  [216]     83.83714     59.41891     60.06543    105.91845     63.27398
#>  [221]     61.70603     62.77501    103.34318     59.48851     55.68139
#>  [226]     57.07324     72.46741     59.37639     59.11850     74.33612
#>  [231]     58.79918     71.59073     68.23903     67.52758     62.37057
#>  [236]     94.61068    159.93455     73.60580     63.67805    127.23178
#>  [241]     58.55840    688.99986     70.69519   4121.94821     87.14791
#>  [246]     94.20568    253.47475    161.20353     64.73662     55.82200
#>  [251]     92.27056     54.39851     61.15161     80.33780    243.32106
#>  [256]     60.98819     55.72778     65.58424     83.55930     59.06366
#>  [261]     67.65544     64.66055     58.27789     66.71088     73.49585
#>  [266]     74.24857     69.04211     68.99920     69.46804     61.02048
#>  [271]     57.87653     63.08373     56.74218     97.90545     76.79748
#>  [276]     57.96842     61.23749     63.03858     56.50589    155.58881
#>  [281]     66.49477     86.85063     62.60727     69.54260     69.48834
#>  [286]     68.54145     68.73940    212.84082     74.33450     78.43155
#>  [291]    207.47428    140.69253     57.12764     58.27034     58.19628
#>  [296]     92.44664     68.52918     72.94950     68.87316    465.86827
#>  [301]   3108.34050     57.51294     76.61181     44.89714     68.01381
#>  [306]     68.03747     57.42004     61.30007     58.61035    101.95294
#>  [311]     55.65215     59.08031     68.62700   2130.58439    166.97200
#>  [316]     55.63256     69.37487     60.67584     64.13329     58.01920
#>  [321]     77.18747     65.33938     60.52893    131.25503     58.04056
#>  [326]    803.66473     67.50856     53.18615     73.96481     63.09207
#>  [331]     58.86663    112.14315     62.65533     58.32532     68.74873
#>  [336]     54.92244     81.13423     54.33917     55.52548     62.63699
#>  [341]     68.09879    140.86129     72.91646    132.64092     63.66548
#>  [346]     61.36138    133.45358     51.53411     74.50229     71.24879
#>  [351]     54.16836     61.28962    110.82638     51.73819     57.80819
#>  [356]     65.19197     78.43529     62.50143     55.19308     68.37443
#>  [361]     84.61850     57.01453     76.25084     54.84949    640.35941
#>  [366]     79.02795     68.34645     70.12614    310.25210     60.12522
#>  [371]     69.93759     64.87259    351.64311     57.38331     68.57965
#>  [376]     89.58220     66.57895     59.10043    110.18857     72.85455
#>  [381]     54.78482     60.35540     63.69463     62.86734     64.07544
#>  [386]     71.98755     62.34681     60.47549     59.09356     64.82869
#>  [391]     61.31631     63.93612     57.14599     54.59101     78.74865
#>  [396]     72.24992     57.37282     66.42674     60.64511     62.43478
#>  [401]     62.86658     58.57636     63.16282     68.12217     67.00671
#>  [406]     62.60232     58.09791     87.20681     56.59000     83.79661
#>  [411]     91.23566     56.97020     83.98622     55.54844    106.25612
#>  [416]     63.78293     57.17281     66.87685     71.36577     57.31975
#>  [421]     58.80026     83.72828    132.99412     65.72037     89.21296
#>  [426]     61.23670     65.50495     64.91809     64.38782     67.29595
#>  [431]     56.20782     60.85193     60.90047     61.61330     68.98994
#>  [436]     58.64422     55.17179     64.19383     58.84351    155.56595
#>  [441]     60.23052     76.19077     66.49205     64.15565     58.35483
#>  [446]    444.24485    144.85958     56.98832     70.46626     62.88795
#>  [451]     60.04449     63.68614     51.24133     61.77978     58.65538
#>  [456]     67.91436     59.91179     79.57431     89.84263     71.82012
#>  [461]     93.87780     58.93817    493.90460     60.63472     55.01184
#>  [466]    207.38496     55.59914     62.12727     59.60022     61.52486
#>  [471]    278.71227     91.72015     55.61905    136.14624     53.51477
#>  [476]     76.90327     70.55043     57.90755     83.24657     68.68767
#>  [481]     60.40620     64.48385    137.46868     60.78837     58.59953
#>  [486]     66.45101     86.97347     81.03990     64.53207    141.55412
#>  [491]     71.90953     66.36173     98.10604     56.39780     56.35951
#>  [496]     62.54771     61.59685     59.17620     59.27930     67.59486
#>  [501]    171.70278     73.21832     74.40303     75.86221     65.57847
#>  [506]     60.93344     76.85082     83.43102     66.29167     68.11997
#>  [511]     58.59868     63.97234     63.63677     68.97438    155.40049
#>  [516]     85.04103     57.13831     68.11020     59.97613     63.94916
#>  [521]    223.33186     54.85504     56.94679     63.89330     72.44520
#>  [526]     50.75589     77.71277     74.01228     70.66122    131.30375
#>  [531]     65.96739     79.15284     68.17797    276.34554     69.74961
#>  [536]     62.96899     63.34485     56.72600    154.07935     63.81262
#>  [541]     51.35282     56.93875     64.87724     71.57863     74.43169
#>  [546]     73.99297     59.48003     58.26257     58.07529     56.76064
#>  [551]    114.79787    169.62958     57.74281     76.00367    102.31647
#>  [556]     64.50005    604.71974     54.46433     63.75175     59.27143
#>  [561]     64.86992     86.16840     60.48856     88.83229    126.29443
#>  [566]     59.30263     63.92589     56.69535    345.92963    338.21897
#>  [571]    122.52257     64.83094     53.53352     60.60880     56.68142
#>  [576]    100.20927     68.94886     64.92112     72.11517     65.84467
#>  [581]     54.36658     91.36071     86.87519     62.24228     75.93428
#>  [586]     78.14764   1127.41022     79.61100     60.20028     95.28617
#>  [591]     62.57604    298.28754     74.78268     71.49499     71.49710
#>  [596]     72.08968     70.89307     96.18359     58.58995     77.58397
#>  [601]     71.25520     62.69601     63.34983     74.60971     61.45062
#>  [606]     73.90645     59.18944    185.12425     77.31114     56.73972
#>  [611]     70.67020     72.17289     72.47487     66.77513     93.86832
#>  [616]     67.23509     61.77988     88.49934     48.62335     80.84084
#>  [621]     88.78719     60.88053     83.03993     76.01963     57.07433
#>  [626]     65.25108     61.45321    107.05721     62.24284     64.93498
#>  [631]     66.30442     54.51913     67.25402     66.86587     55.66867
#>  [636]     67.62227     61.94732     56.49706    105.65770     66.87711
#>  [641]    158.71174     53.91464     56.98442     58.11810     67.72456
#>  [646]     55.24390     61.06676     58.56345     61.06608     69.52303
#>  [651]     60.88371     70.85951     61.44900     53.85647   9500.09318
#>  [656]     64.57121    895.98044     67.93592     59.18035     63.69566
#>  [661]     64.58301     66.76108     96.42248     82.72861     82.12683
#>  [666]    132.32234     56.16007     54.06764     62.02415     70.82123
#>  [671]     68.41478     75.01526     57.03729     66.42823    117.75331
#>  [676]    162.67045     60.72102     73.19450     62.70677     56.14033
#>  [681]     98.88927     61.54482     69.66405     58.97874     64.73534
#>  [686]     56.22673    131.60561     62.80110     85.09161    110.91269
#>  [691]    100.75845     81.27122     78.09050     60.96334     59.99284
#>  [696]     59.74529     67.68502     64.26889    113.31499     79.43212
#>  [701]     61.88924     59.04272  14917.53878  11353.25379    332.25787
#>  [706]     69.35642     54.81726     66.95445     78.33522    122.17439
#>  [711]    116.96959     52.21677     84.22635     40.12215     53.22359
#>  [716]     78.09840     62.81289    255.61082     56.48226    102.33128
#>  [721]    155.72672     54.31924     60.39905     61.08566     62.47733
#>  [726]     84.04098     65.26220     61.64886     66.62452     69.10017
#>  [731]     54.93726   7256.82353  77391.04874   1162.75409   1096.93671
#>  [736]    195.71835     75.73243     56.78194     58.44832     76.77940
#>  [741]     76.94168    240.02016     52.18836     62.52191     64.62446
#>  [746]     56.71768     61.82367    101.59289   1780.04304     62.31520
#>  [751]     67.71133     62.05875     58.24799     64.01615     62.33211
#>  [756]     66.51228     60.74812     66.18639    100.19012     61.13505
#>  [761]     59.69882     71.96514     68.12000     72.28237     56.27080
#>  [766]     60.86299     65.12793     59.27751     57.43265    224.02039
#>  [771]     64.43167     59.32789     63.39808     57.68464    123.22445
#>  [776]     66.99824     61.58044     55.40621     75.71422     64.49285
#>  [781]     66.21097     58.76527     64.54225     57.03781     67.88706
#>  [786]     77.35825     58.50384     77.00425    150.91805    153.90241
#>  [791]     90.75689     78.75381     71.32347     53.00205     64.99967
#>  [796]     73.36492     57.81670     56.48146     76.64842     52.81254
#>  [801]     50.28869    113.69277     64.75414     62.04456     62.11105
#>  [806]     69.15200     61.60339     56.76662     62.37312     76.17015
#>  [811]     80.23803     86.25096     63.65493     86.10273     59.95766
#>  [816]     63.62184     77.02361     63.03660     62.25916     64.63014
#>  [821]     59.04492    122.65486     55.79537     68.84045     68.85725
#>  [826]     77.27337     73.75191     90.40482     69.55316     77.59876
#>  [831]     82.15212     74.70347     72.77523     94.96549     72.85665
#>  [836]     57.70209     74.34869     58.66157     59.42433     58.69399
#>  [841]     58.84173     66.98621     60.71179    190.04793     73.91226
#>  [846]   2571.57901     75.44513     61.03623     59.86120     73.93858
#>  [851]     58.57878     66.50718     73.37277    118.72165     90.22291
#>  [856]    110.95255     56.79039     60.34597     71.27098     58.05005
#>  [861]     55.49747     57.17385     80.34000     60.89547    245.37288
#>  [866]    324.31604     61.62931     54.45178     71.25444    105.78327
#>  [871]     55.39096     54.64515     95.04481     65.29683     67.92992
#>  [876]     58.31086    647.98352     66.14052     66.61827     72.62641
#>  [881]     73.12787     65.73407    160.35149    744.64573     81.46064
#>  [886]     63.10961     68.36952     58.72140     91.72126     59.92132
#>  [891]     83.76170    212.77716     57.65261     55.78249     89.41470
#>  [896]    532.51361     55.78091     51.27974    169.57083     61.88443
#>  [901]     64.23254     68.97935     62.98173     83.30726     69.40874
#>  [906]     63.37121   5556.09486     55.29998     71.26584     57.31827
#>  [911]     81.94309    118.82262     59.53066     58.65466     56.97774
#>  [916]     65.50464     58.55070     84.93250     86.82070     96.84510
#>  [921]    111.04944    112.37405     85.65156     56.99202     71.41653
#>  [926]     60.95873    122.62278     78.00908    393.93079     65.50823
#>  [931]    306.14603   2058.03920    115.50238     67.84780     68.91248
#>  [936]     62.79255     89.28430    332.84795     77.52556     81.12613
#>  [941]     63.71475     62.03404     64.41527     63.35259     62.16828
#>  [946]     60.89979     95.64985     66.84213     64.86431     64.27731
#>  [951]     86.58011    305.91305     61.92555     52.55413     57.45121
#>  [956]     59.49489     59.95015     55.46049     89.66429     58.87967
#>  [961]     92.32677     53.21554     57.44022     62.02615     78.84187
#>  [966]     91.65831     63.74690     93.17512     63.12474     60.62390
#>  [971]     75.29347     60.86914     58.84135     67.52446     96.76081
#>  [976]     59.70949     75.14018     98.05890     56.27087     54.58431
#>  [981]     94.93326     67.24173     68.17471     53.62531     57.93795
#>  [986]     61.66772     86.36144     62.16125     86.90149     97.08279
#>  [991]     79.52080     62.36185     66.38090     78.40536     46.91224
#>  [996]     61.78270     62.70696     85.46589    169.02240     92.37269
```
