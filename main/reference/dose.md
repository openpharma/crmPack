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
#>  [1]  79.70137  29.33876  29.33876  29.33876  49.83168  49.83168  56.40055
#>  [8]  56.40055  21.14587  49.38908  27.05805 189.08646 189.08646  12.09199
#> [15]  23.43755  23.82602  46.67094  46.67094  46.67094  24.74432

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
#>    [1]     77.71843     55.23633     58.27739     65.74249     60.90184
#>    [6]     82.78008     99.11838     80.44437     76.83174     62.16119
#>   [11]    207.46445     72.09784     61.92354     57.43895     58.16598
#>   [16]     80.86240     91.20408     82.88320     81.12910     65.18020
#>   [21]     58.56379     60.07634     71.28603     77.22677    191.40871
#>   [26]     60.59382     64.95699     61.34731     65.39816     66.37921
#>   [31]     97.43300     63.07641    142.23161    282.25629     82.74860
#>   [36]     59.10407     81.25146     53.82914     68.64633     82.83590
#>   [41]     55.51694     62.42546     58.51214     85.75102     60.21960
#>   [46]     70.28451     69.03760     57.21913     58.50124     56.84866
#>   [51]     63.31195    264.27273     58.35673     60.32256     59.55993
#>   [56]     73.74636     60.30380     61.50456     52.35619     52.88815
#>   [61]     57.94573     78.18017     65.50091     75.46324     65.43938
#>   [66]     71.37246     58.46693     83.51394     84.43730     63.32003
#>   [71]     62.22483     59.80663     63.73664     63.16582     57.83783
#>   [76]     91.86264     62.16733     63.39225     95.60164     54.64212
#>   [81]     60.64293    347.23747     80.90007     59.51697     54.11069
#>   [86]     70.29058     66.33775     57.92318     55.58982    132.14581
#>   [91]     71.58963     72.22493     79.01634     67.88708     78.37754
#>   [96]    138.46664     85.15992     82.44586     70.25247     60.33829
#>  [101]     66.53671     61.15564     58.26877    132.51265     52.42090
#>  [106]     59.29065    172.93414     54.59588     70.51315     60.24035
#>  [111]     58.63494     93.95534     66.64010     59.01302     66.40956
#>  [116]     92.38368     64.18904     58.60065     71.44070     58.87887
#>  [121]     67.21837     59.07695     70.43355    145.41191     76.21025
#>  [126]     59.41203     61.26163     74.52480     90.71198     67.26918
#>  [131]     68.63916     66.31972     56.15867     79.59558    105.77918
#>  [136]     56.78953    156.30682     56.01673     56.64169     66.24336
#>  [141]     77.31746     75.59666     60.95089     76.91568     66.46128
#>  [146]     78.42302     64.93817     57.43796     58.86112     77.85545
#>  [151]     63.54883     66.51822     84.32160     91.26775     57.78466
#>  [156]     54.18874     58.58922     54.88447     65.60982     58.60420
#>  [161]     65.98441     62.62813     76.93990     75.41363     70.16496
#>  [166]     65.12361     61.09382     68.83483     77.34492     55.87492
#>  [171]     60.87880     66.15912     59.26334    150.54549     62.67123
#>  [176]     98.43667     81.98976     47.21286     62.68620     63.25306
#>  [181]     88.44360     75.35795     71.50721     56.77183     68.52457
#>  [186]     54.64710     59.22757     68.74256     60.37337     59.24794
#>  [191]     62.64266     57.48968     60.71316     47.03600     53.80288
#>  [196]     58.80266     82.22826     73.81402     74.59099     55.86284
#>  [201]     60.19836     60.75836     60.96543     60.77034     58.14165
#>  [206]     58.00099     60.12802     64.43780     59.55347     58.25680
#>  [211]     59.25355     67.73985     70.02255     75.90035     63.50548
#>  [216]     63.43012     65.99319     67.13588     76.63625     57.13248
#>  [221]     60.72514     97.88566     99.54058     66.86498    105.79480
#>  [226]     94.57286     57.26414     59.01832     72.99080    104.63076
#>  [231]    783.89234     56.40321     60.71061     82.09400     66.25767
#>  [236]     59.45958     70.34919    120.52298    105.74283     57.19684
#>  [241]     62.33717     61.43871     59.29652     58.72389     65.71438
#>  [246]    153.11568    171.67737     56.01844     56.17443     65.80885
#>  [251]     53.78288     56.25788   2266.66329    567.35538    440.29208
#>  [256]     59.98547     61.98744     64.08614     65.36481     73.38751
#>  [261]     54.63425    155.78425     60.62919     74.31521    177.19385
#>  [266]     90.95485     81.94452     89.03597     66.90537     57.41589
#>  [271]    103.64880     74.49637     63.96161     74.72492    147.83388
#>  [276]     80.43062    107.83795     58.52059     98.48813     79.88288
#>  [281]     83.98923     61.18536     58.34971     59.43810     65.21945
#>  [286]     59.59085     57.33670     54.80311     90.08279     62.93740
#>  [291]     83.45785    107.34668     64.86206     63.70771     68.73195
#>  [296]     63.32768     60.87865     57.97111     66.19796     60.52276
#>  [301]     69.12657     74.89224     58.87436     66.59800     67.69235
#>  [306]     71.99332     63.55412     65.99385     58.21385    136.12996
#>  [311]     74.27746     60.14254     59.46056     60.48462     61.12397
#>  [316]     88.80422     63.35271     56.64146     86.26547     61.23780
#>  [321]     56.70955     61.03094     58.61424    109.27656     57.01246
#>  [326]     68.68193     70.08890    151.58984     83.71299     58.44375
#>  [331]    104.58780    150.09538     66.67568     57.24386     57.56180
#>  [336]     73.45227     82.28814     62.45835   1253.63994    110.39446
#>  [341]     66.33039     61.71816     64.83103     57.37372     58.15870
#>  [346]     85.08588    121.82037     75.89484     49.09689     93.51149
#>  [351]     20.33763   2030.85485     76.74875    277.12124    127.34658
#>  [356]     62.48853     57.81617     69.73513     56.77592     91.29619
#>  [361]     60.22925     59.13832     59.16362     56.51230    195.63153
#>  [366]    576.74790   1153.64775     65.26808    133.67034     56.42676
#>  [371]     61.17700     85.54044     59.16812     53.09090     68.00426
#>  [376]     75.12018    104.99446     54.16812     55.07792     64.19074
#>  [381]     97.13559     56.07174     63.76641     59.92634     65.09361
#>  [386]     79.12035    107.56920     77.47417     77.03615    113.85246
#>  [391]     59.66491     63.34818    100.72530     83.27132     58.81291
#>  [396]     64.85833     63.05025     60.23815     66.48003     62.24983
#>  [401]     61.57325     64.89849     62.98633     55.80303     59.24157
#>  [406]     66.05862     63.80942     58.52618     61.10907    269.79385
#>  [411]     64.92962     61.50399     76.89784     54.42634     62.40432
#>  [416]     74.54757     72.31196     61.17838     68.17495     73.50184
#>  [421]     54.95047     54.38032     60.80674    188.50941     57.53237
#>  [426]     59.73197   1902.64128   1779.09903     60.77060     78.05662
#>  [431]     68.04917     60.40108     90.27291     58.61201     73.60318
#>  [436]     68.06446     61.66996     54.91179     65.05231     61.29991
#>  [441]     63.77614     90.51390     59.46322     70.03888     65.41447
#>  [446]     57.59946     63.93031     83.40346     58.18033     68.49235
#>  [451]     62.27044     70.29342     72.34992     72.82704     56.72989
#>  [456]     59.77187     56.44936     63.00664     59.27195     62.61378
#>  [461]     67.11503     55.95540    101.47835     78.22201     61.17154
#>  [466]     70.12081     54.75802     64.56301     85.20488     68.79877
#>  [471]     90.22861     67.12953     98.71186     55.27850     62.80921
#>  [476]     66.05730     69.21041     60.57912    104.32726     75.78126
#>  [481]     59.38723     59.16413     60.92756     60.09651     65.12023
#>  [486]     89.67379     85.27585     75.90415     58.34206     63.11196
#>  [491]     60.63768     63.35982     62.25559     57.02636     64.08896
#>  [496]    136.45702     48.90154     87.70814     63.55410     66.09787
#>  [501]     60.68463   1023.09985 880623.67057     71.76351    765.14732
#>  [506]     55.67673     61.99839     58.57629     64.59632     60.96175
#>  [511]     71.06917    107.58044     58.47258     80.91538     61.41128
#>  [516]     55.26544     61.23244     58.39438     70.81629     76.96174
#>  [521]     63.59958     68.72614     60.69141     65.96246     77.33158
#>  [526]     76.49262     57.06002     57.81262     59.18111     57.87564
#>  [531]     67.19507     60.44453     71.11922     59.34596     58.25774
#>  [536]     85.52245     96.96754     59.25566     76.02494     65.42603
#>  [541]     58.96390     70.57129     51.75229     84.91924     58.86117
#>  [546]     66.72475     62.97007     58.01036     63.91216     61.21686
#>  [551]     91.36272     74.96131     57.01088    144.24627     74.57763
#>  [556]     98.34025     76.58950     65.64003     64.14706     81.89225
#>  [561]     78.07072     56.79991     61.27925     79.37203     69.05219
#>  [566]     66.22329     69.70028     64.68427    111.83615     85.29970
#>  [571]     55.43823     59.98975     62.71747     66.33692    102.84650
#>  [576]    582.95012     54.64518     80.82003     70.47796     65.94150
#>  [581]    103.61606     52.58930     69.92768     56.29738     61.10833
#>  [586]    176.66768     57.69057     56.81271     62.88893     59.57819
#>  [591]     60.16949     57.42243    113.02290     99.47698     69.85469
#>  [596]    156.77848    109.65708     55.78778     59.92601     62.20720
#>  [601]     64.38006     70.51995     59.37760     59.77356    136.23594
#>  [606]     59.00351     60.17960     65.48483     67.55830     65.66437
#>  [611]    457.41157     54.46874     87.44053     65.27496     68.31117
#>  [616]    283.13422     75.32157     58.62823     74.39646     66.62527
#>  [621]     94.69344     60.76549     59.19316     65.66511     85.68971
#>  [626]     61.76410     91.03594     98.45191     77.01591     62.90757
#>  [631]     72.78657    406.28460    173.56475     62.27714     73.72084
#>  [636]     58.33343     54.71494     56.78461     88.25014     63.66467
#>  [641]     83.19020     67.21808     69.46572    217.47800     74.00434
#>  [646]     59.72439     66.90323     87.50678     60.64678     64.65721
#>  [651]     56.42758     50.46188     83.90945     72.61230     59.93136
#>  [656]     58.35532     96.21714     84.25217     80.85830    103.88405
#>  [661]    265.79560    106.52972    153.86747     84.00458     57.33707
#>  [666]     67.04677     58.51220     68.56487     59.09106     69.03061
#>  [671]     64.97906     64.61132     61.68944     82.32829     79.98974
#>  [676]     77.09584     78.77172     63.93134     83.81815     61.06834
#>  [681]    145.99314     93.91387    110.64736     57.66209    189.34362
#>  [686]     73.80135     75.30656     69.63849     68.14373    111.58861
#>  [691]     65.90910     60.26713     65.99836    188.46625     61.18608
#>  [696]     61.13739     55.54254     83.24079    178.66989    107.57609
#>  [701]   1699.97292    157.13223     57.68028     62.86097     81.77167
#>  [706]     66.37201     89.21607     66.10261     49.60433    125.95002
#>  [711]     57.81246     80.30880     70.61775     86.28909     68.68762
#>  [716]     68.26722     61.63517     56.40610     65.16474     79.31327
#>  [721]     79.92207    106.62606     59.74007     56.46185    103.81413
#>  [726]    127.65167 785283.51148     57.94619     65.26650     73.52521
#>  [731]     57.72541     65.44735     64.72498     84.81027     63.83837
#>  [736]     60.82369     85.92941     59.85843     63.94874    176.67357
#>  [741]    306.31237     73.78207     83.46584     61.59141     54.51958
#>  [746]     59.02373     72.29860    106.97551    335.90441     97.81747
#>  [751]    193.26303     54.53222     69.51699     60.92511    181.96397
#>  [756]     81.44405     64.85377     59.15795     74.84876     87.53752
#>  [761]    118.45758    131.54486     62.18535     78.35958     67.04733
#>  [766]     62.38022     65.04206     98.13026     61.20413     59.48719
#>  [771]     58.64686     79.57935    141.04593     87.08867     54.43598
#>  [776]     61.78819     61.64643     59.28921     62.25249     77.25482
#>  [781]     93.93597     54.76836     80.35082    119.91565     83.98716
#>  [786]     73.26937    111.86308     58.82649     61.43581    152.41387
#>  [791]     61.63031     60.63842     69.33146     57.79325     83.04508
#>  [796]    117.46550     59.32259     62.69361    129.16372     63.09874
#>  [801]     55.09780     64.08857     62.41872     61.50349     99.64523
#>  [806]     63.58946     65.23941     82.09568     67.72138     77.97220
#>  [811]     63.08049     70.03358     80.29319     63.09353     79.20603
#>  [816]     62.73774    104.05042     79.18528     55.80231     57.50023
#>  [821]    102.99748     72.35070     58.29174     62.08139    171.80362
#>  [826]    184.35393     56.51872     65.95924     73.02347     78.50960
#>  [831]     63.70443     59.99840     64.24105     63.82361     67.00639
#>  [836]     57.70834     77.97550     52.93397    371.84094    454.81601
#>  [841]     63.43055     60.72162     60.78778    108.17435    158.34252
#>  [846]     95.23457     54.94672     71.53668     60.81084     66.43921
#>  [851]     58.02137     58.95920     58.59533     56.73219     58.60495
#>  [856]    152.95527     57.84176     59.42140     64.47039    133.95938
#>  [861]     58.03767     58.63816     56.41929     65.99141     60.52331
#>  [866]     81.83476     57.73488     59.27042     65.18709     74.18507
#>  [871]     64.30405     58.72690     88.61871     73.77714     73.51449
#>  [876]     55.60142     64.62563    128.15639     61.07035    531.65973
#>  [881]     65.42576    135.48111    161.45717     62.99593     56.90816
#>  [886]     53.28309     75.67072     74.85389    108.81902     62.81078
#>  [891]     59.14733    100.81444    297.58765     59.73604     92.76141
#>  [896]     58.03229     56.21150     65.55624    234.46722     98.07178
#>  [901]     59.69741     63.65032     59.65762     66.74079    171.43131
#>  [906]    108.96553    116.68705     59.14424     62.43880     60.11786
#>  [911]     60.29119     57.91349    244.11703     51.37430     57.95133
#>  [916]     64.03224     62.84680     58.43522     75.94070     53.95657
#>  [921]     92.67133     64.30739    167.16634     58.01836     61.10602
#>  [926]     59.49987     55.17559    111.40996     57.42298    102.02708
#>  [931]     55.38603     61.58873     59.68896     90.85847     58.21160
#>  [936]     88.26299     97.39414     68.29139     70.21827     57.90883
#>  [941]     60.41806     55.66633     84.78393     61.70643     62.43295
#>  [946]     93.29086    117.84863     52.50367     73.87302    101.99974
#>  [951]    163.47418     63.48721     76.80763     69.16146     71.41123
#>  [956]     67.78154     67.54528     69.82617     61.95548     69.65100
#>  [961]     94.70247     57.65448     63.98839     55.73584    115.01450
#>  [966]     63.19726     60.11656    121.33930    216.61921     55.53224
#>  [971]     62.14891     62.20403    150.33377     71.34257     59.74795
#>  [976]     55.65777     61.55803     58.32179     57.62554     88.48214
#>  [981]     54.69148     86.91123    100.28417     84.93914     50.41658
#>  [986]     99.80240     63.44067     62.50114     60.86921     58.26777
#>  [991]    243.48931     62.51224     62.33825     76.74402     62.26115
#>  [996]     58.37363     62.37635     60.52985     58.01349     85.36584
```
