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
#>  [1]  35.40064  35.40064  42.68101  32.59130  34.20459  27.01611  27.01611
#>  [8]  27.01611  77.11070 328.00793 167.65741 167.65741 167.65741  63.46157
#> [15]  63.46157  63.46157  63.46157  63.46157  63.46157  63.46157

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
#>    [1]     65.70131     67.49300     73.80883     71.03425     59.26900
#>    [6]    102.89727     54.94327     71.40147     59.60734    260.02958
#>   [11]    126.32807    159.48933     98.08921     62.99454     60.30054
#>   [16]     62.10447     63.47549     76.40937     93.77351     52.67575
#>   [21]     56.30092    152.31393     55.77564     59.70483     64.17820
#>   [26]     60.00082     69.08274     71.53242     61.98131     58.24884
#>   [31]     60.23537   1046.82430     83.78108     95.49715    239.86149
#>   [36]     55.15200     57.43295     69.62377     56.74121     62.48676
#>   [41]     67.65010     63.61515     84.12168     63.80211    131.73911
#>   [46]     61.74654     93.54972     65.83448     59.52679     76.44153
#>   [51]     73.32740     61.89119     92.56351     66.72847     80.61830
#>   [56]    120.72363     61.69631     56.93169    326.60644     81.58619
#>   [61]     95.31568    100.72730     60.25986     57.14293     59.68045
#>   [66]     59.71195     60.83241     69.74159     58.97373     97.83467
#>   [71]    132.38614     60.10726     58.34351    182.89905     84.67534
#>   [76]     70.61638     80.55541    226.51020     40.11672     57.02003
#>   [81]     66.02240     55.46813     54.59791     97.80557     63.38238
#>   [86]     55.81309     52.95472     55.54682     74.92850     62.56198
#>   [91]     90.72899     56.05900    150.67537    115.46412     80.17253
#>   [96]   2209.31757     61.58165     59.03468     57.11993     65.69201
#>  [101]     59.37593     72.44123     67.61153     57.92729    109.32080
#>  [106]    135.64274     62.55182     60.99030     93.03554     63.95372
#>  [111]     69.84527     70.45153     76.11858     57.72704     65.81656
#>  [116]     81.87032     67.62299     55.69675     65.69901     70.09247
#>  [121]     63.44648     60.61350     66.57469     68.92941     74.66699
#>  [126]     72.13893     55.65679     56.13535     80.18212     57.43757
#>  [131]     58.12087     54.36526     74.71606    101.23082     58.15550
#>  [136]     59.54625     52.12526     57.65617    156.00239   1064.82490
#>  [141]     99.10344    115.81290     62.19310     64.15309     64.32629
#>  [146]     60.84960     54.29486     73.65122    133.26274     55.90620
#>  [151]     82.10185     61.28460     62.94658     61.96377     63.47827
#>  [156]     83.83742    107.46461     66.84885     58.66229     59.89461
#>  [161]     55.13378     59.35090     55.40399     62.56954     60.98779
#>  [166]     72.67504     51.79119     66.05176     61.97528     67.63313
#>  [171]     67.70055    175.54128     84.77996     61.51169    138.72554
#>  [176]     64.39614     62.25094    100.97195     53.64372     60.14597
#>  [181]     58.74157    150.70602     58.52172     64.52523     89.12419
#>  [186]     65.57513     81.19170    112.72694     74.22531     62.26744
#>  [191]     84.12008     82.45762     67.30945     57.17064     80.68744
#>  [196]     64.80376     79.84122     56.63645     67.70972     54.33812
#>  [201]     77.75105     66.36804     71.16207     56.83796     86.47198
#>  [206]     63.72584     81.32412     86.46663     69.81632     57.81109
#>  [211]     58.20299     60.45014     78.09411     66.05184    124.06096
#>  [216]     66.16348     55.81210     77.33987     78.11235     59.94564
#>  [221]     59.12566     58.60706     58.30573     58.57214     66.40899
#>  [226]     50.43601     53.65417     56.98198     61.29058     59.36760
#>  [231]     58.06332     64.90809     60.03292     60.64623     60.51338
#>  [236]     52.94436     76.18000     60.28283     59.56993     59.74335
#>  [241]     70.25689     77.29021     73.41432     57.36965     73.56245
#>  [246]     61.82008     70.36090     51.50407     74.96707    104.35535
#>  [251]     54.70788     45.42184    452.21425     64.97081     62.06039
#>  [256]    146.75069    196.64272     66.18040     93.78336    311.16597
#>  [261]    425.93985    207.57141     54.31607     56.70176     71.08624
#>  [266]    115.09132     60.38274     69.39636     58.97290     60.60101
#>  [271]     64.49063     71.00488     70.42527     58.32726     69.15418
#>  [276]     59.02167     54.35998     71.97284     60.85257     82.30149
#>  [281]     82.15146     61.29141     66.29041    102.06059     66.70558
#>  [286]     53.74171     61.61271    153.11141    159.68623     94.02770
#>  [291]    127.70423     67.41637     61.98951     68.34967     60.55794
#>  [296]     64.77439     67.12708     59.20577     79.41208    124.91399
#>  [301]     89.37442     70.64436    382.30294     61.66942     58.10448
#>  [306]     65.15660    444.31943     65.94319    336.85192     59.89454
#>  [311]    915.72090     73.55727    182.35238     77.93517     58.07772
#>  [316]     60.74175     58.72625     57.11880    153.84880     66.70473
#>  [321]     72.38411     58.45199     67.40724     66.39622     66.68572
#>  [326]     74.15518     85.59126     73.20828     56.61200     65.54122
#>  [331]     64.73843     65.40612     67.30316     84.55461    103.58064
#>  [336]     67.96392     96.84657     70.34715    759.30990     65.88667
#>  [341]     58.61678     55.33791     61.25914     67.74579     62.34484
#>  [346]     59.40157    116.26707     67.83688     58.25727     66.41414
#>  [351]     59.66204     56.24095     54.65534    176.49109     64.46412
#>  [356]     71.00395     62.15324     56.86161    291.68874    312.62171
#>  [361]     58.62524     70.05916     84.69571     69.29029     65.86781
#>  [366]    109.70443     54.90351     72.84029     58.19423    109.08884
#>  [371]     96.10263     65.94746     72.72711     87.03027     57.08654
#>  [376]     64.95421     94.02913     66.62459    241.98419     82.74057
#>  [381]    112.73481    152.99736     98.18896     77.01512    116.30304
#>  [386]     54.63422    146.00514    257.10135     56.03412     68.06333
#>  [391]     61.80951     66.32031     55.23771     69.15785     62.40277
#>  [396]     60.83609   2083.79580    128.86389     62.58179     74.66467
#>  [401]     56.33228     78.65005     97.99382     57.80264     55.17349
#>  [406]     67.38461     83.15161     53.31439     53.25688     73.54497
#>  [411]     57.46561     66.96043     55.73907    174.85625     60.06988
#>  [416]     60.88874     64.90137    506.05005    129.16865     91.79883
#>  [421]     66.79578     86.47235     59.46650     63.91980     76.91743
#>  [426]     88.62452     57.68211     66.64811     67.40901     87.02678
#>  [431]     56.72836     63.84894     55.16610    102.60577    128.58988
#>  [436]    105.48528     54.86898     76.65077     53.85494     67.81541
#>  [441]     76.86185     51.38711    241.47842     61.21976     86.45216
#>  [446]     58.62340     62.17851     54.62976     68.99682     72.38495
#>  [451]     59.33811     71.87475     56.65805     58.39540     68.70629
#>  [456]     57.44560     51.85615     61.28522     54.77417     60.80610
#>  [461]     61.82532     95.82589     64.83770     59.31977     67.66942
#>  [466]     64.45564     57.21818     59.42102     56.50381     69.90877
#>  [471]     65.14061     61.43125     51.70159     76.11739     59.30757
#>  [476]    107.19174     64.90771     55.63149     58.90264     74.96566
#>  [481]    442.49970     58.70039    124.10987    471.45555    506.08693
#>  [486]     58.49618     62.85254     58.95907     58.40100     68.72187
#>  [491]    117.78183     87.94291     61.36109     64.16134     64.63277
#>  [496]     84.72247    238.35317     61.41643     73.90025  18882.31251
#>  [501]     55.79267     82.41206    261.82377     67.17474     63.65045
#>  [506]     79.97330    143.70647     63.36345     64.64464     63.23009
#>  [511]     80.89000     90.00993     81.25383     93.95383     53.02886
#>  [516]     79.52565    169.76333     56.81897     59.86357     59.50313
#>  [521]     61.66514     57.70569     61.06581     59.92743     68.97532
#>  [526]     61.64543     62.51644     59.83696     62.87489     62.51173
#>  [531]     64.04286     80.77897     63.38288     68.67658     86.06755
#>  [536]     59.15854     53.72301     62.43887    104.89542    128.66738
#>  [541]     61.25651     73.74220    290.58589     60.93156     58.65428
#>  [546]     80.99911     63.85893     59.26664     54.15320     60.48331
#>  [551]     64.17889     57.62158     53.93042     58.55792     56.96808
#>  [556]     60.35227     65.66560     62.77112     58.61408     60.66834
#>  [561]     62.34446     90.06362    705.73908    178.62468     80.24848
#>  [566]     58.04501     58.48879     70.99532     65.97998     63.36288
#>  [571]     56.09704     61.37428     58.21526     57.04825     60.64805
#>  [576]     58.46663     58.46010     61.64159     69.76433     80.99318
#>  [581]     66.26755     53.03206    201.62030    278.88334     67.64773
#>  [586]     66.00449    148.57561    111.65501     56.01036     57.28124
#>  [591]     87.77521     59.40849     82.23796     61.16317    130.85989
#>  [596]     58.25274     74.57634     54.76603     57.69519    370.96145
#>  [601]    310.08109     64.14066     74.58570    361.25729     78.27928
#>  [606]     62.79981     66.68165     62.67392    106.53721     96.83901
#>  [611]     54.95871     68.31848     53.35575     88.66919    113.47348
#>  [616]     73.80998     54.25866     61.31802     55.94986     70.25250
#>  [621]    190.71817     70.67317     61.25715     63.84020     78.48615
#>  [626]     64.10110     62.33404     60.25052     55.49643     59.42799
#>  [631]     62.49466     82.34216     61.39421     57.67735     62.29339
#>  [636]     71.89326     55.97474     68.28922     58.64835     75.23297
#>  [641]     62.23661     75.11284     60.16138    162.68350     48.95958
#>  [646]    102.78219     78.82663     56.58616    562.88173     97.74430
#>  [651]     69.65815     69.39917     56.83063     50.99565     68.13916
#>  [656]     62.24061    121.61779     89.16771     72.25900   2196.40847
#>  [661]     89.12770     77.05569     55.27273     87.30954     72.41262
#>  [666]     60.59920     94.29040     76.57448     85.70094     72.20016
#>  [671]    450.21587    353.93660     59.72968     68.89706     63.57234
#>  [676]     69.84905     56.97895     64.38870     60.41053     49.00770
#>  [681]     64.00724     60.79610     68.76410     68.10754     57.60636
#>  [686]     59.45553    107.46609     65.09216     71.00210     59.98488
#>  [691]     53.82553     49.66398     56.46242     66.89996     63.87928
#>  [696]    251.24378     59.74217     64.93904     66.91581     69.78442
#>  [701]     71.22172     88.19534     52.59561    106.45098    215.54900
#>  [706]     66.65766     65.25337    257.29251     51.81188     57.54453
#>  [711]     61.86217     59.49375     65.36539     75.77906     59.02773
#>  [716]     59.81465     62.00105     49.49014     62.90584    112.23774
#>  [721]    161.02072     69.41888    121.89968     69.70185     70.89737
#>  [726]     58.24480     61.21413    109.30667     56.07799     63.19856
#>  [731]    101.01188     61.10485     61.60979     64.20085     60.67958
#>  [736]    555.40293     67.55696     65.59297     96.38376     68.64459
#>  [741]     62.29490    137.64528     62.17192     79.43149     55.68756
#>  [746]    117.59122    109.08949    114.46562     60.82399     58.91583
#>  [751]     73.89031     57.81539     59.64307     57.49729     57.16890
#>  [756]     61.24193     58.84964     56.33831    268.54725    124.86826
#>  [761]     57.91478     71.18000     80.79551     66.05570     57.98715
#>  [766]     60.07540     66.50939     55.61418     67.22157     79.02291
#>  [771]     58.39446     59.46827     57.64424     66.55584     55.70504
#>  [776]     80.67890    116.23928     55.94280     95.27970     75.16234
#>  [781]     59.00993     59.55579     77.14441    101.50164     67.10149
#>  [786]     56.21216    233.48776   1834.22669     57.27556    121.57119
#>  [791]    155.40095     51.51427     61.30991     56.87561     63.78943
#>  [796]     66.84364     90.64053     71.19222     65.68519    115.45407
#>  [801]     58.46268     64.94981     64.54109     58.91941     70.95821
#>  [806]     56.44071    152.40877     69.69151     93.83522     65.56235
#>  [811]   1692.23063     58.40238     63.58453    164.20592     49.61839
#>  [816]     96.94705     50.34739     62.11978     55.97375     78.15594
#>  [821]     66.63333     68.70703     57.89357     58.80889     58.76687
#>  [826]     60.70550     54.50449     64.50376     59.29680     60.42844
#>  [831]    493.60595     62.27828     56.84179     57.73757     68.74528
#>  [836]     56.34766     56.44293     55.70029     89.60513     62.56393
#>  [841]     58.82456     65.00786     85.24907     58.42749    165.10173
#>  [846]     63.12966     66.90777     71.83472     60.61913     61.63291
#>  [851]     61.63982     64.04849     54.56696    112.99760     60.27091
#>  [856]     69.29241     58.92329     62.53199     61.10262     97.26834
#>  [861]    106.13914     79.36720     60.33662     73.27525     59.84550
#>  [866]     61.37584     62.66960     62.15957     56.28607    184.38837
#>  [871]     82.72070     65.19790     60.03307     60.02250    181.90640
#>  [876] 360671.16915     92.78589    150.46395     61.15968    112.08752
#>  [881]     64.70412     56.03831     62.24763     63.84842     73.78686
#>  [886]     58.49746     59.64498     67.22189     61.61031     56.93860
#>  [891]     54.43519     59.83670     58.76606     55.49554     66.29220
#>  [896]     68.21153     56.43080     52.59546     75.70868     65.36535
#>  [901]    118.33202   2609.91210     52.28225     57.77274     61.98173
#>  [906]     57.71242    488.83693    129.50505    188.06889     69.62623
#>  [911]     97.51705     56.36677     59.67648     57.64856     56.44479
#>  [916]     60.26509     55.23298     70.89879     61.24590     55.83496
#>  [921]    115.24625     68.45817     64.58180     56.54490     59.05435
#>  [926]     70.46462     68.32575     57.02914     62.75904     72.04204
#>  [931]     61.29443    119.48511     55.12038     57.58557     68.07757
#>  [936]     92.34077     79.58424     64.17055     96.45052    143.05424
#>  [941]     59.55387     70.67577     54.68058     64.11468     55.49653
#>  [946]     61.94937     59.96700     73.60421     79.00830     68.20094
#>  [951]     50.11406     65.29893    126.20452     52.57539    134.34994
#>  [956]     61.76418     70.91494     64.09383     53.27115    309.65129
#>  [961]    316.31057     80.14136     58.18411    138.25547     56.58581
#>  [966]     69.88644     74.22344     55.25015     57.78251     80.41262
#>  [971]     67.34584    142.40327     68.18541     68.81826     60.78970
#>  [976]     59.86097     74.37193     59.85288     66.59409     58.72676
#>  [981]     56.03319     60.30325     78.83710     83.63312    359.84892
#>  [986]     55.54252     78.62914     69.22803     61.08867     61.11144
#>  [991]    202.15373    141.83946    104.73708     60.95685     60.55182
#>  [996]     60.01025     73.08673     82.55019    104.53004     68.20737
```
