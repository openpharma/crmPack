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
#>  [1]  29.02983  29.02983  29.02983  29.02983  51.70730  42.67071  42.67071
#>  [8]  42.67071  82.64301  82.64301  82.64301  82.64301 106.98816 106.98816
#> [15] 106.98816  25.34433  63.36507  86.18295  60.12952 180.82204

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
#>    [1]     59.11833     62.84308     62.88730     89.24236     42.88671
#>    [6]   1128.39559     58.18983     68.97023     63.27350     48.22634
#>   [11]     55.86408     69.65646     55.76644     86.63831     78.77457
#>   [16]     65.46695     64.37138     80.91951    105.97914     58.43778
#>   [21]     66.63987     83.45137     79.93674     62.56838     68.00599
#>   [26]     85.93969     72.92264     59.25106     56.81744     69.30857
#>   [31]     59.48687    214.77865     61.90664     67.96630    170.91766
#>   [36]     59.66976     63.67625     64.21298     59.04793     65.09600
#>   [41]     70.69989     64.59236     58.55606    102.21023     58.63749
#>   [46]     73.97395     66.44507     73.79667     68.47745     56.02322
#>   [51]     75.13422     57.06247     59.07673     71.00706     64.60859
#>   [56]     56.87950     62.61672     56.37838     62.70990     68.21072
#>   [61]     71.12276     66.74066     71.20959     58.92936     62.95226
#>   [66]   1154.53524   2599.65537     65.25278     60.11676     66.44391
#>   [71]     85.91275     63.25084     67.30497     61.95081     63.21848
#>   [76]     61.42727     65.73998     64.79146     61.92431     61.35828
#>   [81]     70.96091     64.07443     67.09020     60.87587     58.27242
#>   [86]    124.23867     84.99096     53.33624     92.39273     51.12315
#>   [91]     53.80503     57.02438     57.19092     67.89757     68.73428
#>   [96]     82.11294     58.53115    169.98747     55.69732    112.27017
#>  [101]     64.23205     75.89118     70.32925     53.38967     75.49492
#>  [106]     67.21213     58.44227     63.74159     75.82457     75.16950
#>  [111]     63.71158     56.34320     85.93262     61.03085     52.63316
#>  [116]     55.57222     74.16899    173.69264     64.11590     75.26489
#>  [121]    107.35428     59.40141     60.30847     59.22172     62.64932
#>  [126]     59.79011     63.05177     59.31953    164.55761     47.04656
#>  [131]     65.38783    128.52800     62.82240     58.02126     68.97184
#>  [136]     70.53597  79819.00110   5202.40112    123.17238     55.45249
#>  [141]     78.99925     58.77749     57.40861     96.02980    166.01084
#>  [146]     60.66650     58.02805     68.92998     61.02366     82.87840
#>  [151]     54.90708     53.53180    101.41994     53.46222     57.50463
#>  [156]     57.78987     59.78298     57.54639     76.98339     86.69594
#>  [161]     87.57932     84.15301     64.44541     83.21643     72.40009
#>  [166]     67.79066     74.31329     88.53386     61.59665     60.28637
#>  [171]     58.48291     61.51453     56.76957     64.13418     62.23775
#>  [176]     59.93893    172.37518     87.38512     57.83482     49.79799
#>  [181]     66.69038     70.57913     73.00060     71.21045     88.04752
#>  [186]    830.80357    289.32097     61.74057     97.27167     59.84104
#>  [191]     56.70058    224.31591     63.42529    239.18335     98.55596
#>  [196]     71.19460     61.04558     94.11864     53.06528     71.08594
#>  [201]     90.58501     93.96341     58.72113     58.23918     62.65615
#>  [206]     58.54169     56.65642     65.83771     64.24762     56.97301
#>  [211]     70.36016    117.33902     51.02962     65.53530     62.68253
#>  [216]     69.08971     69.38146     68.77196     94.78059     65.15130
#>  [221]     93.72804     59.14475     65.07023     81.70843     55.03439
#>  [226]     54.19923     54.82961     73.97751     50.25725     62.90730
#>  [231]    218.48773     56.90752     62.37284     55.68930    261.31839
#>  [236]     74.20293     63.28528    135.46368     54.81780    135.60591
#>  [241]     56.94274    178.61488    111.45583    136.86056    115.09670
#>  [246]     57.11748     60.59925     60.64130    425.13997     62.36480
#>  [251]     72.60571    115.00400     93.86764    100.36307     55.75380
#>  [256]     69.99931     56.66100     64.89834     59.27139     65.41943
#>  [261]     56.90814     69.71240     57.20630     71.54809     62.47695
#>  [266]     60.61585     62.75992     55.53321     97.83459     64.72469
#>  [271]     69.97696     98.65535     68.26363     80.68050     79.81753
#>  [276]     62.57061     60.99279     69.77013     67.09782     70.87978
#>  [281]     71.91224     63.01797    106.54106     88.43898    191.47393
#>  [286]     52.28900     61.24985     56.82635     60.14110     61.45462
#>  [291]     73.61600    130.44972     56.66013     68.41723    112.16363
#>  [296]     57.88337     93.64056    107.50486    120.94055     57.55498
#>  [301]     61.53792     72.33520     63.07072     57.17934     70.51323
#>  [306]     66.37644     62.43517     82.71059     62.79426    175.00256
#>  [311]     73.33449     66.01328    145.90609     65.16145     65.01727
#>  [316]     91.62836     57.67951     74.13516     66.15837     66.81801
#>  [321]     62.05042     64.70890    251.63388    167.26058     68.20679
#>  [326]     58.49893     58.70920     62.59539     54.54627     59.91189
#>  [331]    891.46000    129.33649    191.14201     68.51775     97.66639
#>  [336]     66.42166     68.86014     92.52913    230.37828     64.53932
#>  [341]     64.75239     60.42023     59.70402     60.96746    107.97868
#>  [346]    103.02043     71.83461     87.87835     54.20950     67.09256
#>  [351]     62.39869     91.69085     74.92749     52.76512     76.55852
#>  [356]     94.63562     65.92331     56.27652     60.12606     63.27515
#>  [361]     62.46463    109.26681     50.64430    103.62377     61.60971
#>  [366]     59.80524    129.10329     66.09446     77.41043     71.64778
#>  [371]     70.75739     74.20279     63.06770     58.21569     70.42113
#>  [376]     65.93033     53.78000     65.16335     57.93641     62.24547
#>  [381]     76.85949     68.95686     62.86289     61.21848     91.05648
#>  [386]     58.97636     79.17283     63.36297     57.04342     58.94398
#>  [391]     65.22849     62.08096     63.70346     62.84630     60.76529
#>  [396]     65.20643     58.34758    551.73875    125.10163     57.80244
#>  [401]     56.46192     99.14909     59.05121     58.13884     58.42156
#>  [406]     60.83532     64.12957     62.73204     67.34457     65.01273
#>  [411]     76.77682     75.82360     61.94154    384.08385     75.08737
#>  [416]    179.11959     55.50559    116.08599     61.96818     78.09202
#>  [421]     53.93919    127.39279     57.98864     57.60293     60.78927
#>  [426]     60.49665     63.73831    133.30696     74.24423     56.06452
#>  [431]     60.03497     67.78539     67.02168     82.91088     49.60740
#>  [436]     75.51725     73.83294     66.66317     84.58353     73.04232
#>  [441]     61.78114     59.73515     56.15526     63.14511     67.23008
#>  [446]     90.03075     59.24174     64.35370     71.64292     93.69920
#>  [451]     70.60254     70.57079     55.59284     91.65098     59.71404
#>  [456]     47.15706     61.96657     71.51418     74.05401     63.48836
#>  [461]     58.13940     50.13256     50.31526     94.86374     67.32597
#>  [466]     65.65836     69.11110     68.36877     74.47245     70.64812
#>  [471]     54.51577     83.60862     50.85184     55.62853     64.98371
#>  [476]     64.39734     58.49344     63.96686    124.64568     99.05581
#>  [481]     59.72319     63.05634     81.34871     76.85660     59.05662
#>  [486]     63.70804     61.89033     56.92627     73.29831     65.05581
#>  [491]    137.99605     93.03981     81.47045     65.46436     56.41583
#>  [496]     67.38694     61.35839     59.94308     58.22483     88.02480
#>  [501]    114.46245     55.14977     64.44516     65.82658    101.01691
#>  [506]     56.82265     79.07910     53.70402    116.97873     74.73327
#>  [511]     54.18047     62.89072     61.69227     76.88512     56.73640
#>  [516]     61.38902     73.51291     56.71354     68.51440     52.65962
#>  [521]     61.38318     82.56421     54.88183     56.35753     60.82805
#>  [526]     66.95823     65.13553     61.25209     65.89105     60.09467
#>  [531]     64.86329     88.45530     81.95740     66.79127     65.30138
#>  [536]     59.12974     74.31130     78.01587     56.51986    111.16500
#>  [541]     60.09220     93.55986     64.67829     71.08204     79.00769
#>  [546]     58.91794     91.16023     59.61283     57.83201     61.47589
#>  [551]     60.29663     57.29979     59.55219     73.00744     63.61535
#>  [556]     76.99042     87.60370     73.02213     61.43522     61.23444
#>  [561]     61.26181    363.85399     74.93675     76.59196     68.09284
#>  [566]     59.88402     71.58503     76.07826     85.36318     93.54447
#>  [571]     77.91213     85.47452     61.82344    246.04303     82.79442
#>  [576]     59.55827     59.59053     63.39316    101.17605     79.36222
#>  [581]     78.01800    463.72784     63.85696     59.40609     70.93455
#>  [586]     56.03777     63.25804     70.07289     65.47883     90.28699
#>  [591]    117.98861     69.17801     77.69997    103.92959     55.49833
#>  [596]     68.44371     60.39763     65.86094     75.28847     70.37671
#>  [601]     91.40620     59.06602     69.71617     57.02762     55.92181
#>  [606]     53.74240     84.04751     61.57349     56.76346     67.70426
#>  [611]    123.23755     65.97571     66.46606     95.04183     53.91418
#>  [616]     58.54576     61.40379     81.72013     64.37191     80.83835
#>  [621]     65.36100     66.99847     73.79473     56.32789     71.04686
#>  [626]     74.66042     61.32799     64.48075     58.68028     57.28496
#>  [631]     97.29309     61.14774     55.91288     63.97707     58.94515
#>  [636]     69.97587     64.68433     88.59438    148.21884     58.60965
#>  [641]     97.58044     65.97556     58.71806    119.39950     55.79143
#>  [646]     57.96628    134.62373     60.46069     92.72828     97.34036
#>  [651]     81.98561     86.58327     56.78069     60.95342     71.90222
#>  [656]     72.92573     64.58837     60.12980     68.29502    758.53226
#>  [661]     58.42472     66.44847     73.25272    110.31271     68.08578
#>  [666]     55.03532     94.14216    669.10722   3909.07391     63.25281
#>  [671]    105.08127     71.90580    109.43954     63.76147     61.25925
#>  [676]     54.71166     56.93261    100.65292     51.18781     62.49455
#>  [681]     61.86068     80.13644     54.97434    122.91948     66.76887
#>  [686]     61.69747     56.23245     58.76386     63.17802     77.59252
#>  [691]     60.00315    102.46293     63.78933     70.76601     65.09153
#>  [696]     83.05620     49.75029     61.84393     60.71167     66.11373
#>  [701]     63.63608    123.83754     69.81350     68.26241     67.58920
#>  [706]     57.50607     58.58637     96.38830     56.42679     63.43784
#>  [711]     56.57315     61.64953    227.75184     59.56966     91.98080
#>  [716]    102.28560     56.19062    119.73057     62.20869     54.92978
#>  [721]     72.09691     57.52806     68.31510     65.23621     63.39803
#>  [726]    128.49102     90.50993     49.38764    890.38939     78.44943
#>  [731]     67.15183   9042.56702     62.31027     55.87235     57.81044
#>  [736]    118.92359    104.62192     60.43814     63.91450    216.73555
#>  [741]     60.97024     55.70975     71.71439    101.75650     58.70415
#>  [746]     60.80163    316.59175     62.82904     58.37098     65.38750
#>  [751]     63.71188     57.14588     58.74001     54.66624     80.55542
#>  [756]     59.06695 395704.21380    175.20789     77.91228     58.71919
#>  [761]     57.37167    120.94955     98.00325     66.71395     57.79266
#>  [766]     62.19886     77.86361     57.76306     52.43273     63.06129
#>  [771]     79.44283     70.85029     94.61120     74.70283    151.36339
#>  [776]   5258.62212    101.23399     64.60903     59.28833     63.47080
#>  [781]     77.50702     60.85387     54.62156     64.24765     60.50773
#>  [786]     85.97440    549.89059    158.78408     57.25106     55.55152
#>  [791]     89.78975    715.58269    120.72402     63.83792     62.82805
#>  [796]     93.62524     58.70620     62.71754     69.33289     83.31514
#>  [801]     64.51889     62.33123    500.07853    100.60277     59.85070
#>  [806]     63.35719     62.34523     88.19192     70.98101     59.87126
#>  [811]     58.90678     61.84338    146.94722     56.14399     68.03785
#>  [816]     70.46202     68.42294     93.62651     58.05870    106.57786
#>  [821]     63.99119    149.74152     51.57906     67.08360     57.27361
#>  [826]     65.88034     64.03966     58.76411     62.85645     63.31019
#>  [831]    453.22773   5631.32519     81.84098     58.92041     67.98582
#>  [836]     80.41996     58.73507     70.98103     61.77904    108.57666
#>  [841]     59.18844     70.44817     85.69874    200.20772     61.78029
#>  [846]     77.28963    201.27801     54.51561    856.53576     74.32098
#>  [851]     69.08912     79.19645     77.64863     58.18824     59.72818
#>  [856]     64.32407     57.73496     81.60300     51.62377     83.62736
#>  [861]     60.68758     57.58049     57.03602     68.31047     55.93327
#>  [866]    138.62290     76.21459     99.69078     66.98587     62.75554
#>  [871]     74.64598     60.57705     68.81534     71.66873     75.40459
#>  [876]     55.67650     60.80019     65.92155     55.75328     57.77576
#>  [881]     67.47042     79.87347     57.75218     51.66165    561.48354
#>  [886]    415.40804     54.73256  18925.34534 435300.87016     77.85836
#>  [891]    142.38473     61.00000    197.76708    104.36064    422.54533
#>  [896]     64.19812     64.38189    201.65125    126.09196    293.49872
#>  [901]     90.56214     58.56820     95.48040     82.31705     62.93450
#>  [906]     60.98447     61.57101     80.61851     68.93452     70.40941
#>  [911]     81.01510    110.29238     67.13295     57.17411     57.50195
#>  [916]     62.64284     65.45544     59.25950     55.61194     59.82141
#>  [921]     75.80859     61.23195    223.02800     60.62701     59.83964
#>  [926]     72.16861     75.37608     60.46304     63.99504    232.08789
#>  [931]     59.74230     63.53473     56.76997     60.56399     60.57906
#>  [936]     67.41688     78.05749     57.22612     55.41244     99.06131
#>  [941]     68.24101     80.01290    115.46616     72.36550     66.43999
#>  [946]     60.52364     84.26375     51.34804     62.79898     97.47838
#>  [951]    152.86179     63.52026     65.01717     59.48626     56.99302
#>  [956]     64.95725     63.43979     62.69493     54.51867     82.35200
#>  [961]    233.88767     62.50810    171.57332     64.77408     63.18481
#>  [966]     80.55266     67.58804     59.34256     96.20988    104.52507
#>  [971]     49.80914     68.64555     65.65868     63.69451     56.03852
#>  [976]     60.35028     78.57725     57.84726     56.27731     62.03730
#>  [981]     79.43851     62.22352     59.05633     71.52142     75.12140
#>  [986]    119.32763     78.87893     71.09793     53.69151     74.15997
#>  [991]     90.62135     73.37114    104.75102     94.34496     87.30797
#>  [996]     62.27569     54.21119     82.36188     60.58747     60.31592
```
