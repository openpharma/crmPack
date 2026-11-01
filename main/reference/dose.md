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
#>  [1]  30.69685  30.69685  30.69685  30.69685 182.02231 182.02231  50.55368
#>  [8]  50.55368  50.55368  94.29366  94.29366  82.27158  82.27158  29.71101
#> [15]  18.20121  18.20121  18.20121  18.20121  18.20121  25.99083

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
#>    [1]    144.94064     57.78749     83.50746     61.41733     91.56776
#>    [6]     65.50577    111.00923     49.36117    135.29385    158.37022
#>   [11]     71.48719     57.93753    377.45766     62.81612     61.55746
#>   [16]     66.07996     79.49596    118.09878     62.40999     86.37782
#>   [21]     89.65355    108.17089     66.36600     74.46602     94.93933
#>   [26]     98.54161     84.59519     62.50576    184.76264     85.29587
#>   [31]     67.85546     62.22425     60.54876     65.94542     65.46941
#>   [36]     63.81632    136.69036     65.40307     60.88079     58.97159
#>   [41]     56.81829     59.92484     58.91722     59.81910     64.50707
#>   [46]    159.43720     63.02931     64.07690     78.04038     57.12460
#>   [51]     55.84980    563.27876    237.56803     69.33497     56.56691
#>   [56]     57.23556     60.86041     63.83419     55.96158     69.36379
#>   [61]     72.65209     77.77156     53.67335     72.83781     75.63634
#>   [66]     69.09998     60.06503     70.09187     57.21220     56.01171
#>   [71]     55.89883     73.70472     61.43443     68.54299     54.99323
#>   [76]     87.49190     76.05442     55.73715     65.83554     65.80619
#>   [81]     70.55882     64.75078     61.89727     83.86761     61.42206
#>   [86]     65.33410     69.58307     73.30401    168.26579    167.73369
#>   [91]    682.60937    101.19855     84.97642     71.04515     84.82512
#>   [96]     79.90402     66.45083     63.06396     57.00223     65.39083
#>  [101]     61.04658     71.46997     53.92833     61.23148     61.06114
#>  [106]     65.90238     59.89071     69.36357     58.65487     60.79256
#>  [111]     54.74449     81.06606    102.27022     72.05560    104.00188
#>  [116]     95.87723  73245.10633 139095.52949     73.13788     69.91123
#>  [121]     60.34277     49.05913     68.84290     93.86581     60.30438
#>  [126]     69.17954     61.37748     59.35329     60.89991     67.25251
#>  [131]     60.14496     92.24814     80.60613     68.49691     54.72356
#>  [136]     62.84176     59.15483     58.99409    161.27542    105.68512
#>  [141]     65.12083     57.33945    131.40175     63.76531     67.40586
#>  [146]     59.89930     63.26382     61.25585     60.31892     60.88214
#>  [151]     68.78878     62.17892     64.24585     69.40408     59.53050
#>  [156]     60.86031     61.00840     59.01852     61.60209    346.26977
#>  [161]    121.41108     72.91090     79.59933     67.37003     69.06263
#>  [166]     65.97909     51.75844    120.01711    213.37153     54.69190
#>  [171]     97.95053    137.27097     58.23024     67.66461     65.23973
#>  [176]     53.51303     62.28011     58.10857     90.21821     58.76231
#>  [181]     59.90318     57.92194     59.36935     59.60588     97.92625
#>  [186]     57.35336     85.65286    123.75884     69.89263     68.65641
#>  [191]     56.94710    131.23735     65.25117     57.22920     59.39870
#>  [196]     75.90481     59.23147    173.59560    177.32337     54.04010
#>  [201]     54.72754     58.11723    136.78025     56.22584    186.64820
#>  [206]    331.60549    217.12006     86.86063   1012.92550     79.23561
#>  [211]     58.27017     87.57502     93.75276     64.49565     51.91929
#>  [216]    131.61917    775.75053    144.71999     55.13014     75.14830
#>  [221]     60.29723    112.13825     70.28535     59.05308     62.26734
#>  [226]     65.18309     63.75233     69.52048    163.63206     60.40971
#>  [231]    111.07000     64.45786     63.74237     77.27391     68.59945
#>  [236]     71.60175     54.31283     82.06073     73.04032    376.67791
#>  [241]     62.40293     61.50957     60.96482     66.40751     76.96177
#>  [246]    101.10548     64.65679     56.72762     60.63060     72.86742
#>  [251]     74.81721     75.84489     54.98375     65.61966     83.03580
#>  [256]     66.21547     57.50744    207.24029    307.79408     64.16316
#>  [261]     73.53895     69.25912     56.20963     64.32277     63.33452
#>  [266]    101.67110     56.40387     61.83680     85.22191     60.64756
#>  [271]     66.73651     62.45234     48.35731     55.77893     65.84498
#>  [276]     61.14520     79.74418     56.31092     70.52338    107.44694
#>  [281]     58.32834     61.11964     59.26791     59.84295     55.39933
#>  [286]     90.30544    349.35172     67.00773    183.77505    108.13719
#>  [291]     50.48489     59.54757     72.88926     58.20464     75.85137
#>  [296]     73.68770     65.22127    109.18498    333.99738    104.56454
#>  [301]     62.84378     78.05404     55.38570     86.89137    178.28713
#>  [306]     58.70962     50.46137     88.93425     55.07827     55.13894
#>  [311]     75.25120     68.05455     56.69123     78.14174     59.43387
#>  [316]     62.34827     84.65227    136.66044     64.57103    101.69703
#>  [321]     64.99321    152.92565     63.96110     66.19098     72.95327
#>  [326]     62.49791     56.93854     61.29995     52.49259     56.34582
#>  [331]     61.25729    139.80459    174.87888    255.25180     64.60300
#>  [336]     69.49072     59.63408     60.84656    138.91600    580.89156
#>  [341]     66.27200     54.83516     67.61087     62.57780     72.11298
#>  [346]     62.09627    235.75960     60.62303     64.93339     61.77610
#>  [351]     54.89173     58.38288     63.82669     76.27638     74.73880
#>  [356]     77.49385     59.23042    167.80917     55.43354    151.11693
#>  [361]     68.96547     57.05451    255.62235     78.96888     61.50203
#>  [366]     66.27284    265.85552    100.18185     70.70542     58.51099
#>  [371]     70.97204     55.39147     56.97129     62.36170     66.68593
#>  [376]     91.26013     60.83747     65.46924     63.89979     64.02952
#>  [381]     61.25387     65.32630     84.60877    111.74159     55.25879
#>  [386]    131.72477     52.87093     66.97269     60.69637     74.88708
#>  [391]     66.96243     52.97187    137.28924     58.19105     55.99350
#>  [396]     65.56878     68.92605     62.19367     62.02814     68.57172
#>  [401]     67.90136     83.94958     67.00864    260.87290    104.71632
#>  [406]     57.82303     64.47750     62.55759     57.10915     59.23080
#>  [411]     70.64923     60.33999     56.06667     66.63338     61.30266
#>  [416]     64.15771     63.62325     65.55637     70.86746     63.00057
#>  [421]     58.30478     62.11546     64.57016     60.56331     80.75735
#>  [426]     76.32926     71.23811     62.60155     56.96641     63.65790
#>  [431]     62.68993     68.63277    119.85225     60.75911     59.02915
#>  [436]     68.09753    120.78830     58.49657     82.58242     99.98576
#>  [441]     61.77934     67.66745     53.68418     60.00684     68.68427
#>  [446]     70.31604     58.39065     56.93348     62.38102     99.47810
#>  [451]     60.73058     53.13690     65.91814     55.14258     73.39629
#>  [456]     59.48456    160.03539     65.67903    157.47081    192.74920
#>  [461]    103.53732     65.51558     62.67602     66.87106     59.42066
#>  [466]     85.25165     70.81888     66.74321     66.25723     64.22774
#>  [471]     86.10448   1125.47211     72.04542    538.65646     76.56386
#>  [476]     58.38446     66.22836  30057.14565    403.38660     69.71753
#>  [481]     84.67216    628.10274     92.33833     60.37619     68.44126
#>  [486]     70.31352     57.68801     56.99075    178.33245     56.54372
#>  [491]     80.45118    484.79606     99.29100     81.96692     65.38968
#>  [496]     60.62655     76.22007    211.60357     54.47076     70.86661
#>  [501]    145.99939    134.78743     97.08392   5373.86858     68.55342
#>  [506]     58.21861    144.74504     71.17821     76.79267     55.12565
#>  [511]     64.25567     63.13194    141.50732     75.51122    146.31813
#>  [516]     63.46617     60.69882     74.97670     64.18827     68.38824
#>  [521]     84.80735     53.01193     73.80531     86.96021     57.43354
#>  [526]     73.85053     57.93872     85.33555     61.30592     92.13529
#>  [531]     61.85823     52.86136     56.28944     72.11452   1734.83063
#>  [536] 299251.03731     60.22355    159.30198    251.98603     55.52301
#>  [541]     60.01296     65.13859     58.58052     57.32665     63.99470
#>  [546]     53.87309     58.65503     63.17786     57.52931     76.38181
#>  [551]     57.15245     59.99203    104.99505     85.76530     68.30815
#>  [556]     86.20828     74.82867    121.29630    329.40740    599.70038
#>  [561]    140.74593     60.40898     91.15025     66.55281     52.68390
#>  [566]     65.54458     65.42405     66.91360     55.38653     56.64306
#>  [571]     62.09181     58.11605     64.99949     94.23274     58.52236
#>  [576]    241.51159     56.61990     60.42610     59.78668     57.75045
#>  [581]     67.19053     56.89831     54.93055     60.27830     72.59644
#>  [586]     68.33299     95.71511     56.66816     72.36737     89.15796
#>  [591]     58.47820     58.48520     59.37739     57.92841     58.11796
#>  [596]     62.81773     59.17578     83.76838     69.83372     57.72999
#>  [601]    215.41688     77.35778     60.60096     91.56838     70.26220
#>  [606]     51.75327     57.03390     77.71696     66.09968     78.04426
#>  [611]     67.14005    128.89912     53.04980     56.55159     55.83043
#>  [616]     61.04835     72.14177    691.66006    121.29236     46.77861
#>  [621]     73.04825     85.86672     55.36112     54.55022     54.50841
#>  [626]     61.41159     69.53741     62.16059     60.34341     70.61311
#>  [631]     74.21664     60.22722     68.20995     64.38401    153.57969
#>  [636]    358.39266     62.13056     70.33771     58.77145     61.22899
#>  [641]     65.55316     50.45498     56.95000     73.98298     77.58053
#>  [646]     69.82602     58.56317     67.14674     59.76532     69.41377
#>  [651]     59.43422    100.93975     65.14470     99.61358    108.71100
#>  [656]     56.90907     62.04215     58.91229     57.76272     83.16476
#>  [661]    236.81732     85.34786    519.63683   2876.54971     74.69312
#>  [666]     58.25325     55.27424     57.07795    108.59231     68.78039
#>  [671]     97.63000    132.94757     65.08035     62.75282     62.44282
#>  [676]     75.78283     65.15657     70.02955     86.13635     86.82037
#>  [681]     55.16024    134.06246    128.98744    133.23965     59.26169
#>  [686]     58.68073     71.83255     69.41191     82.27162     64.22818
#>  [691]     67.35524     83.61113    130.08019     62.17661     79.07836
#>  [696]     53.36500     58.87264     85.91152     61.25093     76.05121
#>  [701]     54.30167     59.83073     60.16007    172.52276     64.79437
#>  [706]     79.98076     97.24937    151.76824    246.70427     99.77422
#>  [711]     61.16474     63.88953     68.58795     56.93738    120.10884
#>  [716]    605.28328    119.64238     64.26393     65.88258     67.23990
#>  [721]     86.60575    142.21441    175.17621     82.34175     58.47617
#>  [726]     63.55746     49.39159     83.91705     57.50366     61.29598
#>  [731]     51.79362     55.81130     58.26552     58.81376     89.82409
#>  [736]     57.01683     58.86000     77.48177     53.70126     62.53407
#>  [741]     75.65899    201.65549     90.65441     67.70583     55.99902
#>  [746]    142.08224     63.04947     59.40672     55.90087     71.81336
#>  [751]     57.64849     67.36282     70.09530     61.18134    177.75381
#>  [756]     71.82241     57.41971     68.85868    125.19691     65.20875
#>  [761]     91.54390     66.02564     56.66340     58.00460     57.25783
#>  [766]     88.88928     73.93509     62.73539     58.19890     62.02630
#>  [771]    151.91713    109.57341     67.83240     67.83157     72.50502
#>  [776]     66.98055     76.70547    195.70387     46.98551     61.69944
#>  [781]     67.81822     92.69092     61.10327    127.90607    350.94925
#>  [786]     73.85161    203.31160     61.32988     85.83418     64.11781
#>  [791]     56.81985     59.13054     51.86490    590.74819     63.61369
#>  [796]     55.41766     54.01007     56.08638     74.12783     58.20414
#>  [801]     56.75720    329.28700    195.76309    152.76721    118.82214
#>  [806]     66.43898    108.37300     72.82574     60.88211     61.95925
#>  [811]     57.87713     58.66087     87.22714     61.62056     77.27207
#>  [816]     93.94031     69.86856     58.83765     60.32835     71.53887
#>  [821]     64.76214    300.96716     70.37734     63.80732     58.58703
#>  [826]     58.83580     70.16922    121.66268     64.28658     54.00809
#>  [831]     46.24775     60.72396     55.90850     72.87380     68.50788
#>  [836]     75.57383     64.58731     56.64243     67.14690     56.31660
#>  [841]     74.77206     69.94361     55.57841     57.10045     59.46203
#>  [846]     63.44037     59.22315     71.38306    156.65581     68.79052
#>  [851]    100.43982     58.63503     63.39554     86.44699    198.49003
#>  [856]     60.78501     58.74548     80.84087     68.53346     62.40250
#>  [861]     58.55208     59.94274     60.62246     58.20589    101.60675
#>  [866]     83.15948     58.38299     54.66347     79.18405     81.88654
#>  [871]    167.03889     53.29463     55.88988     61.78423     63.22480
#>  [876]     64.42295     60.58456     89.51190     57.85599     81.80615
#>  [881]     61.61034     57.74608     58.61418     59.90750     62.45382
#>  [886]     58.46202     58.84154     65.54320     58.57921     66.69788
#>  [891]     72.92092     56.62389     64.55343     64.71354     64.61821
#>  [896]     62.72298     64.60454     58.87324     67.98312    340.20052
#>  [901]    126.86424    121.14620    107.56819     64.19450     57.62600
#>  [906]     65.53232     59.42939     56.23451    114.35456  26845.98482
#>  [911]     61.53071     60.72086     57.53431     65.65668     59.36768
#>  [916]     92.82334    217.97347    186.85846     81.59748     88.98461
#>  [921]     76.15935     72.75405     63.37664     63.51364    116.08562
#>  [926]     72.84896     56.60980     88.68137     77.83181     67.39747
#>  [931]     57.55693     60.14969     63.73179    110.61979     59.72839
#>  [936]     61.46196    146.96677     75.38258    156.53401     60.51686
#>  [941]     95.81823     78.22258     59.56957     55.94229     64.44369
#>  [946]     61.65941     57.54875     59.78944     60.51588     75.77436
#>  [951]     60.80577     58.15425    103.77925     72.08286     80.44688
#>  [956]     96.73730     61.35364     76.81202     81.89389     69.34041
#>  [961]     72.49099    130.81608     80.10841    232.97447     61.09880
#>  [966]     71.76821     56.50806    177.20364     59.74584     85.33380
#>  [971]     60.72550     55.05976     79.85672     64.91849     86.96470
#>  [976]     59.43592     67.47132     63.24000     45.42608     63.23010
#>  [981]     80.48324     59.50353     70.15556     79.68571     68.75293
#>  [986]     60.22294     65.82668     65.36844    468.74487     55.17517
#>  [991]     73.16581     62.46555     57.71634    128.10496     50.82436
#>  [996]     98.69677    106.84303     96.25445     59.78402     72.77167
```
