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
#>  [1]  7.755579  7.755579  7.755579  7.755579  7.755579 17.346920 14.448266
#>  [8] 14.448266  2.137147 79.004358 28.732815 28.732815 28.732815 39.353043
#> [15] 39.353043 39.353043 39.353043 70.411813 49.136061 49.136061

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
#>    [1]     66.28342     66.48370     59.09117     62.21330    157.59846
#>    [6]     56.87584    126.36559     58.61777     58.24709     96.49016
#>   [11]     63.42293     72.73106     58.17428     62.75810     60.62318
#>   [16]     61.21440     65.37432     62.55247     63.94495     59.31864
#>   [21]     71.03471     55.59991     65.18080     55.06939     65.88522
#>   [26]    189.89782    208.96910   1223.06567     57.32099     60.43691
#>   [31]     79.57047     61.53330     58.95189     67.41323     59.23796
#>   [36]     96.97044     68.51563     54.89577     65.79650     61.08843
#>   [41]     61.42615     69.13864     85.30818     55.71968     67.58967
#>   [46]     64.02467     82.17453     67.78110     62.60550     66.60683
#>   [51]    114.30164     56.61276     97.41600    290.49125     79.88777
#>   [56]     58.56794     69.87204     61.71111     62.30718     80.56502
#>   [61]     53.64863    275.01247     50.45336     60.42029     55.55111
#>   [66]     64.98759    934.45291     67.85062     65.06309     71.63733
#>   [71]     97.45711    328.29848     52.89071   2520.30057   2453.61696
#>   [76]     60.83037     74.35352     70.86964     73.50548     87.73213
#>   [81]     65.78243     61.05705     54.92784     54.26899     63.37937
#>   [86]     55.10677     56.16622     63.50436     61.46615     63.26081
#>   [91]     59.35917     57.28998     63.37399     62.88204     55.62731
#>   [96]     95.19647     86.69374     64.18140     63.18990     94.79078
#>  [101]     71.48416     73.60968     68.51764   1947.73982   2379.40771
#>  [106]     67.90415     89.33447     54.64331     80.46435     52.68010
#>  [111]     80.26447     55.02576     78.29962     63.44035     59.29884
#>  [116]     58.39069     60.59003     68.67736     83.27278     58.33660
#>  [121]     62.78908     65.41887     63.37684     66.90215     73.98594
#>  [126]     75.20193     86.69416     75.84814     65.27304     69.47986
#>  [131]    164.80161     79.11822     59.19735     61.82159     92.65177
#>  [136]     69.68632     65.15204     98.28696     85.57655     83.05918
#>  [141]     55.74003     61.98067     56.31374    126.38936    205.40243
#>  [146]     60.29247     61.06577     66.67418     88.55861     81.51414
#>  [151]     96.23283     49.00438     55.80528     74.94193     86.40026
#>  [156]    111.91953     54.89170    137.51892     67.97001     60.53574
#>  [161]     66.54640     62.13299     71.84396     56.55456     65.82063
#>  [166]     67.17299    129.05975     55.00041    100.91928     59.05884
#>  [171]    117.37554     70.29468    133.11187    102.79317     63.64046
#>  [176]     58.06853     63.42804     57.83925     82.62088     79.58759
#>  [181]     59.58973     84.74862     72.06348    110.84938     56.13851
#>  [186]     92.14021     74.66703     62.79409     53.15086    254.40925
#>  [191]    133.31162     58.83366    105.61383    168.64335     78.80862
#>  [196]     59.99921     58.14356    114.74221     71.54721     60.09031
#>  [201]     62.74234     75.16680     67.43859     77.34220     63.18320
#>  [206]     54.58391     58.41633     59.06561     55.87488     57.76206
#>  [211]     61.07142     57.57716     58.38398     73.42957     64.37555
#>  [216]    110.76005     70.04070   1706.17942     57.30900     67.65784
#>  [221]     59.43716     55.66850     96.99528     53.48965     57.02009
#>  [226]     65.59621     60.65656     66.54337     80.02930     67.32199
#>  [231]     59.03525     62.91291     66.32234    104.38571    108.03591
#>  [236]     58.49379     67.94045     62.04255     57.36474     58.28671
#>  [241]     67.70614     59.42545     57.95772     85.01721     63.79772
#>  [246]   2695.54226  12744.92858    242.43027     82.01738    210.34531
#>  [251]     56.43094     64.88549     67.70653     60.58760     77.11656
#>  [256]     66.13574     64.41044     69.04331     89.64844     85.68112
#>  [261]    108.14431     68.75325     66.61551     64.32576     56.28043
#>  [266]     58.59173     81.38790     71.90975     50.34373     56.72366
#>  [271]     58.93092     59.89895     84.56206     66.16320     57.89449
#>  [276]     83.07063     64.95515     62.31846     58.48557     63.99267
#>  [281]     61.15408     61.02959    117.02364    103.04421     76.00817
#>  [286]     55.45778     61.17846     65.48861     58.11726     65.68468
#>  [291]     71.07546     57.07542     65.31495     94.26816     57.48225
#>  [296]     55.75810     74.62749     62.34321     67.74233    147.38407
#>  [301]    140.04113     64.59169     95.45209    104.74794     99.90185
#>  [306]    141.44301     70.45353     59.75091     60.26046     59.67483
#>  [311]     83.31219     56.73274     58.95148     87.64756     86.65411
#>  [316]     63.56692     65.31614     62.78389     61.95273     62.17709
#>  [321]     58.91591     66.15479     91.46599    154.57020     60.48758
#>  [326]    124.15746     61.28180     65.57337    200.66489     77.88531
#>  [331]     60.99649     57.11499     77.97074    103.96122     69.60159
#>  [336]     80.84402     62.10895     52.25142    554.50801     86.38064
#>  [341]     66.53310     56.06798     60.60037     61.35714     71.02987
#>  [346]     83.29527     66.03285     57.40681     65.41802     75.45771
#>  [351]     74.89570     54.80053     63.36169     57.17031    121.94710
#>  [356]     49.29533     54.40253    100.53747     68.67516     85.81166
#>  [361]    611.24310    193.62798    300.20130     98.48597     64.27710
#>  [366]    148.59364     60.22460     73.95627     90.79540     75.89068
#>  [371]    120.54866     61.15935     57.26941     65.34638     73.76251
#>  [376]    128.41727     84.63643   3378.56829     71.92723     75.22106
#>  [381]     56.12384     58.90630     63.88444     60.28780     67.24995
#>  [386]    137.09734     59.27163    806.29778 927567.68079    787.94576
#>  [391]     61.72939     75.81162     66.42413     61.13594     89.68769
#>  [396]     56.54150     65.67668     97.71232     70.76995     55.14614
#>  [401]    242.54044     70.36672     58.28685     62.78825     66.74971
#>  [406]    376.92256     51.74611     57.39047     60.08215     62.16934
#>  [411]    212.92791     76.82768     87.20318     56.70061     63.78489
#>  [416]     60.41385     67.40721     60.97550     73.23670     62.95405
#>  [421]     81.45843     68.00300     72.05002     55.69683     58.14382
#>  [426]     61.93478     60.25425     60.69883     61.54663     66.00750
#>  [431]     68.28517     65.52940     65.09026    130.97667    203.14190
#>  [436]     61.26938     60.11076    225.68644    285.32552     73.84634
#>  [441]     83.94097     61.81223     74.13340     61.72339     64.33934
#>  [446]     58.36921     60.43007     69.92613     71.93817     63.78004
#>  [451]     55.22236     54.26484     56.41669     55.61650     72.97733
#>  [456]     60.87428     59.02371     89.82936     80.12322     80.40872
#>  [461]     44.74659     81.00029     68.61356     50.79497     61.28380
#>  [466]     60.86846    135.18923    105.15773    173.45331     59.82736
#>  [471]     56.18169     64.49026     86.78365     55.72939     68.41826
#>  [476]     56.93655     57.31858    148.46855     54.99007     92.24853
#>  [481]     50.84368     57.96414     86.85154     55.02289    620.30197
#>  [486]     65.72197     61.99192     95.41578     53.85504     75.46674
#>  [491]     55.95360     62.92194     59.20101    103.31516     57.53924
#>  [496]     69.62307     80.72999    290.86219    352.50502     62.73368
#>  [501]     95.36700     62.31009     61.79525     63.22051    119.47575
#>  [506]     60.27678     62.27805     88.34711     60.10281     60.37992
#>  [511]     81.36475     62.18010     64.98994     54.52893     66.90592
#>  [516]     61.85970     57.31077    361.63207     61.21731     59.88602
#>  [521]     61.94390    112.35642    101.12229     57.69897     61.14331
#>  [526]     55.39745     65.78922     56.31001     60.65209    123.86699
#>  [531]     58.06429    434.35811    165.06577    343.75551     69.43168
#>  [536]     96.42657     53.67994     59.64025     59.09902     72.98772
#>  [541]    187.12129     69.32413     62.82730     67.78088     61.62354
#>  [546]     67.59269     71.10944    300.17949     56.90329     60.83003
#>  [551]     62.46019     70.55375     64.42317     65.67562     57.87061
#>  [556]     75.37452     58.25171     69.83203     56.45713     80.46618
#>  [561]     53.38192     66.74197     80.97084     54.05795    129.56865
#>  [566]     77.05155    109.68250    170.46873     58.85548     87.61074
#>  [571]     53.27907     68.22997     53.15889     57.98584     63.70439
#>  [576]     59.10240     83.29031     56.01667     58.17371     61.50600
#>  [581]     61.55499     64.92123     94.61995     91.70067     59.96952
#>  [586]     64.21263     66.03956     62.12963    232.64315    259.53693
#>  [591]     60.17749     57.59000     62.35392     54.97906     90.19015
#>  [596]     72.95431    119.24626     57.86809     73.18754     72.57859
#>  [601]    102.66454     68.34564     71.41178     57.39568     70.81665
#>  [606]    418.73375     62.49052     66.05932     81.17820    126.79653
#>  [611]     58.09399     90.13111     74.84682     87.57301     78.82376
#>  [616]     64.52188     73.73314     58.90675     63.60032    122.58077
#>  [621]    114.76689     58.16024     58.00963     64.51105     82.78519
#>  [626]     66.99740    119.67263     60.31392     59.74704     66.54917
#>  [631]     52.70293    145.86602    158.41143    102.10625    131.52858
#>  [636]     56.33503     80.63272     68.74357     66.36395     67.32175
#>  [641]     81.72333    135.09221     73.74164     49.38469     42.97186
#>  [646]     76.97501     74.41169     74.78423     78.15330     62.17379
#>  [651]     57.12701     58.88507    112.40020     57.94274     57.91649
#>  [656]    116.58086     72.19551     72.82033     98.38004     70.01068
#>  [661]     63.30636     60.46318     51.35746     69.29098     83.34382
#>  [666]     92.28641     61.37840     60.94484     55.57848    158.38421
#>  [671]     63.02890    110.27960     92.28146     63.19005     52.10271
#>  [676]    182.42343     62.99871     56.93545     59.92782     65.65069
#>  [681]     66.26411     82.50597     82.36464     69.53827   1708.38679
#>  [686]     98.94283     59.80162     92.14242    254.12333     65.62825
#>  [691]     57.82638     57.64245     57.51557    277.58453     56.86889
#>  [696]     58.32686     57.63767    127.13723    162.68697    145.70831
#>  [701]     80.60022     84.26561     62.11493     55.43537     79.91482
#>  [706]     58.33929     75.78085     66.15259     57.82036     78.23461
#>  [711]     55.93451     64.01037     59.77854     59.40829     77.42391
#>  [716]     69.87118    165.13176     58.94236     62.80242     56.75439
#>  [721]     66.63862     60.86363    125.79277     66.53662     60.82157
#>  [726]     59.03265     70.00281     63.00104     62.84814     94.77395
#>  [731]     80.71828     68.68349     81.41414     70.97808     56.56828
#>  [736]     57.49881     68.80135     61.46552     56.58538     68.22255
#>  [741]     68.03510     73.55838     66.87146     63.22420     60.27432
#>  [746]     61.43215     80.83442     56.98703    110.97046     61.47371
#>  [751]    197.79712     54.54811     50.22990     62.89901     53.29310
#>  [756]     62.08662     68.84154     64.55014    112.82264    138.32908
#>  [761]    678.89752    280.67289    115.41574     58.22795     71.90196
#>  [766]     57.66655     67.43884     75.56972     55.42184     72.44868
#>  [771]     71.75711     66.76073     59.16395     75.49115     65.29080
#>  [776]     63.60601     60.57595     73.22665     65.03851     58.21222
#>  [781]     64.19975     70.47166     63.69923     59.71886     48.59549
#>  [786]     67.37949    128.27921    181.83134   1020.65552     80.82589
#>  [791]     89.28246     89.87135     62.01205     56.26708     64.79250
#>  [796]     75.67151     54.71202     66.20570     51.30303     67.85093
#>  [801]     59.85548    117.39816     58.08260     60.96187    165.30804
#>  [806]    371.41469     93.94424     52.27871     83.65137     86.49465
#>  [811]     59.54108     62.85224     61.01413     66.06975     84.49248
#>  [816]     75.00935     61.18186     63.68916     78.35692     65.55448
#>  [821]     73.24575     58.33899     60.12679     53.65264     58.04463
#>  [826]     58.75577     92.04588     57.29345     90.71725     56.13640
#>  [831]     71.32481     55.10873     59.51764     66.58585     57.53643
#>  [836]     64.97550     93.26259     56.54950     88.03013     55.55390
#>  [841]     71.13482     59.35758    100.17152     63.32438    124.28540
#>  [846]     54.18068     74.61406     91.74470     66.26108    252.85011
#>  [851]     66.18726     57.60959     76.35732     79.08288    484.85442
#>  [856]     56.01084     61.67728     68.11885     96.21951     65.46848
#>  [861]     58.94806    124.59084     55.82755     77.26833    169.99904
#>  [866]     70.45073     76.54497     61.76898     56.82495     61.73464
#>  [871]     59.87818     58.62227     70.20541     85.90878     93.49568
#>  [876]     78.07969     58.86136     58.22552     86.20147     83.64305
#>  [881]     88.79516    162.50719     50.73161     64.51446     94.37945
#>  [886]     80.86774     57.22549     64.25340     71.61471    116.01225
#>  [891]     64.44106     90.30329     56.69837     79.73285    361.31894
#>  [896]     85.64826     67.42658     58.64729     52.23125     55.73401
#>  [901]     72.73150     63.73527    181.70733    106.30195     50.80780
#>  [906]     60.51486    105.07646     58.43357     54.94201     62.03579
#>  [911]     64.65694     57.72130     68.66573     60.07984     95.78213
#>  [916]     66.86523     66.79180     59.38640     65.16539     62.90529
#>  [921]     59.57668     60.67980     72.89931    152.89417     60.87777
#>  [926]     85.65955     76.11963    124.42381    139.43546     60.81033
#>  [931]     58.12566     79.96740     95.14898     57.25285     58.42371
#>  [936]     61.34397     85.87171     99.60802     59.68030     56.29730
#>  [941]     59.90607     60.18778     61.75173     61.18828     63.97443
#>  [946]     63.48320     90.60976     58.61324     55.08690     82.14334
#>  [951]     60.53809     65.81876     70.96271     67.61680     77.02669
#>  [956]    190.70973     87.39813    189.39964     64.80826     64.92556
#>  [961]     77.82963     73.14338     57.02740     61.04396     64.05603
#>  [966]    105.84964    130.86195     54.28795     69.85608    518.89906
#>  [971]     90.89103     94.19899    129.37656     68.41878     57.27247
#>  [976]     62.10606     64.76213     63.20420     64.68160     67.45794
#>  [981]     56.10507    107.13198     63.31750     76.46627     56.43322
#>  [986]     66.12884     58.83244     63.75338     58.71504     52.76395
#>  [991]     99.75185    125.59695     82.19810     57.11480     73.68825
#>  [996]     62.30870     60.18690     60.65430     67.56290     67.76412
```
