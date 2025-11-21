# Compute Gain Values based on Pseudo DLE and a Pseudo Efficacy Models and Using Optional Samples.

**\[stable\]**

## Usage

``` r
gain(dose, model_dle, samples_dle, model_eff, samples_eff, ...)

# S4 method for class 'numeric,ModelTox,Samples,ModelEff,Samples'
gain(dose, model_dle, samples_dle, model_eff, samples_eff, ...)

# S4 method for class 'numeric,ModelTox,missing,Effloglog,missing'
gain(dose, model_dle, samples_dle, model_eff, samples_eff, ...)
```

## Arguments

- dose:

  (`number` or `numeric`)  
  the dose which is targeted. The following recycling rule applies when
  samples are not missing: vectors of size 1 will be recycled to the
  size of the sample. Otherwise, `dose` must have the same size as the
  sample.

- model_dle:

  (`ModelTox`)  
  pseudo DLE (dose-limiting events)/toxicity model.

- samples_dle:

  (`Samples`)  
  the samples of model's parameters that will be used to compute
  toxicity probabilities. Can also be missing for some models.

- model_eff:

  (`ModelEff`)  
  the efficacy model with pseudo data prior.

- samples_eff:

  (`Samples`)  
  samples of model's parameters that will be used to compute expected
  efficacy values. Can also be missing for some models.

- ...:

  not used.

## Value

The gain values.

## Details

This function computes the gain values for a given dose level, pseudo
DLE and Efficacy models as well as a given DLE and Efficacy samples.

## Functions

- `gain( dose = numeric, model_dle = ModelTox, samples_dle = Samples, model_eff = ModelEff, samples_eff = Samples )`:

- `gain( dose = numeric, model_dle = ModelTox, samples_dle = missing, model_eff = Effloglog, samples_eff = missing )`:
  Compute the gain value for a given dose level, pseudo DLE and Efficacy
  models without DLE and the Efficacy samples.

## Examples

``` r
# Obtain the gain value for a given dose, a pseudo DLE and efficacy models
# as well as DLE and efficacy samples.
emptydata <- DataDual(doseGrid = seq(25, 300, 25), placebo = FALSE)
mcmc_opts <- McmcOptions(burnin = 100, step = 2, samples = 200)

# DLE model and samples.
model_dle <- LogisticIndepBeta(
  binDLE = c(1.05, 1.8),
  DLEweights = c(3, 3),
  DLEdose = c(25, 300),
  data = emptydata
)

samples_dle <- mcmc(emptydata, model_dle, mcmc_opts)

# Efficacy model (Effloglog) and samples.
model_effloglog <- Effloglog(
  eff = c(1.223, 2.513),
  eff_dose = c(25, 300),
  nu = c(a = 1, b = 0.025),
  data = emptydata
)

samples_effloglog <- mcmc(emptydata, model_effloglog, mcmc_opts)

# Gain values for dose level 75 and Effloglog efficacy model.
gain(
  dose = 75,
  model_dle = model_dle,
  samples_dle = samples_dle,
  model_eff = model_effloglog,
  samples_eff = samples_effloglog
)
#>   [1] 1.3724463 1.1163011 1.1876543 0.5601303 1.6102540 0.6156309 1.0702110
#>   [8] 1.4812699 1.4689083 0.5991908 0.9135514 1.4188588 0.8914392 0.9510920
#>  [15] 0.9962461 0.8646416 1.4064603 0.6449197 0.6616455 0.8855359 0.6516058
#>  [22] 0.4573677 0.2922851 1.5184059 0.7864663 0.7575013 0.6550523 1.4341707
#>  [29] 0.6397446 0.5902374 1.3455265 0.5084815 0.6228258 0.2742999 1.1028332
#>  [36] 1.2683656 1.4552391 1.7914810 1.2640065 0.8954424 0.4557674 0.7072762
#>  [43] 0.6508919 0.9339104 0.6563602 1.0341947 0.5346614 0.9638692 1.2067207
#>  [50] 1.3048691 1.3450987 0.2951774 1.2150462 1.2154166 0.6994934 0.9380233
#>  [57] 1.6054284 1.4631007 0.7170195 1.1514112 1.2336247 0.7694697 0.6456181
#>  [64] 1.2906157 1.3531875 0.9304359 1.2255894 1.2325904 1.2405950 1.7368240
#>  [71] 0.8030362 1.3514498 1.2226824 0.4009393 1.0961200 1.3827692 1.3408870
#>  [78] 1.0155697 1.3858920 0.7852963 1.0389329 0.7076404 0.9516520 1.2265820
#>  [85] 0.6453395 1.1631187 1.1300966 1.0829065 1.3729595 0.6465427 0.9005294
#>  [92] 1.0391913 1.0106432 1.2346600 0.9347928 0.4895462 0.4382956 1.3669563
#>  [99] 1.2218775 0.8111870 0.4934214 1.1457987 1.1405332 1.1489580 1.4535811
#> [106] 0.3077070 1.2252347 1.5443211 1.3362472 1.1712998 1.0014473 1.1369257
#> [113] 0.8171996 0.2649928 1.1712471 0.4232131 1.0481358 1.4055183 1.3802208
#> [120] 1.1189702 0.9606329 1.2678369 1.2700036 0.5651039 1.8531221 1.5108585
#> [127] 1.5765327 1.1742238 1.5648763 1.0839926 0.9758415 0.8558122 1.1538830
#> [134] 1.1561811 1.4922312 0.9087434 0.6369058 1.1785576 1.2719208 0.7536225
#> [141] 0.4610063 1.0802367 1.2740320 0.7842036 1.5808339 1.4593170 0.6581335
#> [148] 0.5965571 1.1217183 1.6115560 0.9235499 1.4685061 0.9808467 0.4309496
#> [155] 1.4693931 1.1060406 1.7424236 0.6815545 0.9528618 1.3946497 1.0070073
#> [162] 0.9412750 0.6710952 1.3174514 1.4569902 0.9076429 0.1721784 0.6274587
#> [169] 1.0012481 0.6883088 1.0717720 1.3573660 1.2150049 1.7473766 1.1468327
#> [176] 0.6136378 1.1348749 1.0056950 1.2825347 1.2099100 1.1442010 1.3166977
#> [183] 0.6235300 1.0662774 0.8839399 0.8752322 1.2075558 0.8035865 1.1490294
#> [190] 0.8053042 0.8663338 1.7354374 0.9656245 0.7848750 1.0084461 1.1694038
#> [197] 1.1076468 1.2395853 1.3728410 1.1775367

# Efficacy model (EffFlexi) and samples.
model_effflexi <- EffFlexi(
  eff = c(1.223, 2.513),
  eff_dose = c(25, 300),
  sigma2W = c(a = 0.1, b = 0.1),
  sigma2betaW = c(a = 20, b = 50),
  rw1 = FALSE,
  data = emptydata
)

samples_effflexi <- mcmc(emptydata, model_effflexi, mcmc_opts)

# Gain values for dose level 75 and EffFlexi efficacy model.
gain(
  dose = 75,
  model_dle = model_dle,
  samples_dle = samples_dle,
  model_eff = model_effflexi,
  samples_eff = samples_effflexi
)
#>   [1] -7.797762012 -0.199336986  2.523438672  0.214818998 -0.547251404
#>   [6] -0.828895061 -0.077208139  4.198925893 -2.514839633 -0.993067176
#>  [11] -2.132389402  3.077140329  1.501540022  0.965118633  2.448944419
#>  [16]  1.603548539  0.184034225  0.557697919 -1.952719644  2.492728818
#>  [21] -0.846354589  1.614736279  0.041034314 -1.658099056  0.062305215
#>  [26]  0.929473739  1.467771682  5.695943231  2.757768442 -0.081225987
#>  [31]  6.972030665  3.929777824  0.289435533  0.816732419  2.506860558
#>  [36] -0.425530328 -2.334480198  0.586245417 -3.720766029  0.137125761
#>  [41]  1.426266668  2.928028475  2.507657658  0.594944379  1.404831845
#>  [46] -1.769188692  2.344694257  2.425595600 -3.724049447  5.721211847
#>  [51]  1.240789611  0.661480509 -0.935456834  1.859864240  3.598570254
#>  [56]  4.066092601 -0.283984842 -1.789499719  2.291953834 -0.340493900
#>  [61]  2.729746082  3.088494146  0.002428012  5.549212571  2.237065518
#>  [66] -1.146073828 -1.215629407  4.093263442  4.365740674 -3.579946118
#>  [71]  0.868637136 -0.796092404  1.728214732  1.480074653  3.188259955
#>  [76]  1.805344902 -1.316897949  0.917944815  2.347651429 -2.057661335
#>  [81]  1.924687634 -0.059670211  2.093612794 -0.844486312 -1.187736649
#>  [86]  4.166443525 -6.163708089 -2.062453988  0.389062104 -0.293947515
#>  [91]  3.633662621  7.428196399 -0.427327028  0.842044527  1.278787634
#>  [96] -1.228131627  1.172618349  7.690292673 -2.492784535  1.276139945
#> [101]  0.093697215 -0.505052555 -0.776274633  0.063380404 -2.593648360
#> [106]  0.261357399 -1.711652634  1.622242271 -0.362393167  4.461089005
#> [111]  3.450033563  6.051686737 -1.351043509  1.375461317 -3.742672152
#> [116]  0.242883309 -3.645677222  4.822342119  2.679937625  2.173108830
#> [121]  0.348438489  0.070456192  4.777059451  5.306383005  3.369429858
#> [126]  6.363568789  2.947605736  0.669428991 -4.638862209 -0.544622733
#> [131]  0.993147986  0.850065911  4.797947307 -0.434228667 -0.487685678
#> [136] -0.263474416 -0.130119997 -1.364954217  1.327120227  1.435113453
#> [141]  1.800903129  2.340832586 -2.214719403 -2.190862346 -1.090888958
#> [146] -0.571400968  1.106625717  3.326360639  0.832149924  4.986322978
#> [151]  4.470591160 -0.643807544  3.225924662  1.616645210 -3.171086291
#> [156] -1.400515992 -7.580812166 -1.147022482  3.689024159  5.515599083
#> [161] -0.133504677  1.728566992  2.175123544  8.539899820  2.385615370
#> [166]  6.408512552 -0.585672716 -1.243101730  7.299838519  0.975173504
#> [171]  4.320059944 -4.355521566 -0.778941163  2.320029619  4.591606364
#> [176]  1.794935961  3.213033813 -1.320620632  3.283390555  3.408311801
#> [181]  2.528529997  0.216659653 -0.362232228  0.796268488  3.334793341
#> [186]  3.359729037  3.392465865  1.565422596  5.336320093 -0.466097505
#> [191]  1.968541610  3.428795440 -0.408071475  1.622276788 -4.710188753
#> [196] -1.358776379  1.004604940  2.729044949  2.522123248 -0.558105107
# Obtain the gain value for a given dose, a pseudo DLE and efficacy models
# without DLE and efficacy samples.
emptydata <- DataDual(doseGrid = seq(25, 300, 25), placebo = FALSE)
data <- Data(doseGrid = seq(25, 300, 25), placebo = FALSE)
mcmc_opts <- McmcOptions(burnin = 100, step = 2, samples = 200)

# DLE model and samples.
model_dle <- LogisticIndepBeta(
  binDLE = c(1.05, 1.8),
  DLEweights = c(3, 3),
  DLEdose = c(25, 300),
  data = data
)

# Efficacy model and samples.
model_eff <- Effloglog(
  eff = c(1.223, 2.513),
  eff_dose = c(25, 300),
  nu = c(a = 1, b = 0.025),
  data = emptydata
)

# Gain value for dose level 75.
gain(
  dose = 75,
  model_dle = model_dle,
  model_eff = model_eff
)
#> [1] 1.020657
```
