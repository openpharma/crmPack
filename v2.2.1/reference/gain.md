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

  (`number` or `numeric`)\
  the dose which is targeted. The following recycling rule applies when
  samples are not missing: vectors of size 1 will be recycled to the
  size of the sample. Otherwise, `dose` must have the same size as the
  sample.

- model_dle:

  (`ModelTox`)\
  pseudo DLE (dose-limiting events)/toxicity model.

- samples_dle:

  (`Samples`)\
  the samples of model's parameters that will be used to compute
  toxicity probabilities. Can also be missing for some models.

- model_eff:

  (`ModelEff`)\
  the efficacy model with pseudo data prior.

- samples_eff:

  (`Samples`)\
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
  Compute gain values from toxicity and efficacy model samples.

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
#>   [1] 0.6408328 0.9328737 1.3317937 0.9755601 1.5632863 0.6305203 0.9138837
#>   [8] 1.3243097 1.6300890 0.6922057 0.6948791 1.1977047 0.7107659 1.3493244
#>  [15] 1.1315107 0.5192456 1.6181089 1.1255197 0.6139932 1.1130316 0.2994175
#>  [22] 0.7625404 0.2999797 0.8641572 1.2187100 1.2814324 0.3824490 0.9207420
#>  [29] 1.1403403 0.3695379 1.4697527 0.9047391 0.4072838 0.4086533 0.9336283
#>  [36] 1.0253846 1.2746207 1.2389333 1.5941384 1.1963421 0.5599394 0.5972302
#>  [43] 1.0144189 0.8002580 0.4502802 1.1163495 0.5809296 1.4244017 0.9590778
#>  [50] 1.2828274 1.5297917 0.8138715 0.7212816 1.4461675 1.0349981 0.7453069
#>  [57] 1.0210496 1.2419379 1.3741230 1.0986051 0.8758285 1.1735655 0.5976080
#>  [64] 0.8904322 1.6233541 0.8059716 1.4919759 1.1787462 1.0145394 1.3836082
#>  [71] 1.2268082 1.1704848 1.2750769 0.9169179 0.5963439 1.5454189 0.7963324
#>  [78] 1.7116639 0.9309376 0.9485817 0.9387339 1.1573696 0.6048362 0.9319197
#>  [85] 1.1612720 0.9907762 0.9403920 1.1415660 1.3232608 0.9664984 0.6543996
#>  [92] 0.9952130 0.8423264 1.0979808 0.7735316 1.0236896 0.3128155 0.7172526
#>  [99] 1.2987465 1.3156077 0.8861968 0.8041023 1.3887307 0.7626237 1.0574967
#> [106] 0.5654114 0.9387753 1.1332396 1.3187461 1.3750455 1.2840815 1.0980457
#> [113] 0.5106619 0.3654438 0.9253451 0.8131477 0.9423602 1.2746120 1.4926014
#> [120] 1.2882855 0.9578128 0.6690747 1.7255752 0.6915817 1.1805667 1.5462893
#> [127] 1.4799990 1.2547675 1.4885465 1.5331700 0.8000799 0.9242831 0.8972873
#> [134] 0.5822894 1.4211837 1.4711264 1.0189077 1.1383444 0.5706135 1.3398664
#> [141] 0.6468929 0.4029541 1.3724370 1.1276486 1.0439208 1.5059942 1.3998324
#> [148] 0.1770508 1.4076756 1.1154960 1.5349931 1.0484810 1.4391473 0.4710791
#> [155] 0.5403335 0.7005458 1.8050641 1.4848396 0.7156361 1.3325150 0.7362993
#> [162] 1.3438726 0.8285547 0.3849198 1.5678709 1.2341540 0.5106456 0.2668566
#> [169] 0.5504141 1.3424511 0.9247626 1.0576609 1.3647710 1.7021687 1.6352920
#> [176] 0.3140200 1.0421455 1.1013259 1.5020743 0.6462236 1.4313372 1.3540828
#> [183] 0.7369552 0.6403422 0.9502782 1.2130977 0.7939327 1.1204203 1.1277037
#> [190] 1.3037997 0.3774526 1.2877474 0.9799113 0.8576480 1.0937620 1.1248835
#> [197] 1.2573625 1.4749750 0.8422842 1.3569855

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
#>   [1] -3.641258479 -0.166594269  2.829620278  0.373952783 -0.531216668
#>   [6] -0.849105460 -0.065925460  3.754745902 -2.791016053 -1.147059424
#>  [11] -1.622033918  2.597111740  1.197175328  1.369203404  2.781684703
#>  [16]  0.962836870  0.211708598  0.973357798 -1.811698890  3.131962728
#>  [21] -0.389066338  2.691613664  0.042115612 -0.943475453  0.096550011
#>  [26]  1.572014038  0.857323140  3.656854731  4.916892544 -0.050870353
#>  [31]  7.615495164  6.990399000  0.189376787  1.216857254  2.122802942
#>  [36] -0.343985681 -2.044233401  0.405325747 -4.692310210  0.183203671
#>  [41]  1.751732491  2.472171778  3.907644282  0.509817220  0.963978404
#>  [46] -1.910024280  2.548437823  3.584017283 -2.961184300  5.625251529
#>  [51]  1.411365107  1.823892403 -0.555407059  2.214243352  5.325509511
#>  [56]  3.230107442 -0.180554045 -1.519353539  4.392940449 -0.324944427
#>  [61]  1.937776867  4.713343578  0.002247316  3.826853736  2.683337672
#>  [66] -0.992753550 -1.479189994  3.914210627  3.570068719 -2.851355623
#>  [71]  1.326984049 -0.689519459  1.802278904  3.385245313  1.734210398
#>  [76]  2.017888700 -0.782043480  1.546538605  1.576855927 -2.485716994
#>  [81]  1.739378275 -0.097576690  1.330695648 -0.641331468 -2.137687696
#>  [86]  3.549640751 -5.127964328 -2.174882417  0.374979399 -0.439305187
#>  [91]  2.640830551  7.110037040 -0.355985551  0.748719357  1.057974680
#>  [96] -2.568168383  0.837028308  4.035940395 -2.650119496  2.069409492
#> [101]  0.168311973 -0.354353317 -0.945395850  0.042061322 -1.886457022
#> [106]  0.480350580 -1.311068287  1.189967273 -0.357601100  5.239007064
#> [111]  4.423448654  5.845130739 -0.844561461  1.896738757 -2.956209402
#> [116]  0.466760455 -3.277578269  4.374041653  2.898278267  2.501704743
#> [121]  0.347510835  0.037178121  6.489170359  6.496274308  2.145448273
#> [126]  6.512057312  2.766912660  0.715474276 -4.413271023 -0.770319686
#> [131]  0.814217755  0.918800050  3.731321024 -0.218731762 -0.464509686
#> [136] -0.426412729 -0.208224708 -1.318230637  0.595482918  2.551353508
#> [141]  2.526157319  0.873345642 -2.385976068 -3.150363936 -0.720407718
#> [146] -0.589728368  2.352961488  0.987534174  1.044477211  3.451487245
#> [151]  7.431615588 -0.459645433  4.732831808  1.766993618 -1.165560079
#> [156] -0.887049698 -7.853038676 -2.497354236  2.770452213  5.268456605
#> [161] -0.097615890  2.467869827  2.685456166  2.494766869  2.567079551
#> [166]  8.717333592 -1.737184651 -0.528517049  4.013972818  1.902246000
#> [171]  3.727450039 -3.394227948 -0.874971561  2.259698149  6.547521168
#> [176]  0.918450022  2.950616237 -1.445793249  3.845205913  1.820144472
#> [181]  3.163183920  0.222833102 -0.428068347  0.478277681  3.585246116
#> [186]  4.655750990  2.230958985  2.182741536  5.235442639 -0.754730037
#> [191]  0.857802805  2.543688576 -0.414278366  1.772843364 -5.108692368
#> [196] -1.306825956  1.140343944  3.245963440  1.547776394 -0.642978970
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
