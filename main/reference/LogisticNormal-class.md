# `LogisticNormal`

**\[stable\]**

`LogisticNormal` is the class for the usual logistic regression model
with a bivariate normal prior on the intercept and slope.

## Usage

``` r
LogisticNormal(mean, cov, ref_dose = 1)

.DefaultLogisticNormal()
```

## Arguments

- mean:

  (`numeric`)\
  the prior mean vector.

- cov:

  (`matrix`)\
  the prior covariance matrix. The precision matrix `prec` is internally
  calculated as an inverse of `cov`.

- ref_dose:

  (`number`)\
  the reference dose \\x\*\\ (strictly positive number).

## Details

The covariate is the natural logarithm of the dose \\x\\ divided by the
reference dose \\x\*\\, i.e.: \$\$logit\[p(x)\] = alpha0 + alpha1 \*
log(x/x\*),\$\$ where \\p(x)\\ is the probability of observing a DLT for
a given dose \\x\\. The prior \$\$(alpha0, alpha1) ~ Normal(mean,
cov).\$\$

## Note

Typically, end users will not use the `.DefaultLogisticNormal()`
function.

## See also

[`ModelLogNormal`](https://docs.crmpack.org/reference/ModelLogNormal-class.md),
[`LogisticLogNormal`](https://docs.crmpack.org/reference/LogisticLogNormal-class.md),
[`LogisticLogNormalSub`](https://docs.crmpack.org/reference/LogisticLogNormalSub-class.md),
[`ProbitLogNormal`](https://docs.crmpack.org/reference/ProbitLogNormal-class.md),
[`ProbitLogNormalRel`](https://docs.crmpack.org/reference/ProbitLogNormalRel-class.md),
[`LogisticNormalMixture`](https://docs.crmpack.org/reference/LogisticNormalMixture-class.md).

## Examples

``` r
# Define the dose-grid.
empty_data <- Data(doseGrid = c(1, 3, 5, 10, 15, 20, 25, 40, 50, 80, 100))

my_model <- LogisticNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2)
)

my_options <- McmcOptions(burnin = 10, step = 2, samples = 100)

samples <- mcmc(empty_data, my_model, my_options)
samples
#> An object of class "Samples"
#> Slot "data":
#> $alpha0
#>   [1] -1.09253636 -0.59477108 -1.82467295 -0.76146109 -1.26638399 -1.74993764
#>   [7]  0.23754262 -0.96401122 -0.15937951  0.15272558  0.26243114 -0.44729561
#>  [13]  1.09370657  0.52372615  0.64018136 -1.30369779  1.09023218 -0.92003661
#>  [19] -0.79775352 -1.13683524 -0.61517430 -1.81131837 -0.40654794 -0.79087075
#>  [25] -1.06516209 -1.07812202 -0.37872940  0.46956128 -0.41488288 -0.34057775
#>  [31] -0.64995571 -2.07427043 -0.88148253 -1.69571573  0.54894135 -1.77697118
#>  [37] -1.05490183 -0.77166496  1.35083727  0.04007284 -2.09536772 -2.15183482
#>  [43] -2.73054041 -0.44187521  0.59988244 -0.75110851 -0.75363586 -0.39588505
#>  [49] -1.28134305 -0.82339506  1.02407345 -0.37973945  0.50199635 -0.26786960
#>  [55] -0.61419090 -0.44481948 -0.72046692  1.74732708 -2.64731761 -0.87499439
#>  [61] -1.41016639 -3.24946471 -0.25070171  0.69156285 -0.18659647  0.42267016
#>  [67] -1.64710769 -0.16873464 -0.39314602  0.24700440 -0.47715149 -0.42750258
#>  [73]  0.78746378 -0.95220415 -0.88971763  0.41974047 -0.53030015 -1.26773693
#>  [79] -1.46423276 -1.36000524 -2.91948072 -1.25300338 -1.65643582  0.36242701
#>  [85] -0.08935474 -1.63425724 -1.87473751  0.03766448 -1.02408344  0.22457904
#>  [91] -0.01602413 -1.27084969  0.41931208 -1.91523009  1.88443069 -0.22267726
#>  [97] -0.19520865 -1.24102665 -0.04324189  0.33736286
#> 
#> $alpha1
#>   [1]  1.4096450435 -0.3115053853  1.2149743597  1.7378668789  1.5763554016
#>   [6]  1.2738434228  2.0179126804  2.5831307625  2.2564231759  0.0715376280
#>  [11]  0.6331097754  1.3144323704 -0.6989955488  2.4298138628  0.8558330309
#>  [16]  1.9821378669  0.5068759397  1.1601533719  0.8697467892  1.1665831489
#>  [21]  0.3704341311  0.8376450743 -0.0002981322  1.4173430454  1.4801469614
#>  [26]  0.0604059127 -0.0614913811  0.3646639658  1.2316932277  0.7326747873
#>  [31] -0.1157717458  1.3106721301  2.0385517291  0.7808937325  1.5271009299
#>  [36]  1.8218138738  0.2597470213  0.9151590004 -0.2872666143 -0.8145426654
#>  [41]  2.3846210442  0.5548584745  1.6587387590  0.8655458197  1.6955605801
#>  [46]  0.1421969375 -0.0044842419  0.3068651741  0.8502405242 -0.9578462805
#>  [51] -0.7827301836  0.6509079204  0.6155604434  0.4665981237  0.9515239893
#>  [56]  1.4748312077  0.9743130217 -2.1999491732  0.8677850656  1.2142304558
#>  [61]  1.6228558871  2.3539059919  0.5921200926 -1.4630823167  0.8224175626
#>  [66] -1.2619430059  1.0490962906  0.3349931014  2.1386768400  0.3556790114
#>  [71]  0.7530140333  0.7368700083  1.6926975255  1.2255273972  1.2962744471
#>  [76] -1.2845552597  1.5649994716  0.6037091488  0.6078578764  0.4187874779
#>  [81]  2.1351022766  1.4225728963  1.2608167531 -0.2378997409  1.2293356758
#>  [86]  0.0325144369  2.8923105286  1.7947882564 -0.0437287083  0.7019259109
#>  [91]  0.1476597640  2.1808820866 -0.8763415302  0.6851426112 -0.2377909710
#>  [96]  0.6451240081  1.9907134481  0.6580110696  1.8695440935  0.4177421724
#> 
#> 
#> Slot "options":
#> An object of class "McmcOptions"
#> Slot "iterations":
#> [1] 210
#> 
#> Slot "burnin":
#> [1] 10
#> 
#> Slot "step":
#> [1] 2
#> 
#> Slot "rng_kind":
#> [1] NA
#> 
#> Slot "rng_seed":
#> [1] NA
#> 
#> 
```
