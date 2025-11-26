# Ordinal CRM

``` r
library(crmPack)
#> Loading required package: ggplot2
#> Registered S3 method overwritten by 'crmPack':
#>   method       from  
#>   print.gtable gtable
#> Type crmPackHelp() to open help browser
#> Type crmPackExample() to open example
```

## Introduction

The original CRM model introduced by (O’Quigley, Pepe, and Fisher 1990)
dichotomises toxicity events as either “Not toxic” or “DLT”. The ordinal
CRM generalises this model by classifying toxicities on an ordinal scale
with an arbitrary number of categories (though use of more than three or
four would be unusual).

This approach is particularly useful in non-oncology settings, where
there is a greater interest in adverse events that are not dose limiting
but are nonetheless undesirable.

## Implementation

### Ordinal data

`crmPack` uses the `DataOrdinal` class to record data observed during an
ordinal CRM trial. The `OrdinalData` class differs from the `Data` class
only in that it contains an extra slot, `yCategories`, that defines both
the number of toxicity grades and their labels.For example:

``` r
empty_ordinal_data <- DataOrdinal(
  doseGrid = c(seq(from = 10, to = 100, by = 10)),
  yCategories = c("No tox" = 0L, "Sub-tox AE" = 1L, "DLT" = 2L),
  placebo = FALSE
)
```

defines a `DataOrdinal` object with three toxicity grades, labelled “No
tox\`”, “Sub-tox AE” and “DLT”.

> Note that the `yCategories` slot must be an integer vector with values
> ordered from `0` to `length(yCategories) - 1`. Its labels must be
> unique. The first entry, which must have value `0`, is always regarded
> as the “no event” category. See \[The LogisticLogNormalOrdinal class\]
> below.

The `update`, `plot` and `dose_grid_range` methods work exactly as they
do for `Data` objects:

``` r
dose_grid_range(empty_ordinal_data)
#> [1]  10 100

ordinal_data <- update(empty_ordinal_data, x = 10, y = 0)
ordinal_data <- update(ordinal_data, x = 20, y = 0)
ordinal_data <- update(ordinal_data, x = 30, y = 0)
ordinal_data <- update(ordinal_data, x = 40, y = 0)
ordinal_data <- update(ordinal_data, x = 50, y = c(0, 1, 0))
ordinal_data <- update(ordinal_data, x = 60, y = c(0, 1, 2))

plot(ordinal_data)
```

![A graph showing Patient ID on the x axis and dose administered on the
y axis. The shape and colour of the symbols indicate the toxicity status
of the patient: red triangles for DLTs, orange circles for sub-toxic AEs
and black triangles for no reported toxicities. Patients 1 to 4 are
dosed at 10, 20, 30 and 40, with no toxicitis reported. patients 5 to 7
are dosed at 50, with patient 6 reporting a sub-toxic AE. Patients 8 to
10 are treated at 60. Patient 9 reports a sub-toxic AE and patient 10 a
DLT.](ordinal-crm_files/figure-html/data-ordinal-2-1.png)

### The `LogisticLogNormalOrdinal` class

`crmPack` fits a constrained logistic log normal model to ordinal data.
The logit of the probability of toxicity at each grade for a given dose
is modelled in the log odds space as a linear regression with common
slope and a different intercept for each toxicity grade.

> Note, unlike other model classes, `LogisticLogNormalOrdinal` requires
> a diagonal covariance matrix. This is because the constraints on the
> \$alpha;s - the intercept parameters - imposes a correlation on the
> model’s parameters. Thus, any covariance structure requested by the
> end user could not be honoured by the model.

Let p_(k)(d) be the probability that the response of a patient treated
at dose d is in category k or higher, k=0, …, K; d=1, …, D.

Then

``` math
 \log \left( \frac{p}{1-p}\right) = \alpha_k + \beta \cdot \log \left( \frac{d}{d_{ref}} \right)  
```
for k=1, …, K \[p₀(d) = 1 by definition\] where d_(ref) is a reference
dose.

The αs are constrained such that α₁ \> α₂ \> … \> α_(K).

The priors for the model’s parameters are:

``` math
 \alpha_k \sim N(\mu_{\alpha_k}, \sigma_{\alpha_k}^2) 
```

and

``` math
 \log(\beta) \sim N(\mu_\beta, \sigma_\beta^2) 
```

A `LogisticLogOrdinal` is initialised in exactly the same way as a
`LogisticLogNormal` object:

``` r
ordinal_model <- LogisticLogNormalOrdinal(
  mean = c(3, 4, 0),
  cov = diag(c(4, 3, 1)),
  ref_dose = 55
)
```

The entries in the `mean` and `cov` parameters define the hyper priors
for α₁ to α_(K-1) and β in that order.

### Model fitting

`mcmc` works as expected with ordinal models:

``` r
opts <- .DefaultMcmcOptions()

samples <- mcmc(ordinal_data, ordinal_model, opts)
```

> The warning message is expected and can be ignored. It will be
> suppressed in a future version of `crmPack`. See issue 748.

The `Samples` object returned by `mcmc` is a standard `Samples object`.
The names of the entries in its `data` slot are

``` r
names(samples@data)
#> [1] "alpha1" "alpha2" "beta"
```

It can be passed to the `fit` method, using the `grade` parameter to
specify the toxicity grade for which cumulative probabilities of
toxicity are required:

``` r
fit(samples, ordinal_model, ordinal_data, grade = 1L)
#>    dose     middle        lower     upper
#> 1    10 0.03855757 1.076031e-09 0.2463214
#> 2    20 0.07605314 4.593902e-06 0.3372746
#> 3    30 0.13132954 5.633086e-04 0.4012156
#> 4    40 0.21759469 1.938897e-02 0.4908212
#> 5    50 0.34777324 1.170223e-01 0.6521039
#> 6    60 0.49754434 1.984262e-01 0.8302917
#> 7    70 0.60673961 2.372440e-01 0.9537513
#> 8    80 0.67618534 2.577604e-01 0.9896415
#> 9    90 0.72281667 2.786083e-01 0.9975670
#> 10  100 0.75614201 2.925154e-01 0.9992769
fit(samples, ordinal_model, ordinal_data, grade = 2L)
#>    dose     middle        lower     upper
#> 1    10 0.02016547 5.112575e-10 0.1410101
#> 2    20 0.03988352 1.621383e-06 0.1863759
#> 3    30 0.06972421 2.410074e-04 0.2399123
#> 4    40 0.11875434 7.296209e-03 0.3209480
#> 5    50 0.20280095 5.116302e-02 0.4316586
#> 6    60 0.32896715 9.129258e-02 0.6831390
#> 7    70 0.44827421 1.203345e-01 0.8981507
#> 8    80 0.53467116 1.410097e-01 0.9759629
#> 9    90 0.59604565 1.539994e-01 0.9943637
#> 10  100 0.64109741 1.641662e-01 0.9984177
```

The `cumulative` flag can be used to request grade-specific
probabilities.

``` r
fit(samples, ordinal_model, ordinal_data, grade = 1L, cumulative = FALSE)
#>    dose     middle        lower     upper
#> 1    10 0.01839211 3.054200e-10 0.1418845
#> 2    20 0.03616963 1.577942e-06 0.2097671
#> 3    30 0.06160533 1.718853e-04 0.2685978
#> 4    40 0.09884035 2.043999e-03 0.3479699
#> 5    50 0.14497229 5.454564e-03 0.4400918
#> 6    60 0.16857719 6.672490e-03 0.4954574
#> 7    70 0.15846540 6.155849e-03 0.4826306
#> 8    80 0.14151418 3.423292e-03 0.4633617
#> 9    90 0.12677101 1.268829e-03 0.4392432
#> 10  100 0.11504460 4.425481e-04 0.4263083
fit(samples, ordinal_model, ordinal_data, grade = 2L, cumulative = FALSE)
#>    dose     middle        lower     upper
#> 1    10 0.02016547 5.112575e-10 0.1410101
#> 2    20 0.03988352 1.621383e-06 0.1863759
#> 3    30 0.06972421 2.410074e-04 0.2399123
#> 4    40 0.11875434 7.296209e-03 0.3209480
#> 5    50 0.20280095 5.116302e-02 0.4316586
#> 6    60 0.32896715 9.129258e-02 0.6831390
#> 7    70 0.44827421 1.203345e-01 0.8981507
#> 8    80 0.53467116 1.410097e-01 0.9759629
#> 9    90 0.59604565 1.539994e-01 0.9943637
#> 10  100 0.64109741 1.641662e-01 0.9984177
```

> Note that, for `grade == K - 1`, the cumulative and grade-specific
> probabilities of toxicities are identical.

The `plot` method also takes `grade` and `cumulative` parameters.

``` r
plot(samples, ordinal_model, ordinal_data, grade = 2L)
```

![A graph of the posterior probability of toxicity (DLT only) against
dose. The mean probability of toxicity is barely above 0% at a dose of
zero and rises in a sigmoidal curve to around 65% at a dose of 100. The
confidence interval is relatively narrow for low doses but widens
considerably for doses over 60, extending from around 15% to 100% for a
dose of 100.](ordinal-crm_files/figure-html/plot1-1.png)

``` r
plot(samples, ordinal_model, ordinal_data, grade = 1L)
```

![A graph of the posterior cumulative probability of toxicity (sub-toxic
AE or DLT) against dose. The mean probability of toxicity is barely
above 0% at a dose of zero and rises in a sigmoidal curve to around 75%
at a dose of 100. The confidence interval is relatively narrow for low
doses but widens considerably for doses over 60, extending from around
30% to 100% for a dose of
100.](ordinal-crm_files/figure-html/plot2-1.png)

``` r
plot(samples, ordinal_model, ordinal_data, grade = 1L, cumulative = FALSE)
```

![A graph of the posterior probability of sub toxic AE against dose. The
mean probability of toxicity is barely above 0% at a dose of zero, rises
to a peak of about 18% at a dose of 60 before falling to around 12% at a
dose of 100. The confidence interval is relatively narrow for low doses
but widens considerably for doses over 60, extending from around 30% to
100% for a dose of 100.](ordinal-crm_files/figure-html/plot3-1.png)

### `Rules` classes for ordinal models

For each class of `Rule` (that is, `CohortSize`, `Increments`,
`NextBest` and `Stopping`), `crmPack` provides a single wrapper class
that allows the `Rule` to be applied in trials using ordinal CRM models.
The wrapper class has the name `<Rule>Ordinal` and takes two parameters,
`rule` and `grade`. `rule` defines the standard `crmPck` `Rule` and
`grade` the toxicity grade at which the rule should be applied.

For example

``` r
dlt_rule <- CohortSizeDLT(intervals = 0:2, cohort_size = c(1, 3, 5))
ordinal_rule_1 <- CohortSizeOrdinal(grade = 1L, rule = dlt_rule)
ordinal_rule_2 <- CohortSizeOrdinal(grade = 2L, rule = dlt_rule)

size(ordinal_rule_1, 50, empty_ordinal_data)
#> [1] 1
size(ordinal_rule_2, 50, empty_ordinal_data)
#> [1] 1
size(ordinal_rule_1, 50, ordinal_data)
#> [1] 5
size(ordinal_rule_2, 50, ordinal_data)
#> [1] 3
```

`Rules` based on different toxicity grades can be combined to produce
complex rules. Here we define two `Increments` rules, one based on
toxicity grade 1, the other on toxicity grade 2. Recall two sub toxic
AEs and one DLT have been reported in the example data set.

Thus, the rule based on sub-toxic AEs allows a maximum increment of 0.67
because three events have been reported, giving a maximum permitted dose
of 100.2. As only one DLT has been reported, the second rule allows an
increment of 0.5, giving a maximum permitted dose of 90.

``` r
ordinal_rule_1 <- IncrementsOrdinal(
  grade = 1L,
  rule = IncrementsRelativeDLT(intervals = 0:2, increments = c(3, 1.5, 0.67))
)
maxDose(ordinal_rule_1, ordinal_data)
#> [1] 100.2
ordinal_rule_2 <- IncrementsOrdinal(
  grade = 2L,
  rule = IncrementsRelativeDLT(intervals = 0:1, increments = c(3, 0.5))
)
maxDose(ordinal_rule_2, ordinal_data)
#> [1] 90
```

The two grade-specific rules can be combined into a single rule using
`IncrementsMin`:

``` r
trial_rule <- IncrementsMin(list(ordinal_rule_1, ordinal_rule_2))
maxDose(trial_rule, ordinal_data)
#> [1] 90
```

## On the need for a diagonal covariance matrix

Consider a standard logistic log Normal CRM model:

``` r
model <- LogisticLogNormal(
  mean = c(-3, 1),
  cov = matrix(c(4, -0.5, -0.5, 3), ncol = 2),
  ref_dose = 45
)

model@params@cov
#>      [,1] [,2]
#> [1,]  4.0 -0.5
#> [2,] -0.5  3.0
```

We can estimate the prior using an empty `Data` object…

``` r
data <- Data(doseGrid = seq(10, 100, 10))
options <- McmcOptions(
  samples = 30000,
  rng_kind = "Mersenne-Twister",
  rng_seed = 8191316
)
samples <- mcmc(data, model, options)
```

and then obtain the correlation between the model’s parameters
\[recalling that the prior is defined in terms of log(alpha1)\]…

``` r
d <- as.matrix(cbind(samples@data$alpha0, log(samples@data$alpha1)))
sigmaHat <- cov(d)
sigmaHat
#>            [,1]       [,2]
#> [1,]  4.0094331 -0.5416752
#> [2,] -0.5416752  3.0363958
```

So we requested a covariance of -0.5 and got -0.5416755.2. Pretty good!

Now look an ordinal CRM model with non-zero correlation between its
parameters.

To begin, take a copy of the current `LogisticLogNormalOrdinal` model
and give it a non-diagonal covariance matrix by accessing its
`params@cov` slot directly, deliberately avoiding object validation.

> NB This is poor practice and not recommended. It is done here purely
> for illustration.

``` r
ordinal_model_temp <- ordinal_model
ordinal_model_temp@params@cov <- matrix(c(4, -0.5, -0.5, -0.5, 3, -0.5, -0.5, -0.5, 1), ncol = 3)

ordinal_model_temp@params@cov
#>      [,1] [,2] [,3]
#> [1,]  4.0 -0.5 -0.5
#> [2,] -0.5  3.0 -0.5
#> [3,] -0.5 -0.5  1.0
```

Fit the revised model to obtain the prior.

``` r
ordinal_data <- DataOrdinal(doseGrid = seq(10, 100, 10))
ordinal_samples <- mcmc(ordinal_data, ordinal_model_temp, options)
```

Finally, look at the covariance matrix, remembering to use `log(beta)`
rather than `beta`…

``` r
ordinalD <- as.matrix(
  cbind(
    ordinal_samples@data$alpha1,
    ordinal_samples@data$alpha2,
    log(ordinal_samples@data$beta)
  )
)
sigmaHat <- cov(ordinalD)
sigmaHat
#>             [,1]        [,2]         [,3]
#> [1,]  4.00158899 2.768345336 -0.001112980
#> [2,]  2.76834534 2.924696828  0.008697924
#> [3,] -0.00111298 0.008697924  1.012033823
```

The correlations are nothing like what we requested. This is due to the
constraints imposed on the intercepts by the model. The situation will
most likely worsen as the number of toxicity categories increases.

We have an open issue - \#755 -to examine options for allowing end users
to specify correlation structures for ordinal CRM models. If you would
like to contribute, please do so.

## Some observations

- We are currently considering the need for making grade-specific
  functionality available across more `crmPack` methods. If you have a
  specific use case that is not currently supported, please contact us.
- If you have a need for ordinal CRM in dual endpoint models, please let
  us know.
- Had `crmPack` supported ordinal CRM from the outset, the classes that
  support standard, binary, CRM models would have been sub-classes of
  the more general ordinal implementations. We did consider taking this
  approach when adding support for ordinal CRM models to the existing
  code. We decided against doing so for purely defensive and
  conservative reasons. Had we introduced the ordinal classes as parents
  of the existing classes, changes to the code base would have been much
  more substantial and we were concerned that we might miss some
  implicit assumptions about the dimensionality of the existing models.
  We therefore chose to implement ordinal classes as siblings, rather
  than parents, of the existing classes. This approach minimises the
  risk of breaking existing end-user code at the risk of slightly
  greater complexity in using the new classes.

## Environment

    #> R version 4.5.0 (2025-04-11)
    #> Platform: x86_64-pc-linux-gnu
    #> Running under: Ubuntu 24.04.2 LTS
    #> 
    #> Matrix products: default
    #> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
    #> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
    #> 
    #> locale:
    #>  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
    #>  [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
    #>  [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
    #>  [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
    #>  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
    #> [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
    #> 
    #> time zone: Etc/UTC
    #> tzcode source: system (glibc)
    #> 
    #> attached base packages:
    #> [1] stats     graphics  grDevices utils     datasets  methods   base     
    #> 
    #> other attached packages:
    #> [1] crmPack_2.0.0.9002 ggplot2_4.0.1     
    #> 
    #> loaded via a namespace (and not attached):
    #>  [1] sass_0.4.10          generics_0.1.4       xml2_1.5.0          
    #>  [4] futile.options_1.0.1 lattice_0.22-7       stringi_1.8.7       
    #>  [7] digest_0.6.39        magrittr_2.0.4       evaluate_1.0.5      
    #> [10] grid_4.5.0           RColorBrewer_1.1-3   mvtnorm_1.3-3       
    #> [13] fastmap_1.2.0        jsonlite_2.0.0       backports_1.5.0     
    #> [16] formatR_1.14         gridExtra_2.3        viridisLite_0.4.2   
    #> [19] scales_1.4.0         textshaping_1.0.4    jquerylib_0.1.4     
    #> [22] Rdpack_2.6.4         cli_3.6.5            rlang_1.1.6         
    #> [25] rbibutils_2.4        futile.logger_1.4.3  parallelly_1.45.1   
    #> [28] withr_3.0.2          cachem_1.1.0         yaml_2.3.10         
    #> [31] parallel_4.5.0       tools_4.5.0          coda_0.19-4.1       
    #> [34] checkmate_2.3.3      dplyr_1.1.4          lambda.r_1.2.4      
    #> [37] kableExtra_1.4.0     vctrs_0.6.5          R6_2.6.1            
    #> [40] lifecycle_1.0.4      stringr_1.6.0        GenSA_1.1.14.1      
    #> [43] fs_1.6.6             htmlwidgets_1.6.4    ragg_1.5.0          
    #> [46] rjags_4-17           pkgconfig_2.0.3      desc_1.4.3          
    #> [49] pkgdown_2.2.0        pillar_1.11.1        bslib_0.9.0         
    #> [52] gtable_0.3.6         glue_1.8.0           systemfonts_1.3.1   
    #> [55] xfun_0.54            tibble_3.3.0         tidyselect_1.2.1    
    #> [58] rstudioapi_0.17.1    knitr_1.50           dichromat_2.0-0.1   
    #> [61] farver_2.1.2         htmltools_0.5.8.1    labeling_0.4.3      
    #> [64] rmarkdown_2.30       svglite_2.2.2        compiler_4.5.0      
    #> [67] S7_0.2.1

## References

O’Quigley, John, Margaret Pepe, and Lloyd Fisher. 1990. “Continual
Reassessment Method: A Practical Design for Phase 1 Clinical Trials in
Cancer.” *Biometrics* 46 (1): 33–48.
