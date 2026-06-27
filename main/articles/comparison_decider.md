# Comparison with the decider package

In this vignette we want to compare the combination design implemented
in `crmPack` and explained in this
[vignette](https://docs.crmpack.org/articles/combo_designs.Rmd) with the
implementation in the [`decider`
package](https://boehringer-ingelheim.github.io/decider/) (Schroeter
2023). Please note that the `decider` package is not available on CRAN,
therefore the vignette is only executing the code chunks if the package
is available on this system.

## Example

We are going to use the example as described in the `decider` vignette
[here](https://boehringer-ingelheim.github.io/decider/articles/intro_jointBLRM.html#setting-up-and-evaluating-priors-using-scenario_jointblrm):

- Three arms:
  - Arm A: monotherapy of compound 1
  - Arm B: combination of compound 1 and compound 2
  - Arm C: historical data from compound 2
- Arm B can start when certain doses of Arm A have been cleared
- Logistic log-normal models for compound 1 and compound 2
- Target interval is 16-33% DLT rate
- Prior specification
  - uniform prior for the correlation between intercept and log-slope in
    the logistic log-normal model
  - for the hyper-means (i.e. mean of parameters across trials)
  - between-trial heterogeneity (i.e. standard deviation of parameters
    across trials)

## Using `decider`

``` r

library(decider)
#> This is development version `0.0.0.9012` of the `decider` package:
#> DECIsion making in oncology Dose Escalation trials with logistic Regression.
```

This is the data from the historical Arm C:

``` r

historical_data <- list(
  dose1 = c(0, 0, 0, 0, 0),
  dose2 = c(2, 4, 8, 12, 16),
  n.pat = c(3, 3, 3, 9, 12),
  n.dlt = c(0, 0, 0, 1, 2),
  trial = c("H1", "H1", "H1", "H1", "H1")
)
```

The monotherapy dose grid for Arm A is:

``` r

d1 <- c(0.1, 0.2, 0.4, 0.8, 1.6, 2.4, 3.6, 5, 6)
```

The dose grid for compound 2 in Arm B is more sparse:

``` r

d2 <- c(8, 12)
```

The overall dose grid for combination Arm B is therefore:

``` r

doses_of_interest <- rbind(
    c(d1, rep(d1, times = length(d2))),
    c(rep(0, length(d1)), rep(d2, each = length(d1)))
)
```

The reference doses to be used in the models are:

``` r

dose_ref1 <- 6
dose_ref2 <- 12
```

We further need to specify the arms and types of the arms as follows:

``` r

trials_of_interest <- c("A", "B")
types_of_interest <- c("mono1", "combi")
```

The prior for the hypermeans is specified like this:

``` r

#                Parameter   Mean         SD
prior_mu <- list(mu_a1  =  c(logit(0.33), 2),
                 mu_b1  =  c(0,           1), # standard normal
                 mu_a2  =  c(logit(0.33), 2),
                 mu_b2  =  c(0,           1), # standard normal
                 mu_eta =  c(0,           1.121))
```

The prior mean for $`\mu_{\alpha_{1}}`$ is set to
$`\text{logit}(0.33)`$, which implies that we assume the reference dose
has a prior median DLT rate of 33%.

Note that we use a normal prior here on the interaction parameter
$`\eta`$, thus allowing both positive and negative interactions. The
standard deviation is set such that
$`\exp(1.96 \cdot 1.121) \approx 9`$, thus allowing for a 95% prior
interval of $`[1 / 9, 9]`$ for the odds changes for a DLT at the
reference dose. So $`1.121 = \log(9) / z_{0.975}`$.

The prior for the between-trial heterogeneity parameters is specified
like this:

``` r

#                 Parameter    Mean        SD
prior_tau <- list(tau_a1  =  c(log(0.25),  log(2) / 1.96),
                  tau_b1  =  c(log(0.125), log(2) / 1.96),
                  tau_a2  =  c(log(0.25),  log(2) / 1.96),
                  tau_b2  =  c(log(0.125), log(2) / 1.96),
                  tau_eta =  c(log(0.125), log(2) / 1.96))
```

These are all the log normal prior parameters for the corresponding
$`\tau`$ parameters. These are all “moderate” degrees of heterogeneity,
according to Neuenschwander et al. (2014).

Then we look at the following scenario, where two cohorts of patients
are available from Arm A:

``` r

scenario1 <- list(
  dose1 = c(0.1, 0.2),
  dose2 = c(0, 0),
  n.pat = c(3, 3),
  n.dlt = c(0, 1),
  trial = c("A",  "A")
)
```

We note that the `trial` specification here needs to match the name used
in `trials_of_interest` above.

Now we can call the scenario function:

``` r

result1 <- scenario_jointBLRM(
  data = scenario1,
  historical.data = historical_data,
  doses.of.interest = doses_of_interest,
  dose.ref1 = dose_ref1,
  dose.ref2 = dose_ref2,
  trials.of.interest = trials_of_interest,
  types.of.interest = types_of_interest,
  prior.mu = prior_mu,
  prior.tau = prior_tau
)
```

We can look at the results:

``` r

result1
#> $`trial-A`
#>          mean      sd  q.2.5%   q.50% q.97.5% P([0,0.16)) P([0.16,0.33))
#> 0.1+0 0.11504 0.10904 0.00309 0.08155 0.40743     0.74205        0.20274
#> 0.2+0 0.15390 0.12868 0.00677 0.11922 0.48337     0.61852        0.27468
#> 0.4+0 0.20721 0.15498 0.01303 0.17223 0.58491     0.47006        0.32619
#> 0.8+0 0.27586 0.18747 0.02232 0.24056 0.70532     0.33254        0.32214
#> 1.6+0 0.35595 0.22116 0.03428 0.32512 0.82509     0.22696        0.28034
#> 2.4+0 0.40497 0.23854 0.04186 0.37985 0.88234     0.18252        0.24882
#> 3.6+0 0.45347 0.25288 0.05011 0.43744 0.92472     0.14682        0.21829
#> 5+0   0.49140 0.26195 0.05661 0.48464 0.94964     0.12440        0.19463
#> 6+0   0.51169 0.26599 0.06053 0.51071 0.96006     0.11331        0.18408
#>       P([0.33,1])
#> 0.1+0     0.05521
#> 0.2+0     0.10680
#> 0.4+0     0.20375
#> 0.8+0     0.34532
#> 1.6+0     0.49270
#> 2.4+0     0.56866
#> 3.6+0     0.63489
#> 5+0       0.68097
#> 6+0       0.70261
#> 
#> $`trial-B`
#>           mean      sd  q.2.5%   q.50% q.97.5% P([0,0.16)) P([0.16,0.33))
#> 0.1+8  0.18525 0.12592 0.02779 0.15531 0.50905     0.51675        0.35643
#> 0.2+8  0.21987 0.14217 0.03551 0.18745 0.57746     0.41183        0.39440
#> 0.4+8  0.26686 0.16295 0.04651 0.23269 0.66081     0.30198        0.40060
#> 0.8+8  0.32784 0.18827 0.05989 0.29310 0.75821     0.20625        0.36352
#> 1.6+8  0.40081 0.21628 0.07391 0.37050 0.85632     0.13772        0.29604
#> 2.4+8  0.44643 0.23289 0.08072 0.42248 0.90335     0.11205        0.25324
#> 3.6+8  0.49172 0.24974 0.08230 0.47928 0.94148     0.09830        0.21420
#> 5+8    0.52645 0.26432 0.07842 0.52754 0.96391     0.09594        0.18646
#> 6+8    0.54435 0.27320 0.07328 0.55432 0.97355     0.09824        0.17230
#> 0.1+12 0.22315 0.12690 0.05211 0.19725 0.53967     0.36549        0.45639
#> 0.2+12 0.25618 0.14131 0.06135 0.22807 0.60183     0.28054        0.46438
#> 0.4+12 0.30105 0.16021 0.07318 0.27114 0.68017     0.19806        0.43631
#> 0.8+12 0.35936 0.18417 0.08718 0.32899 0.77452     0.13313        0.36882
#> 1.6+12 0.42915 0.21278 0.09730 0.40337 0.86801     0.09208        0.28551
#> 2.4+12 0.47254 0.23182 0.09735 0.45433 0.91507     0.08414        0.23745
#> 3.6+12 0.51478 0.25370 0.08717 0.51092 0.95221     0.08734        0.19718
#> 5+12   0.54579 0.27468 0.07015 0.55737 0.97388     0.10089        0.16914
#> 6+12   0.56087 0.28791 0.05751 0.58400 0.98267     0.11253        0.15518
#>        P([0.33,1])
#> 0.1+8      0.12682
#> 0.2+8      0.19377
#> 0.4+8      0.29742
#> 0.8+8      0.43023
#> 1.6+8      0.56624
#> 2.4+8      0.63471
#> 3.6+8      0.68750
#> 5+8        0.71760
#> 6+8        0.72946
#> 0.1+12     0.17812
#> 0.2+12     0.25508
#> 0.4+12     0.36563
#> 0.8+12     0.49805
#> 1.6+12     0.62241
#> 2.4+12     0.67841
#> 3.6+12     0.71548
#> 5+12       0.72997
#> 6+12       0.73229
```

For each trial of interest, the posterior toxicities previously
designated to be of interest are shown.

Under the hood, the implementation works as follows:

- [`post_tox_jointBLRM()`](https://github.com/Boehringer-Ingelheim/decider/blob/main/R/sampling_jointBLRM.R#L232)
  is called to sample from the posterior, which in turn uses
- [`sampling_jointBLRM()`](https://github.com/Boehringer-Ingelheim/decider/blob/main/R/sampling_jointBLRM.R#L17)
  which then calls
  [`rstan::sampling()`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html)
  on
- [`stanmodels$jointBLRM`](https://github.com/Boehringer-Ingelheim/decider/blob/main/R/stanmodels.R#L11)
  which is the constant Stan model sourced from
- [`jointBLRM.stan`](https://github.com/Boehringer-Ingelheim/decider/blob/main/inst/stan/jointBLRM.stan)

So we can compare this with the implementation in `crmPack` which is
based on JAGS.

## Using `crmPack`

Now we are going to define the same design and scenario in `crmPack`.

We start with the monotherapy model for compound 1:

``` r

library(crmPack)
#> Loading required package: ggplot2
#> Registered S3 method overwritten by 'crmPack':
#>   method       from  
#>   print.gtable gtable
#> Type crmPackHelp() to open help browser
#> Type crmPackExample() to open example
#> 
#> Attaching package: 'crmPack'
#> The following object is masked from 'package:decider':
#> 
#>     logit

mono_model1 <- LogisticLogNormal(
  mean = c(logit(0.33), 0),
  cov = diag(c(2, 1)^2),
  ref_dose = dose_ref1
)
```

And for compound 2 the same:

``` r

mono_model2 <- LogisticLogNormal(
    mean = c(logit(0.33), 0),
    cov = diag(c(2, 1)^2),
    ref_dose = dose_ref2
)
```

Then we define the combination model:

``` r

combo_model <- TwoDrugsCombo(
    list(
        compound1 = mono_model1,
        compound2 = mono_model2
    ),
    gamma = 0, # prior mean for the interaction parameter
    tau = 1 / (1.121^2) # prior precision for the interaction parameter
)
```

We define the historical data which is already available:

``` r

hist_data_comp2 <- Data(
    x = rep(historical_data$dose2, historical_data$n.pat),
    y = c(
        rep(0, sum(historical_data$n.pat) - sum(historical_data$n.dlt)), 
        rep(1, sum(historical_data$n.dlt))
    ),
    doseGrid = historical_data$dose2
)
#> Used default patient IDs!
#> Used best guess cohort indices!
```

We are going to use simple rules here (they are not relevant for the
current scenario comparison):

``` r

my_stopping <- StoppingMinPatients(nPatients = 50)
my_increments <- IncrementsRelative(0, 2)
myNextBest <- NextBestNCRM(
    target = c(0.16, 0.33), 
    overdose = c(0.33, 1), 
    max_overdose_prob = 0.25
)
my_cohort_size <- CohortSizeConst(size = 3)
my_increments_combo <- IncrementsComboOneDrugOnly()
```

Then we define the design arms accordingly:

``` r

designArmA <- DesignArm(
    "A",
    design = Design(
        data = Data(doseGrid = d1),
        startingDose = d1[1],
        model = mono_model1,
        stopping = my_stopping,
        increments = my_increments,
        nextBest = myNextBest,
        cohort_size = my_cohort_size
    )
)

designArmB <- DesignArm(
    "B",
    design = DesignCombo(
        data = DataCombo(doseGrid = list(compound1 = d1, compound2 = c(0, d2))),
        startingDose = c(compound1 = d1[1], compound2 = 0),
        model = combo_model,
        stopping = my_stopping,
        increments = my_increments_combo,
        nextBest = myNextBest,
        cohort_size = my_cohort_size
    ),
    open_when = ArmMinDoseCondition("A", min_dose = d1[2])
)

designArmC <- HistoricalArm(
    "C",
    data = hist_data_comp2,
    model = mono_model2
)
```

Now we can define the hierarchical design:

``` r

design_hierarchical <- HierarchicalDesign(
    designArmA,
    designArmB,
    designArmC,
    exchangeable_parameters = list(
        comp1_intercept = list(
            A = "alpha0",
            B = "alpha0[1]"
        ),
        comp1_slope = list(
            A = "alpha1",
            B = "alpha1[1]"
        ),
        comp2_intercept = list(
            B = "alpha0[2]",
            C = "alpha0"
        ),
        comp2_slope = list(
            B = "alpha1[2]",
            C = "alpha1"    
        )
    ),
    pool_correlations = list(
        comp1 = c("comp1_intercept", "comp1_slope"),
        comp2 = c("comp2_intercept", "comp2_slope")
    ),
    pool_priors = list(
        comp1_intercept = list(
            mu = prior_mu$mu_a1,
            tau = prior_tau$tau_a1
        ),
        comp1_slope = list(
            mu = prior_mu$mu_b1,
            tau = prior_tau$tau_b1
        ),
        comp2_intercept = list(
            mu = prior_mu$mu_a2,
            tau = prior_tau$tau_a2
        ),
        comp2_slope = list(
            mu = prior_mu$mu_b2,
            tau = prior_tau$tau_b2
        )
    )
)
```

Note that each entry in `pool_correlations` can correlate exactly two
scalar exchangeable parameter pools. In this example, `comp1` correlates
the compound 1 intercept pool with the compound 1 slope pool, and
`comp2` does the same for compound 2. Correlated blocks with three or
more parameters are not currently supported.

Then we define the scenario:

``` r

scenario_hierarchical <- HierarchicalData(
    A = Data(
        x = c(0.1, 0.1, 0.1, 0.2, 0.2, 0.2),
        y = c(0, 0, 0, 0, 0, 1),
        doseGrid = designArmA@design@data@doseGrid
    ),
    B = designArmB@design@data,
    C = designArmC@design@data
)
#> Used default patient IDs!
#> Used best guess cohort indices!
```

And then we can use the
[`scenario()`](https://docs.crmpack.org/reference/scenario.md) function:

``` r

result1CrmPack <- scenario(
    design_hierarchical,
    data = scenario_hierarchical,
    mcmcOptions = McmcOptions()
)
```

We can look at the fit results:

``` r

result1CrmPack$fit
#> $A
#>   dose    middle       lower     upper
#> 1  0.1 0.1020983 0.002163291 0.3271358
#> 2  0.2 0.1365121 0.004777047 0.4061310
#> 3  0.4 0.1841166 0.009133880 0.5282048
#> 4  0.8 0.2459235 0.018316515 0.6589567
#> 5  1.6 0.3183434 0.031097078 0.7970370
#> 6  2.4 0.3628936 0.038160054 0.8598901
#> 7  3.6 0.4073546 0.045491178 0.9142268
#> 8  5.0 0.4425527 0.052537276 0.9416913
#> 9  6.0 0.4615878 0.055787154 0.9532542
#> 
#> $B
#>    compound1 compound2    middle       lower     upper
#> 1        0.1         0 0.1067474 0.001313233 0.3942998
#> 2        0.2         0 0.1402635 0.003088968 0.4733915
#> 3        0.4         0 0.1858647 0.006644515 0.5776503
#> 4        0.8         0 0.2452121 0.013651469 0.6956522
#> 5        1.6         0 0.3162142 0.024548158 0.8124045
#> 6        2.4         0 0.3607407 0.033066369 0.8698323
#> 7        3.6         0 0.4056387 0.040210845 0.9150201
#> 8        5.0         0 0.4413723 0.046354991 0.9420549
#> 9        6.0         0 0.4607316 0.049297639 0.9542332
#> 10       0.1         8 0.1617463 0.014177380 0.4531076
#> 11       0.2         8 0.1932532 0.020190888 0.5196158
#> 12       0.4         8 0.2362043 0.028863799 0.6094323
#> 13       0.8         8 0.2923024 0.043244683 0.7180217
#> 14       1.6         8 0.3598383 0.061853272 0.8302752
#> 15       2.4         8 0.4024605 0.072947233 0.8888104
#> 16       3.6         8 0.4455389 0.075675996 0.9321454
#> 17       5.0         8 0.4795954 0.071694454 0.9584294
#> 18       6.0         8 0.4977221 0.065870361 0.9695651
#> 19       0.1        12 0.2084278 0.044662445 0.4913545
#> 20       0.2        12 0.2383010 0.053025662 0.5520767
#> 21       0.4        12 0.2791012 0.063952920 0.6354440
#> 22       0.8        12 0.3325393 0.080509569 0.7330134
#> 23       1.6        12 0.3970703 0.094695887 0.8425558
#> 24       2.4        12 0.4377560 0.098308083 0.8999518
#> 25       3.6        12 0.4783809 0.087343872 0.9433112
#> 26       5.0        12 0.5094017 0.068605350 0.9681216
#> 27       6.0        12 0.5250938 0.056050822 0.9782262
#> 
#> $C
#>   dose     middle        lower      upper
#> 1    2 0.01482531 1.659515e-08 0.08811764
#> 2    4 0.02635061 4.503450e-06 0.11622674
#> 3    8 0.05713085 9.784553e-04 0.16692149
#> 4   12 0.10677198 2.089737e-02 0.23946693
#> 5   16 0.18706339 5.425085e-02 0.39699568
```

We can also check the probabilities to be in target and overdosing
intervals:

``` r

result1CrmPack$next_best$A$probs
#>       dose target overdose
#>  [1,]  0.1 0.1875   0.0248
#>  [2,]  0.2 0.2628   0.0635
#>  [3,]  0.4 0.3183   0.1561
#>  [4,]  0.8 0.3244   0.2745
#>  [5,]  1.6 0.3112   0.4006
#>  [6,]  2.4 0.2909   0.4771
#>  [7,]  3.6 0.2763   0.5354
#>  [8,]  5.0 0.2453   0.5950
#>  [9,]  6.0 0.2318   0.6211
result1CrmPack$next_best$B$probs
#>    compound1 compound2 target_prob overdose_prob not_eligible
#> 1        0.1         0      0.1793        0.0463        FALSE
#> 2        0.2         0      0.2440        0.0863        FALSE
#> 3        0.4         0      0.2974        0.1617        FALSE
#> 4        0.8         0      0.3129        0.2749         TRUE
#> 5        1.6         0      0.3024        0.4004         TRUE
#> 6        2.4         0      0.2800        0.4745         TRUE
#> 7        3.6         0      0.2597        0.5400         TRUE
#> 8        5.0         0      0.2422        0.5900         TRUE
#> 9        6.0         0      0.2290        0.6167         TRUE
#> 10       0.1         8      0.3367        0.0846        FALSE
#> 11       0.2         8      0.3925        0.1352        FALSE
#> 12       0.4         8      0.4106        0.2263        FALSE
#> 13       0.8         8      0.3945        0.3461         TRUE
#> 14       1.6         8      0.3450        0.4758         TRUE
#> 15       2.4         8      0.2996        0.5485         TRUE
#> 16       3.6         8      0.2596        0.6090         TRUE
#> 17       5.0         8      0.2310        0.6449         TRUE
#> 18       6.0         8      0.2109        0.6628         TRUE
#> 19       0.1        12      0.4657        0.1407        FALSE
#> 20       0.2        12      0.4844        0.2085        FALSE
#> 21       0.4        12      0.4661        0.3084         TRUE
#> 22       0.8        12      0.4118        0.4374         TRUE
#> 23       1.6        12      0.3272        0.5634         TRUE
#> 24       2.4        12      0.2755        0.6252         TRUE
#> 25       3.6        12      0.2328        0.6636         TRUE
#> 26       5.0        12      0.1992        0.6826         TRUE
#> 27       6.0        12      0.1788        0.6887         TRUE
```

## Comparison of fit

Based on this we can first compare the fit results.

Let’s look at the results for Arm A:

``` r

fitTrialADecider <- result1$`trial-A` |> as.data.frame()
fitTrialACrmPack <- result1CrmPack$fit$A
probsTrialACrmPack <- result1CrmPack$next_best$A$probs |> as.data.frame()
diffTrialA <- data.frame(
    dose = fitTrialACrmPack$dose,
    center = fitTrialADecider$mean - fitTrialACrmPack$middle,
    lower = fitTrialADecider$`q.2.5%` - fitTrialACrmPack$lower,
    upper = fitTrialADecider$`q.97.5%` - fitTrialACrmPack$upper,
    target = fitTrialADecider$`P([0.16,0.33))` - probsTrialACrmPack$target,
    overdose = fitTrialADecider$`P([0.33,1])` - probsTrialACrmPack$overdose
)
diffTrialA
#>   dose     center        lower       upper   target overdose
#> 1  0.1 0.01294175 0.0009267092 0.080294211  0.01524  0.03041
#> 2  0.2 0.01738793 0.0019929533 0.077239047  0.01188  0.04330
#> 3  0.4 0.02309342 0.0038961198 0.056705229  0.00789  0.04765
#> 4  0.8 0.02993654 0.0040034847 0.046363261 -0.00226  0.07082
#> 5  1.6 0.03760659 0.0031829219 0.028052965 -0.03086  0.09210
#> 6  2.4 0.04207638 0.0036999465 0.022449912 -0.04208  0.09156
#> 7  3.6 0.04611537 0.0046188220 0.010493202 -0.05801  0.09949
#> 8  5.0 0.04884735 0.0040727236 0.007948674 -0.05067  0.08597
#> 9  6.0 0.05010218 0.0047428459 0.006805786 -0.04772  0.08151
```

And then the results for Arm B:

``` r

fitTrialBDecider <- result1$`trial-B` |> as.data.frame()
fitTrialBCrmPack <- result1CrmPack$fit$B |> dplyr::filter(compound2 > 0)
probsTrialBCrmPack <- result1CrmPack$next_best$B$probs |>
    as.data.frame() |>
    dplyr::filter(compound2 > 0)
diffTrialB <- data.frame(
    dose1 = fitTrialBCrmPack$compound1,
    dose2 = fitTrialBCrmPack$compound2,
    center = fitTrialBDecider$mean - fitTrialBCrmPack$middle,
    lower = fitTrialBDecider$`q.2.5%` - fitTrialBCrmPack$lower,
    upper = fitTrialBDecider$`q.97.5%` - fitTrialBCrmPack$upper,
    target = fitTrialBDecider$`P([0.16,0.33))` - probsTrialBCrmPack$target,
    overdose = fitTrialBDecider$`P([0.33,1])` - probsTrialBCrmPack$overdose
)
diffTrialB
#>    dose1 dose2     center         lower       upper   target overdose
#> 1    0.1     8 0.02350370  0.0136126200 0.055942446  0.01973  0.04222
#> 2    0.2     8 0.02661679  0.0153191117 0.057844183  0.00190  0.05857
#> 3    0.4     8 0.03065566  0.0176462014 0.051377739 -0.01000  0.07112
#> 4    0.8     8 0.03553756  0.0166453166 0.040188298 -0.03098  0.08413
#> 5    1.6     8 0.04097174  0.0120567275 0.026044787 -0.04896  0.09044
#> 6    2.4     8 0.04396953  0.0077727675 0.014539568 -0.04636  0.08621
#> 7    3.6     8 0.04618112  0.0066240042 0.009334620 -0.04540  0.07850
#> 8    5.0     8 0.04685463  0.0067255464 0.005480614 -0.04454  0.07270
#> 9    6.0     8 0.04662794  0.0074096387 0.003984867 -0.03860  0.06666
#> 10   0.1    12 0.01472218  0.0074475545 0.048315508 -0.00931  0.03742
#> 11   0.2    12 0.01787900  0.0083243379 0.049753262 -0.02002  0.04658
#> 12   0.4    12 0.02194876  0.0092270798 0.044726003 -0.02979  0.05723
#> 13   0.8    12 0.02682070  0.0066704306 0.041506617 -0.04298  0.06065
#> 14   1.6    12 0.03207969  0.0026041127 0.025454214 -0.04169  0.05901
#> 15   2.4    12 0.03478398 -0.0009580827 0.015118212 -0.03805  0.05321
#> 16   3.6    12 0.03639908 -0.0001738718 0.008898849 -0.03562  0.05188
#> 17   5.0    12 0.03638833  0.0015446504 0.005758449 -0.03006  0.04737
#> 18   6.0    12 0.03577615  0.0014591785 0.004443819 -0.02362  0.04359
```

So these differences look relatively small, and there does not seem to
be any systematic bias in the differences.

## Comparison of model code

Let’s compare the model code used in `decider` and `crmPack`, in order
to make sure that they really match and implement the same priors and
models:

### `decider`

Here we have the following Stan model:

    /*Stan model for joint BLRM
    --------------------------------------------------------------------------------
      Implements the joint BLRM as described in Neuenschwander et al., 2016,
      "On the use of co-data in clinical trials".
      A non-centered parametrization  is implemented by obtaining
      multivariate normals via multiplication with cholesky factors.
      The cholesky decomposition is implemented by hand, as it is
      available analytically in the required 2x2-case.
    */
    functions{
      /*counts mono observations based on input dose levels
        Note: first input vector signals the component to be counted*/
      int count_n_mono(vector dose_1, vector dose_2, int n_obs){
        int res = 0;
        for(i in 1:n_obs){
          if(dose_1[i]>0 && dose_2[i]==0){
            res+=1;
          }
        }
        return res;
      }
      /*Computes permutation of input data, so that the first n_obs1 observations
        are mono1, the subsequent n_obs2 observations are mono2, and the remaining
        ones are combination therapy.
        Returns matrix with two rows, first row is the permutation for sorting, and
        second row contains the inverse permutation (to reverse sorted input to
        normal order).*/
      int[,] sort_idx(vector dose_1, vector dose_2,
                     int n_obs, int n_obs1, int n_obs2)
      {
        int res[2, n_obs] = rep_array(0, 2, n_obs);
        //n_obs1/n_obs2 allow to compute offsets for sorting by counting
        int cnt1 = 0;
        int cnt2 = 0;
        int cnt = 0;
        //loop over input and save correct placement
        for(i in 1:n_obs){
          if(dose_1[i]>0 && dose_2[i]==0){
            res[1, cnt1+1] = i;
            res[2, i] = cnt1+1;
            cnt1 += 1;
          }else if(dose_1[i]==0 && dose_2[i]>0){
            res[1, n_obs1 + 1 + cnt2] = i;
            res[2, i] = n_obs1 + 1 + cnt2;
            cnt2 += 1;
          }else if(dose_1[i]>0 && dose_2[i]>0){
            res[1, n_obs1 + n_obs2 + 1 + cnt] = i;
            res[2, i] = n_obs1 + n_obs2 + 1 + cnt;
            cnt += 1;
          }
        }
        return res;
      }
    }
    data{
      //number of observations/cohorts
      int<lower=0> n_obs;
      //number of studies
      int<lower=0> n_studies;
      //number of patients for each cohort
      int<lower=0> n[n_obs];
      //number of DLTs for each cohort
      int<lower=0> r[n_obs];
      //study number for cohorts
      int<lower=1> s[n_obs];
      //indicates whether a MAP prior is computed
      int<lower=0, upper=1> doMAP;
      //indicates whether linear or saturating
      //interaction term is used
      int<lower=0, upper=1> saturating;
      //reference doses
      vector<lower=0>[2] dose_c;
      //dose levels component 1 and 2 for each cohort
      vector<lower=0>[n_obs] dose_1;
      vector<lower=0>[n_obs] dose_2;
      /*hyper priors
        Notation and order of entries:
        mu =  (mu_alpha1,  mu_beta1,  mu_alpha2,  mu_beta2,  mu_eta)
        tau = (tau_alpha1, tau_beta1, tau_alpha2, tau_beta2, tau_eta)
      */
      //mean of hyper SD tau
      vector[5] mean_tau;
      //sd's of hyper SD tau
      vector<lower=0>[5] sd_tau;
      //mean of hyper mean mu
      vector[5] mean_mu;
      //mean of hyper sd mu
      vector<lower=0>[5] sd_mu;
    }
    transformed data{
      //internally generates a study without observations for MAP prior
      int<lower=1> num_s = doMAP? n_studies+1 : n_studies;
      //count number of mono observations
      int<lower=0, upper=n_obs> n_obs1 = count_n_mono(dose_1, dose_2, n_obs);
      int<lower=0, upper=n_obs> n_obs2 = count_n_mono(dose_2, dose_1, n_obs);
      //compute sort indices (only done once per call to stan for efficiency)
      int srt_idx[2, n_obs] = sort_idx(dose_1, dose_2, n_obs, n_obs1, n_obs2);
      //sort by applying computed sorting permutation
      int n_srt[n_obs] = n[srt_idx[1, 1:n_obs]];
      int r_srt[n_obs] = r[srt_idx[1, 1:n_obs]];
      int s_srt[n_obs] = s[srt_idx[1, 1:n_obs]];
      //doses are also rescaled by reference dose after sorting
      vector[n_obs] dose_1_srt = dose_1[srt_idx[1, 1:n_obs]]/dose_c[1];
      vector[n_obs] dose_2_srt = dose_2[srt_idx[1, 1:n_obs]]/dose_c[2];
      vector[n_obs] ldose_1_srt = log(dose_1_srt);
      vector[n_obs] ldose_2_srt = log(dose_2_srt);
    }
    parameters{
      //hyper SDs
      real<lower=0> tau_1a;
      real<lower=0> tau_1b;
      real<lower=0> tau_2a;
      real<lower=0> tau_2b;
      real<lower=0> tau_eta;
      //correlation coefficients
      real<lower=-1, upper=1> rho12;
      real<lower=-1, upper=1> rho34;
      /*For non-centered parametrization:
        Sample only raw standard normal variables. These are later transformed to
        bivariate normals by multiplying with cholesky factor*/
      //matrix for log(alpha_ij), log(beta_ij) and eta_j (for comp i, study j)
      matrix[num_s, 5] log_ab_raw;
      //for hyper means
      real mu_raw[5];
    }
    transformed parameters{
      real mu_1a;
      real mu_1b;
      real mu_2a;
      real mu_2b;
      real mu_eta;
      matrix[num_s,5] log_ab;
      vector<lower=0, upper=1>[n_obs] p_srt;
      vector<lower=0, upper=1>[n_obs-n_obs1-n_obs2] p_2;
      vector<lower=0, upper=1>[n_obs-n_obs1-n_obs2] p_1;
      vector<lower=0, upper=1>[n_obs-n_obs1-n_obs2] p_0;
      //transform raw hyper means to correct distribution
      mu_1a = mean_mu[1] + sd_mu[1]*mu_raw[1];
      mu_1b = mean_mu[2] + sd_mu[2]*mu_raw[2];
      mu_2a = mean_mu[3] + sd_mu[3]*mu_raw[3];
      mu_2b = mean_mu[4] + sd_mu[4]*mu_raw[4];
      mu_eta = mean_mu[5] + sd_mu[5]*mu_raw[5];
      /*Hard-coded matrix multiplication with lower cholesky factor
        of covariance matrix. This can be done without saving the
        cholesky factor itself, as it is available analytically.
        The following means:
        log_ab = mu + L*log_ab_raw,
        where L is a lower triangular matrix with L*L^T=Sigma,
        for a covariance matrix Sigma.
        Note: For general
        Sigma = tau_1^2           rho*tau_1*tau_2
                rho*tau_1*tau_2   tau_2^2
        the lower cholesky factor is
        L =     tau_1         0
                tau_2*rho     tau_2*squareroot(1-rho^2)
        */
      log_ab[1:num_s,1] = mu_1a + tau_1a*log_ab_raw[1:num_s, 1];
      log_ab[1:num_s,2] = mu_1b + tau_1b*rho12*log_ab_raw[1:num_s, 1] +
                          tau_1b*sqrt(1-square(rho12))*log_ab_raw[1:num_s, 2];
      log_ab[1:num_s,3] = mu_2a + tau_2a*log_ab_raw[1:num_s, 3];
      log_ab[1:num_s,4] = mu_2b + tau_2b*rho34*log_ab_raw[1:num_s, 3] +
                          tau_2b*sqrt(1-square(rho34))*log_ab_raw[1:num_s, 4];
      log_ab[1:num_s,5] = mu_eta + tau_eta*log_ab_raw[1:num_s, 5];
      //toxicity models for mono and combination treatment are vectorized
      if(n_obs1>0){
        //treatments mono 1
        p_srt[1:n_obs1] = inv_logit(log_ab[s_srt[1:n_obs1],1] +
                               (exp(log_ab[s_srt[1:n_obs1],2]).*
                               ldose_1_srt[1:n_obs1]));
      }
      if(n_obs2>0){
        //treatments mono 2
         p_srt[(n_obs1+1):(n_obs1+n_obs2)] =
             inv_logit(log_ab[s_srt[(n_obs1+1):(n_obs1 + n_obs2)],3] +
                       (exp(log_ab[s_srt[(n_obs1+1): (n_obs1 + n_obs2)],4]).*
                       ldose_2_srt[(n_obs1+1): (n_obs1 + n_obs2)]));
      }
      if(n_obs-n_obs1-n_obs2>0){
        //treatments combination
        p_2[1 : (n_obs-n_obs1-n_obs2)] =
            inv_logit(log_ab[s_srt[(n_obs1 + n_obs2 + 1) : n_obs],3] +
                      (exp(log_ab[s_srt[(n_obs1 + n_obs2 + 1) : n_obs],4]).*
                      ldose_2_srt[(n_obs1 + n_obs2 + 1) : n_obs]));
        p_1[1 : (n_obs-n_obs1-n_obs2)] =
            inv_logit(log_ab[s_srt[(n_obs1 + n_obs2 + 1) : n_obs],1] +
                      (exp(log_ab[s_srt[(n_obs1 + n_obs2 + 1) : n_obs],2]).*
                      ldose_1_srt[(n_obs1 + n_obs2 + 1) : n_obs]));
        p_0[1 :(n_obs-n_obs1-n_obs2)] = p_1[1 : (n_obs-n_obs1-n_obs2)] +
                                     p_2[1 : (n_obs-n_obs1-n_obs2)] -
                                     (p_1[1 : (n_obs-n_obs1-n_obs2)] .*
                                     p_2[1 : (n_obs-n_obs1-n_obs2)]);
        if(saturating){
          p_srt[(n_obs1 + n_obs2 + 1) : n_obs] =
              inv_logit(logit(p_0[1 : (n_obs-n_obs1-n_obs2)]) +
                        (2*log_ab[s_srt[(n_obs1 + n_obs2 + 1) : n_obs],5].*
                        (dose_1_srt[(n_obs1 + n_obs2 + 1) : n_obs].*
                        dose_2_srt[(n_obs1 + n_obs2 + 1) : n_obs] )./
                        (1 + dose_1_srt[(n_obs1 + n_obs2 + 1) : n_obs].*
                             dose_2_srt[(n_obs1 + n_obs2 + 1) : n_obs])
                        ));
        }else{
          p_srt[(n_obs1 + n_obs2 + 1) : n_obs] =
              inv_logit(logit(p_0[1 : (n_obs-n_obs1-n_obs2)]) +
                        log_ab[s_srt[(n_obs1 + n_obs2 + 1) : n_obs],5].*
                        dose_1_srt[(n_obs1 + n_obs2 + 1) : n_obs].*
                        dose_2_srt[(n_obs1 + n_obs2 + 1) : n_obs] );
        }
      }
    }
    model{
      //priors for hyper means (non-centered)
      mu_raw ~  std_normal();
      //priors for hyper SD
      tau_1a ~ lognormal(mean_tau[1], sd_tau[1]);
      tau_1b ~ lognormal(mean_tau[2], sd_tau[2]);
      tau_2a ~ lognormal(mean_tau[3], sd_tau[3]);
      tau_2b ~ lognormal(mean_tau[4], sd_tau[4]);
      tau_eta ~ lognormal(mean_tau[5], sd_tau[5]);
      //priors for correlation coefficients
      rho12 ~ uniform(-1,1);
      rho34 ~ uniform(-1,1);
      //priors for regression parameters (non-centered)
      for(k in 1:num_s){
        log_ab_raw[k, 1:5] ~ std_normal();
      }
      //binomial likelihood
      r_srt ~ binomial(n_srt, p_srt);
    }
    generated quantities{
      //just to provide the sorted toxicity parameters as output
      vector<lower=0, upper=1>[n_obs] p = p_srt[srt_idx[2,1:n_obs]];
    }

### `crmPack`

Here we have the following JAGS model:

    {
        for (i in 1:nObs_A) {
            logit(p_A[i]) <- alpha0_A + alpha1_A * log(x_A[i]/ref_dose_A)
            y_A[i] ~ dbern(p_A[i])
        }
        for (i in 1:nObs_B) {
            x_drug1_B[i] <- x_B[i, 1L]
        }
        for (i in 1:nObs_B) {
            logit(p_drug1_B[i]) <- alpha0_drug1_B + alpha1_drug1_B * 
                log(x_drug1_B[i]/ref_dose_drug1_B)
            p_single_B[i, 1L] <- p_drug1_B[i]
        }
        for (i in 1:nObs_B) {
            x_drug2_B[i] <- x_B[i, 2L]
        }
        for (i in 1:nObs_B) {
            logit(p_drug2_B[i]) <- alpha0_drug2_B + alpha1_drug2_B * 
                log(x_drug2_B[i]/ref_dose_drug2_B)
            p_single_B[i, 2L] <- p_drug2_B[i]
        }
        for (i in 1:nObs_B) {
            combo_interaction_B[i] <- x_drug1_B[i]/ref_dose_drug1_B * 
                (x_drug2_B[i]/ref_dose_drug2_B)
        }
        for (i in 1:nObs_B) {
            p0_B[i] <- p_single_B[i, 1] + p_single_B[i, 2] - p_single_B[i, 
                1] * p_single_B[i, 2]
            logit(p_B[i]) <- log(p0_B[i]/(1 - p0_B[i])) + eta_B * 
                combo_interaction_B[i]
            y_B[i] ~ dbern(p_B[i])
        }
        for (i in 1:nObs_C) {
            logit(p_C[i]) <- alpha0_C + alpha1_C * log(x_C[i]/ref_dose_C)
            y_C[i] ~ dbern(p_C[i])
        }
    }
    {
        alpha0_A <- theta_A[1]
        alpha1_A <- exp(theta_A[2])
        alpha0_drug1_B <- theta_drug1_B[1]
        alpha1_drug1_B <- exp(theta_drug1_B[2])
        alpha0_drug2_B <- theta_drug2_B[1]
        alpha1_drug2_B <- exp(theta_drug2_B[2])
        alpha0_B[1L] <- alpha0_drug1_B
        alpha0_B[2L] <- alpha0_drug2_B
        alpha1_B[1L] <- alpha1_drug1_B
        alpha1_B[2L] <- alpha1_drug2_B
        eta_B ~ dnorm(eta_gamma_B, eta_tau_B)
        alpha0_C <- theta_C[1]
        alpha1_C <- exp(theta_C[2])
        theta_A[1:2] ~ dmnorm(mu_comp1_corr[], prec_comp1_corr[, 
            ])
        theta_drug1_B[1:2] ~ dmnorm(mu_comp1_corr[], prec_comp1_corr[, 
            ])
        mu_comp1_corr[1] <- mu_comp1_intercept
        mu_comp1_corr[2] <- mu_comp1_slope
        rho_comp1 ~ dunif(rho_comp1_lower, rho_comp1_upper)
        prec_comp1_corr[1, 1] <- 1/(pow(tau_comp1_intercept, 2) * 
            (1 - pow(rho_comp1, 2)))
        prec_comp1_corr[2, 2] <- 1/(pow(tau_comp1_slope, 2) * (1 - 
            pow(rho_comp1, 2)))
        prec_comp1_corr[1, 2] <- -rho_comp1/(tau_comp1_intercept * 
            tau_comp1_slope * (1 - pow(rho_comp1, 2)))
        prec_comp1_corr[2, 1] <- prec_comp1_corr[1, 2]
        mu_comp1_intercept ~ dnorm(mu_comp1_intercept_mean, pow(mu_comp1_intercept_sd, 
            -2))
        tau_comp1_intercept ~ dlnorm(tau_comp1_intercept_meanlog, 
            pow(tau_comp1_intercept_sdlog, -2))
        mu_comp1_slope ~ dnorm(mu_comp1_slope_mean, pow(mu_comp1_slope_sd, 
            -2))
        tau_comp1_slope ~ dlnorm(tau_comp1_slope_meanlog, pow(tau_comp1_slope_sdlog, 
            -2))
        theta_drug2_B[1:2] ~ dmnorm(mu_comp2_corr[], prec_comp2_corr[, 
            ])
        theta_C[1:2] ~ dmnorm(mu_comp2_corr[], prec_comp2_corr[, 
            ])
        mu_comp2_corr[1] <- mu_comp2_intercept
        mu_comp2_corr[2] <- mu_comp2_slope
        rho_comp2 ~ dunif(rho_comp2_lower, rho_comp2_upper)
        prec_comp2_corr[1, 1] <- 1/(pow(tau_comp2_intercept, 2) * 
            (1 - pow(rho_comp2, 2)))
        prec_comp2_corr[2, 2] <- 1/(pow(tau_comp2_slope, 2) * (1 - 
            pow(rho_comp2, 2)))
        prec_comp2_corr[1, 2] <- -rho_comp2/(tau_comp2_intercept * 
            tau_comp2_slope * (1 - pow(rho_comp2, 2)))
        prec_comp2_corr[2, 1] <- prec_comp2_corr[1, 2]
        mu_comp2_intercept ~ dnorm(mu_comp2_intercept_mean, pow(mu_comp2_intercept_sd, 
            -2))
        tau_comp2_intercept ~ dlnorm(tau_comp2_intercept_meanlog, 
            pow(tau_comp2_intercept_sdlog, -2))
        mu_comp2_slope ~ dnorm(mu_comp2_slope_mean, pow(mu_comp2_slope_sd, 
            -2))
        tau_comp2_slope ~ dlnorm(tau_comp2_slope_meanlog, pow(tau_comp2_slope_sdlog, 
            -2))
    }

### Conclusion

There are still some minor differences:

- In `decider`, the $`\eta`$ parameter is part of the 5-parameter
  hierarchical vector and has its own hypermean and heterogeneity. In
  `crmPack`, the $`\eta`$ parameter is not part of the hierarchical
  vector. When there is only combo arm then this does not matter.
- JAGS uses Bernoulli observations, Stan uses binomial cohort counts,
  but the likelihoods are equivalent.

Apart from these, the probabilistic models are equivalent. We could also
see that in the fit results which are very close to each other.

## References

Neuenschwander, Beat, Alessandro Matano, Zhongwen Tang, Satrajit
Roychoudhury, Simon Wandel, and SA Bailey. 2014. “Bayesian Industry
Approach to Phase I Combination Trials in Oncology.” *Statistical
Methods in Drug Combination Studies*, 95–135.

Schroeter, Lukas. 2023. *Decider: Decision Making in Multiple-Arm
Oncology Dose Escalation Trials with Logistic Regression*.
<https://Boehringer-Ingelheim.github.io/decider/>.
