# Trial Design: basic sanity checks

> The [`tidy()`](https://openpharma.github.io/crmPack/reference/tidy.md)
> methods used in this vignette follow the pattern established by the
> `broom` package and turn objects of various classes into tibbles.
> Currently, they are defined in a code chunk within this tibble that is
> not displayed, but they will be added to the main `crmPack` package at
> a later date.

## Introduction

This vignette contains an example of a very basic and superficial
examination of the operating characteristics of a basic CRM trial. The
design uses a 2-parameter logistic regression model with a log normal
prior distribution, and custom rules for choosing the cohort size,
deciding whether to stop or continue the trial, defining the maximum
permitted increment and selecting the dose for the nest study.

The first step is to define the trial design in `crmPack`…

## Study definition

``` r
library(crmPack)


# Define the dose grid and an empty data object
dose_grid <- c(1, 3, 5, 10, 15, 20, 25, 40, 50, 80, 100)
empty_data <- Data(doseGrid = dose_grid)

# Initialize the CRM model.
initial_model <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 56
)

# Choose the rule for selecting the next dose.
next_best <- NextBestNCRM(
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)

# Choose the rule for the cohort size.
cohort_size1 <- CohortSizeRange(
  intervals = c(0, 30),
  cohort_size = c(1, 3)
)
cohort_size2 <- CohortSizeDLT(
  intervals = c(0, 1),
  cohort_size = c(1, 3)
)
cohort_size <- maxSize(cohort_size1, cohort_size2)

# Choose the rule for stopping.
stopping_success1 <- StoppingMinCohorts(nCohorts = 3)
stopping_success2 <- StoppingTargetProb(
  target = c(0.2, 0.35),
  prob = 0.5
)
stopping_futility <- StoppingMinPatients(nPatients = 20)
stopping_trial <- (stopping_success1 & stopping_success2) | stopping_futility

# Choose the rule for dose increments.
increment_rule <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)

# Initialize the design.
design <- Design(
  model = initial_model,
  nextBest = next_best,
  stopping = stopping_trial,
  increments = increment_rule,
  cohort_size = cohort_size,
  data = empty_data,
  startingDose = 3
)
```

## Incoherence and rigidity

The `examine` function lists the dose recommendations made by CRM design
after the first DLT is reported. The output shows, for each dose and
number of DLTs reported, the dose recommendation (`nextBest` dose) made
by the model on the assumption that no earlier DLTs were reported at
lower doses.

``` r
examine(design) %>% kable()
```

| dose | DLTs | nextDose | stop  | increment |
|-----:|-----:|---------:|:------|----------:|
|    3 |    0 |        5 | FALSE |        67 |
|    3 |    1 |       NA | FALSE |        NA |
|    5 |    0 |       10 | FALSE |       100 |
|    5 |    1 |        5 | FALSE |         0 |
|   10 |    0 |       20 | FALSE |       100 |
|   10 |    1 |       10 | FALSE |         0 |
|   20 |    0 |       25 | FALSE |        25 |
|   20 |    1 |       20 | FALSE |         0 |
|   25 |    0 |       25 | FALSE |         0 |
|   25 |    1 |       25 | FALSE |         0 |
|   25 |    0 |       25 | FALSE |         0 |
|   25 |    1 |       25 | FALSE |         0 |
|   25 |    0 |       25 | FALSE |         0 |
|   25 |    1 |       25 | FALSE |         0 |
|   25 |    0 |       25 | FALSE |         0 |
|   25 |    1 |       25 | FALSE |         0 |
|   25 |    0 |       25 | FALSE |         0 |
|   25 |    1 |       25 | FALSE |         0 |
|   25 |    0 |       25 | FALSE |         0 |
|   25 |    1 |       25 | FALSE |         0 |
|   25 |    0 |       25 | FALSE |         0 |
|   25 |    1 |       25 | FALSE |         0 |
|   25 |    0 |       25 | FALSE |         0 |
|   25 |    1 |       25 | FALSE |         0 |
|   25 |    0 |       25 | FALSE |         0 |
|   25 |    1 |       25 | FALSE |         0 |
|   25 |    0 |       25 | FALSE |         0 |
|   25 |    1 |       25 | FALSE |         0 |
|   25 |    0 |       25 | FALSE |         0 |
|   25 |    1 |       25 | FALSE |         0 |
|   25 |    0 |       25 | FALSE |         0 |
|   25 |    1 |       25 | FALSE |         0 |
|   25 |    0 |       25 | FALSE |         0 |
|   25 |    1 |       25 | FALSE |         0 |
|   25 |    0 |       25 | FALSE |         0 |
|   25 |    1 |       25 | FALSE |         0 |
|   25 |    0 |       25 | FALSE |         0 |
|   25 |    1 |       25 | FALSE |         0 |
|   25 |    0 |       25 | TRUE  |         0 |
|   25 |    1 |       25 | TRUE  |         0 |

Cheung (Cheung 2013) (p146) describes a CRM trial\* as *rigid* if there
exists a dose from which escalation is not possible regardless of the
number of participants who do not experience when treated at that dose.
He then (p164) defines a CRM trial as *incoherent* if, among other
criteria, it recommends a dose escalation immediately following the
first report of a DLT.

> \*: Strictly, Cheung defines incoherence and rigidity *only* in terms
> of a trial’s dose-toxicity model. We extend the terms to include all
> of a trial’s design features, including its escalation, dose
> selection, increments and stopping rules.

The output from `examine` shows that our initial design is not
incoherent (because there is no recommendation to increase the dose used
in the next cohort following the first report of a DLT), but it is
rigid. It is rigid because, even when no toxicities are reported at 25
mg, the model is unable to recommend escalation before the trial’s
futility stopping rule (defined by `stopping_futility` above) is
satisfied.

This is clearly unsatisfactory. The problem lies with the increments
rule, which allows a maximum escalation of 33% between one dose and the
next for doses above 20 mg. The dose grid being used is 1, 3, 5, 10, 15,
20, 25, 40, 50, 80, 100. The ratio between 20 and 25 is 1.25, but
between 25 and 40 is 1.6. We can confirm this by checking the behaviour
of `maxDose` with artificial data:

``` r
no_tox_below_25_data <- Data(
  doseGrid = dose_grid,
  x = c(1, 3, 5, 10, 15, 20, 25),
  y = rep(0, 7),
  ID = 1L:7L,
  cohort = 1L:7L
)

next_dose <- maxDose(increment_rule, no_tox_below_25_data)
next_dose
#> [1] 33.25
```

Thus, the initial dose rule permanently prevents escalation above 25 mg
because the highest permitted dose above 25 mg is 33.25 but the lowest
such dose in the dose grid is 40. The problem can be solved in at least
two ways: we can either relax the increments rule or we can introduce
one or more intermediate doses. (We could, of course, do both.) We
choose to relax the increments rule and check that, ceteris paribus,
this permits escalation from every dose:

``` r
revised_increment_rule <- IncrementsRelative(
  intervals = c(0, 20, 80),
  increments = c(1, 0.67, 0.33)
)
tibble(
  HighestDoseUsed = dose_grid,
  MaxPermittedDose = c(
    NA,
    sapply(
      seq_along(dose_grid[-1]),
      function(n) {
        tmp <- dose_grid[-1]
        d <- Data(
          doseGrid = dose_grid,
          x = tmp[1:n],
          y = rep(0, n),
          ID = as.integer(1:n),
          cohort = as.integer(1:n)
        )
        maxDose(revised_increment_rule, d)
      }
    )
  )
) %>%
  mutate(EscalationPermitted = lead(HighestDoseUsed < MaxPermittedDose)) %>%
  kable()
```

| HighestDoseUsed | MaxPermittedDose | EscalationPermitted |
|----------------:|-----------------:|:--------------------|
|               1 |               NA | TRUE                |
|               3 |             6.00 | TRUE                |
|               5 |            10.00 | TRUE                |
|              10 |            20.00 | TRUE                |
|              15 |            30.00 | TRUE                |
|              20 |            33.40 | TRUE                |
|              25 |            41.75 | TRUE                |
|              40 |            66.80 | TRUE                |
|              50 |            83.50 | TRUE                |
|              80 |           106.40 | TRUE                |
|             100 |           133.00 | NA                  |

Yes, it does. So we update the design:

``` r
revised_design <- Design(
  model = initial_model,
  nextBest = next_best,
  stopping = stopping_trial,
  increments = revised_increment_rule,
  cohort_size = cohort_size,
  data = empty_data,
  startingDose = 3
)
```

## Does the prior make sense?

``` r
examine(revised_design) %>% kable()
```

| dose | DLTs | nextDose | stop  | increment |
|-----:|-----:|---------:|:------|----------:|
|    3 |    0 |        5 | FALSE |        67 |
|    3 |    1 |       NA | FALSE |        NA |
|    5 |    0 |       10 | FALSE |       100 |
|    5 |    1 |        3 | FALSE |       -40 |
|   10 |    0 |       20 | FALSE |       100 |
|   10 |    1 |       10 | FALSE |         0 |
|   20 |    0 |       25 | FALSE |        25 |
|   20 |    1 |       20 | FALSE |         0 |
|   25 |    0 |       40 | FALSE |        60 |
|   25 |    1 |       25 | FALSE |         0 |
|   40 |    0 |       50 | FALSE |        25 |
|   40 |    1 |       40 | FALSE |         0 |
|   40 |    2 |       25 | FALSE |       -38 |
|   40 |    3 |       20 | FALSE |       -50 |
|   50 |    0 |       50 | FALSE |         0 |
|   50 |    1 |       50 | FALSE |         0 |
|   50 |    2 |       40 | FALSE |       -20 |
|   50 |    3 |       40 | FALSE |       -20 |
|   50 |    0 |       50 | FALSE |         0 |
|   50 |    1 |       50 | FALSE |         0 |
|   50 |    2 |       50 | FALSE |         0 |
|   50 |    3 |       40 | FALSE |       -20 |
|   50 |    0 |       50 | FALSE |         0 |
|   50 |    1 |       50 | FALSE |         0 |
|   50 |    2 |       50 | FALSE |         0 |
|   50 |    3 |       50 | FALSE |         0 |
|   50 |    0 |       50 | TRUE  |         0 |
|   50 |    1 |       50 | TRUE  |         0 |
|   50 |    2 |       50 | TRUE  |         0 |
|   50 |    3 |       50 | TRUE  |         0 |

Hmmm. We now appear to be stuck at 50 mg. Why is this? Let’s examine the
state of the model after escalation to 50 mg without toxicity…

``` r
no_tox_below_50_data <- Data(
  doseGrid = dose_grid,
  x = c(1, 3, 5, 10, 15, 20, 25, 40, 40, 40, 50, 50, 50),
  y = rep(0, 13),
  ID = 1L:13L,
  cohort = c(1L:7L, rep(8L:9L, each = 3))
)

default_mcmc_options <- McmcOptions(burnin = 1000, step = 2, samples = 1000)
no_tox_50_samples <- mcmc(no_tox_below_50_data, initial_model, default_mcmc_options)
recommended_dose <- nextBest(
  next_best,
  doselimit = Inf,
  samples = no_tox_50_samples,
  model = initial_model,
  data = no_tox_below_50_data
)
recommended_dose$value
#> [1] 50
recommended_dose$probs
#>       dose target overdose
#>  [1,]    1  0.000    0.000
#>  [2,]    3  0.000    0.000
#>  [3,]    5  0.000    0.000
#>  [4,]   10  0.000    0.000
#>  [5,]   15  0.000    0.000
#>  [6,]   20  0.001    0.000
#>  [7,]   25  0.002    0.000
#>  [8,]   40  0.027    0.001
#>  [9,]   50  0.122    0.016
#> [10,]   80  0.145    0.797
#> [11,]  100  0.071    0.911

maxDose(revised_increment_rule, no_tox_below_50_data)
#> [1] 83.5
```

Whilst the increments rule allows escalation to 83.5 mg, the toxicity
estimates provided by the model do not: the current estimate of toxicity
at 80 mg is 0.797, well above the limit of 0.25 defined in the dose
recommendation rule (`next_best`).

Again, we have several options. We can introduce intermediate doses,
thus slowing escalation and allowing the prior model more time to adapt
(but probably requiring a larger trial), or we can modify the prior so
that it allows faster, but still reasonable, escalation. Let’s try
creating a minimally informative prior that is consistent with our first
attempt.

``` r
# Fitting the min_inf_model is slow.
if (file.exists("minInfModel.Rds")) {
  min_inf_model <- readRDS("minInfModel.Rds")
} else {
  min_inf_model <- MinimalInformative(
    dose_grid,
    56,
    threshmin = 0.1,
    threshmax = 0.4,
    probmin = 0.05,
    probmax = 0.05
  )

  as_tibble(min_inf_model$required) %>%
    add_column(Dose = dose_grid) %>%
    add_column(Type = "Required") %>%
    bind_rows(
      as_tibble(min_inf_model$quantiles) %>%
        add_column(Dose = dose_grid) %>%
        add_column(Type = "Fitted")
    ) %>%
    ggplot() +
    geom_line(aes(x = Dose, y = median, colour = Type), linetype = "solid") +
    geom_line(aes(x = Dose, y = lower, colour = Type), linetype = "dotted") +
    geom_line(aes(x = Dose, y = upper, colour = Type), linetype = "dotted")

  saveRDS(min_inf_model, "minInfModel.Rds")
}

min_inf_model$model@params
```

The prior for θ is given by
``` math
 \boldsymbol\theta = \begin{bmatrix}\alpha \\ \beta\end{bmatrix}\sim N \left(\begin{bmatrix} 0.98 \\  1.35\end{bmatrix} , \begin{bmatrix} 1.92 &  0.05 \\ 0.05 &  0.00\end{bmatrix} \right) 
```

The minimally informative model has a prior distribution of

``` math
\binom{\alpha}{log(\beta)} \sim N \left( \left[\begin{array}{rr} 0.979 \\  1.348 \end{array}\right], \left[\begin{array}{rr}  1.923 &  0.045 \\  0.045 &  0.002\end{array}\right]\right)
```

Does this fix the rigidity problem?

``` r
revised_model <- min_inf_model$model

revised_design1 <- Design(
  model = revised_model,
  nextBest = next_best,
  stopping = stopping_trial,
  increments = revised_increment_rule,
  cohort_size = cohort_size,
  data = empty_data,
  startingDose = 3
)

examine(revised_design1) %>% kable()
```

| dose | DLTs | nextDose | stop  | increment |
|-----:|-----:|---------:|:------|----------:|
|    3 |    0 |        5 | FALSE |        67 |
|    3 |    1 |        3 | FALSE |         0 |
|    5 |    0 |       10 | FALSE |       100 |
|    5 |    1 |        3 | FALSE |       -40 |
|   10 |    0 |       10 | FALSE |         0 |
|   10 |    1 |        5 | FALSE |       -50 |
|   10 |    0 |       15 | FALSE |        50 |
|   10 |    1 |        5 | FALSE |       -50 |
|   15 |    0 |       20 | FALSE |        33 |
|   15 |    1 |       10 | FALSE |       -33 |
|   20 |    0 |       25 | FALSE |        25 |
|   20 |    1 |       10 | FALSE |       -50 |
|   25 |    0 |       25 | FALSE |         0 |
|   25 |    1 |       15 | FALSE |       -40 |
|   25 |    0 |       25 | FALSE |         0 |
|   25 |    1 |       20 | FALSE |       -20 |
|   25 |    0 |       25 | FALSE |         0 |
|   25 |    1 |       20 | FALSE |       -20 |
|   25 |    0 |       40 | FALSE |        60 |
|   25 |    1 |       25 | FALSE |         0 |
|   40 |    0 |       50 | FALSE |        25 |
|   40 |    1 |       40 | FALSE |         0 |
|   40 |    2 |       25 | FALSE |       -38 |
|   40 |    3 |       20 | FALSE |       -50 |
|   50 |    0 |       80 | FALSE |        60 |
|   50 |    1 |       50 | FALSE |         0 |
|   50 |    2 |       40 | FALSE |       -20 |
|   50 |    3 |       25 | FALSE |       -50 |
|   80 |    0 |      100 | FALSE |        25 |
|   80 |    1 |       50 | FALSE |       -38 |
|   80 |    2 |       50 | FALSE |       -38 |
|   80 |    3 |       40 | FALSE |       -50 |

Yes, it does. We can now escalate to dose 80, but there are few plateaux
as we do so. First, two toxicity-free participants, rather than just
one, are needed to escalate from 20 mg to 25 mg. Similarly, four
toxicity-free participants are required to escalate from 25 mg to 40 mg.
Further fine tuning of the prior would probably allow these numbers to
be reduced, but for the purposes of this vignette, we do not explore
this possibility.

Now create some data that represent a trial that escalates without any
reports of toxicity, and fit the revised model …

``` r
no_tox_data <- Data(
  doseGrid = c(1, 3, 5, 10, 15, 20, 25, 40, 50, 80, 100),
  x = c(c(1, 3, 5, 10, 15, 20), rep(c(25, 40, 50, 80, 100), each = 3)),
  y = rep(0, 21),
  cohort = as.integer(c(1:6, rep(7:11, each = 3))),
  ID = as.integer(1:21)
)

no_tox_samples <- mcmc(no_tox_data, revised_model, default_mcmc_options)

recommended_dose <- nextBest(
  next_best,
  doselimit = Inf,
  samples = no_tox_samples,
  model = initial_model,
  data = no_tox_data
)
recommended_dose$probs
#>       dose target overdose
#>  [1,]    1  0.000    0.000
#>  [2,]    3  0.000    0.000
#>  [3,]    5  0.000    0.000
#>  [4,]   10  0.000    0.000
#>  [5,]   15  0.000    0.000
#>  [6,]   20  0.000    0.000
#>  [7,]   25  0.000    0.000
#>  [8,]   40  0.015    0.000
#>  [9,]   50  0.048    0.000
#> [10,]   80  0.356    0.027
#> [11,]  100  0.465    0.095

stopTrial(stopping_trial, recommended_dose$value, no_tox_samples, initial_model, no_tox_data)
#> [1] TRUE
#> attr(,"message")
#> attr(,"message")[[1]]
#> attr(,"message")[[1]][[1]]
#> [1] "Number of cohorts is 11 and thus reached the prespecified minimum number 3"
#> 
#> attr(,"message")[[1]][[2]]
#> [1] "Probability for target toxicity is 46 % for dose 100 and thus below the required 50 %"
#> 
#> 
#> attr(,"message")[[2]]
#> [1] "Number of patients is 21 and thus reached the prespecified minimum number 20"
#> 
#> attr(,"individual")
#> attr(,"individual")[[1]]
#> [1] FALSE
#> attr(,"message")
#> attr(,"message")[[1]]
#> [1] "Number of cohorts is 11 and thus reached the prespecified minimum number 3"
#> 
#> attr(,"message")[[2]]
#> [1] "Probability for target toxicity is 46 % for dose 100 and thus below the required 50 %"
#> 
#> attr(,"individual")
#> attr(,"individual")[[1]]
#> [1] TRUE
#> attr(,"message")
#> [1] "Number of cohorts is 11 and thus reached the prespecified minimum number 3"
#> attr(,"report_label")
#> [1] "≥ 3 cohorts dosed"
#> 
#> attr(,"individual")[[2]]
#> [1] FALSE
#> attr(,"message")
#> [1] "Probability for target toxicity is 46 % for dose 100 and thus below the required 50 %"
#> attr(,"report_label")
#> [1] "P(0.2 ≤ prob(DLE | NBD) ≤ 0.35) ≥ 0.5"
#> 
#> attr(,"report_label")
#> [1] NA
#> 
#> attr(,"individual")[[2]]
#> [1] TRUE
#> attr(,"message")
#> [1] "Number of patients is 21 and thus reached the prespecified minimum number 20"
#> attr(,"report_label")
#> [1] "≥ 20 patients dosed"
#> 
#> attr(,"report_label")
#> [1] NA
```

More importantly, these results illustrate something that should have
been obvious from the outset: expecting the trial to identify the MTD
definitively in fewer than 20 participants is unrealistic (unless the
MTD is very much towards the left hand - lower - end of the dose grid)
because it takes almost 20 participants to travel from the lower end of
the dose grid to the upper. Ignoring the plateauing mentioned in the
previous paragraph, the increments rule we defined means that we need at
least one participant at doses of 1, 3, 5, 10, 15 and 20 mg, and three
at each of 25, 40, 50, 80 and 100 mg. That’s a total of 6 x 1 + 5 x 3 =
21 participants. So we hit the futility limit before we finish
escalation over the dose grid, even if no DLTs are reported. That’s
simply not realistic.

So our final alteration is to increase the futility rule from 20
participants to 40.

``` r
revised_stopping_futility <- StoppingMinPatients(nPatients = 40)
revised_stopping_trial <- (stopping_success1 & stopping_success2) | revised_stopping_futility

revised_design2 <- Design(
  model = revised_model,
  nextBest = next_best,
  stopping = revised_stopping_trial,
  increments = revised_increment_rule,
  cohort_size = cohort_size,
  data = empty_data,
  startingDose = 3
)
```

This new design allows us to escalate over the full extent of the dose
grid before the futility stopping rule kicks in. Further refinement of
the prior model may remove the need for additional cohorts before
escalation from 20, 25 and 50 mg. We leave that as an exercise for the
reader.

We now have a design that is worthy of more detailed investigation of
its operating characteristics. We will do this in the next vignette in
the series, which is yet to be written.

## Final observation

The simulations used in this vignette have used relatively short chains
of 1000 samples, purely in the interests of speed. When investigating
the properties of a real trial, much longer chain lengths should be
used. For example, to estimate a binomial probability to an accuracy of
±1%, an effective sample size (ESS) of around 40,000 is required.

## References

Cheung, Ken. 2013. *: Dose-Finding by the Continual Reassessment
Method*.
<https://www.routledge.com/Dose-Finding-by-the-Continual-Reassessment-Method/Cheung/p/book/9781420091519>.
