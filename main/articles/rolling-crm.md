# Rolling CRM Example

## Example 1: Recommend a dose for the next cohort

### Setting up the data

``` r
library(crmPack)
```

    ## Loading required package: ggplot2

    ## Registered S3 method overwritten by 'crmPack':
    ##   method       from  
    ##   print.gtable gtable

    ## Type crmPackHelp() to open help browser
    ## Type crmPackExample() to open example

``` r
data <- DataDA(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = c(0, 0, 1, 1, 0, 0, 1, 0),
  ID = as.integer(1:8),
  cohort = as.integer(c(1, 2, 3, 4, 5, 6, 6, 6)),
  doseGrid =
    c(
      0.1, 0.5, 1.5, 3, 6,
      seq(from = 10, to = 80, by = 2)
    ),
  u = c(42, 30, 15, 5, 20, 25, 30, 60),
  t0 = rep(0, 8),
  Tmax = 60
)

emptydata <- DataDA(
  doseGrid = c(
    0.1, 0.5, 1, 1.5, 3, 6,
    seq(from = 10, to = 80, by = 2)
  ),
  Tmax = 60
)
```

### Structure of the model class

``` r
npiece_ <- 10
Tmax_ <- 60

lambda_prior <- function(k) {
  npiece_ / (Tmax_ * (npiece_ - k + 0.5))
}

model <- DALogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 56,
  npiece = npiece_,
  l = as.numeric(t(apply(as.matrix(c(1:npiece_), 1, npiece_), 2, lambda_prior))),
  c_par = 2
)
```

### Obtain the posterior

``` r
options <- McmcOptions(
  burnin = 10,
  step = 2,
  samples = 1e2
)

set.seed(94)
samples <- mcmc(data, model, options)
```

### Use ggmcmc to diagnose

``` r
library(ggmcmc)
```

    ## Loading required package: dplyr

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    ## Loading required package: tidyr

``` r
alpha0samples <- get(samples, "alpha0")

print(ggs_traceplot(alpha0samples))
```

![A trace plot for alpha0. It looks like skyscrapers ina big city, but
there are only just over 200 samples in the
chain.](rolling-crm_files/figure-html/Diagnose-1-1.png)

``` r
print(ggs_autocorrelation(alpha0samples))
```

![An auto correlation plot for aplha0. There is significant
auto-correlation of 0.25 or more even at lags of 50. There is
seasonality too, with three groups of negative auto-correlation and four
of positive.](rolling-crm_files/figure-html/Diagnose-2-1.png)

### Plot the model fit

``` r
plot(samples, model, data, hazard = TRUE)
```

![Two plots in a single row. The first shows the posterior mean and ci
for the probability of toxicity by dose. The second shows 100 times the
posterior hazard by time.](rolling-crm_files/figure-html/Fit-1-1.png)

``` r
plot(samples, model, data, hazard = FALSE)
```

![Two plots in a single row. Both show the posterior mean and ci for the
probability of toxicity by dose on the y axis. In the first plot, the x
axis is dose. In the second, it is
time.](rolling-crm_files/figure-html/Fit-2-1.png)

### prior mean curve

``` r
emptydata <- DataDA(doseGrid = c(
  0.1, 0.5, 1.5, 3, 6,
  seq(from = 10, to = 80, by = 2)
), Tmax = 60)

Priorsamples <- mcmc(emptydata, model, options)

plot(Priorsamples, model, emptydata, hazard = FALSE)
```

![Two plots in a single row. Both show the prior mean and ci for the
probability of toxicity by dose on the y axis. In the first plot, the x
axis is dose. In the second, it is
time.](rolling-crm_files/figure-html/Prior-1.png)

### Escalation rules

Need to fill in (use the same rule in the section 8 of “using the
package crmPack: introductory examples”)

``` r
myIncrements <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)

nextMaxDose <- maxDose(myIncrements, data = data)

myNextBest <- NextBestNCRM(
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)

mySize1 <- CohortSizeRange(intervals = c(0, 30), cohort_size = c(1, 3))
mySize2 <- CohortSizeDLT(intervals = c(0, 1), cohort_size = c(1, 3))
mySize <- maxSize(mySize1, mySize2)

myStopping1 <- StoppingTargetProb(target = c(0.2, 0.35), prob = 0.5)
myStopping2 <- StoppingMinPatients(nPatients = 50)
myStopping <- (myStopping1 | myStopping2)
```

### Recommended dose for the next cohort

``` r
doseRecommendation <- nextBest(myNextBest,
  doselimit = nextMaxDose,
  samples = samples,
  model = model,
  data = data
)

doseRecommendation$plot
```

![Two graphs arranged in a single column. The upper graph shoes green
lines of various heights that show the probability each dose is in the
target toxicity range. There is a big arrow pointing to the bar at a
dose of 0.5, that this is the recommended dose for the next cohort. The
bars for other doses are higher, but they are not eligible for dosing
because of the overdose rule illustrated in the second graph below. The
lower graph as a similar series of red lines, indicating the probability
that each dose is in the overdose range. There is a horizontal black
dashed line at 25%, indicating that this is the highest acceptable
probability of being in the overdose range. The red bars for doses above
0.5 all extend above 25%, indicating that their toxicity is
unacceptable. The toxicity for doses of 0.1 and 0.5 lie below
25%.](rolling-crm_files/figure-html/Recommend-1.png)

``` r
doseRecommendation$value
```

    ## [1] 0.1

## Example 2: Run a simulation to evaluate operating characteristics

### Set up safety window and `DADesign` to be completed

``` r
mysafetywindow <- SafetyWindowConst(c(6, 2), 7, 7)

design <- DADesign(
  model = model,
  increments = myIncrements,
  nextBest = myNextBest,
  stopping = myStopping,
  cohort_size = mySize,
  data = emptydata,
  safetyWindow = mysafetywindow,
  startingDose = 3
)
```

### Set up true curves

``` r
myTruth <- probFunction(model, alpha0 = 2, alpha1 = 3)
curve(myTruth(x), from = 0, to = 100, ylim = c(0, 1))
```

![A logistic dose response curverising from 0 at dose 0 to almost 100%
for a dose of 100.](rolling-crm_files/figure-html/Truth-1.png)

``` r
onset <- 15
exp_cond.cdf <- function(x) {
  1 - (pexp(x, 1 / onset, lower.tail = FALSE) - pexp(28, 1 / onset, lower.tail = FALSE)) / pexp(28, 1 / onset)
}
```

### Perform the simulations

``` r
mySims <- simulate(design,
  args = NULL,
  truthTox = myTruth,
  truthSurv = exp_cond.cdf,
  trueTmax = 80,
  nsim = 2,
  seed = 819,
  mcmcOptions = options,
  firstSeparate = TRUE,
  deescalate = FALSE,
  parallel = FALSE
)
```

### Interpret the simulation results

Use a similar way as section 9.2 in the “using the package crmPack:
introductory examples” document

``` r
a <- summary(mySims, truth = myTruth)
b <- mySims@data[[1]]

plot(mySims)
plot(b)
```

![Two graphs in a single column, summarising the results of a single
simulated trial. The upper one plots patient number on the x axis and
dose andministered on the y axis. Different symbols indicate whether or
not each participant reported a toxicity. Sixteen patients were
enrolled, four of which reported toxicities. The points rise and fall
like waves in response to changes in the model's recommended dose. The
lower one plots time on the x axis and patient number on the y axis. For
each patient, a horizontal line runs from their enrolment time to the
time at which they reported a toxicity, completed their safety
evaluatiuon window or (at the end of the trial) were censored. Different
coloured and shaped symbols at the right hand end of each line indicate
whether or not the participant reported a
toxicity.](rolling-crm_files/figure-html/Interpret-1.png)

``` r
mySims@stop_reasons[[2]]
```

    ## [[1]]
    ## [1] "Probability for target toxicity is 53 % for dose 18 and thus above the required 50 %"
    ## 
    ## [[2]]
    ## [1] "Number of patients is 12 and thus below the prespecified minimum number 50"

``` r
# nolint end
```
