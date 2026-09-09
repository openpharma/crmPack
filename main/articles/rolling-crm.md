# Rolling CRM Example

## Example 1: Recommend a dose for the next cohort

### Setting up the data

[`library`](https://rdrr.io/r/base/library.html)`(`[`crmPack`](https://docs.crmpack.org/)`)`` ``data`` ``<-`` `[`DataDA`](https://docs.crmpack.org/reference/DataDA-class.md)`(`` `` x ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0.1``, ``0.5``, ``1.5``, ``3``, ``6``, ``10``, ``10``, ``10``)``,`` `` y ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0``, ``0``, ``1``, ``1``, ``0``, ``0``, ``1``, ``0``)``,`` `` ID ``=`` `[`as.integer`](https://rdrr.io/r/base/integer.html)`(``1``:``8``)``,`` `` cohort ``=`` `[`as.integer`](https://rdrr.io/r/base/integer.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``1``, ``2``, ``3``, ``4``, ``5``, ``6``, ``6``, ``6``)``)``,`` `` doseGrid ``=`` `` `[`c`](https://rdrr.io/r/base/c.html)`(`` `` ``0.1``, ``0.5``, ``1.5``, ``3``, ``6``,`` `` `[`seq`](https://rdrr.io/r/base/seq.html)`(``from ``=`` ``10``, to ``=`` ``80``, by ``=`` ``2``)`` `` ``)``,`` `` u ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``42``, ``30``, ``15``, ``5``, ``20``, ``25``, ``30``, ``60``)``,`` `` t0 ``=`` `[`rep`](https://rdrr.io/r/base/rep.html)`(``0``, ``8``)``,`` `` Tmax ``=`` ``60`` ``)`` `` ``emptydata`` ``<-`` `[`DataDA`](https://docs.crmpack.org/reference/DataDA-class.md)`(`` `` doseGrid ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(`` `` ``0.1``, ``0.5``, ``1``, ``1.5``, ``3``, ``6``,`` `` `[`seq`](https://rdrr.io/r/base/seq.html)`(``from ``=`` ``10``, to ``=`` ``80``, by ``=`` ``2``)`` `` ``)``,`` `` Tmax ``=`` ``60`` ``)`

### Structure of the model class

`npiece_`` ``<-`` ``10`` ``Tmax_`` ``<-`` ``60`` `` ``lambda_prior`` ``<-`` ``function``(``k``)`` ``{`` `` ``npiece_`` ``/`` ``(``Tmax_`` ``*`` ``(``npiece_`` ``-`` ``k`` ``+`` ``0.5``)``)`` ``}`` `` ``model`` ``<-`` `[`DALogisticLogNormal`](https://docs.crmpack.org/reference/DALogisticLogNormal-class.md)`(`` `` mean ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``-``0.85``, ``1``)``,`` `` cov ``=`` `[`matrix`](https://rdrr.io/r/base/matrix.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``1``, ``-``0.5``, ``-``0.5``, ``1``)``, nrow ``=`` ``2``)``,`` `` ref_dose ``=`` ``56``,`` `` npiece ``=`` ``npiece_``,`` `` l ``=`` `[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(`[`t`](https://rdrr.io/r/base/t.html)`(`[`apply`](https://rdrr.io/r/base/apply.html)`(`[`as.matrix`](https://rdrr.io/r/base/matrix.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``1``:``npiece_``)``, ``1``, ``npiece_``)``, ``2``, ``lambda_prior``)``)``)``,`` `` c_par ``=`` ``2`` ``)`

### Obtain the posterior

`options`` ``<-`` `[`McmcOptions`](https://docs.crmpack.org/reference/McmcOptions-class.md)`(`` `` burnin ``=`` ``10``,`` `` step ``=`` ``2``,`` `` samples ``=`` ``1e2``,`` `` rng_kind ``=`` ``"Mersenne-Twister"``,`` `` rng_seed ``=`` ``3819`` ``)`` `` `[`set.seed`](https://rdrr.io/r/base/Random.html)`(``94``)`` ``samples`` ``<-`` `[`mcmc`](https://docs.crmpack.org/reference/mcmc.md)`(``data``, ``model``, ``options``)`

### Use ggmcmc to diagnose

[`library`](https://rdrr.io/r/base/library.html)`(`[`ggmcmc`](http://xavier-fim.net/packages/ggmcmc/)`)`` ``alpha0samples`` ``<-`` `[`get`](https://rdrr.io/r/base/get.html)`(``samples``, ``"alpha0"``)`` `` `[`print`](https://rdrr.io/r/base/print.html)`(`[`ggs_traceplot`](https://rdrr.io/pkg/ggmcmc/man/ggs_traceplot.html)`(``alpha0samples``)``)`

![A trace plot for alpha0. It looks like skyscrapers ina big city, but
there are only just over 200 samples in the
chain.](rolling-crm-figures/Diagnose-1-1.png)

plot of chunk Diagnose-1

[`print`](https://rdrr.io/r/base/print.html)`(`[`ggs_autocorrelation`](https://rdrr.io/pkg/ggmcmc/man/ggs_autocorrelation.html)`(``alpha0samples``)``)`

![An auto correlation plot for aplha0. There is significant
auto-correlation of 0.25 or more even at lags of 50. There is
seasonality too, with three groups of negative auto-correlation and four
of positive.](rolling-crm-figures/Diagnose-2-1.png)

plot of chunk Diagnose-2

### Plot the model fit

[`plot`](https://rdrr.io/r/graphics/plot.default.html)`(``samples``, ``model``, ``data``, hazard ``=`` ``TRUE``)`

![Two plots in a single row. The first shows the posterior mean and ci
for the probability of toxicity by dose. The second shows 100 times the
posterior hazard by time.](rolling-crm-figures/Fit-1-1.png)

plot of chunk Fit-1

[`plot`](https://rdrr.io/r/graphics/plot.default.html)`(``samples``, ``model``, ``data``, hazard ``=`` ``FALSE``)`

![Two plots in a single row. Both show the posterior mean and ci for the
probability of toxicity by dose on the y axis. In the first plot, the x
axis is dose. In the second, it is
time.](rolling-crm-figures/Fit-2-1.png)

plot of chunk Fit-2

### prior mean curve

`emptydata`` ``<-`` `[`DataDA`](https://docs.crmpack.org/reference/DataDA-class.md)`(``doseGrid ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(`` `` ``0.1``, ``0.5``, ``1.5``, ``3``, ``6``,`` `` `[`seq`](https://rdrr.io/r/base/seq.html)`(``from ``=`` ``10``, to ``=`` ``80``, by ``=`` ``2``)`` ``)``, Tmax ``=`` ``60``)`` `` ``Priorsamples`` ``<-`` `[`mcmc`](https://docs.crmpack.org/reference/mcmc.md)`(``emptydata``, ``model``, ``options``)`` `` `[`plot`](https://rdrr.io/r/graphics/plot.default.html)`(``Priorsamples``, ``model``, ``emptydata``, hazard ``=`` ``FALSE``)`

![Two plots in a single row. Both show the prior mean and ci for the
probability of toxicity by dose on the y axis. In the first plot, the x
axis is dose. In the second, it is
time.](rolling-crm-figures/Prior-1.png)

plot of chunk Prior

### Escalation rules

Need to fill in (use the same rule in the section 8 of “using the
package crmPack: introductory examples”)

`myIncrements`` ``<-`` `[`IncrementsRelative`](https://docs.crmpack.org/reference/IncrementsRelative-class.md)`(`` `` intervals ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0``, ``20``)``,`` `` increments ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``1``, ``0.33``)`` ``)`` `` ``nextMaxDose`` ``<-`` `[`maxDose`](https://docs.crmpack.org/reference/maxDose.md)`(``myIncrements``, data ``=`` ``data``)`` `` ``myNextBest`` ``<-`` `[`NextBestNCRM`](https://docs.crmpack.org/reference/NextBestNCRM-class.md)`(`` `` target ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0.2``, ``0.35``)``,`` `` overdose ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0.35``, ``1``)``,`` `` max_overdose_prob ``=`` ``0.25`` ``)`` `` ``mySize1`` ``<-`` `[`CohortSizeRange`](https://docs.crmpack.org/reference/CohortSizeRange-class.md)`(``intervals ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0``, ``30``)``, cohort_size ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``1``, ``3``)``)`` ``mySize2`` ``<-`` `[`CohortSizeDLT`](https://docs.crmpack.org/reference/CohortSizeDLT-class.md)`(``intervals ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0``, ``1``)``, cohort_size ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``1``, ``3``)``)`` ``mySize`` ``<-`` `[`maxSize`](https://docs.crmpack.org/reference/maxSize.md)`(``mySize1``, ``mySize2``)`` `` ``myStopping1`` ``<-`` `[`StoppingTargetProb`](https://docs.crmpack.org/reference/StoppingTargetProb-class.md)`(``target ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0.2``, ``0.35``)``, prob ``=`` ``0.5``)`` ``myStopping2`` ``<-`` `[`StoppingMinPatients`](https://docs.crmpack.org/reference/StoppingMinPatients-class.md)`(``nPatients ``=`` ``50``)`` ``myStopping`` ``<-`` ``(``myStopping1`` ``|`` ``myStopping2``)`

### Recommended dose for the next cohort

`doseRecommendation`` ``<-`` `[`nextBest`](https://docs.crmpack.org/reference/nextBest.md)`(``myNextBest``,`` `` doselimit ``=`` ``nextMaxDose``,`` `` samples ``=`` ``samples``,`` `` model ``=`` ``model``,`` `` data ``=`` ``data`` ``)`` `` ``doseRecommendation``$``plot`

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
25%.](rolling-crm-figures/Recommend-1.png)

plot of chunk Recommend

`doseRecommendation``$``value`` ``#> [1] 0.1`

## Example 2: Run a simulation to evaluate operating characteristics

### Set up safety window and `DADesign` to be completed

`mysafetywindow`` ``<-`` `[`SafetyWindowConst`](https://docs.crmpack.org/reference/SafetyWindowConst-class.md)`(`[`c`](https://rdrr.io/r/base/c.html)`(``6``, ``2``)``, ``7``, ``7``)`` `` ``design`` ``<-`` `[`DADesign`](https://docs.crmpack.org/reference/DADesign-class.md)`(`` `` model ``=`` ``model``,`` `` increments ``=`` ``myIncrements``,`` `` nextBest ``=`` ``myNextBest``,`` `` stopping ``=`` ``myStopping``,`` `` cohort_size ``=`` ``mySize``,`` `` data ``=`` ``emptydata``,`` `` safetyWindow ``=`` ``mysafetywindow``,`` `` startingDose ``=`` ``3`` ``)`

### Set up true curves

`myTruth`` ``<-`` `[`probFunction`](https://docs.crmpack.org/reference/probFunction.md)`(``model``, alpha0 ``=`` ``2``, alpha1 ``=`` ``3``)`` `[`curve`](https://rdrr.io/r/graphics/curve.html)`(``myTruth``(``x``)``, from ``=`` ``0``, to ``=`` ``100``, ylim ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0``, ``1``)``)`

![A logistic dose response curve rising from 0 at dose 0 to almost 100%
for a dose of 100.](rolling-crm-figures/Truth-1.png)

plot of chunk Truth

` ``onset`` ``<-`` ``15`` ``exp_cond.cdf`` ``<-`` ``function``(``x``)`` ``{`` `` ``1`` ``-`` ``(`[`pexp`](https://rdrr.io/r/stats/Exponential.html)`(``x``, ``1`` ``/`` ``onset``, lower.tail ``=`` ``FALSE``)`` ``-`` `[`pexp`](https://rdrr.io/r/stats/Exponential.html)`(``28``, ``1`` ``/`` ``onset``, lower.tail ``=`` ``FALSE``)``)`` ``/`` `[`pexp`](https://rdrr.io/r/stats/Exponential.html)`(``28``, ``1`` ``/`` ``onset``)`` ``}`

### Perform the simulations

`mySims`` ``<-`` `[`simulate`](https://rdrr.io/r/stats/simulate.html)`(``design``,`` `` args ``=`` ``NULL``,`` `` truthTox ``=`` ``myTruth``,`` `` truthSurv ``=`` ``exp_cond.cdf``,`` `` trueTmax ``=`` ``80``,`` `` nsim ``=`` ``2``,`` `` seed ``=`` ``819``,`` `` mcmcOptions ``=`` ``options``,`` `` firstSeparate ``=`` ``TRUE``,`` `` deescalate ``=`` ``FALSE``,`` `` parallel ``=`` ``FALSE`` ``)`

### Interpret the simulation results

Use a similar way as section 9.2 in the “using the package crmPack:
introductory examples” document

`a`` ``<-`` `[`summary`](https://rdrr.io/r/base/summary.html)`(``mySims``, truth ``=`` ``myTruth``)`` ``b`` ``<-`` ``mySims``@``data``[[``1``]``]`` `` `[`plot`](https://rdrr.io/r/graphics/plot.default.html)`(``mySims``)`` `[`plot`](https://rdrr.io/r/graphics/plot.default.html)`(``b``)`

![Two graphs in a single column, summarising the results of a single
simulated trial. The upper one plots patient number on the x axis and
dose administered on the y axis. Different symbols indicate whether or
not each participant reported a toxicity. Sixteen patients were
enrolled, four of which reported toxicities. The points rise and fall
like waves in response to changes in the model's recommended dose. The
lower one plots time on the x axis and patient number on the y axis. For
each patient, a horizontal line runs from their enrolment time to the
time at which they reported a toxicity, completed their safety
evaluation window or (at the end of the trial) were censored. Different
coloured and shaped symbols at the right hand end of each line indicate
whether or not the participant reported a
toxicity.](rolling-crm-figures/Interpret-1.png)

plot of chunk Interpret

` ``mySims``@``stop_reasons``[[``2``]``]`` ``#> [[1]]`` ``#> [1] "Probability for target toxicity is 58 % for dose 32 and thus above the required 50 %"`` ``#> `` ``#> [[2]]`` ``#> [1] "Number of patients is 19 and thus below the prespecified minimum number 50"`` `` ``# nolint end`
