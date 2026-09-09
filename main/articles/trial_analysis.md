# Trial Analysis

## Introduction

This vignette picks up where the previous one ([Trial
Definition](https://docs.crmpack.org/articles/trial_definition.md)),
ends. To recap, our trial defines the six fundamental elements of a CRM
trial as

#### The dose grid

The trial will use a dose grid consisting of the following doses: 1, 3,
9, 20, 30, 45, 60, 80 and 100. The units in which doses are defined is
irrelevant to the operation of the CRM.

#### The dose-toxicity model

The trial uses a logistic log Normal dose toxicity model

``` math
 log(\frac{p_i}{1 - p_i}) = \alpha + \beta log(d_i / d^*) 
```

where the prior joint distribution of $`\alpha`$ and $`\beta`$ is

``` math
 
\begin{bmatrix}
\alpha \\
log(\beta)
\end{bmatrix}   \sim
N\begin{pmatrix}
\begin{bmatrix}
-0.85\\0
\end{bmatrix}   ,
\begin{bmatrix}
1 & -0.5 \\
-0.5 & 1
\end{bmatrix}   
\end{pmatrix}.
```

#### The increment rule

The maximum increment for doses greater than `0` and less than `20` is
100 x (1 + 1)%, or 200% of the highest dose used so far, whereas for
`20` or more, the maximum increment is 100 x (1 + 0.5)%, or 150% of the
highest dose used so far.

Note that a 2-fold *increment* corresponds to a 3-fold *escalation*.

#### The dose selection rule

Here, we choose to use Neuenschwander’s rule (Neuenschwander et al.
2008), in which the dose for the next cohort to be the dose (amongst
those doses that are eligible for selection according to the escalation
rule) that has the highest posterior chance of having a probability of
toxicity in the target range - here \[0.2, 0.35) - provided that the
dose’s chance of having a probability in the overdose range - here
\[0.35, 1.0\] - is less than 0.25.

#### The cohort size

Whilst the dose for the next cohort is 20 or less *and* no DLTs have
been observed, the minimum cohort size is 1. Otherwise, it is 3.

#### The stopping rule

The trial will stop when *either*

- Twenty patients have been recruited, or.
- Both of the following conditions are true
  - At least three cohorts must have been treated AND
  - The probability that the current estimate of the MTD is in the
    target toxicity range must be at least 0.5.

#### Trial definition

The code to define these elements of the trial design is given in the
[Trial
Definition](https://docs.crmpack.org/articles/trial_definition.Rmd)
vignette.

## Analysing a trial

Given the trial design constructed above, the process of analysing a
real life instance of the trial is simply a matter of providing the
model with the actual toxicity status of the participants treated so
far. The escalation rules we defined earlier allow the use of a single
patient run-in until either the first DLT is observed or until dose 20
has been administered.

### The single patient run-in

Assume that the first three patients - dosed at `1`, `3`, and `9` -
completed the trial without incident, but that the fourth patient -
treated at `20` - experienced a DLT.

We provide this information to `crmPack` via a `Data` object:

`firstFour`` ``<-`` `[`Data`](https://docs.crmpack.org/reference/Data-class.md)`(`` `` x ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``1``, ``3``, ``9``, ``20``)``,`` `` y ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0``, ``0``, ``0``, ``1``)``,`` `` ID ``=`` ``1``:``4``,`` `` cohort ``=`` ``1``:``4``,`` `` doseGrid ``=`` ``doseGrid`` ``)`

Within a `Data` object, the doses at which each patient is treated are
given by the `x` slot and their toxicity status (a Boolean where a
toxicity is represented by a truthy value) by the `y` slot.

The observed data is easily visualised

[`plot`](https://rdrr.io/r/graphics/plot.default.html)`(``firstFour``)`

![A visual representation of the data from the first four participants.
The first three, treated at doses 1, 3 and 9, do not report any
toxicities. The fourth, treated at 20,
does.](trial_analysis-figures/unnamed-chunk-5-1.png)

plot of chunk unnamed-chunk-5

and, since the `plot` method returns a `ggplot` object, it is easily
customised.

[`plot`](https://rdrr.io/r/graphics/plot.default.html)`(``firstFour``)`` ``+`` `[`theme_light`](https://ggplot2.tidyverse.org/reference/ggtheme.html)`(``)`

![The same graph as above, but with a white background to the plot area
rather than a grey one.](trial_analysis-figures/unnamed-chunk-6-1.png)

plot of chunk unnamed-chunk-6

We first define the MCMC options, explicitly setting a seed and the kind
for the random number generator, in order to make sure that the results
are reproducible:

`vignetteMcmcOptions`` ``<-`` `[`McmcOptions`](https://docs.crmpack.org/reference/McmcOptions-class.md)`(`` `` burnin ``=`` ``100``,`` `` step ``=`` ``2``,`` `` samples ``=`` ``1000``,`` `` rng_seed ``=`` ``321``,`` `` rng_kind ``=`` ``"Wichmann-Hill"`` ``)`

Note that in practice one would use larger numbers for `burnin` and
`samples` than those used here for the sake of saving computation time
on the CRAN checks.

Now, we can update the model to obtain the posterior estimate of the
dose-toxicity curve:

`postSamples`` ``<-`` `[`mcmc`](https://docs.crmpack.org/reference/mcmc.md)`(`` `` data ``=`` ``firstFour``,`` `` model ``=`` ``model``,`` `` options ``=`` ``vignetteMcmcOptions`` ``)`

The posterior estimate of the dose toxicity curve is easily visualised:

[`plot`](https://rdrr.io/r/graphics/plot.default.html)`(``postSamples``, ``model``, ``firstFour``)`

![A plot of the posterior after the first four participants. The mean
probability of toxicity increases smoothly, with a slight convex curve,
from about zero percent at a dose of zero to about 65% at a dose of 100.
The confidence interval extends from 0% to about 25% at a dose of zero
and from about 30% to about 90% at a dose of
100.](trial_analysis-figures/unnamed-chunk-9-1.png)

plot of chunk unnamed-chunk-9

A visual representation of the model’s state is obtained with:

[`nextBest`](https://docs.crmpack.org/reference/nextBest.md)`(`` `` ``my_next_best``,`` `` doselimit ``=`` ``100``,`` `` samples ``=`` ``postSamples``,`` `` model ``=`` ``model``,`` `` data ``=`` ``empty_data`` ``)``$``plot`

![Two graphs arranged in a single column. The upper graph shoes green
lines of various heights that show the probability each dose is in the
target toxicity range. There is a big arrow pointing to the bar at a
dose of 20, indicating tat this dose has the highest probability of
being in the target toxicity range. The lower graph as a similar series
of red lines, indicating the probability that each dose is in the
overdose range. There is a horizontal black dashed line at 25%,
indicating that this is the highest acceptable probability of being in
the overdose range. The red bars for doses of 30 and above all extend
above 25%, indicating that their toxicity is unacceptable. The toxicity
for doses of 20 and below lie below
25%.](trial_analysis-figures/unnamed-chunk-10-1.png)

plot of chunk unnamed-chunk-10

The lower panel of the plot shows the posterior probability that each
dose is in the overdose range. The dashed horizontal black line shows
the acceptable risk of overdose: Doses with red lines which go above
this line are considered toxic. The upper panel shows the probability
that each dose is in the target toxicity range. Clearly, doses of `30`
and `45` have the highest probability of being in the target toxicity
range. However, the risk that both are in the overdose range is
unacceptable. Therefore, `20` is the dose recommended for the next
cohort.

We can produce a tabulation of the model state with

`tabulatePosterior`` ``<-`` ``function``(``mcmcSamples``, ``observedData``)`` ``{`` `` `[`as_tibble`](https://tibble.tidyverse.org/reference/as_tibble.html)`(`` `` `[`nextBest`](https://docs.crmpack.org/reference/nextBest.md)`(`` `` ``my_next_best``,`` `` doselimit ``=`` ``100``,`` `` samples ``=`` ``mcmcSamples``,`` `` model ``=`` ``model``,`` `` data ``=`` ``observedData`` `` ``)``$``probs`` `` ``)`` `[`%>%`](https://docs.crmpack.org/reference/pipe.md)` `` `[`left_join`](https://dplyr.tidyverse.org/reference/mutate-joins.html)`(`` `` `[`tibble`](https://tibble.tidyverse.org/reference/tibble.html)`(`` `` dose ``=`` ``observedData``@``x``,`` `` WithDLT ``=`` ``observedData``@``y`` `` ``)`` `[`%>%`](https://docs.crmpack.org/reference/pipe.md)` `` `[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``dose``)`` `[`%>%`](https://docs.crmpack.org/reference/pipe.md)` `` `[`summarise`](https://dplyr.tidyverse.org/reference/summarise.html)`(`` `` Treated ``=`` `[`n`](https://dplyr.tidyverse.org/reference/context.html)`(``)``,`` `` WithDLT ``=`` `[`sum`](https://rdrr.io/r/base/sum.html)`(``WithDLT``)``,`` `` .groups ``=`` ``"drop"`` `` ``)``,`` `` by ``=`` ``"dose"`` `` ``)`` `[`%>%`](https://docs.crmpack.org/reference/pipe.md)` `` `[`replace_na`](https://tidyr.tidyverse.org/reference/replace_na.html)`(`[`list`](https://rdrr.io/r/base/list.html)`(``Treated ``=`` ``0``, WithDLT ``=`` ``0``)``)`` `[`%>%`](https://docs.crmpack.org/reference/pipe.md)` `` `[`select`](https://dplyr.tidyverse.org/reference/select.html)`(``dose``, ``Treated``, ``WithDLT``, ``target``, ``overdose``)`` `[`%>%`](https://docs.crmpack.org/reference/pipe.md)` `` ``kableExtra``::`[`kable`](https://rdrr.io/pkg/knitr/man/kable.html)`(`` `` col.names ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"Dose"``, ``"Treated"``, ``"With DLT"``, ``"Target range"``, ``"Overdose range"``)``,`` `` digits ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0``, ``0``, ``0``, ``3``, ``3``)`` `` ``)`` `[`%>%`](https://docs.crmpack.org/reference/pipe.md)` `` ``kableExtra``::`[`add_header_above`](https://rdrr.io/pkg/kableExtra/man/add_header_above.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``" "`` ``=`` ``1``, ``"Participants"`` ``=`` ``2``, ``"Probability that dose is in "`` ``=`` ``2``)``)`` ``}`` `` ``tabulatePosterior``(``postSamples``, ``firstFour``)`

[TABLE]

From these presentations, we can see that:

1.  The highest dose so far administered is `20`, so the escalation rule
    permits doses up to and including `40` to be considered as the dose
    for the next cohort. However…
2.  Doses of `30` and above are considered unsafe
3.  Of the remaining doses, `20` has the highest posterior probability
    of being in the target toxicity range
4.  A DLT has been reported

Items 1 and 4 in the list tell us both that the size of the next cohort
should be three. Items 2 and 3 together imply that the highest dose that
can be used in the next cohort is `20`.

`nextMaxDose`` ``<-`` `[`maxDose`](https://docs.crmpack.org/reference/maxDose.md)`(``my_increments``, ``firstFour``)`` ``nextMaxDose`` ``#> [1] 40`` `` ``doseRecommendation`` ``<-`` `[`nextBest`](https://docs.crmpack.org/reference/nextBest.md)`(`` `` ``my_next_best``,`` `` doselimit ``=`` ``nextMaxDose``,`` `` samples ``=`` ``postSamples``,`` `` model ``=`` ``model``,`` `` data ``=`` ``firstFour`` ``)`` ``doseRecommendation``$``value`` ``#> [1] 20`

Thus, the model’s recommendation is that the next cohort should consist
of three patients, each treated at `20`.

However, given that the probability that `20` is in the overdose range
is only just less than the threshold of 0.25 (and because the only
participant so far treated at `20` experienced a DLT) it would be a
perfectly reasonable clinical decision to treat the next cohort at
`10` - or, indeed, at any other dose below `20`. *There is absolutely no
obligation to follow the CRM dose recommendation without consideration
of other factors that might affect the choice of the most appropriate
dose for the next cohort.* However, for the purpose of exposition, we
will treat the next cohort at `20`, as recommended by the model.

We can confirm that the trial’s stopping rules have not been satisfied:

[`stopTrial`](https://docs.crmpack.org/reference/stopTrial.md)`(`` `` ``my_stopping``,`` `` dose ``=`` ``doseRecommendation``$``value``,`` `` ``postSamples``,`` `` ``model``,`` `` ``firstFour`` ``)`` ``#> [1] FALSE`` ``#> attr(,"message")`` ``#> attr(,"message")[[1]]`` ``#> attr(,"message")[[1]][[1]]`` ``#> [1] "Number of cohorts is 4 and thus reached the prespecified minimum number 3"`` ``#> `` ``#> attr(,"message")[[1]][[2]]`` ``#> [1] "Probability for target toxicity is 27 % for dose 20 and thus below the required 50 %"`` ``#> `` ``#> `` ``#> attr(,"message")[[2]]`` ``#> [1] "Number of patients is 4 and thus below the prespecified minimum number 20"`` ``#> `` ``#> attr(,"individual")`` ``#> attr(,"individual")[[1]]`` ``#> [1] FALSE`` ``#> attr(,"message")`` ``#> attr(,"message")[[1]]`` ``#> [1] "Number of cohorts is 4 and thus reached the prespecified minimum number 3"`` ``#> `` ``#> attr(,"message")[[2]]`` ``#> [1] "Probability for target toxicity is 27 % for dose 20 and thus below the required 50 %"`` ``#> `` ``#> attr(,"individual")`` ``#> attr(,"individual")[[1]]`` ``#> [1] TRUE`` ``#> attr(,"message")`` ``#> [1] "Number of cohorts is 4 and thus reached the prespecified minimum number 3"`` ``#> attr(,"report_label")`` ``#> [1] "≥ 3 cohorts dosed"`` ``#> `` ``#> attr(,"individual")[[2]]`` ``#> [1] FALSE`` ``#> attr(,"message")`` ``#> [1] "Probability for target toxicity is 27 % for dose 20 and thus below the required 50 %"`` ``#> attr(,"report_label")`` ``#> [1] "P(0.2 ≤ prob(DLE | NBD) ≤ 0.35) ≥ 0.5"`` ``#> `` ``#> attr(,"report_label")`` ``#> [1] NA`` ``#> `` ``#> attr(,"individual")[[2]]`` ``#> [1] FALSE`` ``#> attr(,"message")`` ``#> [1] "Number of patients is 4 and thus below the prespecified minimum number 20"`` ``#> attr(,"report_label")`` ``#> [1] "≥ 20 patients dosed"`` ``#> `` ``#> attr(,"report_label")`` ``#> [1] NA`

### The first full cohort

Assume that none of the three patients in the first full cohort report a
DLT:

`firstFullCohort`` ``<-`` `[`Data`](https://docs.crmpack.org/reference/Data-class.md)`(`` `` x ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``1``, ``3``, ``9``, ``20``, ``20``, ``20``, ``20``)``,`` `` y ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0``, ``0``, ``0``, ``1``, ``0``, ``0``, ``0``)``,`` `` ID ``=`` ``1``:``7``,`` `` cohort ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``1``:``4``, `[`rep`](https://rdrr.io/r/base/rep.html)`(``5``, ``3``)``)``,`` `` doseGrid ``=`` ``doseGrid`` ``)`

Update the model:

`postSamples1`` ``<-`` `[`mcmc`](https://docs.crmpack.org/reference/mcmc.md)`(`` `` data ``=`` ``firstFullCohort``,`` `` model ``=`` ``model``,`` `` options ``=`` ``vignetteMcmcOptions`` ``)`

Tabulate the posterior:

`tabulatePosterior``(``postSamples1``, ``firstFullCohort``)`

[TABLE]

Should the trial stop? If not, what dose should be used for the next
cohort?

`nextMaxDose`` ``<-`` `[`maxDose`](https://docs.crmpack.org/reference/maxDose.md)`(``my_increments``, ``firstFullCohort``)`` ``nextMaxDose`` ``#> [1] 40`` `` ``doseRecommendation`` ``<-`` `[`nextBest`](https://docs.crmpack.org/reference/nextBest.md)`(`` `` ``my_next_best``,`` `` doselimit ``=`` ``nextMaxDose``,`` `` samples ``=`` ``postSamples1``,`` `` model ``=`` ``model``,`` `` data ``=`` ``firstFullCohort`` ``)`` ``doseRecommendation``$``value`` ``#> [1] 30`` `` ``x`` ``<-`` `[`stopTrial`](https://docs.crmpack.org/reference/stopTrial.md)`(`` `` ``my_stopping``,`` `` dose ``=`` ``doseRecommendation``$``value``,`` `` ``postSamples1``,`` `` ``model``,`` `` ``firstFullCohort`` ``)`` `[`attributes`](https://rdrr.io/r/base/attributes.html)`(``x``)`` ``<-`` ``NULL`` ``x`` ``#> [1] FALSE`

So the trial should continue, treating three patients in the next cohort
at `30`.

### The second full cohort

Assume that none of the three patients in the next cohort report a DLT:

`secondFullCohort`` ``<-`` `[`Data`](https://docs.crmpack.org/reference/Data-class.md)`(`` `` x ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``1``, ``3``, ``9``, ``20``, ``20``, ``20``, ``20``, ``30``, ``30``, ``30``)``,`` `` y ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0``, ``0``, ``0``, ``1``, ``0``, ``0``, ``0``, ``0``, ``0``, ``0``)``,`` `` ID ``=`` ``1``:``10``,`` `` cohort ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``1``:``4``, `[`rep`](https://rdrr.io/r/base/rep.html)`(``5``, ``3``)``, `[`rep`](https://rdrr.io/r/base/rep.html)`(``6``, ``3``)``)``,`` `` doseGrid ``=`` ``doseGrid`` ``)`

Update the model:

`postSamples2`` ``<-`` `[`mcmc`](https://docs.crmpack.org/reference/mcmc.md)`(`` `` data ``=`` ``secondFullCohort``,`` `` model ``=`` ``model``,`` `` options ``=`` ``vignetteMcmcOptions`` ``)`

Tabulate the posterior:

`tabulatePosterior``(``postSamples2``, ``secondFullCohort``)`

[TABLE]

The dose with the highest posterior probability of being in the target
toxicity range is now `45`, but this dose almost has an unacceptably
high probability of being in the overdose range. So the team decides to
go for a next cohort at `30`:

`nextMaxDose`` ``<-`` `[`maxDose`](https://docs.crmpack.org/reference/maxDose.md)`(``my_increments``, ``secondFullCohort``)`` ``nextMaxDose`` ``#> [1] 45`` `` ``doseRecommendation`` ``<-`` `[`nextBest`](https://docs.crmpack.org/reference/nextBest.md)`(`` `` ``my_next_best``,`` `` doselimit ``=`` ``nextMaxDose``,`` `` samples ``=`` ``postSamples2``,`` `` model ``=`` ``model``,`` `` data ``=`` ``secondFullCohort`` ``)`` ``doseRecommendation``$``value`` ``#> [1] 45`` `` ``x`` ``<-`` `[`stopTrial`](https://docs.crmpack.org/reference/stopTrial.md)`(`` `` ``my_stopping``,`` `` dose ``=`` ``30``, ``# team decision.`` `` ``postSamples2``,`` `` ``model``,`` `` ``secondFullCohort`` ``)`` `[`attributes`](https://rdrr.io/r/base/attributes.html)`(``x``)`` ``<-`` ``NULL`` ``x`` ``#> [1] FALSE`

### The third full cohort

Assume that none of the three patients in the third cohort report a DLT:

`thirdFullCohort`` ``<-`` `[`Data`](https://docs.crmpack.org/reference/Data-class.md)`(`` `` x ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``1``, ``3``, ``9``, `[`rep`](https://rdrr.io/r/base/rep.html)`(``20``, ``4``)``, `[`rep`](https://rdrr.io/r/base/rep.html)`(``30``, ``6``)``)``,`` `` y ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0``, ``0``, ``0``, ``1``, `[`rep`](https://rdrr.io/r/base/rep.html)`(``0``, ``9``)``)``,`` `` ID ``=`` ``1``:``13``,`` `` cohort ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``1``:``4``, `[`rep`](https://rdrr.io/r/base/rep.html)`(``5``, ``3``)``, `[`rep`](https://rdrr.io/r/base/rep.html)`(``6``, ``3``)``, `[`rep`](https://rdrr.io/r/base/rep.html)`(``7``, ``3``)``)``,`` `` doseGrid ``=`` ``doseGrid`` ``)`

Update the model:

`postSamples3`` ``<-`` `[`mcmc`](https://docs.crmpack.org/reference/mcmc.md)`(`` `` data ``=`` ``thirdFullCohort``,`` `` model ``=`` ``model``,`` `` options ``=`` ``vignetteMcmcOptions`` ``)`

Tabulate the posterior:

`tabulatePosterior``(``postSamples3``, ``thirdFullCohort``)`

[TABLE]

`45` is still the dose with the highest posterior probability of being
in the target toxicity range, and its probability of being in the
overdose range is now acceptable. Therefore, the trial should continue
and the next cohort should be treated at `45`:

`nextMaxDose`` ``<-`` `[`maxDose`](https://docs.crmpack.org/reference/maxDose.md)`(``my_increments``, ``thirdFullCohort``)`` ``nextMaxDose`` ``#> [1] 45`` `` ``doseRecommendation`` ``<-`` `[`nextBest`](https://docs.crmpack.org/reference/nextBest.md)`(`` `` ``my_next_best``,`` `` doselimit ``=`` ``nextMaxDose``,`` `` samples ``=`` ``postSamples3``,`` `` model ``=`` ``model``,`` `` data ``=`` ``thirdFullCohort`` ``)`` ``doseRecommendation``$``value`` ``#> [1] 45`` `` ``x`` ``<-`` `[`stopTrial`](https://docs.crmpack.org/reference/stopTrial.md)`(`` `` ``my_stopping``,`` `` dose ``=`` ``doseRecommendation``$``value``,`` `` ``postSamples3``,`` `` ``model``,`` `` ``thirdFullCohort`` ``)`` `[`attributes`](https://rdrr.io/r/base/attributes.html)`(``x``)`` ``<-`` ``NULL`` ``x`` ``#> [1] FALSE`

### The fourth full cohort

Assume that none of the three patients in the fourth cohort report a
DLT:

`fourthFullCohort`` ``<-`` `[`Data`](https://docs.crmpack.org/reference/Data-class.md)`(`` `` x ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``1``, ``3``, ``9``, `[`rep`](https://rdrr.io/r/base/rep.html)`(``20``, ``4``)``, `[`rep`](https://rdrr.io/r/base/rep.html)`(``30``, ``6``)``, `[`rep`](https://rdrr.io/r/base/rep.html)`(``45``, ``3``)``)``,`` `` y ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0``, ``0``, ``0``, ``1``, `[`rep`](https://rdrr.io/r/base/rep.html)`(``0``, ``12``)``)``,`` `` ID ``=`` ``1``:``16``,`` `` cohort ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``1``:``4``, `[`rep`](https://rdrr.io/r/base/rep.html)`(``5``:``8``, each ``=`` ``3``)``)``,`` `` doseGrid ``=`` ``doseGrid`` ``)`

Update the model:

`postSamples4`` ``<-`` `[`mcmc`](https://docs.crmpack.org/reference/mcmc.md)`(`` `` data ``=`` ``fourthFullCohort``,`` `` model ``=`` ``model``,`` `` options ``=`` ``vignetteMcmcOptions`` ``)`

Tabulate the posterior:

`tabulatePosterior``(``postSamples4``, ``fourthFullCohort``)`

[TABLE]

`60` is now the dose with the highest posterior probability of being in
the target toxicity range, but its probability of being in the overdose
range is unacceptable. Therefore, the trial should continue and the next
cohort should be treated at `45`:

`nextMaxDose`` ``<-`` `[`maxDose`](https://docs.crmpack.org/reference/maxDose.md)`(``my_increments``, ``fourthFullCohort``)`` ``nextMaxDose`` ``#> [1] 67.5`` `` ``doseRecommendation`` ``<-`` `[`nextBest`](https://docs.crmpack.org/reference/nextBest.md)`(`` `` ``my_next_best``,`` `` doselimit ``=`` ``nextMaxDose``,`` `` samples ``=`` ``postSamples4``,`` `` model ``=`` ``model``,`` `` data ``=`` ``fourthFullCohort`` ``)`` ``doseRecommendation``$``value`` ``#> [1] 45`` `` ``x`` ``<-`` `[`stopTrial`](https://docs.crmpack.org/reference/stopTrial.md)`(`` `` ``my_stopping``,`` `` dose ``=`` ``doseRecommendation``$``value``,`` `` ``postSamples4``,`` `` ``model``,`` `` ``fourthFullCohort`` ``)`` `[`attributes`](https://rdrr.io/r/base/attributes.html)`(``x``)`` ``<-`` ``NULL`` ``x`` ``#> [1] FALSE`

### The fifth full cohort

Assume that two of the three patients in the fourth cohort report a DLT:

`fifthFullCohort`` ``<-`` `[`Data`](https://docs.crmpack.org/reference/Data-class.md)`(`` `` x ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``1``, ``3``, ``9``, `[`rep`](https://rdrr.io/r/base/rep.html)`(``20``, ``4``)``, `[`rep`](https://rdrr.io/r/base/rep.html)`(``30``, ``6``)``, `[`rep`](https://rdrr.io/r/base/rep.html)`(``45``, ``6``)``)``,`` `` y ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0``, ``0``, ``0``, ``1``, `[`rep`](https://rdrr.io/r/base/rep.html)`(``0``, ``13``)``, ``1``, ``1``)``,`` `` ID ``=`` ``1``:``19``,`` `` cohort ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``1``:``4``, `[`rep`](https://rdrr.io/r/base/rep.html)`(``5``:``9``, each ``=`` ``3``)``)``,`` `` doseGrid ``=`` ``doseGrid`` ``)`

Update the model:

`postSamples5`` ``<-`` `[`mcmc`](https://docs.crmpack.org/reference/mcmc.md)`(`` `` data ``=`` ``fifthFullCohort``,`` `` model ``=`` ``model``,`` `` options ``=`` ``vignetteMcmcOptions`` ``)`

Tabulate the posterior:

`tabulatePosterior``(``postSamples5``, ``fifthFullCohort``)`

[TABLE]

`45` remains the dose with the highest posterior probability of being in
the target toxicity range, and its probability of being in the overdose
range is acceptable. Moreover, the probability that `45` is in the
target toxicity range is above 0.5 and more than three cohorts have been
treated in total. Therefore, the trial should stop and conclude that
`45` is the MTD:

`nextMaxDose`` ``<-`` `[`maxDose`](https://docs.crmpack.org/reference/maxDose.md)`(``my_increments``, ``fifthFullCohort``)`` ``nextMaxDose`` ``#> [1] 67.5`` `` ``doseRecommendation`` ``<-`` `[`nextBest`](https://docs.crmpack.org/reference/nextBest.md)`(`` `` ``my_next_best``,`` `` doselimit ``=`` ``nextMaxDose``,`` `` samples ``=`` ``postSamples5``,`` `` model ``=`` ``model``,`` `` data ``=`` ``fifthFullCohort`` ``)`` ``doseRecommendation``$``value`` ``#> [1] 45`` `` ``x`` ``<-`` `[`stopTrial`](https://docs.crmpack.org/reference/stopTrial.md)`(`` `` ``my_stopping``,`` `` dose ``=`` ``doseRecommendation``$``value``,`` `` ``postSamples5``,`` `` ``model``,`` `` ``fifthFullCohort`` ``)`` ``x`` ``#> [1] TRUE`` ``#> attr(,"message")`` ``#> attr(,"message")[[1]]`` ``#> attr(,"message")[[1]][[1]]`` ``#> [1] "Number of cohorts is 9 and thus reached the prespecified minimum number 3"`` ``#> `` ``#> attr(,"message")[[1]][[2]]`` ``#> [1] "Probability for target toxicity is 53 % for dose 45 and thus above the required 50 %"`` ``#> `` ``#> `` ``#> attr(,"message")[[2]]`` ``#> [1] "Number of patients is 19 and thus below the prespecified minimum number 20"`` ``#> `` ``#> attr(,"individual")`` ``#> attr(,"individual")[[1]]`` ``#> [1] TRUE`` ``#> attr(,"message")`` ``#> attr(,"message")[[1]]`` ``#> [1] "Number of cohorts is 9 and thus reached the prespecified minimum number 3"`` ``#> `` ``#> attr(,"message")[[2]]`` ``#> [1] "Probability for target toxicity is 53 % for dose 45 and thus above the required 50 %"`` ``#> `` ``#> attr(,"individual")`` ``#> attr(,"individual")[[1]]`` ``#> [1] TRUE`` ``#> attr(,"message")`` ``#> [1] "Number of cohorts is 9 and thus reached the prespecified minimum number 3"`` ``#> attr(,"report_label")`` ``#> [1] "≥ 3 cohorts dosed"`` ``#> `` ``#> attr(,"individual")[[2]]`` ``#> [1] TRUE`` ``#> attr(,"message")`` ``#> [1] "Probability for target toxicity is 53 % for dose 45 and thus above the required 50 %"`` ``#> attr(,"report_label")`` ``#> [1] "P(0.2 ≤ prob(DLE | NBD) ≤ 0.35) ≥ 0.5"`` ``#> `` ``#> attr(,"report_label")`` ``#> [1] NA`` ``#> `` ``#> attr(,"individual")[[2]]`` ``#> [1] FALSE`` ``#> attr(,"message")`` ``#> [1] "Number of patients is 19 and thus below the prespecified minimum number 20"`` ``#> attr(,"report_label")`` ``#> [1] "≥ 20 patients dosed"`` ``#> `` ``#> attr(,"report_label")`` ``#> [1] NA`

## Summarising the trial results

crmPack provides a wealth of information about the trial’s results. The
following code snippets illustrate some of the many possibilities for
how the trial might be summarised.

[`plot`](https://rdrr.io/r/graphics/plot.default.html)`(``fifthFullCohort``)`

![A visual representation of the data after nineteen participants have
been treated. One each at doses 1, 3 and 9; four at a dose of 20; 6 at a
dose of 30 and 6 at a dose of 45. Toxicitiues were reported by
participants 4 (at a dose of 20) and 18 and 19 (both at a dose of
45).](trial_analysis-figures/unnamed-chunk-34-1.png)

plot of chunk unnamed-chunk-34

[`plot`](https://rdrr.io/r/graphics/plot.default.html)`(``postSamples5``, ``model``, ``fifthFullCohort``)`

![A plot of the posterior after nineteen participants have been treated.
The mean probability of toxicity increases smoothly from about zero
percent at a dose of zero to about 55% at a dose of 100. The confidence
interval extends from 0% to about 6% at a dose of zero and from about
22% to about 90% at a dose of
100.](trial_analysis-figures/unnamed-chunk-35-1.png)

plot of chunk unnamed-chunk-35

`doseRecommendation``$``plot`

![Two graphs arranged in a single column. The upper graph shoes green
lines of various heights that show the probability each dose is in the
target toxicity range. There is a big arrow pointing to the bar at a
dose of 45, indicating that this dose has the highest probability of
being in the target toxicity range. The lower graph as a similar series
of red lines, indicating the probability that each dose is in the
overdose range. There is a horizontal black dashed line at 25%,
indicating that this is the highest acceptable probability of being in
the overdose range. The red bars for doses of 60 and above all extend
above 25%, indicating that their toxicity is unacceptable. The toxicity
for doses of 45 and below lie below
25%.](trial_analysis-figures/unnamed-chunk-36-1.png)

plot of chunk unnamed-chunk-36

With a little bit of work, we can obtain a more detailed summary and
plot of the posterior probabilities of toxicity at each dose:

`slotNames``(``model``)`` ``#> [1] "params" "ref_dose" "datamodel" "priormodel" `` ``#> [5] "modelspecs" "init" "datanames" "datanames_prior"`` ``#> [9] "sample"`` `` ``fullSamples`` ``<-`` `[`tibble`](https://tibble.tidyverse.org/reference/tibble.html)`(`` `` Alpha ``=`` ``postSamples5``@``data``$``alpha0``,`` `` Beta ``=`` ``postSamples5``@``data``$``alpha1`` ``)`` `[`%>%`](https://docs.crmpack.org/reference/pipe.md)` `` `[`expand`](https://tidyr.tidyverse.org/reference/expand.html)`(`[`nesting`](https://tidyr.tidyverse.org/reference/expand.html)`(``Alpha``, ``Beta``)``, Dose ``=`` ``doseGrid``)`` `[`%>%`](https://docs.crmpack.org/reference/pipe.md)` `` `[`rowwise`](https://dplyr.tidyverse.org/reference/rowwise.html)`(``)`` `[`%>%`](https://docs.crmpack.org/reference/pipe.md)` `` `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``P ``=`` `[`probFunction`](https://docs.crmpack.org/reference/probFunction.md)`(``model``, alpha0 ``=`` ``Alpha``, alpha1 ``=`` ``Beta``)``(``dose ``=`` ``Dose``)``)`` `[`%>%`](https://docs.crmpack.org/reference/pipe.md)` `` `[`ungroup`](https://dplyr.tidyverse.org/reference/group_by.html)`(``)`` `` ``fullSummary`` ``<-`` ``fullSamples`` `[`%>%`](https://docs.crmpack.org/reference/pipe.md)` `` `[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``Dose``)`` `[`%>%`](https://docs.crmpack.org/reference/pipe.md)` `` `[`summarise`](https://dplyr.tidyverse.org/reference/summarise.html)`(`` `` Mean ``=`` `[`mean`](https://rdrr.io/r/base/mean.html)`(``P``)``,`` `` Median ``=`` `[`median`](https://rdrr.io/r/stats/median.html)`(``P``)``,`` `` Q ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`[`quantile`](https://rdrr.io/r/stats/quantile.html)`(``P``, probs ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0.05``, ``0.1``, ``0.25``, ``0.75``, ``0.9``, ``0.95``)``, na.rm ``=`` ``TRUE``)``)`` `` ``)`` `[`%>%`](https://docs.crmpack.org/reference/pipe.md)` `` `[`unnest_wider`](https://tidyr.tidyverse.org/reference/unnest_wider.html)`(`` `` col ``=`` ``Q``,`` `` names_repair ``=`` ``function``(``.x``)`` ``{`` `` `[`ifelse`](https://rdrr.io/r/base/ifelse.html)`(`` `` ``str_detect``(``.x``, ``"\\d+%"``)``,`` `` `[`sprintf`](https://rdrr.io/r/base/sprintf.html)`(``"Q%02.0f"``, `[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(``str_remove_all``(``.x``, ``"%"``)``)``)``,`` `` ``.x`` `` ``)`` `` ``}`` `` ``)`` `` ``fullSummary`` `[`%>%`](https://docs.crmpack.org/reference/pipe.md)` `` ``kableExtra``::`[`kable`](https://rdrr.io/pkg/knitr/man/kable.html)`(`` `` col.names ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"Dose"``, ``"Mean"``, ``"Median"``, ``"5th"``, ``"10th"``, ``"25th"``, ``"75th"``, ``"90th"``, ``"95th"``)``,`` `` digits ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0``, `[`rep`](https://rdrr.io/r/base/rep.html)`(``3``, ``8``)``)`` `` ``)`` `[`%>%`](https://docs.crmpack.org/reference/pipe.md)` `` `[`add_header_above`](https://rdrr.io/pkg/kableExtra/man/add_header_above.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``" "`` ``=`` ``3``, ``"Quantiles"`` ``=`` ``6``)``)`` `[`%>%`](https://docs.crmpack.org/reference/pipe.md)` `` `[`add_header_above`](https://rdrr.io/pkg/kableExtra/man/add_header_above.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``" "`` ``=`` ``1``, ``"P(Toxicity)"`` ``=`` ``8``)``)`

[TABLE]

` ``fullSamples`` `[`%>%`](https://docs.crmpack.org/reference/pipe.md)` `` `[`filter`](https://dplyr.tidyverse.org/reference/filter.html)`(``Dose`` ``>`` ``9``)`` `[`%>%`](https://docs.crmpack.org/reference/pipe.md)` `` `[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)`(``)`` ``+`` `` `[`geom_density`](https://ggplot2.tidyverse.org/reference/geom_density.html)`(`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``x ``=`` ``P``, color ``=`` `[`as.factor`](https://rdrr.io/r/base/factor.html)`(``Dose``)``)``)`` ``+`` `` `[`theme_light`](https://ggplot2.tidyverse.org/reference/ggtheme.html)`(``)`` ``+`` `` `[`theme`](https://ggplot2.tidyverse.org/reference/theme.html)`(`` `` axis.text.y ``=`` `[`element_blank`](https://ggplot2.tidyverse.org/reference/element.html)`(``)``,`` `` axis.title.y ``=`` `[`element_blank`](https://ggplot2.tidyverse.org/reference/element.html)`(``)``,`` `` axis.ticks.y ``=`` `[`element_blank`](https://ggplot2.tidyverse.org/reference/element.html)`(``)`` `` ``)`` ``+`` `` `[`labs`](https://ggplot2.tidyverse.org/reference/labs.html)`(`` `` title ``=`` ``"Posterior PDFs for doses > 9"``,`` `` colour ``=`` ``"Dose"`` `` ``)`

![A graph showing the posterior density of of the probability of
toxicity for all doses greater than nine. The mode of each density moves
to the right as dose increases. The densities for low doses are heaviliy
skewed to the left. Densities for higher doses are more symmetric and
flatter.](trial_analysis-figures/unnamed-chunk-37-1.png)

plot of chunk unnamed-chunk-37

`fullSummary`` `[`%>%`](https://docs.crmpack.org/reference/pipe.md)` `` `[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)`(`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``x ``=`` ``Dose``)``)`` ``+`` `` `[`geom_ribbon`](https://ggplot2.tidyverse.org/reference/geom_ribbon.html)`(`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``ymin ``=`` ``Q05``, ymax ``=`` ``Q95``)``, fill ``=`` ``"steelblue"``, alpha ``=`` ``0.25``)`` ``+`` `` `[`geom_ribbon`](https://ggplot2.tidyverse.org/reference/geom_ribbon.html)`(`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``ymin ``=`` ``Q10``, ymax ``=`` ``Q90``)``, fill ``=`` ``"steelblue"``, alpha ``=`` ``0.25``)`` ``+`` `` `[`geom_ribbon`](https://ggplot2.tidyverse.org/reference/geom_ribbon.html)`(`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``ymin ``=`` ``Q25``, ymax ``=`` ``Q75``)``, fill ``=`` ``"steelblue"``, alpha ``=`` ``0.25``)`` ``+`` `` `[`geom_line`](https://ggplot2.tidyverse.org/reference/geom_path.html)`(`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``y ``=`` ``Mean``)``, colour ``=`` ``"black"``)`` ``+`` `` `[`geom_line`](https://ggplot2.tidyverse.org/reference/geom_path.html)`(`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``y ``=`` ``Median``)``, colour ``=`` ``"blue"``)`` ``+`` `` `[`theme_light`](https://ggplot2.tidyverse.org/reference/ggtheme.html)`(``)`` ``+`` `` `[`labs`](https://ggplot2.tidyverse.org/reference/labs.html)`(`` `` title ``=`` ``"Posterior Dose toxicity curve"``,`` `` colour ``=`` ``"Dose"``,`` `` y ``=`` ``"P(Toxicity)"`` `` ``)`

![A visual representation of the posterior dose - toxicity curve. Very
closely spaced solid lines in black and blue, representing the mean and
median estimate of toxicity for each dose rise almost linearly from zero
percent for a dose of zero to about 55% for a dose of 100. Shading
extends to each side of the two solid lines. The transparency of the
shading increases with distance from the solid lines. The shading is
funnel shaped, with a narrow mneck at a dose of 100 and a wider mouth at
a dose of 100. The shading represents the central 90%, 80% and 50%
confidence intervals for the posterior mean estimate of toxicity at each
dose.](trial_analysis-figures/unnamed-chunk-38-1.png)

plot of chunk unnamed-chunk-38

## Note

The analyses presented in this vignette have used chains of a very short
length. This is purely for convenience. Analyses of real trials should
use considerably longer chains. As an example, an effective sample size
of approximately 40,000 is required to estimate a percentage to within
±1%.

## References

Neuenschwander, Beat, Michael Branson, and Thomas Gsponer. 2008.
“Critical aspects of the Bayesian approach to phase I cancer trials.”
*Statistics in Medicine* 27 (13): 2420–39.
<https://onlinelibrary.wiley.com/doi/10.1002/sim.3230>.
