# Simulation study example

## Simulation setting

Here the simulation study setting is defined.

\
`id`` ``<-`` ``1`\
`onset`` ``<-`` ``3`\
`a0`` ``<-`` ``2`\
`a1`` ``<-`` ``3`\
`refDose`` ``<-`` ``56`\
\
`# True dose-DLT relationship`\
`myTruth`` ``<-`` ``function``(``dose``)`` ``{`\
`  ``StandLogDose`` ``<-`` `[`log`](https://rdrr.io/r/base/Log.html)`(``dose`` ``/`` ``refDose``)`\
`  `[`plogis`](https://rdrr.io/r/stats/Logistic.html)`(``a0`` ``+`` ``a1`` ``*`` ``StandLogDose``)`\
`}`\
\
`# The conditional CDF of the PEM`\
`if`` ``(``onset`` ``==`` ``30``)`` ``{`\
`  ``onset`` ``<-`` ``15`\
`  ``exp_cond_cdf`` ``<-`` ``function``(``x``)`` ``{`\
`    ``(`[`pexp`](https://rdrr.io/r/stats/Exponential.html)`(``42`` ``-`` ``x``, ``1`` ``/`` ``onset``, lower.tail ``=`` ``FALSE``)`` ``-`` `[`pexp`](https://rdrr.io/r/stats/Exponential.html)`(``t_max``, ``1`` ``/`` ``onset``, lower.tail ``=`` ``FALSE``)``)`` ``/`` `[`pexp`](https://rdrr.io/r/stats/Exponential.html)`(``t_max``, ``1`` ``/`` ``onset``)`\
`  ``}`\
`}`` ``else`` ``{`\
`  ``exp_cond_cdf`` ``<-`` ``function``(``x``)`` ``{`\
`    ``1`` ``-`` ``(`[`pexp`](https://rdrr.io/r/stats/Exponential.html)`(``x``, ``1`` ``/`` ``onset``, lower.tail ``=`` ``FALSE``)`` ``-`` `[`pexp`](https://rdrr.io/r/stats/Exponential.html)`(``t_max``, ``1`` ``/`` ``onset``, lower.tail ``=`` ``FALSE``)``)`` ``/`` `[`pexp`](https://rdrr.io/r/stats/Exponential.html)`(``t_max``, ``1`` ``/`` ``onset``)`\
`  ``}`\
`}`

## Design definition

Here the the dose escalation designs are defined: in this example the
TITE-CRM is used. Similarly the code can be adapted for the rolling-CRM
which is implemented in `DALogisticLogNormal`. Note that another
alternative is `TITELogisticLogNormalSub` which is a submodel of
`LogisticLogNormalSub`, which uses again the subtraction of the
reference dose from the dose level in the regression model.

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`crmPack`](https://docs.crmpack.org/)`)`\
`t_max`` ``<-`` ``42`\
\
`model`` ``<-`` `[`TITELogisticLogNormal`](https://docs.crmpack.org/reference/TITELogisticLogNormal-class.md)`(`\
`  mean ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``1.33``, ``1.49``)``,`\
`  cov ``=`` `[`matrix`](https://rdrr.io/r/base/matrix.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``1.826``, ``0.0209``, ``0.0209``, ``0.0245``)``, nrow ``=`` ``2``)``,`\
`  ref_dose ``=`` ``refDose`\
`)`\
\
`myIncrements`` ``<-`` `[`IncrementsRelative`](https://docs.crmpack.org/reference/IncrementsRelative-class.md)`(`\
`  intervals ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0``, ``20``)``,`\
`  increments ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``10``, ``3``)`\
`)`\
\
`myNextBest`` ``<-`` `[`NextBestMTD`](https://docs.crmpack.org/reference/NextBestMTD-class.md)`(`\
`  target ``=`` ``0.3``,`\
`  derive ``=`\
`    ``function``(``mtd_samples``)`` ``{`\
`      `[`mean`](https://rdrr.io/r/base/mean.html)`(``mtd_samples``)`\
`    ``}`\
`)`\
\
`myStopping`` ``<-`` `[`StoppingMinPatients`](https://docs.crmpack.org/reference/StoppingMinPatients-class.md)`(``nPatients ``=`` ``48``)`\
\
`mySize`` ``<-`` `[`CohortSizeConst`](https://docs.crmpack.org/reference/CohortSizeConst-class.md)`(``size ``=`` ``3``)`\
\
`emptydata`` ``<-`` `[`DataDA`](https://docs.crmpack.org/reference/DataDA-class.md)`(``doseGrid ``=`` `[`seq`](https://rdrr.io/r/base/seq.html)`(``from ``=`` ``2``, to ``=`` ``50``, by ``=`` ``2``)``, Tmax ``=`` ``t_max``)`\
\
`mysafetywindow`` ``<-`` `[`SafetyWindowConst`](https://docs.crmpack.org/reference/SafetyWindowConst-class.md)`(`[`c`](https://rdrr.io/r/base/c.html)`(``7``, ``7``)``, ``7``, ``7``)`\
\
`design`` ``<-`` `[`DADesign`](https://docs.crmpack.org/reference/DADesign-class.md)`(`\
`  model ``=`` ``model``,`\
`  increments ``=`` ``myIncrements``,`\
`  nextBest ``=`` ``myNextBest``,`\
`  stopping ``=`` ``myStopping``,`\
`  cohort_size ``=`` ``mySize``,`\
`  data ``=`` ``emptydata``,`\
`  safetyWindow ``=`` ``mysafetywindow``,`\
`  startingDose ``=`` ``8`\
`)`

## Simulation run

In order to obtain stable results, increase the simulation parameters
appropriately (step, samples, nsim).

\
`options`` ``<-`` `[`McmcOptions`](https://docs.crmpack.org/reference/McmcOptions-class.md)`(`\
`  burnin ``=`` ``20``,`\
`  step ``=`` ``1``,`\
`  samples ``=`` ``50`\
`)`\
`mySims`` ``<-`` `[`simulate`](https://rdrr.io/r/stats/simulate.html)`(``design``,`\
`  args ``=`` ``NULL``,`\
`  truthTox ``=`` ``myTruth``,`\
`  truthSurv ``=`` ``exp_cond_cdf``,`\
`  trueTmax ``=`` ``42``,`\
`  nsim ``=`` ``10``,`\
`  seed ``=`` ``819``,`\
`  mcmcOptions ``=`` ``options``,`\
`  parallel ``=`` ``FALSE`\
`)`

Alternatively we can also use
[`examine()`](https://docs.crmpack.org/reference/examine.md) to look at
possible trial outcomes. Each cohort is reached assuming no additional
DLTs in earlier cohorts. `DLTs` is the hypothetical number of additional
DLTs among the current cohort and patients from earlier cohorts whose
DLT windows are still open at the next decision. Thus it can exceed the
cohort size.

`DLT_scenario` identifies the allocation scenario:
`"no additional DLTs"`, `"late DLTs"` (earlier-enrolled eligible
patients at their available follow-up times), or `"early DLTs"`
(later-enrolled eligible patients with early event times). See
[`?examine`](https://docs.crmpack.org/reference/examine.md) for the
timing details.

`cohort` indexes the current cohort, continuing any indices already
present in the design data. `dose` is that cohort’s dose. `DLT_cohorts`
lists the cohort indices of the additional hypothetical DLTs, separated
by commas and sorted. For example, `"1, 2, 2"` means one DLT in cohort 1
and two in cohort 2; `""` means no additional DLTs. `DLT_time` lists
their corresponding onset times, relative to the start of each patient’s
cohort, in the same order. Previously observed DLTs are excluded from
both columns.

Each hypothetical scenario requires an MCMC fit. `maxNoIncrement` limits
the number of consecutive unchanged dose recommendations on the zero-DLT
path (default: 100); reaching this limit produces a warning.

\
[`set.seed`](https://rdrr.io/r/base/Random.html)`(``321``)`\
`examineTable`` ``<-`` `[`examine`](https://docs.crmpack.org/reference/examine.md)`(`\
`  ``design``,`\
`  mcmcOptions ``=`` ``options`\
`)`\
`examineTable`\
`#>    cohort       DLT_scenario dose DLTs   DLT_cohorts           DLT_time`\
`#> 1       1 no additional DLTs    8    0                                 `\
`#> 2       1          late DLTs    8    1             1                 21`\
`#> 3       1         early DLTs    8    1             1                 15`\
`#> 4       1          late DLTs    8    2          1, 1             21, 21`\
`#> 5       1         early DLTs    8    2          1, 1              8, 15`\
`#> 6       1          late DLTs    8    3       1, 1, 1         21, 21, 21`\
`#> 7       1         early DLTs    8    3       1, 1, 1           1, 8, 15`\
`#> 8       2 no additional DLTs   36    0                                 `\
`#> 9       2          late DLTs   36    1             1                 42`\
`#> 10      2         early DLTs   36    1             2                 15`\
`#> 11      2          late DLTs   36    2          1, 1             42, 42`\
`#> 12      2         early DLTs   36    2          2, 2              8, 15`\
`#> 13      2          late DLTs   36    3       1, 1, 2         42, 42, 21`\
`#> 14      2         early DLTs   36    3       2, 2, 2           1, 8, 15`\
`#> 15      2          late DLTs   36    4    1, 1, 2, 2     42, 42, 21, 21`\
`#> 16      2         early DLTs   36    4    1, 2, 2, 2       22, 1, 8, 15`\
`#> 17      2          late DLTs   36    5 1, 1, 2, 2, 2 42, 42, 21, 21, 21`\
`#> 18      2         early DLTs   36    5 1, 1, 2, 2, 2   22, 22, 1, 8, 15`\
`#>    nextDose  stop increment`\
`#> 1        36 FALSE       350`\
`#> 2        48 FALSE       500`\
`#> 3        38 FALSE       375`\
`#> 4        20 FALSE       150`\
`#> 5        14 FALSE        75`\
`#> 6        12 FALSE        50`\
`#> 7        10 FALSE        25`\
`#> 8        50 FALSE        39`\
`#> 9        32 FALSE       -11`\
`#> 10       24 FALSE       -33`\
`#> 11       30 FALSE       -17`\
`#> 12       28 FALSE       -22`\
`#> 13       16 FALSE       -56`\
`#> 14       22 FALSE       -39`\
`#> 15       14 FALSE       -61`\
`#> 16       26 FALSE       -28`\
`#> 17       16 FALSE       -56`\
`#> 18       14 FALSE       -61`
