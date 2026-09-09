# Simulation study example

## Simulation setting

Here the simulation study setting is defined.

`id`` ``<-`` ``1`` ``onset`` ``<-`` ``3`` ``a0`` ``<-`` ``2`` ``a1`` ``<-`` ``3`` ``refDose`` ``<-`` ``56`` `` ``# True dose-DLT relationship`` ``myTruth`` ``<-`` ``function``(``dose``)`` ``{`` `` ``StandLogDose`` ``<-`` `[`log`](https://rdrr.io/r/base/Log.html)`(``dose`` ``/`` ``refDose``)`` `` `[`plogis`](https://rdrr.io/r/stats/Logistic.html)`(``a0`` ``+`` ``a1`` ``*`` ``StandLogDose``)`` ``}`` `` ``# The conditional CDF of the PEM`` ``if`` ``(``onset`` ``==`` ``30``)`` ``{`` `` ``onset`` ``<-`` ``15`` `` ``exp_cond_cdf`` ``<-`` ``function``(``x``)`` ``{`` `` ``(`[`pexp`](https://rdrr.io/r/stats/Exponential.html)`(``42`` ``-`` ``x``, ``1`` ``/`` ``onset``, lower.tail ``=`` ``FALSE``)`` ``-`` `[`pexp`](https://rdrr.io/r/stats/Exponential.html)`(``t_max``, ``1`` ``/`` ``onset``, lower.tail ``=`` ``FALSE``)``)`` ``/`` `[`pexp`](https://rdrr.io/r/stats/Exponential.html)`(``t_max``, ``1`` ``/`` ``onset``)`` `` ``}`` ``}`` ``else`` ``{`` `` ``exp_cond_cdf`` ``<-`` ``function``(``x``)`` ``{`` `` ``1`` ``-`` ``(`[`pexp`](https://rdrr.io/r/stats/Exponential.html)`(``x``, ``1`` ``/`` ``onset``, lower.tail ``=`` ``FALSE``)`` ``-`` `[`pexp`](https://rdrr.io/r/stats/Exponential.html)`(``t_max``, ``1`` ``/`` ``onset``, lower.tail ``=`` ``FALSE``)``)`` ``/`` `[`pexp`](https://rdrr.io/r/stats/Exponential.html)`(``t_max``, ``1`` ``/`` ``onset``)`` `` ``}`` ``}`

## Design definition

Here the the dose escalation designs are defined: in this example the
TITE-CRM is used. Similarly the code can be adapted for the rolling-CRM
which is implemented in `DALogisticLogNormal`. Note that another
alternative is `TITELogisticLogNormalSub` which is a submodel of
`LogisticLogNormalSub`, which uses again the subtraction of the
reference dose from the dose level in the regression model.

[`library`](https://rdrr.io/r/base/library.html)`(`[`crmPack`](https://docs.crmpack.org/)`)`` ``t_max`` ``<-`` ``42`` `` ``model`` ``<-`` `[`TITELogisticLogNormal`](https://docs.crmpack.org/reference/TITELogisticLogNormal-class.md)`(`` `` mean ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``1.33``, ``1.49``)``,`` `` cov ``=`` `[`matrix`](https://rdrr.io/r/base/matrix.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``1.826``, ``0.0209``, ``0.0209``, ``0.0245``)``, nrow ``=`` ``2``)``,`` `` ref_dose ``=`` ``refDose`` ``)`` `` ``myIncrements`` ``<-`` `[`IncrementsRelative`](https://docs.crmpack.org/reference/IncrementsRelative-class.md)`(`` `` intervals ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0``, ``20``)``,`` `` increments ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``10``, ``3``)`` ``)`` `` ``myNextBest`` ``<-`` `[`NextBestMTD`](https://docs.crmpack.org/reference/NextBestMTD-class.md)`(`` `` target ``=`` ``0.3``,`` `` derive ``=`` `` ``function``(``mtd_samples``)`` ``{`` `` `[`mean`](https://rdrr.io/r/base/mean.html)`(``mtd_samples``)`` `` ``}`` ``)`` `` ``myStopping`` ``<-`` `[`StoppingMinPatients`](https://docs.crmpack.org/reference/StoppingMinPatients-class.md)`(``nPatients ``=`` ``48``)`` `` ``mySize`` ``<-`` `[`CohortSizeConst`](https://docs.crmpack.org/reference/CohortSizeConst-class.md)`(``size ``=`` ``3``)`` `` ``emptydata`` ``<-`` `[`DataDA`](https://docs.crmpack.org/reference/DataDA-class.md)`(``doseGrid ``=`` `[`seq`](https://rdrr.io/r/base/seq.html)`(``from ``=`` ``2``, to ``=`` ``50``, by ``=`` ``2``)``, Tmax ``=`` ``t_max``)`` `` ``mysafetywindow`` ``<-`` `[`SafetyWindowConst`](https://docs.crmpack.org/reference/SafetyWindowConst-class.md)`(`[`c`](https://rdrr.io/r/base/c.html)`(``7``, ``7``)``, ``7``, ``7``)`` `` ``design`` ``<-`` `[`DADesign`](https://docs.crmpack.org/reference/DADesign-class.md)`(`` `` model ``=`` ``model``,`` `` increments ``=`` ``myIncrements``,`` `` nextBest ``=`` ``myNextBest``,`` `` stopping ``=`` ``myStopping``,`` `` cohort_size ``=`` ``mySize``,`` `` data ``=`` ``emptydata``,`` `` safetyWindow ``=`` ``mysafetywindow``,`` `` startingDose ``=`` ``8`` ``)`

## Simulation run

In order to obtain stable results, increase the simulation parameters
appropriately (step, samples, nsim).

`options`` ``<-`` `[`McmcOptions`](https://docs.crmpack.org/reference/McmcOptions-class.md)`(`` `` burnin ``=`` ``20``,`` `` step ``=`` ``1``,`` `` samples ``=`` ``50`` ``)`` ``mySims`` ``<-`` `[`simulate`](https://rdrr.io/r/stats/simulate.html)`(``design``,`` `` args ``=`` ``NULL``,`` `` truthTox ``=`` ``myTruth``,`` `` truthSurv ``=`` ``exp_cond_cdf``,`` `` trueTmax ``=`` ``42``,`` `` nsim ``=`` ``10``,`` `` seed ``=`` ``819``,`` `` mcmcOptions ``=`` ``options``,`` `` parallel ``=`` ``FALSE`` ``)`
