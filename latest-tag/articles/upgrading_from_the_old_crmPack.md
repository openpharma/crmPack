# Upgrading from crmPack version 1.0

The following vignette describes the changes that were introduced to the
`crmPack` package as a result of the package’s refactoring. One row in
below tables represents a single-type, consistent change.

## Class and slot changes

### Naming convention motivation

To be close to common R style guidelines (Wickham 2019) and use
consistent naming conventions within the `crmPack` package, `CamelCase`
notation is used for `class` names, `method` names and constructor
function names, and `snake_case` notation is used for `slot` names
throughout the package.

### New classes

[TABLE]

### Renamed classes

[TABLE]

### Renamed slots

Please note that this list might not be exhaustive. Please always check
the relevant class documentation for details.

[TABLE]

Strikeout indicates that the class/slot was removed.

## Moved `dose` and `prob` Functions from Slots to Methods

Moved `dose` and `prob` functions from model class slots to model class
methods. Example of usage: `dose`/`prob` function as a true
dose-DLT/DLT-dose relationship.

### Generate data, define a model and get samples

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`crmPack`](https://docs.crmpack.org/)`)`\
\
`empty_data`` ``<-`` `[`Data`](https://docs.crmpack.org/reference/Data-class.md)`(``doseGrid ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``1``, ``3``, ``5``, ``10``, ``15``, ``20``, ``25``, ``40``, ``50``, ``80``, ``100``)``)`\
`my_model`` ``<-`` `[`LogisticNormal`](https://docs.crmpack.org/reference/LogisticNormal-class.md)`(`\
`  mean ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``-``0.85``, ``1``)``,`\
`  cov ``=`` `[`matrix`](https://rdrr.io/r/base/matrix.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``1``, ``-``0.5``, ``-``0.5``, ``1``)``, nrow ``=`` ``2``)`\
`)`\
`my_options`` ``<-`` `[`McmcOptions`](https://docs.crmpack.org/reference/McmcOptions-class.md)`(``burnin ``=`` ``2``, step ``=`` ``2``, samples ``=`` ``20``)`\
`my_samples`` ``<-`` `[`mcmc`](https://docs.crmpack.org/reference/mcmc.md)`(``empty_data``, ``my_model``, ``my_options``)`

### Dose

Here is the example on how the `dose` function can be used in case of
different inputs, i.e. model’s parameters samples or in case of a fixed
model’s parameters values.

\
`# Doses reaching a specific target probability of the occurrence of a DLT (equal to 0.3),`\
`# given a particular models and samples.`\
`` # Every single dose corresponds to one particular sample in `my_samples`. ``\
[`dose`](https://docs.crmpack.org/reference/dose.md)`(``0.3``, ``my_model``, ``my_samples``)`\
`#>  [1] 5.764816e-01 1.003141e+00 1.090389e+00 1.152964e+00 1.834854e-01`\
`#>  [6] 1.892888e+00 6.623943e-03 2.120591e+00 1.107195e+00 7.848503e-05`\
`#> [11] 9.280628e-01 2.527625e+01 4.582232e-01 1.494800e+01 1.449023e+00`\
`#> [16] 2.533996e-01 3.334492e+00 1.409196e+00 1.488667e-01 5.061865e-01`\
\
`# True dose-DLT relationship.`\
`# Say that -0.8 and 1 are the true values for models parameters alpha0 and alpha1 respectively.`\
`` # The `true_dose_fun` takes one argument (target probability of the occurrence of a DLT) ``\
`# and computes the corresponding dose, according to the model chosen and given a fixed values`\
`# of the model's parameters.`\
`true_dose_fun`` ``<-`` `[`doseFunction`](https://docs.crmpack.org/reference/doseFunction.md)`(``my_model``, alpha0 ``=`` ``-``0.8``, alpha1 ``=`` ``1``)`\
`true_dose_fun``(``0.3``)`\
`#> [1] 0.9538033`

### Prob

\
`# Toxicity probabilities for a given dose (equal to 10), model and samples.`\
`` # Every single probability value corresponds to one particular sample in `my_samples`. ``\
[`prob`](https://docs.crmpack.org/reference/prob.md)`(``10``, ``my_model``, ``my_samples``)`\
`#>  [1] 0.7323336 0.9701552 0.9847560 0.9874946 0.8199989 0.9802354 0.3041193`\
`#>  [8] 0.8648901 0.9499393 0.6888586 0.9701999 0.4253856 0.7725674 0.3538183`\
`#> [15] 0.7739564 0.9489853 0.5819845 0.9890029 0.8117047 0.6546124`\
\
`# True DLT-dose relationship.`\
`# Say that -0.8 and 1 are the true values for models parameters alpha0 and alpha1 respectively.`\
`` # The `true_prob_fun` takes one argument (the dose) and computes the corresponding ``\
`# toxicity probability, according to the model chosen and given a fixed values`\
`# of the model's parameters.`\
`true_prob_fun`` ``<-`` `[`probFunction`](https://docs.crmpack.org/reference/probFunction.md)`(``my_model``, alpha0 ``=`` ``-``0.8``, alpha1 ``=`` ``1``)`\
`true_prob_fun``(``10``)`\
`#> [1] 0.8179597`

## New Random Number Generator settings for the MCMC

The Random Number Generator (RNG) settings used by the JAGS for the MCMC
are now configured solely through the `McmcOptions` class. The RNG
settings are: `RNG type` and the `RNG seed` that corresponds to a given
`RNG type`. Find out details in the help page for the `McmcOptions`
class. Any RNG-related user settings at the R session level (such us
those with [`set.seed()`](https://rdrr.io/r/base/Random.html)) are
ignored by the MCMC sampler.

## New no-argument constructors

To aid software development, new no-argument constructs for all
sub-classes of `GeneralModel`, `Increments`, `NextBest` and `Stopping`
have been introduced. The names of these constructors take the form
`.Default<classname>`, where `<classname>` is the name of the class
being created.

These constructors return valid, but not necessarily contextually
sensible, objects of the required class. One reason the objects returned
may not be contextually sensible is that the constructors take no
account of any associated `doseGrid`.

Here are some examples of their use:

\
[`.DefaultStoppingAll`](https://docs.crmpack.org/reference/StoppingAll-class.md)`(``)`

If all of the following rules are `TRUE`:

- ≥ 3 cohorts dosed: If 3 or more cohorts have been treated.

- P(0.2 ≤ prob(DLE \| NBD) ≤ 0.35) ≥ 0.5: If the probability of toxicity
  at the next best dose is in the range \[0.20, 0.35\] is at least 0.50.

- ≥ 20 patients dosed: If 20 or more participants have been treated.

\
\
`class_name`` ``<-`` ``"LogisticNormal"`\
[`eval`](https://rdrr.io/r/base/eval.html)`(`[`parse`](https://rdrr.io/r/base/parse.html)`(``text ``=`` `[`paste0`](https://rdrr.io/r/base/paste.html)`(``".Default"``, ``class_name``, ``"()"``)``)``)`

A logistic log normal model will describe the relationship between dose
and toxicity:
``` math
 p(Tox | d) = f(X = 1 | \theta, d) = \frac{e^{\alpha + \beta \cdot d/d^*}}{1 + e^{\alpha + \beta \cdot d/d^*}} 
```
where d\* denotes a reference dose.

The prior for θ is given by
``` math
 \boldsymbol\theta = \begin{bmatrix}\alpha \\ \beta\end{bmatrix}\sim N \left(\begin{bmatrix}-0.85 \\  1.00\end{bmatrix} , \begin{bmatrix} 1.00 & -0.50 \\ -0.50 &  1.00\end{bmatrix} \right) 
```

The reference dose will be 1.00.

## Handling of `NA` or placebo returned as next dose

For consistent handling how the study is stopped and to facilitate
analysis of stop reasons in the operation characteristics, the handling
of `NA` and placebo returned by `nextBest` methods is changed. In the
previous version of `crmPack` stopping for placebo or `NA` returned by a
`nextBest` method was handled automatically in the generic `Stopping`
method. This is now moved into a new stopping rule
`StoppingMissingDose`. As a consequence, the stopping rule
`StoppingMissingDose` must be specified for those `nextBest` methods
that can return NA, or when placebo is used. Otherwise the simulation
may run into an error if the study is not stopped when `NA` is returned
as the next dose. `nextBest` methods that can return `NA` are
`NextBestNCRM`, `NextBestNCRMLoss` and `NextBestDualEndpoint`.

## Evaluation of stopping rules at a specific dose

Without further specification, stopping rules are evaluated at the dose
returned by the used `nextBest` method. With the new stopping rule
`StoppingSpecificDose` it is possible to evaluate stopping rules at any
dose. For usage see documentation of `StoppingSpecificDose`.

## Further details in class and methods name changes

### Classes

[TABLE]

Strikeout indicates that the class/slot was removed.

### Methods

[TABLE]

Strikeout indicates that the method/argument was removed.

## References

Wickham, Hadley. 2019. *Advanced r, Second Edition*. Chapman & Hall/CRC:
R Series. <https://adv-r.hadley.nz>.
