# Render a `CohortSizeConst` Object

**\[experimental\]**

We provide additional utility functions to allow human-friendly
rendition of crmPack objects in Markdown and Quarto files

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

We provide additional utility functions to allow human-friendly
rendition of crmPack objects in Markdown and Quarto files. This file
contains methods for all design classes, not just those that are direct
descendants of `Design`.

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

We provide additional utility functions to allow human-friendly
rendition of crmPack objects in Markdown and Quarto files

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

## Usage

``` r
# S3 method for class 'CohortSizeConst'
knit_print(x, ..., asis = TRUE, label = c("participant", "participants"))

# S3 method for class 'CohortSizeRange'
knit_print(x, ..., asis = TRUE)

# S3 method for class 'CohortSizeDLT'
knit_print(x, ..., tox_label = "toxicity", asis = TRUE)

# S3 method for class 'CohortSizeParts'
knit_print(x, ..., asis = TRUE, label = c("participant", "participants"))

# S3 method for class 'CohortSizeMax'
knit_print(x, ..., asis = TRUE)

# S3 method for class 'CohortSizeMin'
knit_print(x, ..., asis = TRUE)

# S3 method for class 'CohortSizeOrdinal'
knit_print(x, ..., tox_label = "toxicity", asis = TRUE)

# S3 method for class 'StartingDose'
knit_print(x, ..., asis = TRUE)

# S3 method for class 'RuleDesign'
knit_print(x, ..., level = 2L, title = "Design", sections = NA, asis = TRUE)

# S3 method for class 'Design'
knit_print(x, ..., level = 2L, title = "Design", sections = NA, asis = TRUE)

# S3 method for class 'DualDesign'
knit_print(x, ..., level = 2L, title = "Design", sections = NA, asis = TRUE)

# S3 method for class 'DADesign'
knit_print(x, ..., level = 2L, title = "Design", sections = NA, asis = TRUE)

# S3 method for class 'TDDesign'
knit_print(x, ..., level = 2L, title = "Design", sections = NA, asis = TRUE)

# S3 method for class 'DualResponsesDesign'
knit_print(x, ..., level = 2L, title = "Design", sections = NA, asis = TRUE)

# S3 method for class 'DesignOrdinal'
knit_print(x, ..., level = 2L, title = "Design", sections = NA, asis = TRUE)

# S3 method for class 'DesignGrouped'
knit_print(
  x,
  ...,
  level = 2L,
  title = "Design",
  sections = c(model = "Dose toxicity model", mono = "Monotherapy rules", combo =
    "Combination therapy rules", other = "Other details"),
  asis = TRUE
)

# S3 method for class 'TDsamplesDesign'
knit_print(x, ..., level = 2L, title = "Design", sections = NA, asis = TRUE)

# S3 method for class 'DualResponsesDesign'
knit_print(x, ..., level = 2L, title = "Design", sections = NA, asis = TRUE)

# S3 method for class 'DualResponsesSamplesDesign'
knit_print(x, ..., level = 2L, title = "Design", sections = NA, asis = TRUE)

# S3 method for class 'RuleDesignOrdinal'
knit_print(x, ..., level = 2L, title = "Design", sections = NA, asis = TRUE)

# S3 method for class 'GeneralData'
knit_print(
  x,
  ...,
  asis = TRUE,
  label = c("participant", "participants"),
  full_grid = FALSE,
  summarise = c("none", "dose", "cohort"),
  summarize = summarise,
  units = NA,
  format_func = h_knit_format_func
)

# S3 method for class 'DataParts'
knit_print(
  x,
  ...,
  asis = TRUE,
  label = c("participant", "participants"),
  full_grid = FALSE,
  summarise = c("none", "dose", "cohort"),
  summarize = summarise,
  units = NA,
  format_func = h_knit_format_func
)

# S3 method for class 'DualEndpoint'
knit_print(
  x,
  ...,
  asis = TRUE,
  use_values = TRUE,
  fmt = "%5.2f",
  units = NA,
  tox_label = "toxicity",
  biomarker_label = "PD biomarker"
)

# S3 method for class 'ModelParamsNormal'
knit_print(
  x,
  use_values = TRUE,
  fmt = "%5.2f",
  params = c("alpha", "beta"),
  preamble = "The prior for &theta; is given by\\n",
  asis = TRUE,
  theta = "\\theta",
  ...
)

# S3 method for class 'GeneralModel'
knit_print(
  x,
  ...,
  params = c("alpha", "beta"),
  asis = TRUE,
  use_values = TRUE,
  fmt = "%5.2f",
  units = NA
)

# S3 method for class 'LogisticKadane'
knit_print(
  x,
  ...,
  asis = TRUE,
  use_values = TRUE,
  fmt = "%5.2f",
  units = NA,
  tox_label = "toxicity"
)

# S3 method for class 'LogisticKadaneBetaGamma'
knit_print(
  x,
  ...,
  asis = TRUE,
  use_values = TRUE,
  fmt = "%5.2f",
  tox_label = "toxicity",
  units = NA
)

# S3 method for class 'LogisticLogNormal'
knit_print(
  x,
  ...,
  use_values = TRUE,
  fmt = "%5.2f",
  params = c(`\\alpha` = "alpha", `log(\\beta)` = "beta"),
  preamble = "The prior for &theta; is given by\\n",
  asis = TRUE
)

# S3 method for class 'LogisticLogNormalMixture'
knit_print(x, ..., asis = TRUE, use_values = TRUE, fmt = "%5.2f", units = NA)

# S3 method for class 'LogisticLogNormalSub'
knit_print(
  x,
  ...,
  use_values = TRUE,
  fmt = "%5.2f",
  params = c(`\\alpha` = "alpha", `log(\\beta)` = "beta"),
  preamble = "The prior for &theta; is given by\\n",
  asis = TRUE
)

# S3 method for class 'LogisticNormalMixture'
knit_print(x, ..., asis = TRUE, use_values = TRUE, fmt = "%5.2f", units = NA)

# S3 method for class 'LogisticNormalFixedMixture'
knit_print(x, ..., asis = TRUE, use_values = TRUE, fmt = "%5.2f", units = NA)

# S3 method for class 'OneParLogNormalPrior'
knit_print(
  x,
  ...,
  tox_label = "toxicity",
  asis = TRUE,
  use_values = TRUE,
  fmt = "%5.2f"
)

# S3 method for class 'OneParExpPrior'
knit_print(x, ..., asis = TRUE)

# S3 method for class 'LogisticLogNormalGrouped'
knit_print(
  x,
  ...,
  use_values = TRUE,
  fmt = "%5.2f",
  params = c(`\\alpha` = "alpha", `\\beta` = "beta", `log(\\delta_0)` = "delta_0",
    `log(\\delta_1)` = "delta_1"),
  preamble = "The prior for &theta; is given by\\n",
  asis = TRUE
)

# S3 method for class 'LogisticLogNormalOrdinal'
knit_print(
  x,
  ...,
  use_values = TRUE,
  fmt = "%5.2f",
  params = NA,
  preamble = "The prior for &theta; is given by\\n",
  asis = TRUE
)

# S3 method for class 'LogisticIndepBeta'
knit_print(
  x,
  ...,
  use_values = TRUE,
  fmt = "%5.2f",
  params = NA,
  tox_label = "DLAE",
  preamble = "The prior for &theta; is given by\\n",
  asis = TRUE
)

# S3 method for class 'Effloglog'
knit_print(
  x,
  ...,
  use_values = TRUE,
  fmt = "%5.2f",
  params = NA,
  tox_label = "DLAE",
  eff_label = "efficacy",
  label = "participant",
  preamble = "The prior for &theta; is given by\\n",
  asis = TRUE
)

# S3 method for class 'IncrementsRelative'
knit_print(x, ..., asis = TRUE)

# S3 method for class 'IncrementsRelativeDLT'
knit_print(x, ..., asis = TRUE)

# S3 method for class 'IncrementsDoseLevels'
knit_print(x, ..., asis = TRUE)

# S3 method for class 'IncrementsHSRBeta'
knit_print(x, ..., asis = TRUE)

# S3 method for class 'IncrementsMin'
knit_print(x, ..., asis = TRUE)

# S3 method for class 'IncrementsOrdinal'
knit_print(x, ..., asis = TRUE)

# S3 method for class 'IncrementsRelativeParts'
knit_print(x, ..., asis = TRUE, tox_label = c("toxicity", "toxicities"))

# S3 method for class 'IncrementsRelativeDLTCurrent'
knit_print(x, ..., asis = TRUE, tox_label = c("DLT", "DLTs"))

# S3 method for class 'NextBestMTD'
knit_print(
  x,
  ...,
  target_label = "the 25th centile",
  tox_label = "toxicity",
  asis = TRUE
)

# S3 method for class 'NextBestNCRM'
knit_print(x, ..., tox_label = "toxicity", asis = TRUE)

# S3 method for class 'NextBestThreePlusThree'
knit_print(
  x,
  ...,
  tox_label = c("toxicity", "toxicities"),
  label = "participant",
  asis = TRUE
)

# S3 method for class 'NextBestDualEndpoint'
knit_print(
  x,
  ...,
  tox_label = "toxicity",
  biomarker_label = "the biomarker",
  biomarker_units = ifelse(x@target_relative, "%", ""),
  asis = TRUE
)

# S3 method for class 'NextBestMinDist'
knit_print(x, ..., tox_label = "toxicity", asis = TRUE)

# S3 method for class 'NextBestInfTheory'
knit_print(
  x,
  ...,
  tox_label = "toxicity",
  citation_text = "Mozgunov & Jaki (2019)",
  citation_link = "https://doi.org/10.1002/sim.8450",
  asis = TRUE
)

# S3 method for class 'NextBestTD'
knit_print(x, ..., tox_label = "toxicity", asis = TRUE)

# S3 method for class 'NextBestMaxGain'
knit_print(x, ..., tox_label = "toxicity", asis = TRUE)

# S3 method for class 'NextBestProbMTDLTE'
knit_print(x, ..., tox_label = "toxicity", asis = TRUE)

# S3 method for class 'NextBestProbMTDMinDist'
knit_print(x, ..., tox_label = "toxicity", asis = TRUE)

# S3 method for class 'NextBestNCRMLoss'
knit_print(
  x,
  ...,
  tox_label = "toxicity",
  asis = TRUE,
  format_func = h_knit_format_func
)

# S3 method for class 'NextBestTDsamples'
knit_print(x, ..., tox_label = "toxicity", asis = TRUE)

# S3 method for class 'NextBestMaxGainSamples'
knit_print(x, ..., tox_label = "toxicity", asis = TRUE)

# S3 method for class 'NextBestOrdinal'
knit_print(x, ..., tox_label = "toxicity", asis = TRUE)

# S3 method for class 'SafetyWindow'
knit_print(x, ..., asis = TRUE, time_unit = "day", label = "participant")

# S3 method for class 'SafetyWindowConst'
knit_print(
  x,
  ...,
  asis = TRUE,
  label = "participant",
  ordinals = c("first", "second", "third", "fourth", "fifth", "sixth", "seventh",
    "eighth", "ninth", "tenth"),
  time_unit = "day"
)

# S3 method for class 'SafetyWindowSize'
knit_print(
  x,
  ...,
  asis = TRUE,
  ordinals = c("first", "second", "third", "fourth", "fifth", "sixth", "seventh",
    "eighth", "ninth", "tenth"),
  label = "participant",
  time_unit = "day",
  level = 2L
)

# S3 method for class 'StoppingOrdinal'
knit_print(x, ..., asis = TRUE)

# S3 method for class 'StoppingMaxGainCIRatio'
knit_print(x, ..., asis = TRUE)

# S3 method for class 'StoppingList'
knit_print(x, ..., preamble, indent = 0L, asis = TRUE)

# S3 method for class 'StoppingAny'
knit_print(x, ..., preamble, asis = TRUE)

# S3 method for class 'StoppingAll'
knit_print(x, ..., preamble, asis = TRUE)

# S3 method for class 'StoppingTDCIRatio'
knit_print(
  x,
  ...,
  dose_label = "the next best dose",
  tox_label = "toxicity",
  fmt_string =
    paste0("%sIf, at %s, the ratio of the upper to the lower limit of the posterior ",
    "95%% credible interval for %s (targetting %2.0f%%) is less than or equal to "),
  asis = TRUE
)

# S3 method for class 'StoppingTargetBiomarker'
knit_print(
  x,
  ...,
  dose_label = "the next best dose",
  biomarker_label = "the target biomarker",
  fmt_string =
    paste0("%sIf, at %s, the posterior probability that %s is in the range ",
    "(%.2f, %.2f)%s is %.0f%% or more.\n\n"),
  asis = TRUE
)

# S3 method for class 'StoppingLowestDoseHSRBeta'
knit_print(
  x,
  ...,
  tox_label = "toxicity",
  fmt_string =
    paste0("%sIf, using a Hard Stopping Rule with a prior of Beta(%.0f, %.0f), the ",
    "lowest dose in the dose grid has a posterior probability of %s of ",
    "%.0f%% or more.\n\n"),
  asis = TRUE
)

# S3 method for class 'StoppingMTDCV'
knit_print(
  x,
  ...,
  fmt_string =
    paste0("%sIf the posterior estimate of the robust coefficient of variation of ",
    "the MTD (targetting %2.0f%%), is than or equal to %.0f%%.\n\n"),
  asis = TRUE
)

# S3 method for class 'StoppingMTDdistribution'
knit_print(
  x,
  ...,
  fmt_string =
    "%sIf the mean posterior probability of %s at %.0f%% of %s is at least %4.2f.\n\n",
  dose_label = "the next best dose",
  tox_label = "toxicity",
  asis = TRUE
)

# S3 method for class 'StoppingHighestDose'
knit_print(
  x,
  ...,
  dose_label = "the highest dose in the dose grid",
  asis = TRUE
)

# S3 method for class 'StoppingSpecificDose'
knit_print(x, ..., dose_label = as.character(x@dose), asis = TRUE)

# S3 method for class 'StoppingTargetProb'
knit_print(
  x,
  ...,
  fmt_string =
    paste0("%sIf the probability of %s at %s is in the range [%4.2f, %4.2f] ",
    "is at least %4.2f.\n\n"),
  dose_label = "the next best dose",
  tox_label = "toxicity",
  asis = TRUE
)

# S3 method for class 'StoppingMinCohorts'
knit_print(x, ..., asis = TRUE)

# S3 method for class 'StoppingMinPatients'
knit_print(x, ..., label = "participant", asis = TRUE)

# S3 method for class 'StoppingPatientsNearDose'
knit_print(
  x,
  ...,
  dose_label = "the next best dose",
  label = "participants",
  asis = TRUE
)

# S3 method for class 'StoppingCohortsNearDose'
knit_print(x, ..., dose_label = "the next best dose", asis = TRUE)

# S3 method for class 'StoppingMissingDose'
knit_print(x, ..., asis = TRUE)
```

## Arguments

- x:

  (`ModelParamsNormal`)  
  the object to be rendered

- ...:

  passed to [`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html)

- asis:

  (`flag`)  
  Not used at present

- label:

  (`character`)  
  the term used to label participants

- tox_label:

  (`character`)  
  the term used to describe toxicity

- level:

  (`count`)  
  the markdown level at which the headings for cohort size will be
  printed. An integer between 1 and 6

- title:

  (`character`) The text of the heading of the section describing the
  design

- sections:

  (`character`) a named vector of length at least 4 defining the
  headings used to define the sections corresponding to the design's
  slots. The element names must match the Design's slot names.

- full_grid:

  (`flag`)  
  Should the full dose grid appear in the output table or simply those
  doses for whom at least one evaluable participant is available?
  Ignored unless `summarise == "dose"`.

- summarise:

  (`character`)  
  How to summarise the observed data. The default, `"none"`, lists
  observed data at the participant level. `"dose"` presents participant
  counts by dose and `"cohort"` by cohort.

- summarize:

  (`character`)  
  Synonym for `summarise`

- units:

  (`character`)  
  The units in which the values in `doseGrid` are

- format_func:

  (`function`)  
  The function used to format the range table.

- use_values:

  (`flag`)  
  print the values associated with hyperparameters, or the symbols used
  to define the hyper-parameters. That is, for example, mu or 1.

- fmt:

  (`character`)  
  the `sprintf` format string used to render numerical values. Ignored
  if `use_values` is `FALSE`.

- biomarker_label:

  (`character`)  
  the term used to describe the biomarker

- params:

  (`character`)  
  The names of the model parameters. See Usage Notes below.

- preamble:

  (`character`)  
  the text that introduces the list of rules

- theta:

  (`character`)  
  the LaTeX representation of the theta vector

- eff_label:

  (`character`)  
  the term used to describe efficacy

- target_label:

  (`character`)  
  the term used to describe the target toxicity rate

- biomarker_units:

  (`character`)  
  the units in which the biomarker is measured

- citation_text:

  (`character`)  
  the text used to cite Mozgunov & Jaki

- citation_link:

  (`character`)  
  the link to Mozgunov & Jaki

- time_unit:

  (`character`)  
  the word used to describe units of time. See Usage Notes below.

- ordinals:

  (`character`)  
  a character vector whose nth defines the word used as the written
  representation of the nth ordinal number.

- indent:

  (`integer`)  
  the indent level of the current stopping rule list. Spaces with length
  `indent * 4` will be prepended to the beginning of the rendered
  stopping rule list.

- dose_label:

  (`character`)  
  the term used to describe the target dose

- fmt_string:

  (`character`)  
  the character string that defines the format of the output

## Value

a character string that represents the object in markdown.

The markdown representation of the object, as a character string

a character string that represents the object in markdown.

A character string containing a LaTeX rendition of the object.

a character string that represents the object in markdown.

## Usage Notes

`label` describes the trial's participants.

It should be a character vector of length 1 or 2. If of length 2, the
first element describes a `cohort_size` of 1 and the second describes
all other `cohort_size`s. If of length 1, the character `s` is appended
to the value when `cohort_size` is not 1.

The default value of `col.names` is `c("Lower", "Upper", "Cohort size")`
and that of `caption` is
`"Defined by the dose to be used in the next cohort"`. These values can
be overridden by passing `col.names` and `caption` in the function call.

The by default, the columns are labelled `Lower`, `Upper` and
`Cohort size`. The table's caption is
`Defined by the number of <tox_label[2]> so far observed`. These values
can be overridden by passing `col.names` and `caption` in the function
call.

`label` describes the trial's participants.

It should be a character vector of length 1 or 2. If of length 2, the
first element describes a single participant and the second describes
all other situations. If of length 1, the character `s` is appended to
the value when the number of participants is not 1. The default values
of `col.names` and `caption` vary depending on the summary requested.
The default values can be overridden by passing `col.names` and
`caption` in the function call.

`params` must be a character vector of length equal to that of `x@mean`
(and `x@cov`). Its values represent the parameters of the model as
entries in the vector `theta`, on the left-hand side of "~" in the
definition of the prior. If named, names should be valid LaTeX, escaped
as usual for R character variables. For example, `"\\alpha"` or
`"\\beta_0"`. If unnamed, names are constructed by pre-pending an
escaped backslash to each value provided.

The default value of `col.names` is `c("Min", "Max", "Increment")` and
that of `caption` is `"Defined by highest dose administered so far"`.
These values can be overridden by passing `col.names` and `caption` in
the function call.

The default value of `col.names` is `c("Min", "Max", "Increment")` and
that of `caption` is `"Defined by number of DLTs reported so far"`.
These values can be overridden by passing `col.names` and `caption` in
the function call.

`label` defines how toxicities are described.

It should be a character vector of length 1 or 2. If of length 2, the
first element describes a single toxicity and the second describes all
other toxicity counts. If of length 1, the character `s` is appended to
the value describing a single toxicity.

The default value of `col.names` is `c("Min", "Max", "Increment")` and
that of `caption` is
`"Defined by number of DLTs in the current cohort"`. These values can be
overridden by passing `col.names` and `caption` in the function call.

`tox_label` defines how toxicities are described.

It should be a character vector of length 1 or 2. If of length 2, the
first element describes a single toxicity and the second describes all
other toxicity counts. If of length 1, the character `s` is appended to
the value describing a single toxicity.

This section describes the use of `label` and `tox_label`, collectively
referred to as `label`s. A `label` should be a scalar or a vector of
length 2. If a scalar, it is converted by adding a second element that
is equal to the first, suffixed by `s`. For example, `tox_label = "DLT"`
becomes `tox_label = c("DLT", "DLTs")`. The first element of the vector
is used to describe a count of 1. The second is used in all other cases.

To use a BibTeX-style citation, specify (for example)
`citation_text = "@MOZGUNOV", citation_link = ""`.

`label` should be a character vector of length 1 or 2. If of length 2,
the first element describes a count of 1 and the second describes all
other counts. If of length 1, the character `s` is appended to the value
when the count is not 1.

`label` and `time_unit` are, collectively, labels.

A label should be a character vector of length 1 or 2. If of length 2,
the first element describes a count of 1 and the second describes all
other counts. If of length 1, the character `s` is appended to the value
when the count is not 1.

`label` describes the trial's participants.

It should be a character vector of length 1 or 2. If of length 2, the
first element describes a `cohort_size` of 1 and the second describes
all other `cohort_size`s. If of length 1, the character `s` is appended
to the value when `cohort_size` is not 1.

The default value of `col.names` is `c("Lower", "Upper", "Cohort size")`
and that of `caption` is
`"Defined by the dose to be used in the next cohort"`. These values can
be overridden by passing `col.names` and `caption` in the function call.

The by default, the columns are labelled `Lower`, `Upper` and
`Cohort size`. The table's caption is
`Defined by the number of <tox_label[2]> so far observed`. These values
can be overridden by passing `col.names` and `caption` in the function
call.

`label` describes the trial's participants.

It should be a character vector of length 1 or 2. If of length 2, the
first element describes a single participant and the second describes
all other situations. If of length 1, the character `s` is appended to
the value when the number of participants is not 1. The default values
of `col.names` and `caption` vary depending on the summary requested.
The default values can be overridden by passing `col.names` and
`caption` in the function call.

`params` must be a character vector of length equal to that of `x@mean`
(and `x@cov`). Its values represent the parameters of the model as
entries in the vector `theta`, on the left-hand side of "~" in the
definition of the prior. If named, names should be valid LaTeX, escaped
as usual for R character variables. For example, `"\\alpha"` or
`"\\beta_0"`. If unnamed, names are constructed by pre-pending an
escaped backslash to each value provided.

The default value of `col.names` is `c("Min", "Max", "Increment")` and
that of `caption` is `"Defined by highest dose administered so far"`.
These values can be overridden by passing `col.names` and `caption` in
the function call.

The default value of `col.names` is `c("Min", "Max", "Increment")` and
that of `caption` is `"Defined by number of DLTs reported so far"`.
These values can be overridden by passing `col.names` and `caption` in
the function call.

`label` defines how toxicities are described.

It should be a character vector of length 1 or 2. If of length 2, the
first element describes a single toxicity and the second describes all
other toxicity counts. If of length 1, the character `s` is appended to
the value describing a single toxicity.

The default value of `col.names` is `c("Min", "Max", "Increment")` and
that of `caption` is
`"Defined by number of DLTs in the current cohort"`. These values can be
overridden by passing `col.names` and `caption` in the function call.

`tox_label` defines how toxicities are described.

It should be a character vector of length 1 or 2. If of length 2, the
first element describes a single toxicity and the second describes all
other toxicity counts. If of length 1, the character `s` is appended to
the value describing a single toxicity.

This section describes the use of `label` and `tox_label`, collectively
referred to as `label`s. A `label` should be a scalar or a vector of
length 2. If a scalar, it is converted by adding a second element that
is equal to the first, suffixed by `s`. For example, `tox_label = "DLT"`
becomes `tox_label = c("DLT", "DLTs")`. The first element of the vector
is used to describe a count of 1. The second is used in all other cases.

To use a BibTeX-style citation, specify (for example)
`citation_text = "@MOZGUNOV", citation_link = ""`.

`label` should be a character vector of length 1 or 2. If of length 2,
the first element describes a count of 1 and the second describes all
other counts. If of length 1, the character `s` is appended to the value
when the count is not 1.

`label` and `time_unit` are, collectively, labels.

A label should be a character vector of length 1 or 2. If of length 2,
the first element describes a count of 1 and the second describes all
other counts. If of length 1, the character `s` is appended to the value
when the count is not 1.

## See also

`knit_print` for more details.
