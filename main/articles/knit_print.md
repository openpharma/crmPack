# Describing \`crmPack\` Objects

## Introduction

Objects created by `crmPack` are almost always S4 objects. Like all S4
objects, by default they do not render in a particularly user-friendly
way.

``` r
cs <- CohortSizeDLT(intervals = 0:2, cohort_size = c(1, 3, 5))
cs
```

    ## An object of class "CohortSizeDLT"
    ## Slot "intervals":
    ## [1] 0 1 2
    ## 
    ## Slot "cohort_size":
    ## [1] 1 3 5

Fortunately, a little known feature of `knitr` can put this right at
little or no cost to the end user: in the simplest case, demonstrated
below, all that needs to be done is to reference the object in a
markdown or Quarto chunk.

``` r
cs
```

[TABLE]

Defined by the number of toxicities so far observed

> The `knit_print`methods provided by `crmPack` are not intended to be
> fully customisable or comprehensive. We do believe, however, that they
> cover the vast majority of use-cases and are easily extended using the
> techniques described later in this vignette.

> Formatting of these objects currently only works for HMTL output. If
> another format - such as PDF or Microsoft Word - is required, our
> suggested workaround is to create the HTML output and then print or
> save the document to the required format.

## How is this done?

When running code at the console, the result of an R function or
statement that is not assigned to an object is `print`ed. (Unless, of
course, it is returned `invisible`ly.) The same process appears to
happen when the chunks of a markdown or Quarto document are evaluated.
But that is not quite the case. Instead, the result is passed to an S3
function called `knit_print` (Xie 2024). It is the results of running
`knit_print` on the returned expression that appear in the rendered
document.

As a simple demonstration of the concept, consider:

``` r
knit_print.DustySpringfield <- function(x, ...) {
  "I just don't know what to do with myself"
}

lyric <- 10

lyric
```

    ## [1] 10

``` r
class(lyric) <- "DustySpringfield"

lyric
```

    ## I just don't know what to do with myself

The actions of `knit_print` are entirely arbitrary, but this mechanism
provides developers with an easy way to provide nicely-rendered versions
of any objects that are rendered by `knitr`. We have provided such
methods for (almost) all `crmPack` classes.

## Using `knit_print` in `crmPack`

By default, all that needs to be done is to reference the object to be
printed in a markdown or quarto chunk. This is equivalent to
`knit_print(object)`. However, the `knit_print` methods for most
`crmPack` classes have optional arguments that can be used to customise
the way in which the object is rendered. To change the default value of
any parameter to `knit_print` the function must be called explicitly:
`knit_print(cs, tox_label = "DLAE")`.

### Common customisations

The most commonly needed customisations are to alter the way in which
participants and toxicities are described. These are handled by the
`label` and `tox_label` arguments to `knit_print`.

These arguments can be provided either as a scalar or a vector of length
2. If a vector, the first element is taken to describe a single instance
and the second any other number of instances. If a scalar, it is
converted to a vector, whose first element is the scalar value provided
and the second the scalar with `"s"` appended[^1].

So, for example:

``` r
CohortSizeConst(3)
```

A constant size of 3 participants.

``` r
knit_print(CohortSizeConst(3), label = "subject")
```

A constant size of 3 subjects.

Dose units are defined by the `units` parameter. By default, no units
are printed.

``` r
d <- Data(doseGrid = c(0.1, 0.3, 0.9, 2.5, 5, 10, 15))
d
```

No participants are yet evaluable.

The dose grid is 0.1, 0.3, 0.9, 2.5, 5, 10 and 15.

``` r
knit_print(d, units = "mg/dL")
```

No participants are yet evaluable.

The dose grid is 0.1 mg/dL, 0.3 mg/dL, 0.9 mg/dL, 2.5 mg/dL, 5 mg/dL, 10
mg/dL and 15 mg/dL.

The format used to display dose levels (and other information in other
classes) can be changed with the `fmt` parameter:

``` r
knit_print(d, units = "mg/dL", fmt = "%.2f")
```

No participants are yet evaluable.

The dose grid is 0.10 mg/dL, 0.30 mg/dL, 0.90 mg/dL, 2.50 mg/dL, 5.00
mg/dL, 10.00 mg/dL and 15.00 mg/dL.

`biomarker_label` and `biomarker_units` allow the representation of a
biomarker to be customised.

``` r
x <- .DefaultDualEndpointRW()
x
```

The relationships between dose and toxicity and between dose and PD
biomarker will be modelled simultaneously.

A probit log normal model will describe the relationship between dose
and toxicity:
``` math
 \Phi^{-1}(Tox | d) = f(X = 1 | \theta, d) = \alpha + \beta \cdot log(d/d^*) 
```
where d\* denotes a reference dose.

The prior for θ is given by
``` math
 \boldsymbol\theta = \begin{bmatrix}\alpha \\ \beta\end{bmatrix}\sim N \left(\begin{bmatrix} 0.00 \\  1.00\end{bmatrix} , \begin{bmatrix} 1.00 &  0.00 \\ 0.00 &  1.00\end{bmatrix} \right) 
```

The reference dose will be 1.00.

The PD biomarker response `w` at dose `d` is modelled as
``` math
 w(d) \sim N(f(d), \sigma_w^2) 
```

where f(d) is a first order random walk such that

``` math
 f(d) = \beta_{W_i} - \beta_{W_{i - 1}}\sim N(0, 0.01 \times (d_i - d_{i - 1})) 
```

``` r
knit_print(x, biomarker_name = "CRP", biomarker_units = "mg/dL")
```

The relationships between dose and toxicity and between dose and PD
biomarker will be modelled simultaneously.

A probit log normal model will describe the relationship between dose
and toxicity:
``` math
 \Phi^{-1}(Tox | d) = f(X = 1 | \theta, d) = \alpha + \beta \cdot log(d/d^*) 
```
where d\* denotes a reference dose.

The prior for θ is given by
``` math
 \boldsymbol\theta = \begin{bmatrix}\alpha \\ \beta\end{bmatrix}\sim N \left(\begin{bmatrix} 0.00 \\  1.00\end{bmatrix} , \begin{bmatrix} 1.00 &  0.00 \\ 0.00 &  1.00\end{bmatrix} \right) 
```

The reference dose will be 1.00.

The PD biomarker response `w` at dose `d` is modelled as
``` math
 w(d) \sim N(f(d), \sigma_w^2) 
```

where f(d) is a first order random walk such that

``` math
 f(d) = \beta_{W_i} - \beta_{W_{i - 1}}\sim N(0, 0.01 \times (d_i - d_{i - 1})) 
```

## Rendering complex classes

Some `crmPack` classes have slots whose values are themselves `crmPack`
classes. `CohortSizeMax` is a simple example. In these cases, the slot
values are each passed to `knit_print` in turn.

``` r
.DefaultCohortSizeMax()
```

The maximum of the cohort sizes defined in the following rules:

[TABLE]

Defined by the dose to be used in the next cohort

[TABLE]

Defined by the number of toxicities so far observed

`knit_print` methods for sub-classes of `RuleDesign` (and related
classes) offer slightly more control. Here, an overall header for the
rendition of the object is provided by the `title` parameter (whose
value defaults to “Design” and the slot values are separated by
sub-headers. The styling of the overall header and sub-headers is
controlled by the `level` parameter. The default value of `level` is
`2L`, and the level of slots is defined recursively to be one more than
the level of the parent slot[^2]. Class-specific parameters are passed
to slot-specific `knit_print` methods using `...`.

``` r
knit_print(.DefaultDesign())
```

## Design

### Dose toxicity model

A logistic log normal model will describe the relationship between dose
and toxicity:
``` math
 p(Tox | d) = f(X = 1 | \theta, d) = \frac{e^{\alpha + \beta \cdot log(d/d_{ref})}}{1 + e^{\alpha + \beta \cdot log(d/d_{ref})}} 
```
where d_(ref) denotes a reference dose.

The prior for θ is given by
``` math
 \boldsymbol\theta = \begin{bmatrix}\alpha \\ log(\beta)\end{bmatrix}\sim N \left(\begin{bmatrix}-0.85 \\  1.00\end{bmatrix} , \begin{bmatrix} 1.00 & -0.50 \\ -0.50 &  1.00\end{bmatrix} \right) 
```

The reference dose will be 56.00.

### Stopping rule

If either of the following rules are `TRUE`:

- If both of the following rules are `TRUE`:

  - ≥ 3 cohorts dosed: If 3 or more cohorts have been treated.

  - P(0.2 ≤ prob(DLE \| NBD) ≤ 0.35) ≥ 0.5: If the probability of
    toxicity at the next best dose is in the range \[0.20, 0.35\] is at
    least 0.50.

- ≥ 20 patients dosed: If 20 or more participants have been treated.

### Escalation rule

[TABLE]

Defined by highest dose administered so far

### Use of placebo

Placebo will not be administered in the trial.

### Dose recommendation

The dose recommended for the next cohort will be chosen in the following
way. First, doses that are ineligible according to the increments rule
will be discarded. Next, any dose for which the mean posterior
probability of toxicity being in the overdose range - (0.35, 1\] - is
0.25 or more will also be discarded. Finally, the dose amongst those
remaining which has the highest chance that the mean posterior
probability of toxicity is in the target toxicity range of 0.2 to 0.35
(inclusive) will be selected.

### Cohort size

The maximum of the cohort sizes defined in the following rules:

[TABLE]

Defined by the dose to be used in the next cohort

[TABLE]

Defined by the number of toxicities so far observed

### Observed data

No participants are yet evaluable.

The dose grid is 1, 3, 5, 10, 15, 20, 25, 40, 50, 80 and 100.

### Starting dose

The starting dose is 3.

Slot headers can be customised using the `sections` parameter.
`sections` should be a named vector. Names should be valid slot names
for the object being rendered and values the requested slot headers.

``` r
knit_print(
  .DefaultDesign(),
  level = 4,
  sections = c(
    "nextBest" = "Selection of the dose for the following cohort",
    "startingDose" = "Initial dose"
  )
)
```

``` math
Output not shown.
```

> It is not possible to omit slots from the rendition of a `crmPack`
> object. If you need to do this, you can either render the required
> slots individually, or override the definition of `knit_print` for the
> super class as demonstrated below.

## Restoring console-like behaviour

To restore the default behaviour for `crmPack` objects, simply wrap the
object in a call to `normal_print()`.

``` r
normal_print(cs)
```

    ## An object of class "CohortSizeDLT"
    ## Slot "intervals":
    ## [1] 0 1 2
    ## 
    ## Slot "cohort_size":
    ## [1] 1 3 5

## Accessing the output of `knit_print`

One of the parameters of
[`knitr::knit_print`](https://rdrr.io/pkg/knitr/man/knit_print.html) is
`asis`, with a default value of `TRUE`. `asis` has the same effect as
setting the chunk option `output` to `asis`. This is achieved by
returning an object of class `knit-asis`.

Setting `asis` to `FALSE` will display the raw HTML code generated by
`knit_print` to be displayed. Alternatively, it may allow easier
manipulation of the return value.

``` r
csOutput1 <- knit_print(CohortSizeDLT(intervals = 0:2, cohort_size = c(1, 3, 5)))
class(csOutput1)
```

    ## [1] "knit_asis"

``` r
csOutput1
```

[TABLE]

Defined by the number of toxicities so far observed

``` r
csOutput2 <- knit_print(CohortSizeDLT(intervals = 0:2, cohort_size = c(1, 3, 5)), asis = FALSE)
class(csOutput2)
```

    ## [1] "character"

``` r
csOutput2
```

    ## [1] "<table>\n<caption>Defined by the number of toxicities so far observed</caption>\n <thead>\n<tr>\n<th style=\"border-bottom:hidden;padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; \" colspan=\"2\"><div style=\"border-bottom: 1px solid #ddd; padding-bottom: 5px; \">No of toxicities</div></th>\n<th style=\"empty-cells: hide;border-bottom:hidden;\" colspan=\"1\"></th>\n</tr>\n  <tr>\n   <th style=\"text-align:right;\"> Lower </th>\n   <th style=\"text-align:right;\"> Upper </th>\n   <th style=\"text-align:right;\"> Cohort size </th>\n  </tr>\n </thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:right;\"> 0 </td>\n   <td style=\"text-align:right;\"> 1 </td>\n   <td style=\"text-align:right;\"> 1 </td>\n  </tr>\n  <tr>\n   <td style=\"text-align:right;\"> 1 </td>\n   <td style=\"text-align:right;\"> 2 </td>\n   <td style=\"text-align:right;\"> 3 </td>\n  </tr>\n  <tr>\n   <td style=\"text-align:right;\"> 2 </td>\n   <td style=\"text-align:right;\"> Inf </td>\n   <td style=\"text-align:right;\"> 5 </td>\n  </tr>\n</tbody>\n</table>\n\n"

But with the chunk option `output` set to `asis`…

``` r
cat(csOutput2)
```

    ## <table>
    ## <caption>Defined by the number of toxicities so far observed</caption>
    ##  <thead>
    ## <tr>
    ## <th style="border-bottom:hidden;padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">No of toxicities</div></th>
    ## <th style="empty-cells: hide;border-bottom:hidden;" colspan="1"></th>
    ## </tr>
    ##   <tr>
    ##    <th style="text-align:right;"> Lower </th>
    ##    <th style="text-align:right;"> Upper </th>
    ##    <th style="text-align:right;"> Cohort size </th>
    ##   </tr>
    ##  </thead>
    ## <tbody>
    ##   <tr>
    ##    <td style="text-align:right;"> 0 </td>
    ##    <td style="text-align:right;"> 1 </td>
    ##    <td style="text-align:right;"> 1 </td>
    ##   </tr>
    ##   <tr>
    ##    <td style="text-align:right;"> 1 </td>
    ##    <td style="text-align:right;"> 2 </td>
    ##    <td style="text-align:right;"> 3 </td>
    ##   </tr>
    ##   <tr>
    ##    <td style="text-align:right;"> 2 </td>
    ##    <td style="text-align:right;"> Inf </td>
    ##    <td style="text-align:right;"> 5 </td>
    ##   </tr>
    ## </tbody>
    ## </table>

## Providing your own `knit_print` method

If the methods provided in `crmPack` don’t do what you want, it’s easy
to roll your own, using standard S3 techniques.

The formal arguments to
[`knitr::knit_print`](https://rdrr.io/pkg/knitr/man/knit_print.html) are
`x` and `...`. Additional arguments can be added after `...`.

As an example, consider `knit_print.NextBestNCRM`, which currently
returns a paragraph of text:

``` r
.DefaultNextBestNCRM()
```

The dose recommended for the next cohort will be chosen in the following
way. First, doses that are ineligible according to the increments rule
will be discarded. Next, any dose for which the mean posterior
probability of toxicity being in the overdose range - (0.35, 1\] - is
0.25 or more will also be discarded. Finally, the dose amongst those
remaining which has the highest chance that the mean posterior
probability of toxicity is in the target toxicity range of 0.2 to 0.35
(inclusive) will be selected.

You might feel this is better presented as a bulleted list. You can
achieve this as follows[^3]:

``` r
knit_print.NextBestNCRM <- function(x, ...) {
  knitr::asis_output(
    paste0(
      "The dose recommended for the next cohort will be chosen in the following ",
      "way.\n\n-  First, doses that are ineligible according to the increments rule ",
      "will be discarded.\n-  Next, any dose for which the mean posterior probability of ",
      " toxicity being in the overdose range - (",
      x@overdose[1], ", ", x@overdose[2],
      "] - is ",
      x@max_overdose_prob,
      " or more will also be discarded.\n-  Finally, the dose amongst those remaining ",
      "which has the highest chance that the mean posterior probability of toxicity ",
      "is in the target toxicity range of ",
      x@target[1],
      " to ",
      x@target[2],
      " (inclusive) will be selected.\n\n"
    )
  )
}
registerS3method("knit_print", "NextBestNCRM", knit_print.NextBestNCRM)

.DefaultNextBestNCRM()
```

The dose recommended for the next cohort will be chosen in the following
way.

- First, doses that are ineligible according to the increments rule will
  be discarded.
- Next, any dose for which the mean posterior probability of toxicity
  being in the overdose range - (0.35, 1\] - is 0.25 or more will also
  be discarded.
- Finally, the dose amongst those remaining which has the highest chance
  that the mean posterior probability of toxicity is in the target
  toxicity range of 0.2 to 0.35 (inclusive) will be selected.

## Class coverage

`crmPack` defines 125 classes. Custom `knit_print` methods exist for 91
of them. Of the remaining 34 classes, 23 are virtual classes that will
never be directly instantiated by end users. That leaves 11 classes for
which `knit_print` methods may be useful. These classes are listed
below.

| Class                  |
|:-----------------------|
| DualSimulationsSummary |
| NextBestEWOC           |
| Simulations            |
| StoppingExternal       |
| DualSimulations        |
| GeneralSimulations     |
| Samples                |
| DASimulations          |
| IncrementsMaxToxProb   |
| EffFlexi               |
| McmcOptions            |

The majority of these classes relate to simulation of the operating
characteristics of CRM trials. Reporting of this information is likely
to need customisation that is beyond the scope of a simple function[^4].

Xie, Yihui. 2024. “Custom Print Methods.”
<https://CRAN.R-project.org/package=knitr/vignettes/knit_print.html>.

[^1]: Except for `tox_label = "toxicity"`, which becomes
    `tox_label = c("toxicity", "toxicities")`.

[^2]: Because markdown header styles are defined only for six levels,
    the greatest value for `level`, including values generated by nested
    calls, is `6`.

[^3]: For simplicity, the `tox_label` and `asis` parameters, which are
    defined in the current implementation of the function, are omitted
    in this custom implementation. They should be preserved in any “real
    world” customisation.

[^4]: The `crmPack` team is considering the creation of markdown or
    Quarto templates that may assist in this area, but consider this to
    be a long-term ambition.
