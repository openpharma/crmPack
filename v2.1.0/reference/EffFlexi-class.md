# `EffFlexi`

**\[stable\]**

`EffFlexi` is the class for the efficacy model in flexible form of prior
expressed in form of pseudo data. In this class, a flexible form is used
to describe the relationship between the efficacy responses and the dose
levels and it is specified as \$\$(W \| betaW, sigma2W) ~ Normal(X \*
betaW, sigma2W \* I),\$\$ where \\W\\ is a vector of the efficacy
responses, \\betaW\\ is a column vector of the mean efficacy responses
for all dose levels, and \\X\\ is the design matrix with entries
\\I_i,j\\ that are equal to 1 if subject \\i\\ is allocated to dose
\\j\\, and \\0\\ otherwise. The \\sigma2W\\ is the variance of the
efficacy responses which can be either a fixed number or a number from
an inverse gamma distribution. This flexible form aims to capture
different shapes of the dose-efficacy curve. In addition, the first
(RW1) or second order (RW2) random walk model can be used for smoothing
data. That is the random walk model is used to model the first or the
second order differences of the mean efficacy responses to its
neighboring dose levels of their mean efficacy responses.

The RW1 model is given as \$\$betaW_j - betaW_j-1) ~ Normal(0,
sigma2betaW),\$\$ and for RW2 as \$\$betaW_j-2 - 2 \* betaW_j-1 + beta_j
~ Normal(0, sigma2betaW),\$\$ where \\betaW_j\\ is the vector of mean
efficacy responses at dose j, and the \\sigma2betaW\\ is the prior
variance which can be either a fixed number or a number from an inverse
gamma distribution.

The `eff` and `eff_dose` are the pseudo efficacy responses and dose
levels at which these pseudo efficacy responses are observed. Both,
`eff` and `eff_dose` must be vectors of length at least 2. The positions
of the elements specified in `eff` and `eff_dose` must correspond to
each other between these vectors.

## Usage

``` r
EffFlexi(eff, eff_dose, sigma2W, sigma2betaW, rw1 = TRUE, data)

.DefaultEffFlexi()
```

## Arguments

- eff:

  (`numeric`)  
  the pseudo efficacy responses. Elements of `eff` must correspond to
  the elements of `eff_dose`.

- eff_dose:

  (`numeric`)  
  dose levels that correspond to pseudo efficacy responses in `eff`.

- sigma2W:

  (`numeric`)  
  the prior variance of the efficacy responses. This is either a fixed
  value or a named vector with two positive numbers, the shape (`a`),
  and the rate (`b`) parameters for the inverse gamma distribution.

- sigma2betaW:

  (`numeric`)  
  the prior variance of the random walk model used for smoothing. This
  is either a fixed value or a named vector with two positive numbers,
  the shape (`a`), and the rate (`b`) parameters for the inverse gamma
  distribution.

- rw1:

  (`flag`)  
  used for smoothing data for this efficacy model. If it is `TRUE`, the
  first-order random walk model is used for the mean efficacy responses.
  Otherwise, the random walk of second order is used.

- data:

  (`DataDual`)  
  observed data to update estimates of the model parameters.

## Details

This model will output the updated value or the updated values of the
parameters of the inverse gamma distributions for \\sigma2W\\ and
\\sigma2betaW\\. The `EffFlexi` inherits all slots from
[`ModelEff`](https://openpharma.github.io/crmPack/reference/ModelEff-class.md)
class.

## Slots

- `eff`:

  (`numeric`)  
  the pseudo efficacy responses. Each element here must represent
  responses treated based on one subject. It must be a vector of length
  at least 2 and the order of its elements must correspond to values
  specified in `eff_dose`.

- `eff_dose`:

  (`numeric`)  
  the pseudo efficacy dose levels at which the pseudo efficacy responses
  are observed. It must be a vector of length at least 2 and the order
  of its elements must correspond to values specified in `eff`.

- `sigma2W`:

  (`numeric`)  
  the prior variance of the flexible efficacy form. This is either a
  fixed value or a named vector with two positive numbers, the shape
  (`a`), and the rate (`b`) parameters for the gamma distribution.

- `sigma2betaW`:

  (`numeric`)  
  the prior variance of the random walk model for the mean efficacy
  responses. This is either a fixed value or a named vector with two
  positive numbers, the shape (`a`), and the rate (`b`) parameters for
  the gamma distribution.

- `use_fixed`:

  (`logical`)  
  indicates whether a fixed value for `sigma2W` and `sigma2betaW` (for
  each parameter separately) is used or not. This slot is needed for
  internal purposes and must not be touched by the user.

- `rw1`:

  (`flag`)  
  used for smoothing data for this efficacy model. If it is `TRUE`, the
  first-order random walk model is used for the mean efficacy responses.
  Otherwise, the random walk of second order is used.

- `X`:

  (`matrix`)  
  the design matrix for the efficacy responses. It is based on both the
  pseudo and the observed efficacy responses.

- `RW`:

  (`matrix`)  
  the difference matrix for the random walk model. This slot is needed
  for internal purposes and must not be used by the user.

- `RW_rank`:

  (`integer`)  
  is the rank of the difference matrix. This slot is needed for internal
  purposes and must not be used by the user.

## Note

Typically, end users will not use the `.DefaultEffFlexi()` function.

## Examples

``` r
# Obtain prior estimates for the efficacy model in flexible form, given the pseudo data.
# First define an empty data set by defining the dose levels used in the study.
# There are 12 dose levels used in the study, ranging from 25 to 300 mg with
# increments of 25.
emptydata <- DataDual(doseGrid = seq(25, 300, 25))

# Define the pseudo data, i.e.: fixed 2 dose levels 25 and 300 mg (`eff_dose`)
# and the efficacy responses 1.223 and 2.513 observed at these two dose levels (`eff`).
# The prior variance of the pseudo efficacy responses can be either a fixed value
# or two parameters for the inverse gamma distribution, the shape (a) and the
# rate (b) (`sigma2W`).
# The prior variance of the random walk model can be either a fixed value or two
# parameters for the inverse gamma distribution, the shape (a) and the rate (b)
# (`sigma2betaW`).
my_model <- EffFlexi(
  eff = c(1.223, 2.513),
  eff_dose = c(25, 300),
  sigma2W = c(a = 0.1, b = 0.1),
  sigma2betaW = c(a = 20, b = 50),
  rw1 = FALSE,
  data = emptydata
)

# Obtain estimates from the model given some observed data is available.
data <- DataDual(
  x = c(25, 50, 50, 75, 100, 100, 225, 300),
  y = c(0, 0, 0, 0, 1, 1, 1, 1),
  w = c(0.31, 0.42, 0.59, 0.45, 0.6, 0.7, 0.6, 0.52),
  doseGrid = emptydata@doseGrid
)
#> Used default patient IDs!
#> Used best guess cohort indices!

my_model1 <- EffFlexi(
  eff = c(1.223, 2.513),
  eff_dose = c(25, 300),
  sigma2W = c(a = 0.1, b = 0.1),
  sigma2betaW = c(a = 20, b = 50),
  rw1 = FALSE,
  data = data
)
```
