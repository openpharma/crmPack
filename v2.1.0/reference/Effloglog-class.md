# `Effloglog`

**\[stable\]**

`Effloglog` is the class for the linear log-log efficacy model using
pseudo data prior. It describes the relationship between continuous
efficacy responses and corresponding dose levels in log-log scale. This
efficacy log-log model is given as \$\$y_i = theta1 + theta2 \*
log(log(x_i)) + epsilon_i,\$\$ where \\y_i\\ is the efficacy response
for subject \\i\\, \\x_i\\ is the dose level treated for subject \\i\\
and \\epsilon_i\\ is the random error term of efficacy model at subject
\\i\\. The error term \\epsilon_i\\ is a random variable that follows
normal distribution with mean \\0\\ and variance \\nu^{-1}\\, which is
assumed to be the same for all subjects. There are three parameters in
this model, the intercept \\theta1\\, the slope \\theta2\\ and the
precision \\nu\\ of the efficacy responses, also known as the inverse of
the variance of the pseudo efficacy responses. It can be a fixed
constant or having a gamma distribution. Therefore, a single scalar
value or a vector with two positive numbers values must be specified for
`nu` slot. If there are some observed efficacy responses available, in
the output, `nu` will display the updated value of the precision or the
updated values for the parameters of the gamma distribution. The
`Effloglog` inherits all slots from
[`ModelEff`](https://openpharma.github.io/crmPack/reference/ModelEff-class.md)
class.

## Usage

``` r
Effloglog(eff, eff_dose, nu, data, const = 0)

.DefaultEffloglog()
```

## Arguments

- eff:

  (`numeric`)  
  the pseudo efficacy responses. Elements of `eff` must correspond to
  the elements of `eff_dose`.

- eff_dose:

  (`numeric`)  
  dose levels that correspond to pseudo efficacy responses in `eff`.

- nu:

  (`numeric`)  
  the precision (inverse of the variance) of the efficacy responses.
  This is either a fixed value or a named vector with two positive
  numbers, the shape (`a`), and the rate (`b`) parameters for the gamma
  distribution.

- data:

  (`DataDual`)  
  observed data to update estimates of the model parameters.

- const:

  (`number`)  
  the constant value added to the dose level when the dose level value
  is less than or equal to 1 and a special form of the linear log-log
  has to be applied (Yeung et al. 2015) .

## Details

The prior of this model is specified in form of pseudo data. First, at
least two dose levels are fixed. Then, using e.g. experts' opinion, the
efficacy values that correspond to these dose levels can be obtained,
The `eff` and `eff_dose` arguments represent the prior in form of the
pseudo data. The `eff` represents the pseudo efficacy values. The
`eff_dose` represents the dose levels at which these pseudo efficacy
values are observed. Hence, the positions of the elements specified in
`eff` and `eff_dose` must correspond to each other between these
vectors. Since at least 2 pseudo efficacy values are needed to obtain
modal estimates of the intercept and slope parameters, both `eff` and
`eff_dose` must be vectors of length at least 2.

The joint prior distribution of the intercept \\theta1\\ and the slope
\\theta2\\ of this model follows bivariate normal distribution with mean
\\mu\\ and covariance matrix \\(nu \* Q)^{-1}\\. The mean \\mu\\ is a
\\2 x 1\\ column vector that contains the prior modal estimates of the
intercept and the slope. Scalar \\nu\\ is the precision of the pseudo
efficacy responses and \\Q\\ is the prior or posterior (given that
observed, no DLT data is available) precision matrix. It is specified as
\\Q = X0^T \* X0 + X^T \* X\\, where \\X0\\ is a design matrix that is
based on pseudo dose levels only, and \\X\\ is a design matrix that is
based on dose levels corresponding to the no DLT efficacy responses
observed only (if any). Hence, the \\X0\\ (or \\X\\) will be of size \\r
x 2\\, if there are \\r \>= 2\\ pseudo efficacy responses specified (or
if there are \\r\\ no DLT efficacy responses observed in the `data`).

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

- `nu`:

  (`numeric`)  
  parameter of the prior precision of pseudo efficacy responses. This is
  either a fixed value or a named vector with two positive numbers, the
  shape (`a`), and the rate (`b`) parameters for the gamma distribution.

- `use_fixed`:

  (`flag`)  
  indicates whether `nu` specified is a fixed value or a vector with two
  parameters for gamma distribution. This slot is for internal purposes
  only and must not be used by the user.

- `theta1`:

  (`number`)  
  the intercept in this efficacy log-log model. This slot is used in
  output to display the resulting prior or posterior modal estimates
  obtained based on the pseudo and observed (if any) data.

- `theta2`:

  (`number`)  
  the slope in this efficacy log-log model. This slot is used in output
  to display the resulting prior or posterior modal estimates obtained
  based on the pseudo and observed (if any) data.

- `Pcov`:

  (`matrix`)  
  refers to the \\2 x 2\\ covariance matrix of the estimators of the
  intercept \\theta1\\ and the slope \\theta2\\ parameters in this
  model. This is used in output to display the resulting prior and
  posterior covariance matrix of \\theta1\\ and \\theta2\\ obtained,
  based on the pseudo and observed (if any) data. This slot is needed
  for internal purposes.

- `X`:

  (`matrix`)  
  is the design matrix that is based on either the pseudo dose levels or
  observed dose levels (without DLT). This is used in the output to
  display the design matrix for the pseudo or the observed efficacy
  responses.

- `Y`:

  (`numeric`)  
  is a vector that either contains the pseudo efficacy responses or
  observed efficacy responses (without DLT).

- `mu`:

  (`numeric`)  
  a vector of the prior or the posterior modal estimates of the
  intercept (\\theta1\\) and the slope (\\theta2\\). This slot is used
  in output to display as the mean of the prior or posterior bivariate
  normal distribution for \\theta1\\ and \\theta2\\.

- `Q`:

  (`matrix`)  
  is the prior or posterior (given that observed, no DLT data is
  available) precision matrix. It is specified as \\Q = X0^T \* X0 + X^T
  \* X\\, where \\X0\\ is a design matrix that is based on pseudo dose
  levels only, and \\X\\ is a design matrix that is based on dose levels
  corresponding to the observed, no DLT efficacy values only (if any).

- `const`:

  (`number`)  
  a non-negative number (default to 0), leading to the model form
  described above. In general, the model has the form \\y_i = theta1 +
  theta2 \* log(log(x_i + const)) + epsilon_i\\, such that dose levels
  greater than \\1 - const\\ can be considered as described in Yeung et
  al. (2015) .

## Note

Typically, end users will not use the `.DefaultEffloglog()` function.

## References

Yeung WY, Whitehead J, Reigner B, Beyer U, Diack C, Jaki T (2015).
“Bayesian adaptive dose-escalation procedure for binary and continuous
responses utilizing a gain function.” *Pharmaceutical Statistics*.
[doi:10.1002/pst.1706](https://doi.org/10.1002/pst.1706) , Published
online ahead of print.

## Examples

``` r
# Obtain prior modal estimates given the pseudo data.
# First we use an empty data set such that only the dose levels under
# investigations are given. In total, 12 dose levels are under investigation
# ranging from 25 to 300 mg with increments of 25 (i.e 25, 50, 75, ..., 300).
emptydata <- DataDual(doseGrid = seq(25, 300, 25), placebo = FALSE)

# Define the pseudo data as first by fixing two dose levels 25 and 300 mg (`eff_dose`).
# Then, the efficacy responses observed at these two dose levels are 1.223 and 2.513 (`eff`).
# We specify the prior precision of the pseudo efficacy responses (`nu`) as a vector
# with the shape (a) and the rate (b) parameters for the gamma distribution.
# Obtain modal estimates and other estimates from the model (no observations,
# only pseudo data).
my_model1 <- Effloglog(
  eff = c(1.223, 2.513),
  eff_dose = c(25, 300),
  nu = c(a = 1, b = 0.025),
  data = emptydata
)

# Observed data.
my_data <- DataDual(
  x = c(25, 50, 50, 75, 100, 100, 225, 300),
  y = c(0, 0, 0, 0, 1, 1, 1, 1),
  w = c(0.31, 0.42, 0.59, 0.45, 0.6, 0.7, 0.6, 0.52),
  doseGrid = emptydata@doseGrid
)
#> Used default patient IDs!
#> Used best guess cohort indices!

# Obtain posterior modal estimates and other estimates from the model given some
# observed data.
my_model2 <- Effloglog(
  eff = c(1.223, 2.513),
  eff_dose = c(25, 300),
  nu = c(a = 1, b = 0.025),
  data = my_data
)
```
