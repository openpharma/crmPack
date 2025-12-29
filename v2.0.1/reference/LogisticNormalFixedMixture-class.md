# `LogisticNormalFixedMixture`

**\[stable\]**

`LogisticNormalFixedMixture` is the class for standard logistic
regression model with fixed mixture of multiple bivariate (log) normal
priors on the intercept and slope parameters. The weights of the normal
priors are fixed, hence no additional model parameters are introduced.
This type of prior is often used to better approximate a given posterior
distribution, or when the information is given in terms of a mixture.

## Usage

``` r
LogisticNormalFixedMixture(components, weights, ref_dose, log_normal = FALSE)

.DefaultLogisticNormalFixedMixture()
```

## Arguments

- components:

  (`list`)  
  the specifications of the mixture components, a list with
  [`ModelParamsNormal`](https://openpharma.github.io/crmPack/reference/ModelParamsNormal-class.md)
  objects for each bivariate (log) normal prior.

- weights:

  (`numeric`)  
  the weights of the components; these must be positive and will be
  normalized to sum to 1.

- ref_dose:

  (`number`)  
  the reference dose \\x\*\\ (strictly positive number).

- log_normal:

  (`flag`)  
  should a log normal prior be specified, such that the mean vectors and
  covariance matrices are valid for the intercept and log slope?

## Details

The covariate is the natural logarithm of the dose \\x\\ divided by the
reference dose \\x\*\\, i.e.: \$\$logit\[p(x)\] = alpha0 + alpha1 \*
log(x/x\*),\$\$ where \\p(x)\\ is the probability of observing a DLT for
a given dose \\x\\. The prior \$\$(alpha0, alpha1) ~ w1 \* Normal(mean1,
cov1) + ... + wK \* Normal(meanK, covK),\$\$ if a normal prior is used
and \$\$(alpha0, log(alpha1)) ~ w1 \* Normal(mean1, cov1) + ... + wK \*
Normal(meanK, covK),\$\$ if a log normal prior is used. The weights
\\w1, ..., wK\\ of the components are fixed and sum to 1.

The slots of this class comprise a list with components parameters.
Every single component contains the mean vector and the covariance
matrix of bivariate normal distributions. Remaining slots are the
weights of the components as well as the reference dose. Moreover, a
special indicator slot specifies whether a log normal prior is used.

## Slots

- `components`:

  (`list`)  
  the specifications of the mixture components, a list with
  [`ModelParamsNormal`](https://openpharma.github.io/crmPack/reference/ModelParamsNormal-class.md)
  objects for each bivariate (log) normal prior.

- `weights`:

  (`numeric`)  
  the weights of the components; these must be positive and must sum to
  1.

- `ref_dose`:

  (`positive_number`)  
  the reference dose.

- `log_normal`:

  (`flag`)  
  should a log normal prior be used, such that the mean vectors and
  covariance matrices are valid for the intercept and log slope?

## Note

Typically, end-users will not use the
`.DefaultLogisticNormalFixedMixture()` function.

## See also

[`ModelParamsNormal`](https://openpharma.github.io/crmPack/reference/ModelParamsNormal-class.md),
[`ModelLogNormal`](https://openpharma.github.io/crmPack/reference/ModelLogNormal-class.md),
[`LogisticNormalMixture`](https://openpharma.github.io/crmPack/reference/LogisticNormalMixture-class.md),
[`LogisticLogNormalMixture`](https://openpharma.github.io/crmPack/reference/LogisticLogNormalMixture-class.md).

## Examples

``` r
my_model <- LogisticNormalFixedMixture(
  components = list(
    comp1 = ModelParamsNormal(
      mean = c(-0.85, 1),
      cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2)
    ),
    comp2 = ModelParamsNormal(
      mean = c(1, 1.5),
      cov = matrix(c(1.2, -0.45, -0.45, 0.6), nrow = 2)
    )
  ),
  weights = c(0.3, 0.7),
  ref_dose = 50
)
```
