# `LogisticNormalMixture`

**\[stable\]**

`LogisticNormalMixture` is the class for standard logistic regression
model with a mixture of two bivariate normal priors on the intercept and
slope parameters.

## Usage

``` r
LogisticNormalMixture(comp1, comp2, weightpar, ref_dose)

.DefaultLogisticNormalMixture()
```

## Arguments

- comp1:

  (`ModelParamsNormal`)  
  bivariate normal prior specification of the first component. See
  [`ModelParamsNormal`](https://openpharma.github.io/crmPack/reference/ModelParamsNormal-class.md)
  for more details.

- comp2:

  (`ModelParamsNormal`)  
  bivariate normal prior specification of the second component. See
  [`ModelParamsNormal`](https://openpharma.github.io/crmPack/reference/ModelParamsNormal-class.md)
  for more details.

- weightpar:

  (`numeric`)  
  the beta parameters for the weight of the first component. It must a
  be a named vector of length 2 with names `a` and `b` and with strictly
  positive values.

- ref_dose:

  (`number`)  
  the reference dose \\x\*\\ (strictly positive number).

## Details

The covariate is the natural logarithm of the dose \\x\\ divided by the
reference dose \\x\*\\, i.e.: \$\$logit\[p(x)\] = alpha0 + alpha1 \*
log(x/x\*),\$\$ where \\p(x)\\ is the probability of observing a DLT for
a given dose \\x\\. The prior \$\$(alpha0, alpha1) ~ w \* Normal(mean1,
cov1) + (1 - w) \* Normal(mean2, cov2).\$\$ The weight w for the first
component is assigned a beta prior `B(a, b)`.

## Slots

- `comp1`:

  (`ModelParamsNormal`)  
  bivariate normal prior specification of the first component.

- `comp2`:

  (`ModelParamsNormal`)  
  bivariate normal prior specification of the second component.

- `weightpar`:

  (`numeric`)  
  the beta parameters for the weight of the first component. It must a
  be a named vector of length 2 with names `a` and `b` and with strictly
  positive values.

- `ref_dose`:

  (`positive_number`)  
  the reference dose.

## Note

The weight of the two normal priors is a model parameter, hence it is a
flexible mixture. This type of prior is often used with a mixture of a
minimal informative and an informative component, in order to make the
CRM more robust to data deviations from the informative component.

Typically, end-users will not use the `.DefaultLogisticNormalMixture()`
function.

## See also

[`ModelParamsNormal`](https://openpharma.github.io/crmPack/reference/ModelParamsNormal-class.md),
[`ModelLogNormal`](https://openpharma.github.io/crmPack/reference/ModelLogNormal-class.md),
[`LogisticNormalFixedMixture`](https://openpharma.github.io/crmPack/reference/LogisticNormalFixedMixture-class.md),
[`LogisticLogNormalMixture`](https://openpharma.github.io/crmPack/reference/LogisticLogNormalMixture-class.md).

## Examples

``` r
my_model <- LogisticNormalMixture(
  comp1 = ModelParamsNormal(
    mean = c(-0.85, 1),
    cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2)
  ),
  comp2 = ModelParamsNormal(
    mean = c(1, 1.5),
    cov = matrix(c(1.2, -0.45, -0.45, 0.6), nrow = 2)
  ),
  weightpar = c(a = 1, b = 1),
  ref_dose = 50
)
```
