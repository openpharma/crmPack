# `LogisticKadaneBetaGamma`

**\[experimental\]**

`LogisticKadaneBetaGamma` is the class for the logistic model in the
parametrization of Kadane et al. (1980) , using a beta and a gamma
distribution as the model priors.

## Usage

``` r
LogisticKadaneBetaGamma(theta, xmin, xmax, alpha, beta, shape, rate)

.DefaultLogisticKadaneBetaGamma()
```

## Arguments

- theta:

  (`proportion`)  
  the target toxicity probability.

- xmin:

  (`number`)  
  the minimum of the dose range.

- xmax:

  (`number`)  
  the maximum of the dose range.

- alpha:

  (`number`)  
  the first shape parameter of the Beta prior distribution
  `rho0 = p(xmin)` the probability of a DLT at the minimum dose `xmin`.

- beta:

  (`number`)  
  the second shape parameter of the Beta prior distribution
  `rho0 = p(xmin)` the probability of a DLT at the minimum dose `xmin`.

- shape:

  (`number`)  
  the shape parameter of the Gamma prior distribution `gamma` the dose
  with target toxicity probability `theta`.

- rate:

  (`number`)  
  the rate parameter of the Gamma prior distribution `gamma` the dose
  with target toxicity probability `theta`.

## Details

Let `rho0 = p(xmin)` be the probability of a DLT at the minimum dose
`xmin`, and let `gamma` be the dose with target toxicity probability
`theta`, i.e. \\p(gamma) = theta\\. Then it can easily be shown that the
logistic regression model has intercept \$\$\[gamma \* logit(rho0) -
xmin \* logit(theta)\] / \[gamma - xmin\]\$\$ and slope
\$\$\[logit(theta) - logit(rho0)\] / \[gamma - xmin\].\$\$

The prior for `gamma`, is \$\$gamma ~ Gamma(shape, rate).\$\$. The prior
for `rho0 = p(xmin)`, is \$\$rho0 ~ Beta(alpha, beta).\$\$

## Slots

- `theta`:

  (`proportion`)  
  the target toxicity probability.

- `xmin`:

  (`number`)  
  the minimum of the dose range.

- `xmax`:

  (`number`)  
  the maximum of the dose range.

- `alpha`:

  (`number`)  
  the first shape parameter of the Beta prior distribution of
  `rho0 = p(xmin)` the probability of a DLT at the minimum dose `xmin`.

- `beta`:

  (`number`)  
  the second shape parameter of the Beta prior distribution of
  `rho0 = p(xmin)` the probability of a DLT at the minimum dose `xmin`.

- `shape`:

  (`number`)  
  the shape parameter of the Gamma prior distribution of `gamma` the
  dose with target toxicity probability `theta`.

- `rate`:

  (`number`)  
  the rate parameter of the Gamma prior distribution of `gamma` the dose
  with target toxicity probability `theta`.

## Note

The slots of this class, required for creating the model, are the same
as in the `LogisticKadane` class. In addition, the shape parameters of
the Beta prior distribution of `rho0` and the shape and rate parameters
of the Gamma prior distribution of `gamma`, are required for creating
the prior model.

Typically, end users will not use the `.Default()` function.

## References

Kadane JB, Dickey JM, Winkler RL, Smith WS, Peters SC (1980).
“Interactive Elicitation of Opinion for a Normal Linear Model.” *Journal
of the American Statistical Association*, **75**(372), 845–854. ISSN
01621459, 1537274X,
[doi:10.2307/2287171](https://doi.org/10.2307/2287171) ,
<http://www.jstor.org/stable/2287171>.

## See also

[`ModelLogNormal`](https://openpharma.github.io/crmPack/reference/ModelLogNormal-class.md),
[`LogisticKadane`](https://openpharma.github.io/crmPack/reference/LogisticKadane-class.md).

## Examples

``` r
my_model <- LogisticKadaneBetaGamma(
  theta = 0.3,
  xmin = 0,
  xmax = 7,
  alpha = 1,
  beta = 19,
  shape = 0.5625,
  rate = 0.125
)
```
