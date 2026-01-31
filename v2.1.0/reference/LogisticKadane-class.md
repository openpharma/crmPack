# `LogisticKadane`

**\[stable\]**

`LogisticKadane` is the class for the logistic model in the
parametrization of Kadane et al. (1980) .

## Usage

``` r
LogisticKadane(theta, xmin, xmax)

.DefaultLogisticKadane()
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

## Details

Let `rho0 = p(xmin)` be the probability of a DLT at the minimum dose
`xmin`, and let `gamma` be the dose with target toxicity probability
`theta`, i.e. \\p(gamma) = theta\\. Then it can easily be shown that the
logistic regression model has intercept \$\$\[gamma \* logit(rho0) -
xmin \* logit(theta)\] / \[gamma - xmin\]\$\$ and slope
\$\$\[logit(theta) - logit(rho0)\] / \[gamma - xmin\].\$\$

The priors are \$\$gamma ~ Unif(xmin, xmax).\$\$ and \$\$rho0 ~ Unif(0,
theta).\$\$

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

## Note

The slots of this class, required for creating the model, are the target
toxicity, as well as the minimum and maximum of the dose range. Note
that these can be different from the minimum and maximum of the dose
grid in the data later on.

Typically, end-users will not use the `.DefaultLogisticKadane()`
function.

## References

Kadane JB, Dickey JM, Winkler RL, Smith WS, Peters SC (1980).
“Interactive Elicitation of Opinion for a Normal Linear Model.” *Journal
of the American Statistical Association*, **75**(372), 845–854. ISSN
01621459, 1537274X,
[doi:10.2307/2287171](https://doi.org/10.2307/2287171) ,
<http://www.jstor.org/stable/2287171>.

## See also

[`ModelLogNormal`](https://openpharma.github.io/crmPack/reference/ModelLogNormal-class.md)

## Examples

``` r
my_model <- LogisticKadane(theta = 0.33, xmin = 1, xmax = 200)
```
