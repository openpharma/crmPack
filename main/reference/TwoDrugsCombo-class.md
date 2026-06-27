# `TwoDrugsCombo`

**\[experimental\]**

`TwoDrugsCombo` is the class for a two-drug combination regression model
with fixed priors for the two single-agent dose-toxicity models and an
additional interaction parameter.

## Usage

``` r
TwoDrugsCombo(single_models, gamma = 0, tau = 1, log_normal_eta = FALSE)

.DefaultTwoDrugsCombo()
```

## Arguments

- single_models:

  (`list`) named list of length 2 with compatible single-agent
  [`GeneralModel`](https://docs.crmpack.org/reference/GeneralModel-class.md)
  objects, one per drug.

- gamma:

  (`number`) prior mean parameter for the interaction term.

- tau:

  (`number`) prior precision parameter for the interaction term.

- log_normal_eta:

  (`flag`) should the interaction term use a log-normal prior?

## Details

Let \\p(x_1, x_2)\\ be the probability of DLT at the dose combination
\\(x_1, x_2)\\. The model combines two single-agent models with an
interaction term: \$\$\textrm{odds}(p(x_1, x_2)) =
\textrm{odds}(p_0(x_1, x_2)) \* \exp\left(\eta \* I(x_1,
x_2)\right),\$\$ where \\p_0(x_1, x_2) = 1 - (1 - p_1(x_1))(1 -
p_2(x_2))\\ and each single-agent probability follows a model
\\p_j(x_j)\\. The normalized dose \\\tilde{x}\_j\\ is extracted from the
single-agent model's dose covariate, e.g. \\x_j / x_j^{\*}\\, \\x_j -
x_j^{\*}\\, or \\x_j\\. The interaction parameter \\\eta\\ has either a
normal prior or, if `log_normal_eta = TRUE`, a log-normal prior.

## Slots

- `single_models`:

  (`list`) named list of length 2 containing single-agent
  [`GeneralModel`](https://docs.crmpack.org/reference/GeneralModel-class.md)
  objects, one per drug. Each model must use `nObs`, `y`, and `x` as
  data inputs and contain a Bernoulli likelihood for `y` in its
  `datamodel`.

- `ref_dose`:

  (`numeric`) optional reference doses extracted from `single_models`,
  if provided.

- `drug_names`:

  (`character`) the names of the two drugs.

- `gamma`:

  (`numeric`) prior mean parameter for the interaction term.

- `tau`:

  (`numeric`) prior precision parameter for the interaction term.

- `log_normal_eta`:

  (`flag`) should the interaction term use a log-normal prior?

## Note

Typically, end users will not use the `.DefaultTwoDrugsCombo()`
function.

## See also

[`LogisticLogNormal`](https://docs.crmpack.org/reference/LogisticLogNormal-class.md),
[`DataCombo`](https://docs.crmpack.org/reference/DataCombo-class.md).

## Examples

``` r
my_model <- TwoDrugsCombo(
  single_models = list(
    drug1 = LogisticLogNormal(
      mean = c(-0.85, 1),
      cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
      ref_dose = 10
    ),
    drug2 = LogisticLogNormal(
      mean = c(-0.7, 0.8),
      cov = matrix(c(1.1, -0.3, -0.3, 0.9), nrow = 2),
      ref_dose = 20
    )
  ),
  gamma = 0,
  tau = 1
)

my_model
#> An object of class "TwoDrugsCombo"
#> Slot "single_models":
#> $drug1
#> An object of class "LogisticLogNormal"
#> Slot "params":
#> An object of class "ModelParamsNormal"
#> Slot "mean":
#> [1] -0.85  1.00
#> 
#> Slot "cov":
#>      [,1] [,2]
#> [1,]  1.0 -0.5
#> [2,] -0.5  1.0
#> 
#> Slot "prec":
#>           [,1]      [,2]
#> [1,] 1.3333333 0.6666667
#> [2,] 0.6666667 1.3333333
#> 
#> 
#> Slot "ref_dose":
#> An object of class "positive_number"
#> [1] 10
#> 
#> Slot "datamodel":
#> function() {
#>       for (i in 1:nObs) {
#>         logit(p[i]) <- alpha0 + alpha1 * log(x[i] / ref_dose)
#>         y[i] ~ dbern(p[i])
#>       }
#>     }
#> <bytecode: 0x5594b4a7d1b0>
#> <environment: 0x5594bd12a2a0>
#> 
#> Slot "priormodel":
#> function() {
#>       theta ~ dmnorm(mean, prec)
#>       alpha0 <- theta[1]
#>       alpha1 <- exp(theta[2])
#>     }
#> <bytecode: 0x5594b5339f28>
#> <environment: 0x5594bd12a070>
#> 
#> Slot "modelspecs":
#> function(from_prior) {
#>       ms <- list(mean = params@mean, prec = params@prec)
#>       if (!from_prior) {
#>         ms$ref_dose <- ref_dose
#>       }
#>       ms
#>     }
#> <bytecode: 0x5594b5528000>
#> <environment: 0x5594bd12a070>
#> 
#> Slot "init":
#> function() {
#>       list(theta = c(0, 1))
#>     }
#> <bytecode: 0x5594b55ac560>
#> <environment: 0x5594bd12a070>
#> 
#> Slot "datanames":
#> [1] "nObs" "y"    "x"   
#> 
#> Slot "datanames_prior":
#> character(0)
#> 
#> Slot "sample":
#> [1] "alpha0" "alpha1"
#> 
#> 
#> $drug2
#> An object of class "LogisticLogNormal"
#> Slot "params":
#> An object of class "ModelParamsNormal"
#> Slot "mean":
#> [1] -0.7  0.8
#> 
#> Slot "cov":
#>      [,1] [,2]
#> [1,]  1.1 -0.3
#> [2,] -0.3  0.9
#> 
#> Slot "prec":
#>           [,1]      [,2]
#> [1,] 1.0000000 0.3333333
#> [2,] 0.3333333 1.2222222
#> 
#> 
#> Slot "ref_dose":
#> An object of class "positive_number"
#> [1] 20
#> 
#> Slot "datamodel":
#> function() {
#>       for (i in 1:nObs) {
#>         logit(p[i]) <- alpha0 + alpha1 * log(x[i] / ref_dose)
#>         y[i] ~ dbern(p[i])
#>       }
#>     }
#> <bytecode: 0x5594b4a7d1b0>
#> <environment: 0x5594bcdaecd0>
#> 
#> Slot "priormodel":
#> function() {
#>       theta ~ dmnorm(mean, prec)
#>       alpha0 <- theta[1]
#>       alpha1 <- exp(theta[2])
#>     }
#> <bytecode: 0x5594b5339f28>
#> <environment: 0x5594bcdaeaa0>
#> 
#> Slot "modelspecs":
#> function(from_prior) {
#>       ms <- list(mean = params@mean, prec = params@prec)
#>       if (!from_prior) {
#>         ms$ref_dose <- ref_dose
#>       }
#>       ms
#>     }
#> <bytecode: 0x5594b5528000>
#> <environment: 0x5594bcdaeaa0>
#> 
#> Slot "init":
#> function() {
#>       list(theta = c(0, 1))
#>     }
#> <bytecode: 0x5594b55ac560>
#> <environment: 0x5594bcdaeaa0>
#> 
#> Slot "datanames":
#> [1] "nObs" "y"    "x"   
#> 
#> Slot "datanames_prior":
#> character(0)
#> 
#> Slot "sample":
#> [1] "alpha0" "alpha1"
#> 
#> 
#> 
#> Slot "ref_dose":
#> drug1 drug2 
#>    10    20 
#> 
#> Slot "drug_names":
#> [1] "drug1" "drug2"
#> 
#> Slot "gamma":
#> [1] 0
#> 
#> Slot "tau":
#> [1] 1
#> 
#> Slot "log_normal_eta":
#> [1] FALSE
#> 
#> Slot "datamodel":
#> function () 
#> {
#>     for (i in 1:nObs) {
#>         x_drug1[i] <- x[i, 1L]
#>     }
#>     for (i in 1:nObs) {
#>         logit(p_drug1[i]) <- alpha0_drug1 + alpha1_drug1 * log(x_drug1[i]/ref_dose_drug1)
#>         p_single[i, 1L] <- p_drug1[i]
#>     }
#>     for (i in 1:nObs) {
#>         x_drug2[i] <- x[i, 2L]
#>     }
#>     for (i in 1:nObs) {
#>         logit(p_drug2[i]) <- alpha0_drug2 + alpha1_drug2 * log(x_drug2[i]/ref_dose_drug2)
#>         p_single[i, 2L] <- p_drug2[i]
#>     }
#>     for (i in 1:nObs) {
#>         combo_interaction[i] <- x_drug1[i]/ref_dose_drug1 * (x_drug2[i]/ref_dose_drug2)
#>     }
#>     for (i in 1:nObs) {
#>         p0[i] <- p_single[i, 1] + p_single[i, 2] - p_single[i, 
#>             1] * p_single[i, 2]
#>         logit(p[i]) <- log(p0[i]/(1 - p0[i])) + eta * combo_interaction[i]
#>         y[i] ~ dbern(p[i])
#>     }
#> }
#> <environment: 0x5594bf615bd0>
#> 
#> Slot "priormodel":
#> function () 
#> {
#>     theta_drug1 ~ dmnorm(mean_drug1, prec_drug1)
#>     alpha0_drug1 <- theta_drug1[1]
#>     alpha1_drug1 <- exp(theta_drug1[2])
#>     theta_drug2 ~ dmnorm(mean_drug2, prec_drug2)
#>     alpha0_drug2 <- theta_drug2[1]
#>     alpha1_drug2 <- exp(theta_drug2[2])
#>     alpha0[1L] <- alpha0_drug1
#>     alpha0[2L] <- alpha0_drug2
#>     alpha1[1L] <- alpha1_drug1
#>     alpha1[2L] <- alpha1_drug2
#>     eta ~ dnorm(eta_gamma, eta_tau)
#> }
#> <environment: 0x5594bd12a070>
#> 
#> Slot "modelspecs":
#> function(from_prior) {
#>       specs_name <- if (from_prior) "prior_specs" else "full_specs"
#>       ms <- c(
#>         unlist(lapply(single_model_parts, "[[", specs_name), recursive = FALSE),
#>         list(eta_gamma = gamma, eta_tau = tau)
#>       )
#>       ms
#>     }
#> <bytecode: 0x5594ace210a8>
#> <environment: 0x5594bd1275f0>
#> 
#> Slot "init":
#> function() {
#>         c(
#>           unlist(lapply(single_model_parts, "[[", "inits"), recursive = FALSE),
#>           list(eta = gamma)
#>         )
#>       }
#> <bytecode: 0x5594adfc3b28>
#> <environment: 0x5594bd1275f0>
#> 
#> Slot "datanames":
#> [1] "nObs" "y"    "x"   
#> 
#> Slot "datanames_prior":
#> character(0)
#> 
#> Slot "sample":
#> [1] "alpha0" "alpha1" "eta"   
#> 
```
