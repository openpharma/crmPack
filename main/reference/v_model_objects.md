# Internal Helper Functions for Validation of [`GeneralModel`](https://docs.crmpack.org/reference/GeneralModel-class.md) and [`ModelPseudo`](https://docs.crmpack.org/reference/ModelPseudo-class.md) Objects

**\[stable\]**

These functions are only used internally to validate the format of an
input
[`GeneralModel`](https://docs.crmpack.org/reference/GeneralModel-class.md)
and
[`ModelPseudo`](https://docs.crmpack.org/reference/ModelPseudo-class.md)
or inherited classes and therefore are not exported.

## Usage

``` r
v_general_model(object)

v_model_logistic_kadane(object)

v_model_logistic_kadane_beta_gamma(object)

v_model_logistic_normal_mix(object)

v_model_logistic_normal_fixed_mix(object)

v_model_logistic_log_normal_mix(object)

v_model_dual_endpoint(object)

v_model_dual_endpoint_rw(object)

v_model_dual_endpoint_beta(object)

v_model_dual_endpoint_emax(object)

v_model_logistic_indep_beta(object)

v_model_eff_log_log(object)

v_model_eff_flexi(object)

v_model_da_logistic_log_normal(object)

v_model_tite_logistic_log_normal(object)

v_model_one_par_exp_normal_prior(object)

v_model_one_par_exp_prior(object)

v_logisticlognormalordinal(object)
```

## Arguments

- object:

  (`GeneralModel`) or (`ModelPseudo`)\
  object to validate.

## Value

A `character` vector with the validation failure messages, or `TRUE` in
case validation passes.

## Functions

- `v_general_model()`: validates that the names of the arguments in
  `init` function are included in `datanames` or `datanames_prior`
  slots.

- `v_model_logistic_kadane()`: validates that the logistic Kadane model
  parameters are valid.

- `v_model_logistic_kadane_beta_gamma()`: validates that the logistic
  Kadane model parameters with a beta and gamma prior are valid.

- `v_model_logistic_normal_mix()`: validates that `weightpar` is valid.

- `v_model_logistic_normal_fixed_mix()`: validates that `component` is a
  list with valid `ModelParamsNormal` objects as well as `weights` are
  correct.

- `v_model_logistic_log_normal_mix()`: validates that `share_weight`
  represents probability.

- `v_model_dual_endpoint()`: validates that
  [`DualEndpoint`](https://docs.crmpack.org/reference/DualEndpoint-class.md)
  class slots are valid.

- `v_model_dual_endpoint_rw()`: validates that
  [`DualEndpointRW`](https://docs.crmpack.org/reference/DualEndpointRW-class.md)
  class slots are valid.

- `v_model_dual_endpoint_beta()`: validates that
  [`DualEndpointBeta`](https://docs.crmpack.org/reference/DualEndpointBeta-class.md)
  class slots are valid.

- `v_model_dual_endpoint_emax()`: validates that
  [`DualEndpointEmax`](https://docs.crmpack.org/reference/DualEndpointEmax-class.md)
  class slots are valid.

- `v_model_logistic_indep_beta()`: validates that
  [`LogisticIndepBeta`](https://docs.crmpack.org/reference/LogisticIndepBeta-class.md)
  class slots are valid.

- `v_model_eff_log_log()`: validates that
  [`Effloglog`](https://docs.crmpack.org/reference/Effloglog-class.md)
  class slots are valid.

- `v_model_eff_flexi()`: validates that
  [`EffFlexi`](https://docs.crmpack.org/reference/EffFlexi-class.md)
  class slots are valid.

- `v_model_da_logistic_log_normal()`: validates that
  [`DALogisticLogNormal`](https://docs.crmpack.org/reference/DALogisticLogNormal-class.md)
  class slots are valid.

- `v_model_tite_logistic_log_normal()`: validates that
  [`TITELogisticLogNormal`](https://docs.crmpack.org/reference/TITELogisticLogNormal-class.md)
  class slots are valid.

- `v_model_one_par_exp_normal_prior()`: validates that
  [`OneParLogNormalPrior`](https://docs.crmpack.org/reference/OneParLogNormalPrior-class.md)
  class slots are valid.

- `v_model_one_par_exp_prior()`: validates that
  [`OneParExpPrior`](https://docs.crmpack.org/reference/OneParExpPrior-class.md)
  class slots are valid.

- `v_logisticlognormalordinal()`: confirms that cov is diagonal
