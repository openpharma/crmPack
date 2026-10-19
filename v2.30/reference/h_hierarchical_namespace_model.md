# Add an Arm Suffix to a Hierarchical Model Fragment

**\[experimental\]**

Namespaces one arm-specific model fragment using code supplied by the
arm model class itself.

## Usage

``` r
h_hierarchical_namespace_model(model, arm_name, slot_name)
```

## Arguments

- model:

  (`GeneralModel`)\
  arm-specific model object.

- arm_name:

  (`string`)\
  hierarchical arm name.

- slot_name:

  (`string`)\
  model-function slot to namespace.

## Value

A function containing the namespaced JAGS model fragment.
