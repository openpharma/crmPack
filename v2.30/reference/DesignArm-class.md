# `DesignArm`

**\[experimental\]**

`DesignArm` is a light-weight wrapper around `Design` object for use in
a
[`HierarchicalDesign`](https://docs.crmpack.org/reference/HierarchicalDesign-class.md).
Use `DesignArm()` for active arms and `HistoricalArm()` for
non-enrolling historical arms.

## Usage

``` r
DesignArm(name, design, borrow = TRUE, open_when = NoArmCondition())

HistoricalArm(name, data, model, borrow = TRUE)

.DefaultDesignArm()
```

## Arguments

- name:

  (`string`)\
  see slot definition.

- design:

  (`Design`)\
  see slot definition.

- borrow:

  (`flag`)\
  see slot definition.

- open_when:

  (`ArmCondition`)\
  see slot definition.

- data:

  (`Data` or `DataCombo`)\
  arm data for historical arms.

- model:

  (`GeneralModel` or `TwoDrugsCombo`)\
  arm model for historical arms.

## Slots

- `name`:

  (`string`)\
  the name of the arm.

- `active`:

  (`flag`)\
  whether the arm is enrolling or not (historical).

- `borrow`:

  (`flag`)\
  whether this arm may borrow information from other arms when making
  dose escalation decisions. Only relevant if `active = TRUE`.

- `open_when`:

  (`ArmCondition`)\
  the condition that must be satisfied before this arm opens for
  enrollment.

- `design`:

  (`Design`)\
  the design object for this arm.

## Note

Typically, end users will not use the `.DefaultDesignArm()` function.

## Examples

``` r
design <- .DefaultDesign()
design_arm <- DesignArm(
  name = "Arm A",
  design = design
)
```
