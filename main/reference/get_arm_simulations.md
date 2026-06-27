# Extract Arm-Level Simulations from Hierarchical Simulations

**\[experimental\]**

Rebuilds the simulation object for one hierarchical arm, so that summary
calculations can delegate to the existing
[`Simulations`](https://docs.crmpack.org/reference/Simulations-class.md)
or
[`ComboSimulations`](https://docs.crmpack.org/reference/ComboSimulations-class.md)
methods.

## Usage

``` r
get_arm_simulations(object, arm_name)
```

## Arguments

- object:

  (`HierarchicalSimulations`)\
  hierarchical simulation results.

- arm_name:

  (`string`)\
  hierarchical arm name to extract.

## Value

A
[`Simulations`](https://docs.crmpack.org/reference/Simulations-class.md)
object for a single-agent arm or a
[`ComboSimulations`](https://docs.crmpack.org/reference/ComboSimulations-class.md)
object for a combination arm.

## Examples

``` r
arm_data <- Data(
  x = c(10, 20),
  y = c(0L, 1L),
  doseGrid = c(10, 20),
  ID = 1L:2L,
  cohort = 1L:2L
)
hierarchical_sims <- HierarchicalSimulations(
  data = list(HierarchicalData(arm_a = arm_data)),
  doses = list(list(arm_a = 20)),
  samples = list(HierarchicalSamples(
    data = list(alpha0_arm_a = c(-3, -2)),
    options = McmcOptions(burnin = 1, step = 1, samples = 2),
    arm_samples = list(arm_a = c(alpha0 = "alpha0_arm_a"))
  )),
  fit = list(list(arm_a = data.frame(
    middle = c(0.1, 0.3),
    lower = c(0.05, 0.2),
    upper = c(0.2, 0.4)
  ))),
  stop_reasons = list(list(arm_a = "Minimum patients")),
  stop_report = list(list(arm_a = c(`Minimum patients` = TRUE))),
  additional_stats = list(list(arm_a = list())),
  seed = 123L
)

get_arm_simulations(hierarchical_sims, "arm_a")
#> An object of class 'Simulations' containing 1 simulated trials.
#> Please use 'summary()' to obtain more information.
```
