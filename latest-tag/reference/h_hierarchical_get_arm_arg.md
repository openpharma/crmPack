# Select an Arm-Specific Argument

**\[experimental\]**

Returns the arm-specific value from a named list, or returns the
original value unchanged when a single shared value was supplied.

## Usage

``` r
h_hierarchical_get_arm_arg(arg, arm_name)
```

## Arguments

- arg:

  (`ANY`)\
  either a shared argument or a named list of arguments.

- arm_name:

  (`string`)\
  hierarchical arm name to select.

## Value

The selected arm-specific argument, or `arg` unchanged.
