# Helpers for stripping expressions of `covr`-inserted trace code

Workarounds to allow the package to continue to work while running
`covr` with minimal changes to the package code.

## Usage

``` r
h_covr_active()

h_covr_detrace(expr)

h_is_covr_trace(expr)

h_covr_detrace_call(expr)
```

## Arguments

- expr:

  (`language`)  
  an R expression or call to test or strip of `covr` trace counters.

## Value

A logical value or transformed expression with calls to `covr:::count`
removed.

## Details

When using `covr`, the source code for package objects are modified to
add callbacks for each expression to log its execution. Given an
arbitrary expression, such as:

    expr

The code will be modified before executing any package code to look
like:

    if (TRUE) {
      covr:::count("file.R:1:2:3:4:5:6:7:8")
      expr
    }

These functions are used for stripping expressions of this code so that
the package continues to work as intended while running tests as part of
running `covr` to calculate package coverage.

This method is non-exhaustive, covering only a subset of `covr`'s
tracing behaviors necessary for this package.

## Functions

- `h_covr_active()`: Determine whether `covr` is currently running

- `h_covr_detrace()`: Remove `covr` traces from an expression

- `h_is_covr_trace()`: Determine whether the current expression is a
  `covr`-modified expression

- `h_covr_detrace_call()`: Extract the original expression from a
  `covr`-modified expression
