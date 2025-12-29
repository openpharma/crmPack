# Checking Formals of a Function

**\[experimental\]**

This helper function checks whether a given function `fun` has required
or allowed arguments. The argument check is based only on the names of
the arguments. No any further logic is verified here.

## Usage

``` r
h_check_fun_formals(fun, mandatory = NULL, allowed = NULL)
```

## Arguments

- fun:

  (`function`)  
  a function name whose argument names will be checked.

- mandatory:

  (`character` or `NULL`)  
  the names of the arguments which must be present in `fun`. If
  `mandatory` is specified as `NULL` (default) this requirement is
  ignored.

- allowed:

  (`character` or `NULL`)  
  the names of the arguments which are allowed in `fun`. Names that do
  not belong to `allowed` are simply not allowed. The `allowed`
  parameter is independent from the `mandatory`, in a sense that if
  `mandatory` is specified as a `character` vector, it does not have to
  be repeated in `allowed`. If `allowed` is specified as `NULL`
  (default), then it means that there must be no any arguments in `fun`
  (except these ones which are specified in `mandatory`).
