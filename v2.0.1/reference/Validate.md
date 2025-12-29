# `Validate`

**\[stable\]**

The `Validate` class is a Reference Class to help programming validation
for new S4 classes.

## Details

Starting from an empty `msg` vector, with each check that is returning
`FALSE` the vector gets a new element - the string explaining the
failure of the validation.

## Fields

- `msg`:

  (`character`)  
  the cumulative messages.

## Methods

- `check(test, string = "")`:

  Check whether the `test` is `TRUE`; if so, return `NULL`. Otherwise,
  add the `string` message into the cumulative messages vector `msg`.

- `result()`:

  Return either cumulative messages vector `msg` (which contains the
  error messages from all the checks), or `NULL`, if `msg` is empty
  (i.e. all the checks were successful).
