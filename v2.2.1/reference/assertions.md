# Additional Assertions for `checkmate`

**\[experimental\]**

We provide additional assertion functions that can be used together with
the `checkmate` functions. These are described in individual help pages
linked below.

## Value

Depending on the function prefix.

- `assert_` functions return the object invisibly if successful, and
  otherwise throw an error message.

- `check_` functions return `TRUE` if successful, otherwise a string
  with the error message.

- `test_` functions just return `TRUE` or `FALSE`.

## See also

[`assert_probabilities()`](https://docs.crmpack.org/reference/check_probabilities.md),
[`assert_probability()`](https://docs.crmpack.org/reference/check_probability.md),
[`assert_probability_range()`](https://docs.crmpack.org/reference/check_probability_range.md),
[`assert_length()`](https://docs.crmpack.org/reference/check_length.md).
