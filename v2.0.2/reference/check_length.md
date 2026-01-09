# Check if vectors are of compatible lengths

**\[stable\]**

Two vectors are of compatible size if and only if:  

1.  At least one vector has size 1  

2.  or both vectors are of the same size.  

## Usage

``` r
check_length(x, len)

assert_length(x, len, .var.name = checkmate::vname(x), add = NULL)

test_length(x, len)
```

## Arguments

- x:

  (`any`)  
  the first vector, any object for which
  [`length()`](https://rdrr.io/r/base/length.html) function is defined.

- len:

  (`count`)  
  the length of the second vector.

- .var.name:

  \[`character(1)`\]  
  Name of the checked object to print in assertions. Defaults to the
  heuristic implemented in
  [`vname`](https://mllg.github.io/checkmate/reference/vname.html).

- add:

  \[`AssertCollection`\]  
  Collection to store assertion messages. See
  [`AssertCollection`](https://mllg.github.io/checkmate/reference/AssertCollection.html).

## Value

`TRUE` if successful, otherwise a string with the error message.

## See also

[`assertions`](https://openpharma.github.io/crmPack/reference/assertions.md)
for more details.

## Examples

``` r
check_length(1:5, 1)
#> [1] TRUE
check_length(1:5, 6)
#> [1] "x is of length 5 which is not allowed; the allowed lengths are: 1 or 6"
check_length(1:5, 5)
#> [1] TRUE
check_length(10, 1)
#> [1] TRUE
check_length(10, 9)
#> [1] TRUE
```
