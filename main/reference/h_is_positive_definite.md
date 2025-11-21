# Testing Matrix for Positive Definiteness

**\[experimental\]**

This helper function checks whether a given numerical matrix `x` is a
positive-definite square matrix of a given size, without any missing
values. This function is used to test if a given matrix is a covariance
matrix, since every symmetric positive semi-definite matrix is a
covariance matrix.

## Usage

``` r
h_is_positive_definite(x, size = 2, tol = 1e-08)
```

## Arguments

- x:

  (`matrix`)  
  a matrix to be checked.

- size:

  (`integer`)  
  a size of the square matrix `x` to be checked against for.

- tol:

  (`number`)  
  a given tolerance number used to check whether an eigenvalue is
  positive or not. An eigenvalue is considered as positive if and only
  if it is greater than the `tol`.

## Value

`TRUE` if a given matrix is a positive-definite, `FALSE` otherwise.

## Details

The positive definiteness test implemented in this function is based on
the following characterization valid for real matrices:
`A symmetric matrix is positive-definite if and only if all of its eigenvalues are positive.`
In this function an eigenvalue is considered as positive if and only if
it is greater than the `tol`.
