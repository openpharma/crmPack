# Logical AND Operator for Opening Objects

**\[experimental\]**

Combines two
[`Opening`](https://openpharma.github.io/crmPack/reference/Opening-class.md)
objects with AND logic using the `&` operator. This creates an
[`OpeningAll`](https://openpharma.github.io/crmPack/reference/OpeningAll-class.md)
object.

## Usage

``` r
# S4 method for class 'Opening,Opening'
e1 & e2
```

## Arguments

- e1:

  (`Opening`) the first opening object.

- e2:

  (`Opening`) the second opening object.

## Value

An
[`OpeningAll`](https://openpharma.github.io/crmPack/reference/OpeningAll-class.md)
object combining `e1` and `e2`.

## See also

[`OpeningAll`](https://openpharma.github.io/crmPack/reference/OpeningAll-class.md)
for more details.
