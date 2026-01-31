# Logical OR Operator for Opening Objects

**\[experimental\]**

Combines two
[`Opening`](https://openpharma.github.io/crmPack/reference/Opening-class.md)
objects with OR logic using the `|` operator. This creates an
[`OpeningAny`](https://openpharma.github.io/crmPack/reference/OpeningAny-class.md)
object.

## Usage

``` r
# S4 method for class 'Opening,Opening'
e1 | e2
```

## Arguments

- e1:

  (`Opening`) the first opening object.

- e2:

  (`Opening`) the second opening object.

## Value

An
[`OpeningAny`](https://openpharma.github.io/crmPack/reference/OpeningAny-class.md)
object combining `e1` and `e2`.

## See also

[`OpeningAny`](https://openpharma.github.io/crmPack/reference/OpeningAny-class.md)
for more details.
