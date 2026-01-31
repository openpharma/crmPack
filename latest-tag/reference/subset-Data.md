# Subsetting Operator for the Data Class

**\[stable\]**

Subset observations (patients) from a
[`Data`](https://openpharma.github.io/crmPack/reference/Data-class.md)
object using numeric or logical indexing.

## Usage

``` r
# S4 method for class 'Data,numeric,missing,missing'
x[i]

# S4 method for class 'Data,logical,missing,missing'
x[i]
```

## Arguments

- x:

  (`Data`)  
  what to subset.

- i:

  (`integer` or `logical`)  
  indices or logical vector for subsetting observations.

## Value

A [`Data`](https://openpharma.github.io/crmPack/reference/Data-class.md)
object with the selected observations.
