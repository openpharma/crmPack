# Getting the Slots from a S4 Object

**\[experimental\]**

This helper function extracts requested slots from the S4 class object.
It is a simple wrapper of
[`methods::slot()`](https://rdrr.io/r/methods/slot.html) function.

## Usage

``` r
h_slots(object, names, simplify = FALSE)
```

## Arguments

- object:

  (`S4`)  
  an object from a formally defined S4 class.

- names:

  (`character`)  
  a vector with names of slots to be fetched. This function assumes that
  for every element in `names`, there exists a slot of the same name in
  the `object`.

- simplify:

  (`flag`)  
  should an output be simplified? This has an effect if and only if a
  single slot is about to be extracted, i.e. `names` is just a single
  string.

## Value

`list` with the slots extracted from `object` according to `names`, or
single slot if simplification is required and possible.
