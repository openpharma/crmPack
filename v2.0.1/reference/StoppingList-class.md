# `StoppingList`

**\[stable\]**

`StoppingList` is the class for testing a stopping rule that consists of
many single stopping rules that are in turn the objects of class
`Stopping`. The `summary` slot stores a function that takes a logical
vector of the size of `stop_list` and returns a single logical value.
For example, if the function `all` is specified as a `summary` function,
then that all stopping rules defined in `stop_list` must be satisfied in
order the result of this rule to be `TRUE`.

## Usage

``` r
StoppingList(stop_list, summary)

.DefaultStoppingList()
```

## Arguments

- stop_list:

  (`list`)  
  see slot definition.

- summary:

  (`function`)  
  see slot definition.

## Slots

- `stop_list`:

  (`list`)  
  list of stopping rules.

- `summary`:

  (`function`)  
  a summary function to combine the results of the stopping rules into a
  single result.

## Note

Typically, end users will not use the `.DefaultStoppingList()` function.

## Examples

``` r
# Define some stopping rules.
my_stopping1 <- StoppingMinCohorts(nCohorts = 3)
my_stopping2 <- StoppingTargetProb(target = c(0.2, 0.35), prob = 0.5)
my_stopping3 <- StoppingMinPatients(nPatients = 20)

# Create a list of stopping rules (of class `StoppingList`) which will then be
# summarized (in this specific example) with the `any` function, meaning that
# the study would be stopped if any of the single stopping rules is `TRUE`.
my_stopping <- StoppingList(
  stop_list = c(my_stopping1, my_stopping2, my_stopping3),
  summary = any
)
```
