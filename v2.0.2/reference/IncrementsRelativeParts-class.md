# `IncrementsRelativeParts`

**\[stable\]**

`IncrementsRelativeParts` is the class for increments control based on
relative differences in intervals, with special rules for part 1 and
beginning of part 2.

## Usage

``` r
IncrementsRelativeParts(dlt_start, clean_start, ...)

.DefaultIncrementsRelativeParts()
```

## Arguments

- dlt_start:

  (`count`)  
  see slot definition.

- clean_start:

  (`count`)  
  see slot definition.

- ...:

  Arguments passed on to
  [`IncrementsRelative`](https://openpharma.github.io/crmPack/reference/IncrementsRelative-class.md)

  `intervals`

  :   (`numeric`)  
      see slot definition.

  `increments`

  :   (`numeric`)  
      see slot definition.

## Details

This class works only in conjunction with
[`DataParts`](https://openpharma.github.io/crmPack/reference/DataParts-class.md)
objects. If part 2 will just be started in the next cohort, then the
next maximum dose will be either `dlt_start` (e.g. -1) shift of the last
part 1 dose in case of a DLT in part 1, or `clean_start` shift (e.g. -1)
in case of no DLTs in part 1, given that `clean_start <= 0` (see
description of `clean_start` slot for more details). If part 1 will
still be on in the next cohort, then the next dose level will be the
next higher dose level in the `part1Ladder` slot of the data object. If
part 2 has been started before, the usual relative increment rules
apply, see
[`IncrementsRelative`](https://openpharma.github.io/crmPack/reference/IncrementsRelative-class.md).

## Slots

- `dlt_start`:

  (`integer`)  
  a scalar, the dose level increment for starting part 2 in case of at
  least one DLT event in part 1.

- `clean_start`:

  (`integer`)  
  a scalar, the dose level increment for starting part 2 in case of no
  DLTs in part 1. If `clean_start <= 0`, then the part 1 ladder will be
  used to find the maximum next dose. Otherwise, the relative increment
  rules will be applied to find the next maximum dose level.

## Note

We require that `clean_start >= dlt_start`. However, this precondition
is not a prerequisite for any function (except of the class' validation
function) that works with objects of this class. It is rather motivated
by the semantics. That is, if we observe a DLT in part 1, we cannot be
more aggressive than in case of a clean part 1 without DLT.

Typically, end users will not use the
`.DefaultIncrementsRelativeParts()` function.

## Examples

``` r
my_increments <- IncrementsRelativeParts(dlt_start = 0, clean_start = 1)
```
