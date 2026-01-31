# `ModelEff`

**\[stable\]**

`ModelEff` is the parent class for efficacy models using pseudo data
prior. It is dedicated all efficacy models that have their prior
specified in the form of pseudo data (as if there is some data before
the trial starts).

The `data` must obey the convention of the
[`DataDual`](https://openpharma.github.io/crmPack/reference/DataDual-class.md)
class. This refers to any observed efficacy/biomarker responses (`w` in
[`DataDual`](https://openpharma.github.io/crmPack/reference/DataDual-class.md)),
the dose levels at which these responses are observed (`x` in
[`DataDual`](https://openpharma.github.io/crmPack/reference/DataDual-class.md)),
all dose levels considered in the study (`doseGrid` in
[`DataDual`](https://openpharma.github.io/crmPack/reference/DataDual-class.md)),
and finally other specifications in
[`DataDual`](https://openpharma.github.io/crmPack/reference/DataDual-class.md)
class that can be used to generate prior or posterior modal estimates or
samples estimates for model parameter(s). If no responses are observed,
at least `doseGrid` has to be specified in `data` for which prior modal
estimates or samples can be obtained for model parameters based on the
specified pseudo data.

## Usage

``` r
.DefaultModelEff()
```

## Slots

- `data`:

  (`DataDual`)  
  observed data that is used to obtain model parameters estimates or
  samples (see details above).

## Note

Typically, end users will not use the `.DefaultModelEff()` function.

## See also

[`ModelTox`](https://openpharma.github.io/crmPack/reference/ModelTox-class.md).
