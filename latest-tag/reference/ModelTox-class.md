# `ModelTox`

**\[stable\]**

`ModelTox` is the parent class for DLE (dose-limiting events) models
using pseudo data prior. It is dedicated for DLE models or toxicity
models that have their prior specified in the form of pseudo data (as if
there is some data before the trial starts).

The `data` must obey the convention of the
[`Data`](https://openpharma.github.io/crmPack/reference/Data-class.md)
class. This refers to any observed DLE responses (`y` in
[`Data`](https://openpharma.github.io/crmPack/reference/Data-class.md)),
the dose levels (`x` in
[`Data`](https://openpharma.github.io/crmPack/reference/Data-class.md))
at which these responses are observed, all dose levels considered in the
study (`doseGrid` in
[`Data`](https://openpharma.github.io/crmPack/reference/Data-class.md)),
and finally other specifications in
[`Data`](https://openpharma.github.io/crmPack/reference/Data-class.md)
class that can be used to generate prior or posterior modal estimates or
samples estimates for model parameter(s). If no responses are observed,
at least `doseGrid` has to be specified in `data` for which prior modal
estimates or samples can be obtained for model parameters based on the
specified pseudo data.

## Usage

``` r
.DefaultModelTox()
```

## Slots

- `data`:

  (`Data`)  
  observed data that is used to obtain model parameters estimates or
  samples (see details above).

## Note

Typically, end users will not use the `.DefaultModelTox()` function.

## See also

[`ModelEff`](https://openpharma.github.io/crmPack/reference/ModelEff-class.md).
