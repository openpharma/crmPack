# `NextBestTD`

**\[stable\]**

`NextBestTD` is the class to find a next best dose based on pseudo DLT
model without samples. Namely, it is to find two next best doses, one
for allocation during the trial and the second for final recommendation
at the end of a trial without involving any samples, i.e. only DLT
responses will be incorporated for the dose-allocation. This is based
solely on the probabilities of the occurrence of a DLT obtained by using
the modal estimates of the model parameters. There are two target
probabilities of the occurrence of a DLT that must be specified: target
probability to be used during the trial and target probability to be
used at the end of the trial. It is suitable to use it only with the
[`ModelTox`](https://openpharma.github.io/crmPack/reference/ModelTox-class.md)
model class.

## Usage

``` r
.DefaultNextBestTD()

NextBestTD(prob_target_drt, prob_target_eot)
```

## Arguments

- prob_target_drt:

  (`proportion`)  
  see slot definition.

- prob_target_eot:

  (`proportion`)  
  see slot definition.

## Slots

- `prob_target_drt`:

  (`proportion`)  
  the target probability (except 0 or 1) of the occurrence of a DLT to
  be used during the trial.

- `prob_target_eot`:

  (`proportion`)  
  the target probability (except 0 or 1) of the occurrence of a DLT to
  be used at the end of the trial.

## Note

Typically, end users will not use the `.DefaultNextBestTD()` function.

## Examples

``` r
my_next_best <- NextBestTD(0.35, 0.3)
```
