# `NextBestMaxGain`

**\[stable\]**

`NextBestMaxGain` is the class to find a next best dose with maximum
gain value based on a pseudo DLT and efficacy models without samples. It
is based solely on the probabilities of the occurrence of a DLT and the
values of the mean efficacy responses obtained by using the modal
estimates of the DLT and efficacy model parameters. There are two target
probabilities of the occurrence of a DLT that must be specified: target
probability to be used during the trial and target probability to be
used at the end of the trial. It is suitable to use it only with the
[`ModelTox`](https://openpharma.github.io/crmPack/reference/ModelTox-class.md)
model and
[`ModelEff`](https://openpharma.github.io/crmPack/reference/ModelEff-class.md)
classes (except
[`EffFlexi`](https://openpharma.github.io/crmPack/reference/EffFlexi-class.md)).

## Usage

``` r
NextBestMaxGain(prob_target_drt, prob_target_eot)

.DefaultNextBestMaxGain()
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
  the target probability of the occurrence of a DLT to be used during
  the trial.

- `prob_target_eot`:

  (`proportion`)  
  the target probability of the occurrence of a DLT to be used at the
  end of the trial.

## Note

Typically, end users will not use the `.DefaultNextBestMaxGain()`
function.

## Examples

``` r
my_next_best <- NextBestMaxGain(0.35, 0.3)
```
