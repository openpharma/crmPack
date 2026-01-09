# `FractionalCRM`

**\[stable\]**

`FractionalCRM` is the class for a fractional CRM model based on a one
parameter CRM (with normal prior on the log-power parameter) as well as
Kaplan-Meier based estimation of the conditional probability to
experience a DLT for non-complete observations.

This fractional CRM model follows the paper and code by Yin et al.
(2013) .

## Usage

``` r
FractionalCRM(...)

.DefaultFractionalCRM()
```

## Arguments

- ...:

  Arguments passed on to
  [`OneParLogNormalPrior`](https://openpharma.github.io/crmPack/reference/OneParLogNormalPrior-class.md)

  `skel_probs`

  :   (`numeric`)  
      skeleton prior probabilities. This is a vector of unique and
      sorted probability values between 0 and 1.

  `dose_grid`

  :   (`numeric`)  
      dose grid. It must be must be a sorted vector of the same length
      as `skel_probs`.

  `sigma2`

  :   (`number`)  
      prior variance of log power parameter alpha.

## Note

Typically, end users will not use the
[`.DefaultTITELogisticLogNormal()`](https://openpharma.github.io/crmPack/reference/TITELogisticLogNormal-class.md)
function.

## References

Yin G, Zheng S, Xu J (2013). “Fractional dose-finding methods with
late-onset toxicity in phase I clinical trials.” *Journal of
Biopharmaceutical Statistics*, **23**(4), 856–870.
[doi:10.1080/10543406.2013.789892](https://doi.org/10.1080/10543406.2013.789892)
.

## See also

[`TITELogisticLogNormal`](https://openpharma.github.io/crmPack/reference/TITELogisticLogNormal-class.md).

## Examples

``` r
my_model <- FractionalCRM(
  skel_probs = c(0.1, 0.2, 0.3, 0.4),
  dose_grid = c(10, 30, 50, 100),
  sigma2 = 2
)
```
