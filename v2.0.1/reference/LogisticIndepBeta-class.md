# `LogisticIndepBeta`

**\[stable\]**

`LogisticIndepBeta` is the class for the two-parameters logistic
regression dose-limiting events (DLE) model with prior expressed in form
of pseudo data. This model describes the relationship between the binary
DLE responses and the dose levels. More specifically, it represents the
relationship of the probabilities of the occurrence of a DLE for
corresponding dose levels in log scale. This model is specified as
\$\$p(x) = exp(phi1 + phi2 \* log(x)) / (1 + exp(phi1 + phi2 \*
log(x)))\$\$ where \\p(x)\\ is the probability of the occurrence of a
DLE at dose \\x\\. The two parameters of this model are the intercept
\\phi1\\ and the slope \\phi2\\. The `LogisticIndepBeta` inherits all
slots from
[`ModelTox`](https://openpharma.github.io/crmPack/reference/ModelTox-class.md)
class.

In the context of pseudo data, the following three arguments are used,
`binDLE`, `DLEdose` and `DLEweights`. The `DLEdose` represents fixed
dose levels at which the pseudo DLE responses `binDLE` are observed.
`DLEweights` represents total number of subjects treated per each dose
level in `DLEdose`. The `binDLE` represents the number of subjects
observed with DLE per each dose level in `DLEdose`. Hence, all these
three vectors must be of the same length and the order of the elements
in any of the vectors `binDLE`, `DLEdose` and `DLEweights` must be kept,
so that an element of a given vector corresponds to the elements of the
remaining two vectors (see the example for more insight). Finally, since
at least two DLE pseudo responses are needed to obtain prior modal
estimates (same as the maximum likelihood estimates) for the model
parameters, the `binDLE`, `DLEdose` and `DLEweights` must all be vectors
of at least length 2.

## Usage

``` r
LogisticIndepBeta(binDLE, DLEdose, DLEweights, data)

.DefaultLogisticIndepBeta()
```

## Arguments

- binDLE:

  (`numeric`)  
  the number of subjects observed with a DLE, the pseudo DLE responses,
  depending on dose levels `DLEdose`. Elements of `binDLE` must
  correspond to the elements of `DLEdose` and `DLEweights`.

- DLEdose:

  (`numeric`)  
  dose levels for the pseudo DLE responses. Elements of `DLEdose` must
  correspond to the elements of `binDLE` and `DLEweights`.

- DLEweights:

  (`numeric`)  
  the total number of subjects treated at each of the dose levels
  `DLEdose`, pseudo weights. Elements of `DLEweights` must correspond to
  the elements of `binDLE` and `DLEdose`.

- data:

  (`Data`)  
  the input data to update estimates of the model parameters.

## Details

The pseudo data can be interpreted as if we obtain some observations
before the trial starts. It can be used to express our prior, i.e. the
initial beliefs for the model parameters. The pseudo data is expressed
in the following way. First, fix at least two dose levels, then ask for
experts' opinion on how many subjects are to be treated at each of these
dose levels and on the number of subjects observed with a DLE. At each
dose level, the number of subjects observed with a DLE, divided by the
total number of subjects treated, is the probability of the occurrence
of a DLE at that particular dose level. The probabilities of the
occurrence of a DLE based on this pseudo data are independent and they
follow Beta distributions. Therefore, the joint prior probability
density function of all these probabilities can be obtained. Hence, by a
change of variable, the joint prior probability density function of the
two parameters in this model can also be obtained. In addition, a
conjugate joint prior density function of the two parameters in the
model is used. For details about the form of all these joint prior and
posterior probability density functions, please refer to Whitehead and
Williamson (1998) .

## Slots

- `binDLE`:

  (`numeric`)  
  a vector of total numbers of DLE responses. It must be at least of
  length 2 and the order of its elements must correspond to values
  specified in `DLEdose` and `DLEweights`.

- `DLEdose`:

  (`numeric`)  
  a vector of the dose levels corresponding to It must be at least of
  length 2 and the order of its elements must correspond to values
  specified in `binDLE` and `DLEweights`.

- `DLEweights`:

  (`integer`)  
  total number of subjects treated at each of the pseudo dose level
  `DLEdose`. It must be at least of length 2 and the order of its
  elements must correspond to values specified in `binDLE` and
  `DLEdose`.

- `phi1`:

  (`number`)  
  the intercept of the model. This slot is used in output to display the
  resulting prior or posterior modal estimate of the intercept obtained
  based on the pseudo data and (if any) observed data/responses.

- `phi2`:

  (`number`)  
  the slope of the model. This slot is used in output to display the
  resulting prior or posterior modal estimate of the slope obtained
  based on the pseudo data and (if any) the observed data/responses.

- `Pcov`:

  (`matrix`)  
  refers to the 2x2 covariance matrix of the intercept (\\phi1\\) and
  the slope parameters (\\phi2\\) of the model. This is used in output
  to display the resulting prior and posterior covariance matrix of
  \\phi1\\ and \\phi2\\ obtained, based on the pseudo data and (if any)
  the observed data and responses. This slot is needed for internal
  purposes.

## Note

Typically, end users will not use the `.DefaultLogisticIndepBeta()`
function.

## References

Whitehead J, Williamson D (1998). “Bayesian decision procedures based on
logistic regression models for dose-finding studies.” *Journal of
Biopharmaceutical Statistics*, **8**(3), 445–467.

## Examples

``` r
# Obtain prior modal estimates given the pseudo data.
# First we used an empty data set such that only the dose levels under
# investigations are given. In total, 12 dose levels are under investigation
# ranging from 25 to 300 mg with increments of 25 (i.e 25, 50, 75, ..., 300).
emptydata <- Data(doseGrid = seq(25, 300, 25))

# Fix two dose levels 25 and 300 mg (DLEdose).
# Total number of subjects treated in each of these levels is 3, (DLEweights).
# The number of subjects observed with a DLE is 1.05 at dose 25 mg and 1.8 at dose 300 mg (binDLE).
my_model1 <- LogisticIndepBeta(
  binDLE = c(1.05, 1.8),
  DLEdose = c(25, 300),
  DLEweights = c(3, 3),
  data = emptydata
)

# Use observed DLE responses to obtain posterior modal estimates.
my_data <- Data(
  x = c(25, 50, 50, 75, 100, 100, 225, 300),
  y = c(0, 0, 0, 0, 1, 1, 1, 1),
  doseGrid = emptydata@doseGrid
)
#> Used default patient IDs!
#> Used best guess cohort indices!

my_model2 <- LogisticIndepBeta(
  binDLE = c(1.05, 1.8),
  DLEdose = c(25, 300),
  DLEweights = c(3, 3),
  data = my_data
)
```
