# Open / recruit backfill patients into a cohort?

**\[experimental\]**

## Usage

``` r
openCohort(opening, cohort, data, dose, ...)

# S4 method for class 'OpeningMinDose'
openCohort(opening, cohort, data, dose, ...)

# S4 method for class 'OpeningMinCohorts'
openCohort(opening, cohort, data, dose, ...)

# S4 method for class 'OpeningNone'
openCohort(opening, cohort, data, dose, ...)

# S4 method for class 'OpeningMinResponses'
openCohort(opening, cohort, data, dose, ...)

# S4 method for class 'OpeningList'
openCohort(opening, cohort, data, dose, summary_fun, ...)

# S4 method for class 'OpeningAll'
openCohort(opening, cohort, data, dose, ...)

# S4 method for class 'OpeningAny'
openCohort(opening, cohort, data, dose, ...)
```

## Arguments

- opening:

  (`Opening`)  
  opening rule to be applied.

- cohort:

  (`int`)  
  backfill cohort index.

- data:

  (`Data`)  
  current trial data.

- dose:

  (`numeric`)  
  the recommended next best dose.

- ...:

  further arguments (not used).

- summary_fun:

  (`function`)  
  to apply to the list of results (e.g., `all` or `any`). Only used for
  `OpeningList` and its subclasses.

## Value

`TRUE` if this backfill cohort can be opened / recruited into, `FALSE`
otherwise.

## Functions

- `openCohort(OpeningMinDose)`: method for `OpeningMinDose` class.

- `openCohort(OpeningMinCohorts)`: method for `OpeningMinCohorts` class.

- `openCohort(OpeningNone)`: method for `OpeningNone` class, which never
  opens a cohort, i.e. the trial design will not use backfilling.

- `openCohort(OpeningMinResponses)`: method for `OpeningMinResponses`
  class.

- `openCohort(OpeningList)`: method for `OpeningList` class.

- `openCohort(OpeningAll)`: method for `OpeningAll` class. Returns
  `TRUE` if ALL opening criteria are satisfied.

- `openCohort(OpeningAny)`: method for `OpeningAny` class. Returns
  `TRUE` if ANY opening criteria are satisfied.

## Examples

``` r
# Create a simple data object with some dose information
data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = c(0, 0, 0, 0, 0, 0, 1, 0),
  cohort = c(1, 2, 3, 4, 5, 6, 6, 6),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
)
#> Used default patient IDs!

# Define the opening rule: open backfill cohorts when dose is at least 5
my_opening <- OpeningMinDose(min_dose = 5)

# Check if the first backfill cohort can be opened when the
# cohort has a dose 6, i.e. larger than 5.
# Note that `dose` is not used in this rule.
can_open <- openCohort(my_opening, cohort = 5, data = data, dose = 7)
can_open
#> [1] TRUE

# Check if the first backfill cohort can be opened when the
# cohort has a dose 3, i.e. smaller than 5.
# (should return FALSE because dose < min_dose)
can_open_low <- openCohort(my_opening, cohort = 4, data = data, dose = 15)
can_open_low
#> [1] FALSE
# Create a data object with multiple cohorts
data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = c(0, 0, 0, 0, 0, 0, 1, 0),
  cohort = c(0, 1, 2, 3, 4, 5, 5, 5),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
)
#> Used default patient IDs!

# Define the opening rule: open backfill cohorts after at least 3 cohorts
my_opening <- OpeningMinCohorts(min_cohorts = 3)

# Check if a backfill cohort can be opened
# Current number of cohorts is 6, so it should open (6 >= 3)
can_open <- openCohort(my_opening, cohort = 1, data = data, dose = 25)
can_open
#> [1] TRUE
# Create an OpeningNone object which never opens backfill cohorts
opening <- OpeningNone()

# Create sample trial data
data <- Data(
  x = c(20, 30, 40, 50),
  y = c(0, 0, 1, 0),
  cohort = c(1, 2, 3, 4),
  doseGrid = seq(10, 100, by = 10)
)
#> Used default patient IDs!

# Create dose recommendation
dose <- 60

# Check if backfill cohort should be opened for cohort 5
# OpeningNone always returns FALSE
should_open <- openCohort(opening, cohort = 5, data = data, dose = dose)
print(should_open) # FALSE
#> [1] FALSE
# Create an OpeningMinResponses object requiring 2 responses at the cohort dose
opening <- OpeningMinResponses(min_responses = 2, include_lower_doses = FALSE)

# Create sample trial data with responses at different dose levels
data <- Data(
  x = c(10, 10, 20, 20, 20, 30, 30, 30),
  y = c(0, 1, 0, 1, 0, 1, 1, 0),
  response = c(1, 1, NA, 0, 1, NA, 1, 0),
  cohort = c(1, 1, 2, 2, 2, 3, 3, 3),
  doseGrid = seq(10, 100, by = 10)
)
#> Used default patient IDs!

# Check if backfill cohort can be opened at dose 20
# At dose 20, there is 1 response
should_open <- openCohort(opening, cohort = 2, data = data, dose = 30)
print(should_open) # FALSE
#> [1] FALSE

# Check at dose 10 where there are 2 responses
should_open_2 <- openCohort(opening, cohort = 1, data = data, dose = 30)
print(should_open_2) # TRUE
#> [1] TRUE
# Create sample trial data
data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = c(0, 0, 0, 0, 0, 0, 1, 0),
  cohort = c(1, 2, 3, 4, 5, 6, 6, 6),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
)
#> Used default patient IDs!

# Create opening criteria:
# - opening1: open if dose >= 10
# - opening2: open if at least 5 cohorts have been treated
opening1 <- OpeningMinDose(min_dose = 10)
opening2 <- OpeningMinCohorts(min_cohorts = 5)

# Combine with AND logic: both must be true
opening_all <- opening1 & opening2

# Test if backfill cohort 7 can be opened
# Cohort 6 dose is 10 (>= 10: TRUE), max cohort is 6 (>= 5: TRUE), so AND is TRUE
should_open <- openCohort(opening_all, cohort = 6, data = data, dose = 20)
print(should_open) # TRUE
#> [1] TRUE

# Test if backfill cohort at dose 6 can be opened
# Cohort 4 dose is 3 (>= 10: FALSE), so AND is FALSE
should_open_2 <- openCohort(opening_all, cohort = 4, data = data, dose = 20)
print(should_open_2) # FALSE
#> [1] FALSE
# Create sample trial data
data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = c(0, 0, 0, 0, 0, 0, 1, 0),
  cohort = c(1, 2, 3, 4, 5, 6, 6, 6),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
)
#> Used default patient IDs!

# Create opening criteria:
# - opening1: open if dose >= 20
# - opening2: open if at least 5 cohorts have been treated
opening1 <- OpeningMinDose(min_dose = 20)
opening2 <- OpeningMinCohorts(min_cohorts = 5)

# Combine with OR logic: at least one must be true
opening_any <- opening1 | opening2

# Test if backfill cohort 7 can be opened at dose 10
# Cohort 6 dose is 10 (>= 20: FALSE), max cohort is 6 (>= 5: TRUE), so OR is TRUE
should_open <- openCohort(opening_any, cohort = 6, data = data, dose = 20)
print(should_open) # TRUE
#> [1] TRUE

# Test with a different scenario:
# It does not matter which cohort index we give, because
# the number of cohorts is already above 5.
should_open_2 <- openCohort(opening_any, cohort = 1, data = data, dose = 25)
print(should_open_2) # TRUE
#> [1] TRUE
```
