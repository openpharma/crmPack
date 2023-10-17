#' @include helpers.R
#' @include Data-validity.R
#' @include CrmPackClass-class.R
NULL

# GeneralData-class ----

#' `GeneralData`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`GeneralData`] is a class for general data input.
#'
#' @slot ID (`integer`)\cr unique patient IDs.
#' @slot cohort (`integer`)\cr the cohort (non-negative sorted) indices.
#' @slot nObs (`integer`)\cr number of observations, a single value.
#'
#' @aliases GeneralData
#' @export
#'
.GeneralData <- setClass(
  Class = "GeneralData",
  slots = c(
    ID = "integer",
    cohort = "integer",
    nObs = "integer"
  ),
  prototype = prototype(
    ID = integer(),
    cohort = integer(),
    nObs = 0L
  ),
  contains = "CrmPackClass",
  validity = v_general_data
)

## default constructor ----

#' @rdname DataGeneral-class
#' @note Typically, end users will not use the `.DefaultDataGeneral()` function.
#' @export
.DefaultDataGeneral <- function() {
  stop(paste0("Class DataGeneral cannot be instantiated directly.  Please use one of its subclasses instead."))
}

# Data ----

## class ----

#' `Data`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`Data`] is a class for the data input.
#' It inherits from [`GeneralData`].
#'
#' @slot x (`numeric`)\cr the doses for the patients.
#' @slot y (`integer`)\cr the vector of toxicity events (0 or 1 integers).
#' @slot doseGrid (`numeric`)\cr the vector of all possible doses (sorted),
#'   i.e. the dose grid.
#' @slot nGrid (`integer`)\cr number of gridpoints.
#' @slot xLevel (`integer`)\cr the levels for the doses the patients have been given,
#'   w.r.t `doseGrid`.
#' @slot placebo (`logical`)\cr if `TRUE` the first dose level
#'   in the `doseGrid`is considered as PLACEBO.
#'
#' @aliases Data
#' @export
#'
.Data <- setClass(
  Class = "Data",
  contains = "GeneralData",
  slots = c(
    x = "numeric",
    y = "integer",
    doseGrid = "numeric",
    nGrid = "integer",
    xLevel = "integer",
    placebo = "logical"
  ),
  prototype = prototype(
    x = numeric(),
    y = integer(),
    doseGrid = numeric(),
    nGrid = 0L,
    xLevel = integer(),
    placebo = FALSE
  ),
  validity = v_data
)

## constructor ----

#' @rdname Data-class
#'
#' @details The `cohort` can be missing if and only if `placebo` is equal to
#'   `FALSE`.
#'
#' @note `ID` and `cohort` can be missing. Then a message will be issued
#'   and the variables will be filled with default IDs and best guesses cohort,
#'   i.e. a sorted (in ascending order) sequence of values from `{1, 2, ...}`.
#'
#' @param x (`numeric`)\cr the doses for the patients.
#' @param y (`integer`)\cr the vector of toxicity events (0 or 1).
#'   You can also supply `numeric` vectors, but these will then be converted to
#'   `integer` internally.
#' @param ID (`integer`)\cr unique patient IDs.
#'   You can also supply `numeric` vectors, but these will then be converted to
#'   `integer` internally.
#' @param cohort (`integer`)\cr the cohort (non-negative sorted) indices.
#'   You can also supply `numeric` vectors, but these will then be converted to
#'   `integer` internally.
#' @param doseGrid (`numeric`)\cr all possible doses.
#' @param placebo (`flag`)\cr if `TRUE` the first dose level
#'   in the `doseGrid` is considered as placebo.
#' @param ... not used.
#'
#' @export
#' @example examples/Data-class.R
#'
Data <- function(x = numeric(),
                 y = integer(),
                 ID = integer(),
                 cohort = integer(),
                 doseGrid = numeric(),
                 placebo = FALSE,
                 ...) {
  assert_numeric(x)
  assert_integerish(y, lower = 0, upper = 1, any.missing = FALSE)
  assert_integerish(ID, unique = TRUE, any.missing = FALSE)
  assert_integerish(cohort)
  assert_numeric(doseGrid, any.missing = FALSE, unique = TRUE)
  assert_flag(placebo)

  doseGrid <- sort(doseGrid)
  assert_subset(x, doseGrid)

  if (length(ID) == 0 && length(x) > 0) {
    message("Used default patient IDs!")
    ID <- seq_along(x)
  } else {
    assert_integerish(ID, unique = TRUE)
  }

  if (!placebo && length(cohort) == 0 && length(x) > 0) {
    message("Used best guess cohort indices!")
    # This is just assuming that consecutive patients
    # in the data set are in the same cohort if they
    # have the same dose. Note that this could be wrong,
    # if two subsequent cohorts are at the same dose.
    cohort <- as.integer(c(1, 1 + cumsum(diff(x) != 0)))
  } else {
    assert_integerish(cohort)
  }

  .Data(
    x = as.numeric(x),
    y = as.integer(y),
    ID = as.integer(ID),
    cohort = as.integer(cohort),
    doseGrid = as.numeric(doseGrid),
    nObs = length(x),
    nGrid = length(doseGrid),
    xLevel = match_within_tolerance(x, doseGrid),
    placebo = placebo
  )
}

## default constructor ----

#' @rdname Data-class
#' @note Typically, end users will not use the `.DefaultData()` function.
#' @export
.DefaultData <- function() {
  Data(
    doseGrid = c(1, 3, 5, 10, 15, 20, 25, 40, 50, 80, 100),
    ID = 1L:3L,
    cohort = 1L:3L,
    x = c(1, 3, 5),
    y = rep(0L, 3)
  )
}

# DataDual ----

## class ----

#' `DataDual`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`DataDual`] is a class for the dual endpoint data.
#' It inherits from [`Data`] and it contains additional biomarker information.
#'
#' @slot w (`numeric`)\cr the continuous vector of biomarker values.
#'
#' @aliases DataDual
#' @export
#'
.DataDual <- setClass(
  Class = "DataDual",
  slots = c(w = "numeric"),
  prototype = prototype(w = numeric()),
  contains = "Data",
  validity = v_data_dual
)

## constructor ----

#' @rdname DataDual-class
#'
#' @param w (`numeric`)\cr the continuous vector of biomarker values.
#' @param ... parameters passed to [Data()].
#'
#' @export
#' @example examples/Data-class-DataDual.R
#'
DataDual <- function(w = numeric(),
                     ...) {
  d <- Data(...)
  .DataDual(d, w = w)
}


## default constructor ----

#' @rdname DataDual-class
#' @note Typically, end users will not use the `.DefaultDataDual()` function.
#' @export
.DefaultDataDual <- function() {
  DataDual(
    x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
    y = c(0, 0, 0, 0, 0, 0, 1, 0),
    w = rnorm(8),
    doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2)),
    ID = 1L:8L,
    cohort = as.integer(c(1, 2, 3, 4, 5, 6, 6, 6))
  )
}

# DataParts ----

## class ----

#' `DataParts`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`DataParts`] is a class for the data with two study parts.
#' It inherits from [`Data`] and it contains additional information
#' on the two study parts.
#'
#' @slot part (`integer`)\cr which part does each of the patients belong to?
#' @slot nextPart (`count`)\cr what is the part for the next cohort (1 or 2)?
#' @slot part1Ladder (`numeric`)\cr what is the escalation ladder for
#'   part 1? This shall be an ordered subset of the `doseGrid`.
#'
#' @aliases DataParts
#' @export
#'
.DataParts <- setClass(
  Class = "DataParts",
  slots = c(
    part = "integer",
    nextPart = "integer",
    part1Ladder = "numeric"
  ),
  prototype = prototype(
    part = integer(),
    nextPart = 1L,
    part1Ladder = numeric()
  ),
  contains = "Data",
  validity = v_data_parts
)

## constructor ----

#' @rdname DataParts-class
#'
#' @param part (`integer`)\cr which part does each of the patients belong to?
#' @param nextPart (`count`)\cr what is the part for the next cohort (1 or 2)?
#' @param part1Ladder (`numeric`)\cr what is the escalation ladder for part 1?
#'   This shall be an ordered subset of the `doseGrid`.
#' @param ... parameters passed to [Data()].
#'
#' @export
#' @example examples/Data-class-DataParts.R
#'
DataParts <- function(part = integer(),
                      nextPart = 1L,
                      part1Ladder = numeric(),
                      ...) {
  d <- Data(...)
  .DataParts(
    d,
    part = part,
    nextPart = nextPart,
    part1Ladder = part1Ladder
  )
}

## default constructor ----

#' @rdname DataParts-class
#' @note Typically, end users will not use the `.DefaultDataParts()` function.
#' @export
.DefaultDataParts <- function() {
  DataParts(
    x = c(0.1, 0.5, 1.5),
    y = c(0, 0, 0),
    ID = 1:3,
    cohort = 1:3,
    doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2)),
    part = c(1L, 1L, 1L),
    nextPart = 1L,
    part1Ladder = c(0.1, 0.5, 1.5, 3, 6, 10)
  )
}


# DataMixture ----

## class ----

#' `DataMixture`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`DataMixture`] is a class for the data with mixture sharing.
#' It inherits from [`Data`] and it contains additional information
#' on the mixture sharing.
#'
#' @slot xshare (`numeric`)\cr the doses for the share patients.
#' @slot yshare (`integer`)\cr the vector of toxicity events (0 or 1)
#'   for the share patients.
#' @slot nObsshare (`count`)\cr number of share patients.
#'
#' @aliases DataMixture
#' @export
#'
.DataMixture <- setClass(
  Class = "DataMixture",
  slots = c(
    xshare = "numeric",
    yshare = "integer",
    nObsshare = "integer"
  ),
  prototype = prototype(
    xshare = numeric(),
    yshare = integer(),
    nObsshare = 0L
  ),
  contains = "Data",
  validity = v_data_mixture
)

## constructor ----

#' @rdname DataMixture-class
#'
#' @param xshare (`numeric`)\cr the doses for the share patients.
#' @param yshare (`integer`)\cr the vector of toxicity events (0 or 1)
#'   for the share patients. You can also supply `numeric` vectors,
#'   but these will then be converted to `integer` internally.
#' @param ... parameters passed to [Data()].
#'
#' @export
#' @example examples/Data-class-DataMixture.R
#'
DataMixture <- function(xshare = numeric(),
                        yshare = integer(),
                        ...) {
  d <- Data(...)
  assert_integerish(yshare)
  assert_numeric(xshare)
  .DataMixture(
    d,
    xshare = as.numeric(xshare),
    yshare = as.integer(yshare),
    nObsshare = length(xshare)
  )
}

## default constructor ----

#' @rdname DataMixture-class
#' @note Typically, end users will not use the `.DefaultDataMixture()` function.
#' @export
.DefaultDataMixture <- function() {
  DataMixture(
    xshare = c(12, 14, 16, 18.0),
    yshare = c(0L, 1L, 1L, 1L),
    nObsshare = 4L,
    x = c(0.1, 0.5, 1.5),
    y = c(0, 0, 0),
    ID = 1L:3L,
    cohort = 1L:3L,
    doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
  )
}


# DataDA ----

## class ----

#' `DataDA`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`DataDA`] is a class for the time-to-DLT augmented data.
#' It inherits from [`Data`] and it contains additional DLT free survival times.
#'
#' @note `survival time` here refers to the time period for which the subject
#'   did not experience any DLT, and is not referring to deaths.
#'
#' @slot u (`numeric`)\cr the continuous vector of DLT free survival times.
#' @slot t0 (`numeric`)\cr time of initial dosing for each patient.
#'   Non-negative values sorted in ascending order.
#' @slot Tmax (`number`)\cr the DLT observation period.
#'
#' @aliases DataDA
#' @export
#'
.DataDA <- setClass(
  Class = "DataDA",
  slots = c(
    u = "numeric",
    t0 = "numeric",
    Tmax = "numeric"
  ),
  prototype = prototype(
    u = numeric(),
    t0 = numeric(),
    Tmax = 0 + .Machine$double.xmin
  ),
  contains = "Data",
  validity = v_data_da
)

## constructor ----

#' @rdname DataDA-class
#'
#' @param u (`numeric`)\cr the continuous vector of DLT free survival times.
#' @param t0 (`numeric`)\cr time of initial dosing for each patient.
#'   Non-negative values sorted in ascending order.
#'   Default to vector of 0s of length equal to length of `u`.
#' @param Tmax (`number`)\cr the DLT observation period.
#' @param ... parameters passed to [Data()].
#'
#' @export
#' @example examples/Data-class-DataDA.R
#'
DataDA <- function(u = numeric(),
                   t0 = numeric(length(u)),
                   Tmax = 0 + .Machine$double.xmin,
                   ...) {
  d <- Data(...)
  .DataDA(
    d,
    u = as.numeric(u),
    t0 = as.numeric(t0),
    Tmax = as.numeric(Tmax)
  )
}

## default constructor ----

#' @rdname DataDA-class
#' @note Typically, end users will not use the `.DefaultDataDA()` function.
#' @export
.DefaultDataDA <- function() {
  DataDA(
    u = c(42, 30, 15, 5, 20, 25, 30, 60),
    t0 = c(0, 15, 30, 40, 55, 70, 75, 85),
    Tmax = 60,
    x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
    y = c(0, 0, 1, 1, 0, 0, 1, 0),
    doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2)),
    ID = 1L:8L,
    cohort = as.integer(c(1, 2, 3, 4, 5, 6, 6, 6))
  )
}

# DataOrdinal ----

## class ----

#' `DataOrdinal`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`DataOrdinal`] is a class for ordinal toxicity data.
#' It inherits from [`GeneralData`] and it describes toxicity responses on an
#' ordinal rather than binary scale.
#'
#' @note This class has been implemented as a sibling of the existing `Data` class
#' (rather than as a parent or child) to minimise the risk of unintended side
#' effects on existing classes and methods.
#'
#' The default setting for the `yCategories` slot replicates the behaviour
#' of the existing `Data` class.
#'
#' @aliases DataOrdinal
#' @export
.DataOrdinal <- setClass(
  Class = "DataOrdinal",
  contains = "GeneralData",
  slots = c(
    x = "numeric",
    y = "integer",
    doseGrid = "numeric",
    nGrid = "integer",
    xLevel = "integer",
    yCategories = "integer",
    placebo = "logical"
  ),
  prototype = prototype(
    x = numeric(),
    y = integer(),
    doseGrid = numeric(),
    nGrid = 0L,
    xLevel = integer(),
    yCategories = c("No DLT" = 0L, "DLT" = 1L),
    placebo = FALSE
  ),
  validity = v_data_ordinal
)

## constructor ----

#' @rdname DataOrdinal-class
#' @param yCategories (named `integer`)\cr the names and codes for the
#' toxicity categories used in the data.  Category labels are taken from the
#' names of the vector.  The names of the vector must be unique and its values
#' must be sorted and take the values 0, 1, 2, ...
#' @inheritParams Data
#' @inherit Data details note params
#' @example examples/Data-class-DataOrdinal.R
#' @export
DataOrdinal <- function(x = numeric(),
                        y = integer(),
                        ID = integer(),
                        cohort = integer(),
                        doseGrid = numeric(),
                        placebo = FALSE,
                        yCategories = c("No DLT" = 0L, "DLT" = 1L),
                        ...) {
  assert_numeric(doseGrid, any.missing = FALSE, unique = TRUE)
  assert_integerish(
    yCategories,
    any.missing = FALSE,
    unique = TRUE,
    names = "unique",
    min.len = 2
  )
  assert_flag(placebo)

  doseGrid <- as.numeric(sort(doseGrid))

  if (length(ID) == 0 && length(x) > 0) {
    message("Used default patient IDs!")
    ID <- seq_along(x)
  } else {
    assert_integerish(ID, unique = TRUE)
  }

  if (!placebo && length(cohort) == 0 && length(x) > 0) {
    message("Used best guess cohort indices!")
    # This is just assuming that consecutive patients
    # in the data set are in the same cohort if they
    # have the same dose. Note that this could be wrong,
    # if two subsequent cohorts are at the same dose.
    cohort <- as.integer(c(1, 1 + cumsum(diff(x) != 0)))
  } else {
    assert_integerish(cohort)
  }

  .DataOrdinal(
    x = as.numeric(x),
    y = as.integer(y),
    ID = as.integer(ID),
    cohort = as.integer(cohort),
    doseGrid = doseGrid,
    nObs = length(x),
    nGrid = length(doseGrid),
    xLevel = match_within_tolerance(x = x, table = doseGrid),
    placebo = placebo,
    yCategories = yCategories
  )
}


## default constructor ----

#' @rdname DataOrdinal-class
#' @note Typically, end users will not use the `.DefaultDataOrdinal()` function.
#' @export
.DefaultDataOrdinal <- function () {
  DataOrdinal(
    x = c(10, 20, 30, 40, 50, 50, 50, 60, 60, 60),
    y = as.integer(c(0, 0, 0, 0, 0, 1, 0, 0, 1, 2)),
    ID = 1L:10L,
   cohort = as.integer(c(1:4, 5, 5, 5, 6, 6, 6)),
    doseGrid = c(seq(from = 10, to = 100, by = 10)),
    yCategories = c("No tox" = 0L, "Sub-tox AE" = 1L, "DLT" = 2L),
    placebo = FALSE
  )
}

# DataGrouped ----

## class ----

#' `DataGrouped`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`DataGrouped`] is a class for a two groups dose escalation data set,
#' comprised of a monotherapy (`mono`) and a combination therapy (`combo`)
#' arm. It inherits from [`Data`] and it contains the additional group information.
#'
#' @slot group (`factor`)\cr whether `mono` or `combo` was used.
#'
#' @aliases DataGrouped
#' @export
.DataGrouped <- setClass(
  Class = "DataGrouped",
  slots = c(
    group = "factor"
  ),
  prototype = prototype(
    group = factor(levels = c("mono", "combo"))
  ),
  contains = "Data",
  validity = v_data_grouped
)

#' @rdname DataGrouped-class
#'
#' @param group (`factor` or `character`)\cr whether `mono` or `combo` was used.
#'   If `character` then will be coerced to `factor` with the correct levels
#'   internally.
#' @param ... parameters passed to [Data()].
#'
#' @export
#' @example examples/Data-class-DataGrouped.R
#'
DataGrouped <- function(group = character(),
                        ...) {
  d <- Data(...)
  if (!is.factor(group)) {
    assert_character(group)
    assert_subset(group, choices = c("mono", "combo"))
    group <- factor(group, levels = c("mono", "combo"))
  }
  .DataGrouped(
    d,
    group = group
  )
}

## default constructor ----

#' @rdname DataGrouped-class
#' @note Typically, end users will not use the `.DefaultDataGrouped()` function.
#' @export
.DefaultDataGrouped <- function() {
  DataGrouped()
}
