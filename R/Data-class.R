#' @include helpers.R Data-validity.R
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
  validity = validate_subjects
)

# Data-class ----

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
  validity = validate_data
)

# Data-constructor ----

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
  assert_numeric(y)
  assert_numeric(ID)
  assert_numeric(cohort)
  assert_numeric(doseGrid, any.missing = FALSE, unique = TRUE)
  assert_flag(placebo)

  doseGrid <- as.numeric(sort(doseGrid))

  if (length(ID) == 0 && length(x) > 0) {
    message("Used default patient IDs!")
    ID <- seq_along(x)
  }

  if (!placebo && length(cohort) == 0 && length(x) > 0) {
    message("Used best guess cohort indices!")
    # This is just assuming that consecutive patients
    # in the data set are in the same cohort if they
    # have the same dose. Note that this could be wrong,
    # if two subsequent cohorts are at the same dose.
    cohort <- as.integer(c(1, 1 + cumsum(diff(x) != 0)))
  }

  .Data(
    x = as.numeric(x),
    y = safeInteger(y),
    ID = safeInteger(ID),
    cohort = safeInteger(cohort),
    doseGrid = doseGrid,
    nObs = length(x),
    nGrid = length(doseGrid),
    xLevel = matchTolerance(x = x, table = doseGrid),
    placebo = placebo
  )
}

# DataDual-class ----

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
  validity = validate_data_dual
)

# DataDual-constructor ----

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

# DataParts-class ----

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
  validity = validate_data_parts
)

# DataParts-constructor ----

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

# DataMixture-class ----

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
  validity = validate_data_mixture
)

# DataMixture-constructor ----

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
  .DataMixture(
    d,
    xshare = as.numeric(xshare),
    yshare = safeInteger(yshare),
    nObsshare = length(xshare)
  )
}

# DataDA-class ----

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
  validity = validate_data_DA
)

# DataDA-constructor ----

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
