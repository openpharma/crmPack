#' @slot escalation_threshold The observed DLT rate must be below this threshold to allow for dose escalation.
#' @slot maintenance_threshold The observed DLT rate must be below this threshold to allow for maintaining the same dose level.
.IncrementsRatesDLT <- setClass(
  Class = "IncrementsRatesDLT",
  contains = "Increments",
  slots = c(
    escalation_threshold = "numeric",
    maintenance_threshold = "numeric"
  )
)

# Define a rule for dose increments which:
# - forces dose maintenance or de-escalation if the observed DLT rate is 1/3 or higher in current dose level
# - forces dose de-escalation if the observed DLT rate is 2/3 or higher in current dose level
my_increments <- .IncrementsRatesDLT(
  escalation_threshold = 1 / 3,
  maintenance_threshold = 2 / 3
)

setMethod(
  f = "maxDose",
  signature = signature(
    increments = "IncrementsRatesDLT",
    data = "Data"
  ),
  definition = function(increments, data, ...) {
    last_dose <- data@x[data@nObs]

    # Determine the DLT rate in the the current dose level
    has_this_dose <- data@x == last_dose
    dlt_count_this_dose <- sum(data@y[has_this_dose])
    dlt_rate_this_dose <- dlt_count_this_dose / sum(has_this_dose)

    # return max dose accordingly
    if (dlt_rate_this_dose >= increments@maintenance_threshold) {
      current_level <- data@xLevel[data@nObs]
      if (current_level == 1) {
        return(0)
      } else {
        return(data@doseGrid[current_level - 1])
      }
    } else if (dlt_rate_this_dose >= increments@escalation_threshold) {
      return(last_dose)
    } else {
      # No limit by this rule.
      return(max(data@doseGrid))
    }
  }
)

my_data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 8, 8, 8),
  y = c(0, 0, 0, 0, 0, 0, 1, 0), # Change here to test below
  ID = 1:8,
  cohort = c(0, 1, 2, 3, 4, 5, 5, 5),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, 8, 10:40)
)

# Based on the rule above, the maximum dose allowed is:
maxDose(my_increments, data = my_data)
