#' @include helpers.R
#' @include Rules-validity.R
NULL

# NextBest ----

## class ----

#' `NextBest`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`NextBest`] is a virtual class for finding next best dose, from which all
#' other specific next best dose classes inherit.
#'
#' @seealso [`NextBestMTD`], [`NextBestNCRM`], [`NextBestDualEndpoint`],
#'   [`NextBestThreePlusThree`], [`NextBestDualEndpoint`], [`NextBestMinDist`],
#'   [`NextBestInfTheory`], [`NextBestTD`], [`NextBestTDsamples`],
#'   [`NextBestMaxGain`], [`NextBestMaxGainSamples`].
#'
#' @aliases NextBest
#' @export
#'
setClass(
  Class = "NextBest"
)

# NextBestMTD ----

## class ----

#' `NextBestMTD`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`NextBestMTD`] is the class for next best dose based on MTD estimate.
#'
#' @slot target (`proportion`)\cr target toxicity probability.
#' @slot derive (`function`)\cr a function which derives the final next best MTD
#'   estimate, based on vector of posterior MTD samples. It must therefore accept
#'   one and only one argument, which is a numeric vector, and return a number.
#'
#' @aliases NextBestMTD
#' @export
#'
.NextBestMTD <- setClass(
  Class = "NextBestMTD",
  slots = c(
    target = "numeric",
    derive = "function"
  ),
  prototype = prototype(
    target = 0.3,
    derive = function(mtd_samples) {
      quantile(mtd_samples, probs = 0.3)
    }
  ),
  contains = "NextBest",
  validity = v_next_best_mtd
)

## constructor ----

#' @rdname NextBestMTD-class
#'
#' @param target (`proportion`)\cr see slot definition.
#' @param derive (`function`)\cr see slot definition.
#'
#' @export
#' @example examples/Rules-class-NextBestMTD.R
#'
NextBestMTD <- function(target, derive) {
  .NextBestMTD(
    target = target,
    derive = derive
  )
}

# NextBestNCRM ----

## class ----

#' `NextBestNCRM`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`NextBestNCRM`] is the class for next best dose that finds the next dose
#' with high posterior probability to be in the target toxicity interval.
#'
#' @details To avoid numerical problems, the dose selection algorithm has been
#' implemented as follows: First admissible doses are found, which are those
#' with probability to fall in `overdose` category being below `max_overdose_prob`.
#' Next, within the admissible doses, the maximum probability to fall in the
#' `target` category is calculated. If that is above 5% (i.e. it is not just
#' numerical error), then the corresponding dose is the next recommended dose.
#' Otherwise, the highest admissible dose is the next recommended dose.
#'
#' @slot target (`numeric`)\cr the target toxicity interval (limits included).
#' @slot overdose (`numeric`)\cr the overdose toxicity interval (lower limit
#'   excluded, upper limit included). It is used to filter probability samples.
#' @slot max_overdose_prob (`proportion`)\cr maximum overdose posterior
#'   probability that is allowed.
#'
#' @aliases NextBestNCRM
#' @export
#'
.NextBestNCRM <- setClass(
  Class = "NextBestNCRM",
  slots = c(
    target = "numeric",
    overdose = "numeric",
    max_overdose_prob = "numeric"
  ),
  prototype = prototype(
    target = c(0.2, 0.35),
    overdose = c(0.35, 1),
    max_overdose_prob = 0.25
  ),
  contains = "NextBest",
  validity = v_next_best_ncrm
)

## constructor ----

#' @rdname NextBestNCRM-class
#'
#' @param target (`numeric`)\cr see slot definition.
#' @param overdose (`numeric`)\cr see slot definition.
#' @param max_overdose_prob (`proportion`)\cr see slot definition.
#' @export
#' @example examples/Rules-class-NextBestNCRM.R
#'
NextBestNCRM <- function(target,
                         overdose,
                         max_overdose_prob) {
  .NextBestNCRM(
    target = target,
    overdose = overdose,
    max_overdose_prob = max_overdose_prob
  )
}

# NextBestNCRMLoss ----

## class ----

#' `NextBestNCRMLoss`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`NextBestNCRMLoss`] is the class based on NCRM rule and loss function.
#' This class is similar to [`NextBestNCRM`] class, the only difference is the
#' addition of loss function. As in NCRM rule, first admissible doses are found,
#' which are those with probability to fall in overdose category being below
#' `max_overdose_prob`. Next, within the admissible doses, the loss function is
#' calculated, i.e. `losses` %*% `target`. Finally, the corresponding
#' dose with lowest loss function (Bayes risk) is recommended for the next dose.
#'
#' @slot overdose (`numeric`)\cr the overdose toxicity interval (lower limit
#'   excluded, upper limit included) or the excessive toxicity interval (lower
#'   limit excluded, upper limit included) if
#'   unacceptable is not provided. It is used to filter probability samples.
#' @slot unacceptable (`numeric`)\cr an unacceptable toxicity
#'   interval (lower limit excluded, upper limit included). This must be
#'   specified if the `overdose` does not include 1. Otherwise, it is
#'   c(1, 1) (default), which is essentially a scalar equals 1.
#' @slot losses (`numeric`)\cr a vector specifying the loss function. If the
#'   `unacceptable` is provided, the vector length must be \eqn{4}, otherwise
#'   \eqn{3}.
#'
#' @note The loss function should be a vector of either 3 or 4 values.
#'   This is because the loss function values must be specified for each
#'   interval, that is under-dosing, target toxicity, and overdosing toxicity or
#'   under-dosing, target toxicity, overdosing (excessive) toxicity, and
#'   unacceptable toxicity intervals.
#'
#' @aliases NextBestNCRMLoss
#' @export
#'
.NextBestNCRMLoss <- setClass(
  Class = "NextBestNCRMLoss",
  slots = c(
    unacceptable = "numeric",
    losses = "numeric"
  ),
  prototype = prototype(
    unacceptable = c(1, 1),
    losses = c(1, 0, 2)
  ),
  contains = "NextBestNCRM",
  validity = v_next_best_ncrm_loss
)

## constructor ----

#' @rdname NextBestNCRMLoss-class
#'
#' @param target (`numeric`)\cr target toxicity interval (limits included).
#' @param overdose (`numeric`)\cr the overdose toxicity interval (lower limit
#'   excluded, upper limit included) or the excessive toxicity interval (lower
#'   limit excluded, upper limit included) if
#'   unacceptable is not provided.
#' @param unacceptable (`numeric`)\cr see slot definition.
#' @param max_overdose_prob (`proportion`)\cr the maximum overdose
#'   (overdose or excessive + unacceptable) probability that is allowed.
#' @param losses (`numeric`)\cr see slot definition.
#'
#' @export
#' @example examples/Rules-class-NextBestNCRMLoss.R
#'
NextBestNCRMLoss <- function(target,
                             overdose,
                             unacceptable = c(1, 1),
                             max_overdose_prob,
                             losses) {
  .NextBestNCRMLoss(
    target = target,
    overdose = overdose,
    unacceptable = unacceptable,
    max_overdose_prob = max_overdose_prob,
    losses = losses
  )
}

# NextBestThreePlusThree ----

## class ----

#' `NextBestThreePlusThree`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`NextBestThreePlusThree`] is the class for next best dose that
#' implements the classical 3+3 dose recommendation. No input is required,
#' hence this class has no slots.
#'
#' @aliases NextBestThreePlusThree
#' @export
#'
.NextBestThreePlusThree <- setClass(
  Class = "NextBestThreePlusThree",
  contains = "NextBest"
)

## constructor ----

#' @rdname NextBestThreePlusThree-class
#'
#' @export
#' @examples
#' # Next best dose class object using the classical 3+3 design.
#' my_next_best <- NextBestThreePlusThree()
NextBestThreePlusThree <- function() {
  .NextBestThreePlusThree()
}

# NextBestDualEndpoint ----

## class ----

#' `NextBestDualEndpoint`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`NextBestDualEndpoint`] is the class for next best dose that is based on the
#' dual endpoint model.
#'
#' @details Under this rule, at first admissible doses are found, which are those
#' with toxicity probability to fall in `overdose` category and being below
#' `max_overdose_prob`. Next, it picks (from the remaining admissible doses) the
#' one that maximizes the probability to be in the `target` biomarker range. By
#' default (`target_relative = TRUE`) the target is specified as relative to the
#' maximum biomarker level across the dose grid or relative to the `Emax`
#' parameter in case a parametric model was selected (i.e. [`DualEndpointBeta`],
#' [`DualEndpointEmax`]). However, if `target_relative = FALSE`, then the
#' absolute biomarker range can be used as a target.
#'
#' @slot target (`numeric`)\cr the biomarker target range that needs to be
#'   reached. For example, the target range \eqn{(0.8, 1.0)} and
#'   `target_relative = TRUE` means that we target a dose with at least
#'   \eqn{80\%} of maximum biomarker level. As an other example,
#'   \eqn{(0.5, 0.8)} would mean that we target a dose between \eqn{50\%} and
#'   \eqn{80\%} of the maximum biomarker level.
#' @slot overdose (`numeric`)\cr the overdose toxicity interval (lower limit
#'   excluded, upper limit included).
#' @slot max_overdose_prob (`proportion`)\cr maximum overdose probability that
#'   is allowed.
#' @slot target_relative (`flag`)\cr is `target` specified as relative? If
#'   `TRUE`, then the `target` is interpreted relative to the maximum, so it
#'   must be a probability range. Otherwise, the `target` is interpreted as
#'   absolute biomarker range.
#' @slot target_thresh (`proportion`)\cr a target probability threshold that
#'   needs to be fulfilled before the target probability will be used for
#'   deriving the next best dose (default to \eqn{0.01}).
#'
#' @aliases NextBestDualEndpoint
#' @export
#'
.NextBestDualEndpoint <- setClass(
  Class = "NextBestDualEndpoint",
  slots = c(
    target = "numeric",
    overdose = "numeric",
    max_overdose_prob = "numeric",
    target_relative = "logical",
    target_thresh = "numeric"
  ),
  prototype = prototype(
    target = c(0.9, 1),
    overdose = c(0.35, 1),
    max_overdose_prob = 0.25,
    target_relative = TRUE,
    target_thresh = 0.01
  ),
  contains = "NextBest",
  validity = v_next_best_dual_endpoint
)

## constructor ----

#' @rdname NextBestDualEndpoint-class
#'
#' @param target (`numeric`)\cr see slot definition.
#' @param overdose (`numeric`)\cr see slot definition.
#' @param max_overdose_prob (`proportion`)\cr see slot definition.
#' @param target_relative (`flag`)\cr see slot definition.
#' @param target_thresh (`proportion`)\cr see slot definition.
#'
#' @export
#' @example examples/Rules-class-NextBestDualEndpoint.R
#'
NextBestDualEndpoint <- function(target,
                                 overdose,
                                 max_overdose_prob,
                                 target_relative = TRUE,
                                 target_thresh = 0.01) {
  .NextBestDualEndpoint(
    target = target,
    overdose = overdose,
    max_overdose_prob = max_overdose_prob,
    target_relative = target_relative,
    target_thresh = target_thresh
  )
}

# NextBestMinDist ----

## class ----

#' `NextBestMinDist`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`NextBestMinDist`] is the class for next best dose that is based on minimum
#' distance to target probability.
#'
#' @slot target (`proportion`)\cr single target toxicity probability.
#'
#' @aliases NextBestMinDist
#' @export
#'
.NextBestMinDist <- setClass(
  Class = "NextBestMinDist",
  slots = c(
    target = "numeric"
  ),
  prototype = prototype(
    target = 0.3
  ),
  contains = "NextBest",
  validity = v_next_best_min_dist
)

## constructor ----

#' @rdname NextBestMinDist-class
#'
#' @param target (`proportion`)\cr see slot definition.
#'
#' @export
#'
NextBestMinDist <- function(target) {
  .NextBestMinDist(target = target)
}

# NextBestInfTheory ----

## class ----

#' `NextBestInfTheory`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`NextBestInfTheory`] is the class for next best dose that is based on
#' information theory as proposed in https://doi.org/10.1002/sim.8450.
#'
#' @slot target (`proportion`)\cr target toxicity probability.
#' @slot asymmetry (`number`)\cr value of the asymmetry exponent in the
#'   divergence function that describes the rate of penalization for overly
#'   toxic does. It must be a value from (0, 2) interval.
#'
#' @aliases NextBestInfTheory
#' @export
#'
.NextBestInfTheory <- setClass(
  Class = "NextBestInfTheory",
  slots = c(
    target = "numeric",
    asymmetry = "numeric"
  ),
  prototype = prototype(
    target = 0.3,
    asymmetry = 1
  ),
  contains = "NextBest",
  validity = v_next_best_inf_theory
)

## constructor ----

#' @rdname NextBestInfTheory-class
#'
#' @param target (`proportion`)\cr see slot definition.
#' @param asymmetry (`number`)\cr see slot definition.
#'
#' @export
#'
NextBestInfTheory <- function(target, asymmetry) {
  .NextBestInfTheory(target = target, asymmetry = asymmetry)
}

# NextBestTD ----

## class ----

#' `NextBestTD`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`NextBestTD`] is the class to find a next best dose based on pseudo
#' DLT model without samples. Namely, it is to find two next best doses, one
#' for allocation during the trial and the second for final recommendation at
#' the end of a trial without involving any samples, i.e. only DLT responses
#' will be incorporated for the dose-allocation. This is based solely on the
#' probabilities of the occurrence of a DLT obtained by using the modal estimates
#' of the model parameters. There are two target probabilities of the
#' occurrence of a DLT that must be specified: target probability to be used
#' during the trial and target probability to be used at the end of the trial.
#' It is suitable to use it only with the [`ModelTox`] model class.
#'
#' @slot prob_target_drt (`proportion`)\cr the target probability of the
#'   occurrence of a DLT to be used during the trial.
#' @slot prob_target_eot (`proportion`)\cr the target probability of the
#'   occurrence of a DLT to be used at the end of the trial.
#'
#' @aliases NextBestTD
#' @export
#'
.NextBestTD <- setClass(
  Class = "NextBestTD",
  slots = c(
    prob_target_drt = "numeric",
    prob_target_eot = "numeric"
  ),
  prototype = prototype(
    prob_target_drt = 0.35,
    prob_target_eot = 0.3
  ),
  contains = "NextBest",
  validity = v_next_best_td
)

## constructor ----

#' @rdname NextBestTD-class
#'
#' @param prob_target_drt (`proportion`)\cr see slot definition.
#' @param prob_target_eot (`proportion`)\cr see slot definition.
#'
#' @export
#' @examples
#' my_next_best <- NextBestTD(0.35, 0.3)
NextBestTD <- function(prob_target_drt, prob_target_eot) {
  .NextBestTD(
    prob_target_drt = prob_target_drt,
    prob_target_eot = prob_target_eot
  )
}

# NextBestTDsamples ----

## class ----

#' `NextBestTDsamples`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`NextBestTDsamples`] is the class to find a next best dose based on Pseudo
#' DLT model with samples. Namely, it is to find two next best doses, one
#' for allocation during the trial and the second for final recommendation at
#' the end of a trial. Hence, there are two target probabilities of the
#' occurrence of a DLT that must be specified: target probability to be used
#' during the trial and target probability to be used at the end of the trial.
#'
#' @slot derive (`function`)\cr derives, based on a vector of posterior dose
#'   samples, the target dose that has the probability of the occurrence of
#'   DLT equals to either the `prob_target_drt` or `prob_target_eot`. It must
#'   therefore accept one and only one argument, which is a numeric vector, and
#'   return a number.
#'
#' @aliases NextBestTDsamples
#' @export
#'
.NextBestTDsamples <- setClass(
  Class = "NextBestTDsamples",
  slots = c(
    derive = "function"
  ),
  prototype = prototype(
    derive = function(dose_samples) {
      quantile(dose_samples, prob = 0.3)
    }
  ),
  contains = "NextBestTD",
  validity = v_next_best_td_samples
)

## constructor ----

#' @rdname NextBestTDsamples-class
#'
#' @param prob_target_drt (`proportion`)\cr see slot definition in [`NextBestTD`].
#' @param prob_target_eot (`proportion`)\cr see slot definition in [`NextBestTD`].
#' @param derive (`function`)\cr see slot definition.
#'
#' @export
#' @example examples/Rules-class-NextBestTDsamples.R
#'
NextBestTDsamples <- function(prob_target_drt, prob_target_eot, derive) {
  .NextBestTDsamples(
    prob_target_drt = prob_target_drt,
    prob_target_eot = prob_target_eot,
    derive = derive
  )
}

# NextBestMaxGain ----

## class ----

#' `NextBestMaxGain`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`NextBestMaxGain`] is the class to find a next best dose with maximum gain
#' value based on a pseudo DLT and efficacy models without samples. It is based
#' solely on the probabilities of the occurrence of a DLT and the values
#' of the mean efficacy responses obtained by using the modal estimates of the
#' DLT and efficacy model parameters. There are two target probabilities of the
#' occurrence of a DLT that must be specified: target probability to be used
#' during the trial and target probability to be used at the end of the trial.
#' It is suitable to use it only with the [`ModelTox`] model and [`ModelEff`]
#' classes (except [`EffFlexi`]).
#'
#' @slot prob_target_drt (`proportion`)\cr the target probability of the
#'   occurrence of a DLT to be used during the trial.
#' @slot prob_target_eot (`proportion`)\cr the target probability of the
#'   occurrence of a DLT to be used at the end of the trial.
#'
#' @aliases NextBestMaxGain
#' @export
#'
.NextBestMaxGain <- setClass(
  Class = "NextBestMaxGain",
  slots = c(
    prob_target_drt = "numeric",
    prob_target_eot = "numeric"
  ),
  prototype = prototype(
    prob_target_drt = 0.35,
    prob_target_eot = 0.3
  ),
  contains = "NextBest",
  validity = v_next_best_td
)

## constructor ----

#' @rdname NextBestMaxGain-class
#'
#' @param prob_target_drt (`proportion`)\cr see slot definition.
#' @param prob_target_eot (`proportion`)\cr see slot definition.
#'
#' @export
#' @examples
#' my_next_best <- NextBestMaxGain(0.35, 0.3)
NextBestMaxGain <- function(prob_target_drt, prob_target_eot) {
  .NextBestMaxGain(
    prob_target_drt = prob_target_drt,
    prob_target_eot = prob_target_eot
  )
}

# NextBestMaxGainSamples ----

## class ----

#' `NextBestMaxGainSamples`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`NextBestMaxGainSamples`] is the class to find a next best dose with maximum
#' gain value based on a pseudo DLT and efficacy models and DLT and efficacy
#' samples. There are two target probabilities of the occurrence of a DLT that
#' must be specified: target probability to be used during the trial and target
#' probability to be used at the end of the trial.
#' It is suitable to use it only with the [`ModelTox`] model and [`ModelEff`]
#' classes.
#'
#' @slot derive (`function`)\cr derives, based on a vector of posterior dose
#'   samples, the target dose that has the probability of the occurrence of
#'   DLT equals to either the `prob_target_drt` or `prob_target_eot`. It must
#'   therefore accept one and only one argument, which is a numeric vector, and
#'   return a number.
#' @slot mg_derive (`function`)\cr derives, based on a vector of posterior dose
#'   samples that give the maximum gain value, the final next best estimate of
#'   the dose that gives the maximum gain value. It must therefore accept one
#'   and only one argument, which is a numeric vector, and return a number.
#'
#' @aliases NextBestMaxGainSamples
#' @export
#'
.NextBestMaxGainSamples <- setClass(
  Class = "NextBestMaxGainSamples",
  slots = c(
    derive = "function",
    mg_derive = "function"
  ),
  prototype = prototype(
    prob_target_drt = 0.35,
    prob_target_eot = 0.3,
    derive = function(dose_samples) {
      as.numeric(quantile(dose_samples, prob = 0.3))
    },
    mg_derive = function(dose_samples) {
      as.numeric(quantile(dose_samples, prob = 0.5))
    }
  ),
  contains = "NextBestMaxGain",
  validity = v_next_best_max_gain_samples
)

## constructor ----

#' @rdname NextBestMaxGainSamples-class
#'
#' @param prob_target_drt (`proportion`)\cr see slot definition in [`NextBestMaxGain`].
#' @param prob_target_eot (`proportion`)\cr see slot definition in [`NextBestMaxGain`].
#' @param derive (`function`)\cr see slot definition.
#' @param mg_derive (`function`)\cr see slot definition.
#'
#' @export
#' @example examples/Rules-class-NextBestMaxGainSamples.R
#'
NextBestMaxGainSamples <- function(prob_target_drt,
                                   prob_target_eot,
                                   derive,
                                   mg_derive) {
  .NextBestMaxGainSamples(
    prob_target_drt = prob_target_drt,
    prob_target_eot = prob_target_eot,
    derive = derive,
    mg_derive = mg_derive
  )
}

# Increments ----

## class ----

#' `Increments`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`Increments`] is a virtual class for controlling increments, from which all
#' other specific increments classes inherit.
#'
#' @seealso [`IncrementsRelative`], [`IncrementsRelativeDLT`],
#'   [`IncrementsNumDoseLevels`], [`IncrementsHSRBeta`], [`IncrementMin`].
#'
#' @aliases Increments
#' @export
#'
setClass(
  Class = "Increments"
)

# IncrementsAbsolute ----

## class ----

#' `IncrementsAbsolute`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Increments control based on absolute differences in intervals
#'
#' #' Note that \code{intervals} is to be read as follows. If for example,
#' we want to specify three intervals: First 0 to less than 50, second at least
#' 50 up to less than 100 mg, and third at least 100 mg, then we specify
#' \code{intervals} to be \code{c(0, 50, 100)}. That means, the right
#' bound of the intervals are exclusive to the interval, and the last interval
#' goes from the last value to infinity.
#'
#' @slot intervals a vector with the left bounds of the relevant intervals
#' @slot increments a vector of the same length with the maximum allowable
#' absolute increments in the \code{intervals}
#' @aliases IncrementsAbsolute
#' @export
.IncrementsAbsolute <- setClass(
    Class="IncrementsAbsolute",
    representation(
      intervals="numeric",
      increments="numeric"
    ),
    prototype(
      intervals=c(0, 2),
      increments=c(2, 1)
    ),
    contains="Increments"
)

## constructor ----

#' @rdname IncrementsAbsolute-class
#' @param intervals (`numeric`) \cr a vector of right hand boundaries of the intervals
#' @param increments (`numeric`) \cr a vector of the maximum increment allowed in the corresponding interval
#' @return the \code{\linkS4class{IncrementsAbsolute}} object
#'
#' @export
#' @importClassesFrom crmPack Increments
IncrementsAbsolute <- function(
                        intervals=c(5, 10),
                        increments=c(2, 1)
                      ) {
  .IncrementsAbsolute(intervals=intervals,
                      increments=increments)
}

# IncrementsRelative ----

## class ----

#' `IncrementsRelative`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`IncrementsRelative`] is the class for increments control based on relative
#' differences in intervals.
#'
#' @slot intervals (`numeric`)\cr a vector with the left bounds of the relevant
#'   intervals. This parameters specifies the right bounds of the intervals.
#'   For example, `intervals  = c(0, 50, 100)` specifies three intervals:
#'   (0, 50), [50, 100) and [100, +Inf). That means, the right bound of the
#'   intervals are exclusive to the interval and the last interval goes from the
#'   last value to infinity.
#' @slot increments (`numeric`)\cr a vector of the same length with the maximum
#'   allowable relative increments in the `intervals`.
#'
#' @aliases IncrementsRelative
#' @export
#'
.IncrementsRelative <- setClass(
  Class = "IncrementsRelative",
  slots = c(
    intervals = "numeric",
    increments = "numeric"
  ),
  prototype = prototype(
    intervals = c(0, 2),
    increments = c(2, 1)
  ),
  contains = "Increments",
  validity = v_increments_relative
)

## constructor ----

#' @rdname IncrementsRelative-class
#'
#' @param intervals (`numeric`)\cr see slot definition in [`IncrementsRelative`].
#' @param increments (`numeric`)\cr see slot definition in [`IncrementsRelative`].
#'
#' @export
#' @example examples/Rules-class-IncrementsRelative.R
#'
IncrementsRelative <- function(intervals, increments) {
  .IncrementsRelative(
    intervals = intervals,
    increments = increments
  )
}

# IncrementsRelativeParts ----

## class ----

#' `IncrementsRelativeParts`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`IncrementsRelativeParts`] is the class for increments control based on
#' relative differences in intervals, with special rules for part 1 and
#' beginning of part 2.
#'
#' @details This class works only conjunction with [`DataParts`] objects. If the
#' part 2 will just be started in the next cohort, then the next maximum dose
#' will be either `dlt_start` (e.g. -1) shift of the last part 1 dose in case of
#' a DLT in part 1, or `clean_start` shift (e.g. 0) in case of no DLTs in part 1.
#' If part 1 will still be on in the next cohort, then the next dose level will
#' be the next higher dose level in the `part1Ladder` slot of the data object.
#' If part 2 has been started before, the usual relative increment rules apply,
#' see [`IncrementsRelative`].
#'
#' @slot dlt_start (`integer`)\cr the dose level increment for starting part 2
#'   in case of a DLT in part 1.
#' @slot clean_start (`integer`)\cr the dose level increment for starting part 2
#'   in case of a DLT in part 1. If this is less or equal to 0, then the part 1
#'   ladder will be used to find the maximum next dose. Otherwise, the relative
#'   increment rules will be applied to find the next maximum dose level.
#'
#' @aliases IncrementsRelativeParts
#' @export
#'
.IncrementsRelativeParts <- setClass(
  Class = "IncrementsRelativeParts",
  slots = representation(
    dlt_start = "integer",
    clean_start = "integer"
  ),
  prototype = prototype(
    dlt_start = -1L,
    clean_start = 1L
  ),
  contains = "IncrementsRelative",
  validity = v_increments_relative_parts
)

## constructor ----

#' @rdname IncrementsRelativeParts-class
#'
#' @param dlt_start (`numeric`)\cr see slot definition in [`IncrementsRelativeParts`].
#' @param clean_start (`numeric`)\cr see slot definition in [`IncrementsRelativeParts`].
#' @inheritDotParams IncrementsRelative
#'
#' @export
#' @example examples/Rules-class-IncrementsRelative-DataParts.R
#'
IncrementsRelativeParts <- function(dlt_start, clean_start, ...) {
  .IncrementsRelativeParts(
    dlt_start = safeInteger(dlt_start),
    clean_start = safeInteger(clean_start),
    ...
  )
}

# IncrementsRelativeDLT ----

## class ----

#' `IncrementsRelativeDLT`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`IncrementsRelativeDLT`] is the class for increments control based on
#' relative differences in terms of DLTs.
#'
#' @slot dlt_intervals (`integer`)\cr an vector with the left bounds of the
#'   relevant DLT intervals. This parameters specifies the right bounds of the
#'   intervals. For example, `dlt_intervals  = c(0, 1, 3)` specifies three
#'   intervals (sets of DLTs: first, 0 DLT; second 1 or 2 DLTs; and the third
#'   one, at least 3 DLTs. That means, the right bound of the intervals are
#'   exclusive to the interval and the last interval goes from the last value to
#'   infinity.
#' @slot increments (`numeric`)\cr a vector of maximum allowable relative
#'   increments corresponding to `dlt_intervals`. IT must be of the same length
#'   as the length of `dlt_intervals`.
#'
#' @note This considers all DLTs across all cohorts observed so far.
#'
#' @seealso [IncrementsRelativeDLTCurrent] which only considers the DLTs
#'   in the current cohort.
#'
#' @aliases IncrementsRelativeDLT
#' @export
#'
.IncrementsRelativeDLT <- setClass(
  Class = "IncrementsRelativeDLT",
  slots = representation(
    dlt_intervals = "integer",
    increments = "numeric"
  ),
  prototype = prototype(
    dlt_intervals = c(0L, 1L),
    increments = c(2, 1)
  ),
  contains = "Increments",
  validity = v_increments_relative_dlt
)

## constructor ----

#' @rdname IncrementsRelativeDLT-class
#'
#' @param dlt_intervals (`numeric`)\cr see slot definition in [`IncrementsRelativeDLT`].
#' @param increments (`numeric`)\cr see slot definition in [`IncrementsRelativeDLT`].
#'
#' @export
#' @example examples/Rules-class-IncrementsRelativeDLT.R
#'
IncrementsRelativeDLT <- function(dlt_intervals, increments) {
  assert_integerish(dlt_intervals)

  .IncrementsRelativeDLT(
    dlt_intervals = safeInteger(dlt_intervals),
    increments = increments
  )
}

# IncrementsRelativeDLTCurrent ----

## class ----

#' `IncrementsRelativeDLTCurrent`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`IncrementsRelativeDLTCurrent`] is the class for increments control based on
#' relative differences and current DLTs. The class is based on the number of
#' DLTs observed in the current cohort, but not cumulatively over all cohorts
#' so far.
#'
#' @seealso [IncrementsRelativeDLT].
#'
#' @aliases IncrementsRelativeDLTCurrent
#' @export
#'
.IncrementsRelativeDLTCurrent <- setClass(
  Class = "IncrementsRelativeDLTCurrent",
  contains = "IncrementsRelativeDLT"
)

## constructor ----

#' @rdname IncrementsRelativeDLTCurrent-class
#'
#' @inheritParams IncrementsRelativeDLT
#'
#' @export
#' @example examples/Rules-class-IncrementsRelativeDLTCurrent.R
#'
IncrementsRelativeDLTCurrent <- function(dlt_intervals = c(0, 1),
                                         increments = c(2, 1)) {
  .IncrementsRelativeDLTCurrent(
    dlt_intervals = safeInteger(dlt_intervals),
    increments = increments
  )
}

# nolint end

# IncrementsNumDoseLevels-class ----

#' `IncrementsNumDoseLevels`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @slot maxLevels (`count`)\cr corresponding to the number of maximum
#'   dose levels to increment for the next dose. It defaults to 1,
#' which means that no dose skipping is allowed - the next dose
#' can be maximum one level higher than the current dose.
#' @slot basisLevel (`string`)\cr corresponding to the dose level used to increment from.
#' It can take two possible values `last` or `max`. If `last` (default)
#' is specified the increments is applied to the last given dose and if
#' `max` is specified the increment is applied from the max given dose
#' level.
#'
#' @example examples/Rules-class-IncrementsNumDoseLevels.R
#' @export
.IncrementsNumDoseLevels <- setClass(
  Class = "IncrementsNumDoseLevels",
  contains = "Increments",
  representation = representation(
    maxLevels = "integer",
    basisLevel = "character"
  ),
  prototype(
    maxLevels = 1L,
    basisLevel = "last"
  ),
  validity = v_increments_numdoselevels
)

# IncrementsNumDoseLevels-constructor ----

#' @rdname IncrementsNumDoseLevels-class
#' @param maxLevels see below.
#' @param basisLevel see below.
#' @export
IncrementsNumDoseLevels <- function(maxLevels=1,
                                    basisLevel="last"){
  .IncrementsNumDoseLevels(
    maxLevels=safeInteger(maxLevels),
    basisLevel=basisLevel
  )
}


# IncrementsHSRBeta-class ----

#' `IncrementsHSRBeta`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`IncrementsHSRBeta`] is a class for limiting further increments using
#' a Hard Safety Rule based on the Bin-Beta model.
#' Increment control is based on the number of observed DLTs and number of
#' subjects at each dose level. The probability of toxicity is calculated
#' using a Bin-Beta model with prior (a,b). If the probability exceeds
#' the threshold for a given dose, that dose and all doses above are excluded
#' from further escalation.
#' This is a hard safety rule that limits further escalation based on the
#' observed data per dose level, independent from the underlying model.
#'
#' @slot target (`proportion`)\cr the target toxicity.
#' @slot prob (`proportion`)\cr the threshold probability for a dose being toxic.
#' @slot a (`number`)\cr shape parameter a>0 of probability
#'  distribution Beta (a,b).
#' @slot b (`number`)\cr shape parameter b>0 of probability
#'  distribution Beta (a,b).
#'
#' @aliases IncrementsHSRBeta
#' @export
#'
.IncrementsHSRBeta <- setClass(
  Class = "IncrementsHSRBeta",
  contains = "Increments",
  representation(
    target = "numeric",
    prob = "numeric",
    a = "numeric",
    b = "numeric"
  ),
  prototype(
    target = 0.3,
    prob = 0.95,
    a = 1,
    b = 1
  ),
  validity = v_increments_hsr_beta
)

# IncrementsHSRBeta-constructor ----

#' @rdname IncrementsHSRBeta-class
#'
#' @param target (`proportion`)\cr see slot definition.
#' @param prob (`proportion`)\cr see slot definition.
#' @param a (`number`)\cr see slot definition.
#' @param b (`number`)\cr see slot definition.
#'
#' @example examples/Rules-class-IncrementsHSRBeta.R
#' @export
#'
IncrementsHSRBeta <- function(target = 0.3,
                              prob = 0.95,
                              a = 1,
                              b = 1) {
  .IncrementsHSRBeta(
    target = target,
    prob = prob,
    a = a,
    b = b
  )
}

# nolint start

## -----------------------------------------------------------
## Max increment based on minimum of multiple increment rules
## -----------------------------------------------------------

##' Max increment based on minimum of multiple increment rules
##'
##' This class can be used to combine multiple increment rules with the MIN
##' operation.
##'
##' \code{IncrementsList} contains all increment rules, which are again
##' objects of class \code{\linkS4class{Increments}}. The minimum of these
##' individual increments is taken to give the final maximum increment.
##'
##' @slot IncrementsList list of increment rules
##'
##' @example examples/Rules-class-IncrementMin.R
##' @keywords classes
##' @export
.IncrementMin <-
  setClass(Class="IncrementMin",
           representation(IncrementsList="list"),
           prototype(IncrementsList=
                       list(IncrementsRelativeDLT(dlt_intervals=as.integer(c(0, 1)),
                                                  increments=c(2, 1)),
                            IncrementsRelative(intervals=c(0, 2),
                                               increments=c(2, 1)))),
           contains="Increments",
           validity=
             function(object){
               o <- Validate()

               o$check(all(sapply(object@IncrementsList, is,
                                  "Increments")),
                       "all IncrementsList elements have to be Increments objects")

               o$result()
             })
validObject(.IncrementMin())


##' Initialization function for "IncrementMin"
##'
##' @param IncrementsList see \code{\linkS4class{IncrementMin}}
##' @return the \code{\linkS4class{IncrementMin}} object
##'
##' @export
##' @keywords methods
IncrementMin <- function(IncrementsList)
{
  .IncrementMin(IncrementsList=IncrementsList)
}

# Stopping-class ----

#' `Stopping`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`Stopping`] is a class for for stopping rules.
#'
#' @seealso [`StoppingList`], [`StoppingCohortsNearDose`], [`StoppingPatientsNearDose`],
#'   [`StoppingMinCohorts`], [`StoppingMinPatients`], [`StoppingTargetProb`],
#'   [`StoppingMTDdistribution`], [`StoppingTargetBiomarker`], [`StoppingHighestDose`]
#'   [`StoppingMTDCV`], [`StoppingLowestDoseHSRBeta`].
#'
#' @aliases Stopping
#' @export
#'
setClass(
  Class = "Stopping",
  contains = list("VIRTUAL")
)


## --------------------------------------------------
## Stopping based on number of cohorts near to next best dose
## --------------------------------------------------

##' Stop based on number of cohorts near to next best dose
##'
##' @slot nCohorts number of required cohorts
##' @slot percentage percentage (between 0 and 100) within the next best dose
##' the cohorts must lie
##'
##' @example examples/Rules-class-StoppingCohortsNearDose.R
##' @keywords classes
##' @export
.StoppingCohortsNearDose <-
    setClass(Class="StoppingCohortsNearDose",
             representation(nCohorts="integer",
                            percentage="numeric"),
             prototype(nCohorts=2L,
                       percentage=50),
             contains="Stopping",
             validity=function(object){
                 o <- Validate()

                 o$check((object@nCohorts > 0L) && is.scalar(object@nCohorts),
                         "nCohorts must be positive scalar")
                 o$check(is.probability(object@percentage / 100),
                         "percentage must be between 0 and 100")

                 o$result()
             })
validObject(.StoppingCohortsNearDose())

##' Initialization function for "StoppingCohortsNearDose"
##'
##' @param nCohorts see \code{\linkS4class{StoppingCohortsNearDose}}
##' @param percentage see \code{\linkS4class{StoppingCohortsNearDose}}
##' @return the \code{\linkS4class{StoppingCohortsNearDose}} object
##'
##' @export
##' @keywords methods
StoppingCohortsNearDose <- function(nCohorts,
                                    percentage)
{
    .StoppingCohortsNearDose(nCohorts=safeInteger(nCohorts),
                             percentage=percentage)
}
## --------------------------------------------------
## Stopping based on number of patients near to next best dose
## --------------------------------------------------

##' Stop based on number of patients near to next best dose
##'
##' @slot nPatients number of required patients
##' @slot percentage percentage (between 0 and 100) within the next best dose
##' the patients must lie
##'
##' @example examples/Rules-class-StoppingPatientsNearDose.R
##' @keywords classes
##' @export
.StoppingPatientsNearDose <-
    setClass(Class="StoppingPatientsNearDose",
             representation(nPatients="integer",
                            percentage="numeric"),
             prototype(nPatients=10L,
                       percentage=50),
             contains="Stopping",
             validity=function(object){
                 o <- Validate()

                 o$check((object@nPatients > 0L) && is.scalar(object@nPatients),
                         "nPatients must be positive scalar")
                 o$check(is.probability(object@percentage / 100),
                         "percentage must be between 0 and 100")

                 o$result()
             })
validObject(.StoppingPatientsNearDose())


##' Initialization function for "StoppingPatientsNearDose"
##'
##' @param nPatients see \code{\linkS4class{StoppingPatientsNearDose}}
##' @param percentage see \code{\linkS4class{StoppingPatientsNearDose}}
##' @return the \code{\linkS4class{StoppingPatientsNearDose}} object
##'
##' @export
##' @keywords methods
StoppingPatientsNearDose <- function(nPatients,
                                     percentage)
{
    .StoppingPatientsNearDose(nPatients=safeInteger(nPatients),
                              percentage=percentage)
}


## --------------------------------------------------
## Stopping based on minimum number of cohorts
## --------------------------------------------------

##' Stop based on minimum number of cohorts
##'
##' @slot nCohorts minimum required number of cohorts
##'
##' @example examples/Rules-class-StoppingMinCohorts.R
##' @keywords classes
##' @export
.StoppingMinCohorts <-
    setClass(Class="StoppingMinCohorts",
             representation(nCohorts="integer"),
             prototype(nCohorts=3L),
             contains="Stopping",
             validity=function(object){
                 o <- Validate()

                 o$check((object@nCohorts > 0L) && is.scalar(object@nCohorts),
                         "nCohorts must be positive scalar")

                 o$result()
             })
validObject(.StoppingMinCohorts())



##' Initialization function for "StoppingMinCohorts"
##'
##' @param nCohorts see \code{\linkS4class{StoppingMinCohorts}}
##' @return the \code{\linkS4class{StoppingMinCohorts}} object
##'
##' @export
##' @keywords methods
StoppingMinCohorts <- function(nCohorts)
{
    .StoppingMinCohorts(nCohorts=safeInteger(nCohorts))
}


## --------------------------------------------------
## Stopping based on minimum number of patients
## --------------------------------------------------

##' Stop based on minimum number of patients
##'
##' @slot nPatients minimum allowed number of patients
##'
##' @example examples/Rules-class-StoppingMinPatients.R
##' @keywords classes
##' @export
.StoppingMinPatients <-
    setClass(Class="StoppingMinPatients",
             representation(nPatients="integer"),
             prototype(nPatients=20L),
             contains="Stopping",
             validity=function(object){
                 o <- Validate()

                 o$check((object@nPatients > 0L) && is.scalar(object@nPatients),
                         "nPatients must be positive scalar")

                 o$result()
             })
validObject(.StoppingMinPatients())

##' Initialization function for "StoppingMinPatients"
##'
##' @param nPatients see \code{\linkS4class{StoppingMinPatients}}
##' @return the \code{\linkS4class{StoppingMinPatients}} object
##'
##' @export
##' @keywords methods
StoppingMinPatients <- function(nPatients)
{
    .StoppingMinPatients(nPatients=safeInteger(nPatients))
}


## --------------------------------------------------
## Stopping based on probability of target tox interval
## --------------------------------------------------

##' Stop based on probability of target tox interval
##'
##' @slot target the target toxicity interval, e.g. \code{c(0.2, 0.35)}
##' @slot prob required target toxicity probability (e.g. \code{0.4})
##' for reaching sufficient precision
##'
##' @example examples/Rules-class-StoppingTargetProb.R
##' @keywords classes
##' @export
.StoppingTargetProb <-
    setClass(Class="StoppingTargetProb",
             representation(target="numeric",
                            prob="numeric"),
             prototype(target=c(0.2, 0.35),
                       prob=0.4),
             contains="Stopping",
             validity=
                 function(object){
                     o <- Validate()

                     o$check(is.probRange(object@target),
                             "target must be probability range")
                     o$check(is.probability(object@prob,
                                            bounds=FALSE),
                             "prob must be probability > 0 and < 1")

                     o$result()
                 })
validObject(.StoppingTargetProb())


##' Initialization function for "StoppingTargetProb"
##'
##' @param target see \code{\linkS4class{StoppingTargetProb}}
##' @param prob see \code{\linkS4class{StoppingTargetProb}}
##' @return the \code{\linkS4class{StoppingTargetProb}} object
##'
##' @export
##' @keywords methods
StoppingTargetProb <- function(target,
                               prob)
{
    .StoppingTargetProb(target=target,
                        prob=prob)
}


## --------------------------------------------------
## Stopping based on MTD distribution
## --------------------------------------------------

##' Stop based on MTD distribution
##'
##' Has 90% probability above a threshold of 50% of the current
##' MTD been reached? This class is used for this question.
##'
##' @slot target the target toxicity probability (e.g. 0.33) defining the MTD
##' @slot thresh the threshold relative to the MTD (e.g. 0.5)
##' @slot prob required probability (e.g. 0.9)
##'
##' @example examples/Rules-class-StoppingMTDdistribution.R
##' @keywords classes
##' @export
.StoppingMTDdistribution <-
    setClass(Class="StoppingMTDdistribution",
             representation(target="numeric",
                            thresh="numeric",
                            prob="numeric"),
             prototype(target=0.33,
                       thresh=0.5,
                       prob=0.9),
             contains="Stopping",
             validity=
                 function(object){
                     o <- Validate()

                     o$check(is.probability(object@target,
                                            bounds=FALSE),
                             "target must be probability > 0 and < 1")
                     o$check(is.probability(object@thresh,
                                            bounds=FALSE),
                             "thresh must be probability > 0 and < 1")
                     o$check(is.probability(object@prob,
                                            bounds=FALSE),
                             "prob must be probability > 0 and < 1")

                     o$result()
                 })
validObject(.StoppingMTDdistribution())


##' Initialization function for "StoppingMTDdistribution"
##'
##' @param target see \code{\linkS4class{StoppingMTDdistribution}}
##' @param thresh see \code{\linkS4class{StoppingMTDdistribution}}
##' @param prob see \code{\linkS4class{StoppingMTDdistribution}}
##' @return the \code{\linkS4class{StoppingMTDdistribution}} object
##'
##' @export
##' @keywords methods
StoppingMTDdistribution <- function(target,
                                    thresh,
                                    prob)
{
    .StoppingMTDdistribution(target=target,
                             thresh=thresh,
                             prob=prob)
}

# nolint end

# StoppingMTDCV-class ----

#' `StoppingMTDCV`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`StoppingMTDCV`] is a class for stopping rule based on precision of MTD
#' which is calculated as the coefficient of variation (CV) of the MTD.
#'
#' @slot target (`proportion`)\cr toxicity target of MTD.
#' @slot thresh_cv (`number`)\cr threshold for CV to be considered accurate enough
#'   to stop the trial.
#'
#' @aliases StoppingMTDCV
#' @export
#'
.StoppingMTDCV <- setClass(
  Class = "StoppingMTDCV",
  contains = "Stopping",
  representation = representation(
    target = "numeric",
    thresh_cv = "numeric"
  ),
  prototype(
    target = 0.3,
    thresh_cv = 40
  ),
  validity = v_stopping_mtd_cv
)

# StoppingMTDCV-constructor ----

#' @rdname StoppingMTDCV-class
#'
#' @param target (`proportion`)\cr see slot definition.
#' @param thresh_cv (`number`)\cr see slot definition.
#'
#' @export
#' @example examples/Rules-class-StoppingMTDCV.R
#'
StoppingMTDCV <- function(target = 0.3,
                          thresh_cv = 40) {
  .StoppingMTDCV(
    target = target,
    thresh_cv = thresh_cv
  )
}


# StoppingLowestDoseHSRBeta-class ----

#' `StoppingLowestDoseHSRBeta`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`StoppingLowestDoseHSRBeta`] is a class for stopping based on a Hard Safety
#' Rule using the Beta posterior distribution with Beta(a,b) prior and a
#' Bin-Beta model based on the observed data at the lowest dose level.
#' The rule is triggered when the first dose is considered to be toxic
#' (i.e. above threshold probability) based on the observed data at the
#' lowest dose level and a Beta(a,b) prior distribution.
#' The default prior is Beta(1,1).
#' In case that placebo is used, the rule is evaluated at the second dose of the
#' dose grid, i.e. at the lowest non-placebo dose.
#' Note: this stopping rule is independent from the underlying model.
#'
#' @slot target (`proportion`)\cr the target toxicity.
#' @slot prob (`proportion`)\cr the threshold probability for the lowest
#'  dose being toxic.
#' @slot a (`number`)\cr shape parameter a>0 of probability
#'  distribution Beta (a,b).
#' @slot b (`number`)\cr shape parameter b>0 of probability
#'  distribution Beta (a,b).
#'
#' @aliases StoppingLowestDoseHSRBeta
#' @export
#'
.StoppingLowestDoseHSRBeta <- setClass(
  Class = "StoppingLowestDoseHSRBeta",
  contains = "Stopping",
  representation = representation(
    target = "numeric",
    prob = "numeric",
    a = "numeric",
    b = "numeric"
  ),
  prototype(
    target = 0.3,
    prob = 0.95,
    a = 1,
    b = 1
  ),
  validity = v_stopping_lowest_dose_hsr_beta
)


# StoppingLowestDoseHSRBeta-constructor ----

#' @rdname StoppingLowestDoseHSRBeta-class
#'
#' @param target (`proportion`)\cr see slot definition.
#' @param prob (`proportion`)\cr see slot definition.
#' @param a (`number`)\cr see slot definition.
#' @param b (`number`)\cr see slot definition.
#'
#' @export
#' @example examples/Rules-class-StoppingLowestDoseHSRBeta.R
#'
StoppingLowestDoseHSRBeta <- function(target = 0.3,
                                      prob = 0.95,
                                      a = 1,
                                      b = 1) {
  .StoppingLowestDoseHSRBeta(
    target = target,
    prob = prob,
    a = a,
    b = b
  )
}

# nolint start

## --------------------------------------------------
## Stopping based on probability of target biomarker
## --------------------------------------------------

##' Stop based on probability of target biomarker
##'
##' @slot target the biomarker target range, that
##' needs to be reached. For example, (0.8, 1.0) and \code{scale="relative"}
##' means we target a dose with at least 80% of maximum biomarker level.
##' @slot scale either \code{relative} (default, then the \code{target} is interpreted
##' relative to the maximum, so must be a probability range) or \code{absolute}
##' (then the \code{target} is interpreted as absolute biomarker range)
##' @slot prob required target probability for reaching sufficient precision
##'
##' @example examples/Rules-class-StoppingTargetBiomarker.R
##'
##' @export
.StoppingTargetBiomarker <-
    setClass(Class="StoppingTargetBiomarker",
             representation(target="numeric",
                            scale="character",
                            prob="numeric"),
             prototype(target=c(0.9, 1),
                       scale="relative",
                       prob=0.3),
             contains="Stopping",
             validity=
                 function(object){
                     o <- Validate()

                     o$check(is.scalar(object@scale) && object@scale %in% c("relative", "absolute"),
                             "scale must be either 'relative' or 'absolute'")
                     if(object@scale == "relative")
                     {
                       o$check(is.probRange(object@target),
                               "target has to be a probability range when scale='relative'")
                     } else {
                       o$check(is.range(object@target),
                               "target must be a numeric range")
                     }
                     o$check(is.probability(object@prob,
                                            bounds=FALSE),
                             "prob must be probability > 0 and < 1")

                     o$result()
                 })
validObject(.StoppingTargetBiomarker())


##' Initialization function for `StoppingTargetBiomarker`
##'
##' @param target see \code{\linkS4class{StoppingTargetBiomarker}}
##' @param scale see \code{\linkS4class{StoppingTargetBiomarker}}
##' @param prob see \code{\linkS4class{StoppingTargetBiomarker}}
##' @return the \code{\linkS4class{StoppingTargetBiomarker}} object
##'
##' @export
StoppingTargetBiomarker <- function(target,
                                    scale=c("relative", "absolute"),
                                    prob)
{
  scale <- match.arg(scale)
    .StoppingTargetBiomarker(target=target,
                             scale=scale,
                             prob=prob)
}

## --------------------------------------------------
## Stopping when the highest dose is reached
## --------------------------------------------------

##' Stop when the highest dose is reached
##'
##' @example examples/Rules-class-StoppingHighestDose.R
##' @keywords classes
##' @export
.StoppingHighestDose <-
  setClass(Class="StoppingHighestDose",
           contains="Stopping")
validObject(.StoppingHighestDose())

##' Initialization function for "StoppingHighestDose"
##'
##' @return the \code{\linkS4class{StoppingHighestDose}} object
##'
##' @export
##' @keywords methods
StoppingHighestDose <- function()
{
  .StoppingHighestDose()
}


## --------------------------------------------------
## Stopping based on multiple stopping rules
## --------------------------------------------------

##' Stop based on multiple stopping rules
##'
##' This class can be used to combine multiple stopping rules.
##'
##' \code{stopList} contains all stopping rules, which are again objects of
##' class \code{\linkS4class{Stopping}}, and the \code{summary} is a function
##' taking a logical vector of the size of \code{stopList} and returning a
##' single logical value. For example, if the function \code{all} is given as
##' \code{summary} function, then this means that all stopping rules must be
##' fulfilled in order that the result of this rule is to stop.
##'
##' @slot stopList list of stopping rules
##' @slot summary the summary function to combine the results of the stopping
##' rules into a single result
##'
##' @example examples/Rules-class-StoppingList.R
##' @keywords classes
##' @export
.StoppingList <-
    setClass(Class="StoppingList",
             representation(stopList="list",
                            summary="function"),
             prototype(stopList=
                           list(StoppingMinPatients(50),
                                StoppingMinCohorts(5)),
                       summary=all),
             contains="Stopping",
             validity=
                 function(object){
                     o <- Validate()

                     o$check(all(sapply(object@stopList, is, "Stopping")),
                             "all stopList elements have to Stopping objects")
                     testRes <- object@summary(rep(c(TRUE, FALSE),
                                                   length.out=length(object@stopList)))
                     o$check(is.bool(testRes),
                             "summary function must return a boolean value")

                     o$result()
                 })
validObject(.StoppingList())


##' Initialization function for "StoppingList"
##'
##' @param stopList see \code{\linkS4class{StoppingList}}
##' @param summary see \code{\linkS4class{StoppingList}}
##' @return the \code{\linkS4class{StoppingList}} object
##'
##' @export
##' @keywords methods
StoppingList <- function(stopList,
                         summary)
{
    .StoppingList(stopList=stopList,
                  summary=summary)
}


## --------------------------------------------------
## Stopping based on fulfillment of all multiple stopping rules
## --------------------------------------------------

##' Stop based on fulfillment of all multiple stopping rules
##'
##' This class can be used to combine multiple stopping rules with an AND
##' operator.
##'
##' \code{stopList} contains all stopping rules, which are again objects of
##' class \code{\linkS4class{Stopping}}. All stopping rules must be fulfilled in
##' order that the result of this rule is to stop.
##'
##' @slot stopList list of stopping rules
##'
##' @example examples/Rules-class-StoppingAll.R
##' @keywords classes
##' @export
.StoppingAll <-
    setClass(Class="StoppingAll",
             representation(stopList="list"),
             prototype(stopList=
                           list(StoppingMinPatients(50),
                                StoppingMinCohorts(5))),
             contains="Stopping",
             validity=
                 function(object){
                     o <- Validate()

                     o$check(all(sapply(object@stopList, is, "Stopping")),
                             "all stopList elements have to Stopping objects")

                     o$result()
                 })
validObject(.StoppingAll())


##' Initialization function for "StoppingAll"
##'
##' @param stopList see \code{\linkS4class{StoppingAll}}
##' @return the \code{\linkS4class{StoppingAll}} object
##'
##' @export
##' @keywords methods
StoppingAll <- function(stopList)
{
    .StoppingAll(stopList=stopList)
}


## --------------------------------------------------
## Stopping based on fulfillment of any stopping rule
## --------------------------------------------------

##' Stop based on fulfillment of any stopping rule
##'
##' This class can be used to combine multiple stopping rules with an OR
##' operator.
##'
##' \code{stopList} contains all stopping rules, which are again objects of
##' class \code{\linkS4class{Stopping}}. Any of these rules must be fulfilled in
##' order that the result of this rule is to stop.
##'
##' @slot stopList list of stopping rules
##'
##' @example examples/Rules-class-StoppingAny.R
##' @keywords classes
##' @export
.StoppingAny <-
    setClass(Class="StoppingAny",
             representation(stopList="list"),
             prototype(stopList=
                           list(StoppingMinPatients(50),
                                StoppingMinCohorts(5))),
             contains="Stopping",
             validity=
                 function(object){
                     o <- Validate()

                     o$check(all(sapply(object@stopList, is, "Stopping")),
                             "all stopList elements have to Stopping objects")

                     o$result()
                 })
validObject(.StoppingAny())


##' Initialization function for "StoppingAny"
##'
##' @param stopList see \code{\linkS4class{StoppingAny}}
##' @return the \code{\linkS4class{StoppingAny}} object
##'
##' @export
##' @keywords methods
StoppingAny <- function(stopList)
{
    .StoppingAny(stopList=stopList)
}


##-------------------------------------------------------------------------------------------------------------------
## Stopping based on a target ratio of the 95% credibility interval
## ---------------------------------------------------------------------------------------------------------------

##' Stop based on a target ratio, the ratio of the upper to the lower
##' 95% credibility interval of the estimate of TD end of trial, the dose with probability of DLE equals to the target
##' probability of DLE used at the end of a trial
##' @slot targetRatio the target ratio of the upper to the lower of the 95% credibility interval of the
##' estimate that required to stop a trial
##' @slot targetEndOfTrial the target probability of DLE to be used at the end of a trial
##'
##' @example examples/Rules-class-StoppingTDCIRatio.R
##' @export
##' @keywords classes
.StoppingTDCIRatio <-
  setClass(Class="StoppingTDCIRatio",
           representation(targetRatio="numeric",
                          targetEndOfTrial="numeric"),
           prototype(targetRatio=5,
                     targetEndOfTrial=0.3),
           contains="Stopping",
           validity=
             function(object){
               o <- Validate()

               o$check(is.numeric(object@targetRatio) & object@targetRatio > 0,
                       "targetRatio must be a positive numerical number")
               o$check(is.numeric(object@targetEndOfTrial) & object@targetEndOfTrial >= 0 & object@targetEndOfTrial <= 1,
                       "targetEndOfTrial must be a numerical number lies between 0 and 1")
               o$result()
             })

validObject(.StoppingTDCIRatio())

##' Initialization function for "StoppingTDCIRatio"
##'
##' @param targetRatio please refer to \code{\linkS4class{StoppingTDCIRatio}} class object
##' @param targetEndOfTrial please refer to \code{\linkS4class{StoppingTDCIRatio}} class object
##' @return the \code{\linkS4class{StoppingTDCIRatio}} class object
##'
##' @export
##' @keywords methods
StoppingTDCIRatio <- function(targetRatio,
                              targetEndOfTrial)
{
  .StoppingTDCIRatio(targetRatio=targetRatio,
                     targetEndOfTrial=targetEndOfTrial)
}

## ----------------------------------------------------------------------------------------------------------------
##' Stop based on a target ratio, the ratio of the upper to the lower
##' 95% credibility interval of the estimate of the minimum of the dose which gives the maximum gain (Gstar) and
##' the TD end of trial, the dose with probability of DLE equals to the target
##' probability of DLE used at the end of a trial.
##' @slot targetRatio the target ratio of the upper to the lower of the 95% credibility interval of the
##' estimate that required to stop a trial
##' @slot targetEndOfTrial the target probability of DLE to be used at the end of a trial
##'
##' @example examples/Rules-class-StoppingGstarCIRatio.R
##' @export
##' @keywords classes
.StoppingGstarCIRatio <-
  setClass(Class="StoppingGstarCIRatio",
           representation(targetRatio="numeric",
                          targetEndOfTrial="numeric"),
           prototype(targetRatio=5,
                     targetEndOfTrial=0.3),
           contains="Stopping",
           validity=
             function(object){
               o <- Validate()

               o$check(is.numeric(object@targetRatio) & object@targetRatio > 0,
                       "targetRatio must be a positive numerical number")
               o$check(is.numeric(object@targetEndOfTrial) & object@targetEndOfTrial >= 0 & object@targetEndOfTrial <= 1,
                       "targetEndOfTrial must be a numerical number lies between 0 and 1")
               o$result()
             })

validObject(.StoppingGstarCIRatio())

##' Initialization function for "StoppingGstarCIRatio"
##'
##' @param targetRatio please refer to \code{\linkS4class{StoppingGstarCIRatio}} class object
##' @param targetEndOfTrial please refer to \code{\linkS4class{StoppingGstarCIRatio}} class object
##' @return the \code{\linkS4class{StoppingGstarCIRatio}} class object
##'
##' @export
##' @keywords methods
StoppingGstarCIRatio <- function(targetRatio,
                                 targetEndOfTrial)
{
  .StoppingGstarCIRatio(targetRatio=targetRatio,
                        targetEndOfTrial=targetEndOfTrial)
}



## ============================================================



## --------------------------------------------------
## Virtual class for cohort sizes
## --------------------------------------------------

##' The virtual class for cohort sizes
##'
##' @seealso \code{\linkS4class{CohortSizeMax}},
##' \code{\linkS4class{CohortSizeMin}},
##' \code{\linkS4class{CohortSizeRange}},
##' \code{\linkS4class{CohortSizeDLT}},
##' \code{\linkS4class{CohortSizeConst}},
##' \code{\linkS4class{CohortSizeParts}}
##'
##' @export
##' @keywords classes
setClass(Class="CohortSize",
         contains=list("VIRTUAL"))


## --------------------------------------------------
## Cohort size based on dose range
## --------------------------------------------------

##' Cohort size based on dose range
##'
##' @slot intervals a vector with the left bounds of the relevant dose intervals
##' @slot cohortSize an integer vector of the same length with the cohort
##' sizes in the \code{intervals}
##'
##' @example examples/Rules-class-CohortSizeRange.R
##' @export
##' @keywords classes
.CohortSizeRange <-
    setClass(Class="CohortSizeRange",
             representation(intervals="numeric",
                            cohortSize="integer"),
             prototype(intervals=c(0, 20),
                       cohortSize=as.integer(c(1L, 3L))),
             contains="CohortSize",
             validity=
                 function(object){
                     o <- Validate()

                     o$check(identical(length(object@cohortSize),
                                       length(object@intervals)),
                             "cohortSize must have same length as intervals")
                     o$check(all(object@cohortSize >= 0),
                             "cohortSize must only contain positive integers")
                     o$check(! is.unsorted(object@intervals, strictly=TRUE),
                             "intervals has to be sorted and have unique values")

                     o$result()
                 })
validObject(.CohortSizeRange())

##' Initialization function for "CohortSizeRange"
##'
##' @param intervals see \code{\linkS4class{CohortSizeRange}}
##' @param cohortSize see \code{\linkS4class{CohortSizeRange}}
##' @return the \code{\linkS4class{CohortSizeRange}} object
##'
##' @export
##' @keywords methods
CohortSizeRange <- function(intervals,
                            cohortSize)
{
    .CohortSizeRange(intervals=intervals,
                     cohortSize=safeInteger(cohortSize))
}

## --------------------------------------------------
## Cohort size based on number of DLTs
## --------------------------------------------------

##' Cohort size based on number of DLTs
##'
##' @slot DLTintervals an integer vector with the left bounds of the relevant
##' DLT intervals
##' @slot cohortSize an integer vector of the same length with the cohort
##' sizes in the \code{DLTintervals}
##'
##' @example examples/Rules-class-CohortSizeDLT.R
##' @export
##' @keywords classes
.CohortSizeDLT <-
    setClass(Class="CohortSizeDLT",
             representation(DLTintervals="integer",
                            cohortSize="integer"),
             prototype(DLTintervals=as.integer(c(0, 1)),
                       cohortSize=as.integer(c(1, 3))),
             contains="CohortSize",
             validity=
                 function(object){
                     o <- Validate()

                     o$check(identical(length(object@cohortSize),
                                       length(object@DLTintervals)),
                             "cohortSize must have same length as DLTintervals")
                     o$check(all(object@cohortSize >= 0),
                             "cohortSize must only contain positive integers")
                     o$check(! is.unsorted(object@DLTintervals, strictly=TRUE),
                             "DLTintervals has to be sorted and have unique values")
                     o$check(all(object@DLTintervals >= 0),
                             "DLTintervals must only contain non-negative integers")

                     o$result()
                 })
validObject(.CohortSizeDLT())

##' Initialization function for "CohortSizeDLT"
##'
##' @param DLTintervals see \code{\linkS4class{CohortSizeDLT}}
##' @param cohortSize see \code{\linkS4class{CohortSizeDLT}}
##' @return the \code{\linkS4class{CohortSizeDLT}} object
##'
##' @export
##' @keywords methods
CohortSizeDLT <- function(DLTintervals,
                          cohortSize)
{
    .CohortSizeDLT(DLTintervals=safeInteger(DLTintervals),
                   cohortSize=safeInteger(cohortSize))
}


## --------------------------------------------------
## Constant cohort size
## --------------------------------------------------

##' Constant cohort size
##'
##' This class is used when the cohort size should be kept constant.
##'
##' @slot size the constant integer size
##'
##' @example examples/Rules-class-CohortSizeConst.R
##' @keywords classes
##' @export
.CohortSizeConst <-
    setClass(Class="CohortSizeConst",
             representation(size="integer"),
             prototype(size=3L),
             contains="CohortSize",
             validity=
                 function(object){
                     o <- Validate()

                     o$check(is.scalar(object@size) && (object@size >= 0),
                             "size needs to be positive scalar")

                     o$result()
                 })
validObject(.CohortSizeConst())

##' Initialization function for "CohortSizeConst"
##'
##' @param size see \code{\linkS4class{CohortSizeConst}}
##' @return the \code{\linkS4class{CohortSizeConst}} object
##'
##' @export
##' @keywords methods
CohortSizeConst <- function(size)
{
    .CohortSizeConst(size=safeInteger(size))
}



## --------------------------------------------------
## Cohort size based on the parts
## --------------------------------------------------

##' Cohort size based on the parts
##'
##' This class is used when the cohort size should change for the second part of
##' the dose escalation. Only works in conjunction with
##' \code{\linkS4class{DataParts}} objects.
##'
##' @slot sizes the two sizes for part 1 and part 2
##'
##' @keywords classes
##' @example examples/Rules-class-CohortSizeParts.R
##' @export
.CohortSizeParts <-
    setClass(Class="CohortSizeParts",
             representation(sizes="integer"),
             prototype(sizes=as.integer(c(1, 3))),
             contains="CohortSize",
             validity=
                 function(object){
                     o <- Validate()

                     o$check(all(object@sizes > 0),
                             "the cohort sizes need to be positive")
                     o$check(identical(length(object@sizes), 2L),
                             "2 elements required in sizes")

                     o$result()
                 })
validObject(.CohortSizeParts())

##' Initialization function for "CohortSizeParts"
##'
##' @param sizes see \code{\linkS4class{CohortSizeParts}}
##' @return the \code{\linkS4class{CohortSizeParts}} object
##' @export
##'
##' @keywords methods
CohortSizeParts <- function(sizes)
{
    .CohortSizeParts(sizes=safeInteger(sizes))
}


## --------------------------------------------------
## Size based on maximum of multiple cohort size rules
## --------------------------------------------------

##' Size based on maximum of multiple cohort size rules
##'
##' This class can be used to combine multiple cohort size rules with the MAX
##' operation.
##'
##' \code{cohortSizeList} contains all cohort size rules, which are again
##' objects of class \code{\linkS4class{CohortSize}}. The maximum of these
##' individual cohort sizes is taken to give the final cohort size.
##'
##' @slot cohortSizeList list of cohort size rules
##'
##' @example examples/Rules-class-CohortSizeMax.R
##' @keywords classes
##' @export
.CohortSizeMax <-
    setClass(Class="CohortSizeMax",
             representation(cohortSizeList="list"),
             prototype(cohortSizeList=
                           list(CohortSizeRange(intervals=c(0, 30),
                                                cohortSize=c(1, 3)),
                                CohortSizeDLT(DLTintervals=c(0, 1),
                                              cohortSize=c(1, 3)))),
             contains="CohortSize",
             validity=
                 function(object){
                     o <- Validate()

                     o$check(all(sapply(object@cohortSizeList, is,
                                        "CohortSize")),
                             "all cohortSizeList elements have to be CohortSize objects")

                     o$result()
                 })
validObject(.CohortSizeMax())


##' Initialization function for "CohortSizeMax"
##'
##' @param cohortSizeList see \code{\linkS4class{CohortSizeMax}}
##' @return the \code{\linkS4class{CohortSizeMax}} object
##'
##' @export
##' @keywords methods
CohortSizeMax <- function(cohortSizeList)
{
    .CohortSizeMax(cohortSizeList=cohortSizeList)
}


## --------------------------------------------------
## Size based on minimum of multiple cohort size rules
## --------------------------------------------------

##' Size based on minimum of multiple cohort size rules
##'
##' This class can be used to combine multiple cohort size rules with the MIN
##' operation.
##'
##' \code{cohortSizeList} contains all cohort size rules, which are again
##' objects of class \code{\linkS4class{CohortSize}}. The minimum of these
##' individual cohort sizes is taken to give the final cohort size.
##'
##' @slot cohortSizeList list of cohort size rules
##'
##' @example examples/Rules-class-CohortSizeMin.R
##' @keywords classes
##' @export
.CohortSizeMin <-
    setClass(Class="CohortSizeMin",
             representation(cohortSizeList="list"),
             prototype(cohortSizeList=
                           list(CohortSizeRange(intervals=c(0, 30),
                                                cohortSize=c(1, 3)),
                                CohortSizeDLT(DLTintervals=c(0, 1),
                                              cohortSize=c(1, 3)))),
             contains="CohortSize",
             validity=
                 function(object){
                     o <- Validate()

                     o$check(all(sapply(object@cohortSizeList, is,
                                        "CohortSize")),
                             "all cohortSizeList elements have to be CohortSize objects")

                     o$result()
                 })
validObject(.CohortSizeMin())


##' Initialization function for "CohortSizeMin"
##'
##' @param cohortSizeList see \code{\linkS4class{CohortSizeMin}}
##' @return the \code{\linkS4class{CohortSizeMin}} object
##'
##' @export
##' @keywords methods
CohortSizeMin <- function(cohortSizeList)
{
    .CohortSizeMin(cohortSizeList=cohortSizeList)
}

## --------------------------------------------------
## Virtual class for safety window
## --------------------------------------------------

##' The virtual class for safety window
##'
##' @seealso \code{\linkS4class{SafetyWindowSize}},
##' \code{\linkS4class{SafetyWindowConst}},
##'
##' @export
##' @keywords classes
setClass(Class="SafetyWindow",
         contains=list("VIRTUAL"))


## ============================================================


## --------------------------------------------------
## Safety window length based on cohort size
## --------------------------------------------------

##' Safety window length based on cohort size.
##' This class is used to decide the rolling rule from the clinical perspective.
##'
##' \code{patientGap} is to be used as follows. If for example, the
##' cohort size is 4 and we want to specify three time intervals between these
##' four patients: The interval between the 1st and 2nd patient = 7 units of time
##' , the interval between the 2nd and 3rd patient = 5 units of time, the interval
##' between the 3rd and 4th patient = 3 units of time, then we specify
##' \code{patientGap} to be \code{c(7,5,3)}. Sometimes, we only think the interval
##' between the 1st and 2nd patient should be increased for the safety consideration
##' , and the rest time intervals can be the same, whatever the cohort size is, then
##' we specify \code{patientGap} to be \code{c(7,3)}. The package will automatically
##' repeat the last element of the vector for the rest time intervals.
##'
##' Note that \code{sizeIntervals} is to be read as follows. For instance, When we
##' want to change the `patientGap` based on the cohort size, i.e. the time interval
##' between the 1st and 2nd patient = 9 units of time and the rest time intervals are
##' 5 units of time when the cohort size is equal or larger than 4. And the time
##' interval between the 1st and 2nd patient = 7 units of time and the rest time
##' intervals are 3 units of time when the cohort size is smaller than 4, then we
##' specify \code{sizeIntervals} to be \code{c(0, 4)}. That means, the right
##' bound of the intervals are exclusive to the interval, and the last interval
##' goes from the last value until infinity.
##'
##' @slot patientGap Observed period of the previous patient before the next patient
##' can be dosed
##' @slot sizeIntervals An integer vector with the left bounds of the relevant
##' cohort size intervals
##' @slot patientFollow The period of time that each patient in the cohort needs to be
##' followed before the next cohort open
##' @slot patientFollowMin At least one patient in the cohort needs to be followed at
##' the minimal follow up time
##'
##' @example examples/Rules-class-SafetyWindowSize.R
##' @export
##' @keywords classes
.SafetyWindowSize <-
  setClass(Class="SafetyWindowSize",
           representation(patientGap="list",
                          sizeIntervals="numeric",
                          patientFollow="numeric",
                          patientFollowMin="numeric"),
           prototype(patientGap=list(0),
                     sizeIntervals=as.integer(c(1L,3L)),
                     patientFollow=1,
                     patientFollowMin=1),
           contains="SafetyWindow",
           validity=
             function(object){
               o <- Validate()

               o$check(all(sapply(object@patientGap,function(x){x>=0})),
                       "patientGap should be non-negative number")
               o$check(all(object@sizeIntervals > 0),
                       "sizeIntervals must only contain positive integers")
               o$check(all(object@patientFollow > 0),
                       "patientFollow should be positive number")
               o$check(all(object@patientFollowMin > 0),
                       "patientFollowMin should be positive number")

               o$result()
             })
validObject(.SafetyWindowSize())

##' Initialization function for `SafetyWindowSize`
##'
##' @param patientGap see \code{\linkS4class{SafetyWindowSize}}
##' @param sizeIntervals see \code{\linkS4class{SafetyWindowSize}}
##' @param patientFollow see \code{\linkS4class{SafetyWindowSize}}
##' @param patientFollowMin see \code{\linkS4class{SafetyWindowSize}}
##'
##' @return the \code{\linkS4class{SafetyWindowSize}} object
##'
##' @export
##' @keywords methods
SafetyWindowSize <- function(patientGap,
                             sizeIntervals,
                             patientFollow,
                             patientFollowMin)
{
  if(patientFollow > patientFollowMin)
  {
    warning("the value of patientFollowMin is typically larger than the value of patientFollow")
  }
  .SafetyWindowSize(patientGap=patientGap,
                    sizeIntervals=sizeIntervals,
                    patientFollow=patientFollow,
                    patientFollowMin=patientFollowMin)
}


## ============================================================


## --------------------------------------------------
## Constant safety window length
## --------------------------------------------------

##' Constant safety window length
##'
##' This class is used when the `patientGap` should be kept constant.
##'
##' @slot patientGap the constant gap between patients.
##' @slot patientFollow how long to follow each patient.
##' @slot patientFollowMin minimum follow up.
##'
##' @example examples/Rules-class-SafetyWindowConst.R
##' @keywords classes
##' @export
.SafetyWindowConst <-
  setClass(Class="SafetyWindowConst",
           representation(patientGap="numeric",
                          patientFollow="numeric",
                          patientFollowMin="numeric"),
           prototype(patientGap=0,
                     patientFollow=1,
                     patientFollowMin=1),
           contains="SafetyWindow",
           validity=
             function(object){
               o <- Validate()

               o$check(all(object@patientGap >= 0),
                       "patientGap should be non-negative number")
               o$check(all(object@patientFollow > 0),
                       "patientFollow should be positive number")
               o$check(all(object@patientFollowMin > 0),
                       "patientFollowMin should be positive number")

               o$result()
             })
validObject(.SafetyWindowConst())


##' Initialization function for `SafetyWindowConst`
##'
##' @param patientGap see \code{\linkS4class{SafetyWindowConst}}
##' @param patientFollow see \code{\linkS4class{SafetyWindowConst}}
##' @param patientFollowMin see \code{\linkS4class{SafetyWindowConst}}
##'
##' @return the \code{\linkS4class{SafetyWindowConst}} object
##'
##' @export
##' @keywords methods
SafetyWindowConst <- function(patientGap,
                              patientFollow,
                              patientFollowMin)
{
  if(patientFollow > patientFollowMin)
  {
    warning("the value of patientFollowMin is typically larger than the value of patientFollow")
  }
  .SafetyWindowConst(patientGap=patientGap,
                     patientFollow=patientFollow,
                     patientFollowMin=patientFollowMin)
}


## ============================================================

# nolint end
