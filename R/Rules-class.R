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
#' @slot target (`proportion`)\cr target toxicity probability, except 0 or 1.
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

## default constructor ----

#' @rdname NextBestMTD-class
#' @note Typically, end users will not use the `.DefaultNextBestMTD()` function.
#' @export
.DefaultNextBestMTD <- function() {
  NextBestMTD(
    target = 0.33,
    derive = function(mtd_samples) {
      quantile(mtd_samples, probs = 0.25)
    }
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
#'   probability that is allowed, except 0 or 1.
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

## default constructor ----

#' @rdname NextBestNCRM-class
#' @note Typically, end users will not use the `.DefaultNextBestNCRM()` function.
#' @export
.DefaultNextBestNCRM <- function() {
  NextBestNCRM(target = c(0.2, 0.35), overdose = c(0.35, 1), max_overdose_prob = 0.25)
}

# NextBestNCRMLoss ----

## class ----

#' `NextBestNCRMLoss`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`NextBestNCRMLoss`] is the class based on NCRM rule and loss function.
#' This class is similar to [`NextBestNCRM`] class, but differences are the
#' addition of loss function and re-defined toxicity intervals, see each
#' toxicity interval documentation and the note for details. As in NCRM rule, first admissible doses are found,
#' which are those with probability to fall in overdose category being below
#' `max_overdose_prob`. Next, within the admissible doses, the loss function is
#' calculated, i.e. `losses` %*% `target`. Finally, the corresponding
#' dose with lowest loss function (Bayes risk) is recommended for the next dose.
#'
#' @slot target (`numeric`)\cr the target toxicity interval (limits included).
#'   It has to be a probability range excluding 0 and 1.
#' @slot overdose (`numeric`)\cr the overdose toxicity interval (lower limit
#'   excluded, upper limit included) or the excessive toxicity interval (lower
#'   limit excluded, upper limit included) if unacceptable is not provided.
#'   It has to be a probability range. It is used to filter probability samples.
#' @slot unacceptable (`numeric`)\cr an unacceptable toxicity
#'   interval (lower limit excluded, upper limit included). This must be
#'   specified if the `overdose` does not include 1. Otherwise, it is `c(1, 1)`
#'   (default), which is essentially a scalar equals 1. It has to be a
#'   probability range.
#' @slot losses (`numeric`)\cr a vector specifying the loss function. If the
#'   `unacceptable` is provided, the vector length must be 4, otherwise 3.
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
#' @param target (`numeric`)\cr see slot definition.
#' @param overdose (`numeric`)\cr see slot definition.
#' @param unacceptable (`numeric`)\cr see slot definition.
#' @param max_overdose_prob (`proportion`)\cr see slot definition in [`NextBestNCRM`].
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

## default constructor ----

#' @rdname NextBestNCRMLoss-class
#' @note Typically, end users will not use the `.DefaultNextBestnCRMLoss()` function.
#' @export
.DefaultNextBestNCRMLoss <- function() {
  NextBestNCRMLoss(
    target = c(0.2, 0.35),
    overdose = c(0.35, 0.6),
    unacceptable = c(0.6, 1),
    max_overdose_prob = 0.25,
    losses = c(1, 0, 1, 2)
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

## default constructor ----

#' @rdname NextBestThreePlusThree-class
#' @note Typically, end users will not use the `.DefaultNextBestThreePlusThree()` function.
#' @export
.DefaultNextBestThreePlusThree <- function() {
  NextBestThreePlusThree()
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
#'   deriving the next best dose (default to 0.01).
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

## default constructor ----

#' @rdname NextBestDualEndpoint-class
#' @note Typically, end users will not use the `.DefaultNextBestDualEndpoint()` function.
#' @export
.DefaultNextBestDualEndpoint <- function() {
  NextBestDualEndpoint(
    target = c(200, 300),
    overdose = c(0.35, 1),
    max_overdose_prob = 0.25,
    target_relative = FALSE
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
#' @slot target (`proportion`)\cr single target toxicity probability, except
#'   0 or 1.
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
#' @example examples/Rules-class-NextBestMinDist.R
#'
NextBestMinDist <- function(target) {
  .NextBestMinDist(target = target)
}

## default constructor ----

#' @rdname NextBestMinDist-class
#' @note Typically, end users will not use the `.DefaultNextBestMinDist()` function.
#' @export
.DefaultNextBestMinDist <- function() {
  NextBestMinDist(target = 0.3)
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
#' @slot target (`proportion`)\cr target toxicity probability, except 0 or 1.
#' @slot asymmetry (`number`)\cr value of the asymmetry exponent in the
#'   divergence function that describes the rate of penalization for overly
#'   toxic does. It must be a value from \eqn{(0, 2)} interval.
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

## default constructor ----

#' @rdname NextBestInfTheory-class
#' @note Typically, end users will not use the `.DefaultNextBestInfTheory()` function.
#' @export
.DefaultNextBestInfTheory <- function() {
  NextBestInfTheory(0.33, 1.2)
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
#' @slot prob_target_drt (`proportion`)\cr the target probability (except 0 or 1)
#'   of the occurrence of a DLT to be used during the trial.
#' @slot prob_target_eot (`proportion`)\cr the target probability (except 0 or 1)
#'   of the occurrence of a DLT to be used at the end of the trial.
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

## default constructor ----

#' @rdname NextBestTD-class
#' @note Typically, end users will not use the `.DefaultNextBestTD()` function.
#' @export
.DefaultNextBestTD <- function() {
  NextBestTD(0.35, 0.3)
}

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

## default constructor ----

#' @rdname NextBestTDsamples-class
#' @note Typically, end users will not use the `.DefaultNextBestTDsamples()` function.
#' @export
.DefaultNextBestTDsamples <- function() {
  NextBestTDsamples(
    prob_target_drt = 0.35,
    prob_target_eot = 0.3,
    derive = function(samples) {
      as.numeric(quantile(samples, probs = 0.3))
    }
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

## default constructor ----

#' @rdname NextBestMaxGain-class
#' @note Typically, end users will not use the `.DefaultNextBestMaxGain()` function.
#' @export
.DefaultNextBestMaxGain <- function() {
  NextBestMaxGain(0.35, 0.3)
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

## default constructor ----

#' @rdname NextBestMaxGainSamples-class
#' @note Typically, end users will not use the `.DefaultNextBestMaxGainSamples()` function.
#' @export
.DefaultNextBestMaxGainSamples <- function() {
  NextBestMaxGainSamples(
    prob_target_drt = 0.35,
    prob_target_eot = 0.3,
    derive = function(samples) {
      as.numeric(quantile(samples, prob = 0.3))
    },
    mg_derive = function(mg_samples) {
      as.numeric(quantile(mg_samples, prob = 0.5))
    }
  )
}

# NextBestProbMTDLTE ----

## class ----

#' `NextBestProbMTDLTE`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`NextBestProbMTDLTE`] is the class of finding a next best dose that selects
#' the dose with the highest probability of having a toxicity rate less or equal
#' to the toxicity target.
#' The dose is determined by calculating the posterior toxicity probability
#' for each dose per iteration and select the maximum dose that has a toxicity
#' probability below or equal to the target. The dose with the highest frequency
#' of being selected as MTD across iterations is the next best dose. Placebo
#' is not considered in the calculation and removed from the dose grid for
#' any calculations.
#'
#' @slot target (`numeric`)\cr the target toxicity probability.
#'
#' @aliases NextBestProbMTDLTE
#' @export
#'
.NextBestProbMTDLTE <- setClass(
  Class = "NextBestProbMTDLTE",
  slots = c(target = "numeric"),
  prototype = prototype(target = 0.3),
  contains = "NextBest",
  validity = v_next_best_prob_mtd_lte
)

## constructor ----

#' @rdname NextBestProbMTDLTE-class
#'
#' @param target (`numeric`)\cr see slot definition.
#' @export
#' @example examples/Rules-class-NextBestProbMTDLTE.R
#'
NextBestProbMTDLTE <- function(target) {
  .NextBestProbMTDLTE(target = target)
}

## default constructor ----

#' @rdname NextBestProbMTDLTE-class
#' @note Typically, end users will not use the `.DefaultNextBestProbMTDLTE()` function.
#' @export
.DefaultNextBestProbMTDLTE <- function() {
  NextBestProbMTDLTE(target = 0.3)
}

# NextBestProbMTDMinDist ----

## class ----

#' `NextBestProbMTDMinDist`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`NextBestProbMTDMinDist`] is the class of finding a next best dose that selects
#' the dose with the highest probability of having a toxicity rate with the
#' smallest distance to the toxicity target.
#' The dose is determined by calculating the posterior toxicity probability
#' for each dose per iteration and select the dose that has the smallest toxicity
#' probability distance to the target. The dose with the highest frequency
#' of being selected as MTD across iterations is the next best dose. Placebo
#' is not considered as the next dose and for that reason not used in
#' calculations. I.e. for placebo the toxicity probability distance to target
#' is not calculated and taken into account for determination of the next dose.
#'
#' @slot target (`numeric`)\cr the target toxicity probability.
#'
#' @aliases NextBestProbMTDMinDist
#' @export
#'
.NextBestProbMTDMinDist <- setClass(
  Class = "NextBestProbMTDMinDist",
  slots = c(target = "numeric"),
  prototype = prototype(target = 0.3),
  contains = "NextBest",
  validity = v_next_best_prob_mtd_min_dist
)

## constructor ----

#' @rdname NextBestProbMTDMinDist-class
#'
#' @param target (`numeric`)\cr see slot definition.
#' @export
#' @example examples/Rules-class-NextBestProbMTDMinDist.R
#'
NextBestProbMTDMinDist <- function(target) {
  .NextBestProbMTDMinDist(target = target)
}

## default constructor ----

#' @rdname NextBestProbMTDMinDist-class
#' @note Typically, end users will not use the `.DefaultNextBestProbMTDMinDist()` function.
#' @export
.DefaultNextBestProbMTDMinDist <- function() {
  NextBestProbMTDMinDist(target = 0.3)
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
#'   [`IncrementsDoseLevels`], [`IncrementsHSRBeta`], [`IncrementsMin`].
#'
#' @aliases Increments
#' @export
#'
setClass(
  Class = "Increments"
)

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
#'   intervals. For example, `intervals  = c(0, 50, 100)` specifies three intervals:
#'   \eqn{(0, 50)}, \eqn{[50, 100)} and \eqn{[100, +Inf)}. That means, the right
#'   bound of the intervals are exclusive to the interval and the last interval
#'   goes from the last value to infinity.
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
#' @param intervals (`numeric`)\cr see slot definition.
#' @param increments (`numeric`)\cr see slot definition.
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

## default constructor ----

#' @rdname IncrementsRelative-class
#' @note Typically, end users will not use the `.DefaultIncrementsRelative()` function.
#' @export
.DefaultIncrementsRelative <- function() {
  IncrementsRelative(intervals = c(0, 20), increments = c(1, 0.33))
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
#' @slot dlt_intervals (`integer`)\cr a vector with the left bounds of the
#'   relevant DLT intervals. For example, `dlt_intervals  = c(0, 1, 3)` specifies
#'   three intervals (sets of DLTs: first, 0 DLT; second 1 or 2 DLTs; and the third
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
#' @param dlt_intervals (`numeric`)\cr see slot definition.
#' @param increments (`numeric`)\cr see slot definition.
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

## default constructor ----

#' @rdname IncrementsRelativeDLT-class
#' @note Typically, end users will not use the `.DefaultIncrementsRelativeDLT()` function.
#' @export
.DefaultIncrementsRelativeDLT <- function() {
  IncrementsRelativeDLT(dlt_intervals = c(0L, 1L, 3L), increments = c(1, 0.33, 0.2))
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

## default constructor ----

#' @rdname IncrementsRelativeDLTCurrent-class
#' @note Typically, end users will not use the `.DefaultIncrementsRelativeDLTCurrent()` function.
#' @export
.DefaultIncrementsRelativeDLTCurrent <- function() { # nolint
  IncrementsRelativeDLTCurrent(dlt_intervals = c(0L, 1L, 3L), increments = c(1, 0.33, 0.2))
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
#' @details This class works only in conjunction with [`DataParts`] objects. If
#' part 2 will just be started in the next cohort, then the next maximum dose
#' will be either `dlt_start` (e.g. -1) shift of the last part 1 dose in case
#' of a DLT in part 1, or `clean_start` shift (e.g. -1) in case of no DLTs in
#' part 1, given that `clean_start <= 0` (see description of `clean_start`
#' slot for more details). If part 1 will still be on in the next cohort,
#' then the next dose level will be the next higher dose level in the
#' `part1Ladder` slot of the data object. If part 2 has been started before,
#' the usual relative increment rules apply, see [`IncrementsRelative`].
#'
#' @slot dlt_start (`integer`)\cr a scalar, the dose level increment for starting
#'   part 2 in case of at least one DLT event in part 1.
#' @slot clean_start (`integer`)\cr a scalar, the dose level increment for
#'   starting part 2 in case of no DLTs in part 1. If `clean_start <= 0`,
#'   then the part 1 ladder will be used to find the maximum next dose.
#'   Otherwise, the relative increment rules will be applied to find the next
#'   maximum dose level.
#'
#' @note We require that `clean_start >= dlt_start`. However, this precondition
#'   is not a prerequisite for any function (except of the class' validation
#'   function) that works with objects of this class. It is rather motivated by
#'   the semantics. That is, if we observe a DLT in part 1, we cannot be more
#'   aggressive than in case of a clean part 1 without DLT.
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
#' @param dlt_start (`count`)\cr see slot definition.
#' @param clean_start (`count`)\cr see slot definition.
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

## default constructor ----

#' @rdname IncrementsRelativeParts-class
#' @note Typically, end users will not use the `.DefaultIncrementsRelativeParts()` function.
#' @export
.DefaultIncrementsRelativeParts <- function() {
  IncrementsRelativeParts(dlt_start = 0L, clean_start = 1L)
}

# IncrementsDoseLevels ----

## class ----

#' `IncrementsDoseLevels`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`IncrementsDoseLevels`] is the class for increments control based on the
#' number of dose levels.
#'
#' @slot levels (`count`)\cr maximum number of dose levels to increment for
#'   the next dose. It defaults to 1, which means that no dose skipping is
#'   allowed, i.e. the next dose can be maximum one level higher than the current
#'   base dose. The current base dose level is the dose level used to increment
#'   from (see `basis_level` parameter).
#' @slot basis_level (`string`)\cr defines the current base dose level. It can
#'   take one out of two possible values: `last` or `max`.
#'   If `last` is specified (default), the current base dose level is set to the
#'   last dose given. If `max` is specified, then the current base dose level is
#'   set to the maximum dose level given.
#'
#' @aliases IncrementsDoseLevels
#' @export
#'
.IncrementsDoseLevels <- setClass(
  Class = "IncrementsDoseLevels",
  slots = representation(
    levels = "integer",
    basis_level = "character"
  ),
  prototype = prototype(
    levels = 1L,
    basis_level = "last"
  ),
  contains = "Increments",
  validity = v_increments_dose_levels
)

## constructor ----

#' @rdname IncrementsDoseLevels-class
#'
#' @param levels (`count`)\cr see slot definition.
#' @param basis_level (`string`)\cr see slot definition.
#'
#' @export
#' @example examples/Rules-class-IncrementsDoseLevels.R
#'
IncrementsDoseLevels <- function(levels = 1L, basis_level = "last") {
  .IncrementsDoseLevels(
    levels = safeInteger(levels),
    basis_level = basis_level
  )
}

## default constructor ----

#' @rdname IncrementsDoseLevels-class
#' @note Typically, end users will not use the `.DefaultIncrementsDoseLevels()` function.
#' @export
.DefaultIncrementsDoseLevels <- function() {
  IncrementsDoseLevels(levels = 2L, basis_level = "last")
}

# IncrementsHSRBeta ----

## class ----

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
#' @slot target (`proportion`)\cr the target toxicity, except
#'   0 or 1.
#' @slot prob (`proportion`)\cr the threshold probability (except 0 or 1) for
#'   a dose being toxic.
#' @slot a (`number`)\cr shape parameter \eqn{a > 0} of probability distribution
#'   Beta (a,b).
#' @slot b (`number`)\cr shape parameter \eqn{b > 0} of probability distribution
#'   Beta (a,b).
#'
#' @aliases IncrementsHSRBeta
#' @export
#'
.IncrementsHSRBeta <- setClass(
  Class = "IncrementsHSRBeta",
  slots = c(
    target = "numeric",
    prob = "numeric",
    a = "numeric",
    b = "numeric"
  ),
  prototype = prototype(
    target = 0.3,
    prob = 0.95,
    a = 1,
    b = 1
  ),
  contains = "Increments",
  validity = v_increments_hsr_beta
)

## constructor ----

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

## default constructor ----

#' @rdname IncrementsHSRBeta-class
#' @note Typically, end users will not use the `.DefaultIncrementsHSRBeta()` function.
#' @export
.DefaultIncrementsHSRBeta <- function() {
  IncrementsHSRBeta(target = 0.3, prob = 0.95)
}

# IncrementsMin ----

## class ----

#' `IncrementsMin`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`IncrementsMin`] is the class that combines multiple increment rules with
#' the `minimum` operation. Slot `increments_list` contains all increment rules,
#' which are itself the objects of class [`Increments`]. The minimum of these
#' individual increments is taken to give the final maximum increment.
#'
#' @slot increments_list (`list`)\cr list with increment rules.
#'
#' @aliases IncrementsMin
#' @export
#'
.IncrementsMin <- setClass(
  Class = "IncrementsMin",
  slots = c(increments_list = "list"),
  prototype = prototype(
    increments_list = list(
      IncrementsRelativeDLT(dlt_intervals = c(0L, 1L), increments = c(2, 1)),
      IncrementsRelative(intervals = c(0, 2), increments = c(2, 1))
    )
  ),
  contains = "Increments",
  validity = v_increments_min
)

## constructor ----

#' @rdname IncrementsMin-class
#'
#' @param increments_list (`list`)\cr see slot definition.
#'
#' @example examples/Rules-class-IncrementsMin.R
#' @export
#'
IncrementsMin <- function(increments_list) {
  .IncrementsMin(increments_list = increments_list)
}
## default constructor ----

#' @rdname IncrementsMin-class
#' @note Typically, end users will not use the `.DefaultIncrementsMin()` function.
#' @export
.DefaultIncrementsMin <- function() {
  IncrementsMin(
    increments_list = list(
      IncrementsRelativeDLT(dlt_intervals = c(0, 1, 3), increments = c(1, 0.33, 0.2)),
      IncrementsRelative(intervals = c(0, 20), increments = c(1, 0.33))
    )
  )
}

# Stopping ----

## class ----

#' `Stopping`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`Stopping`] is a class for stopping rules.
#'
#' @slot report_label (`string`)\cr a label for the stopping report. The meaning
#'   of this parameter is twofold. If it is equal to `NA_character_` (default),
#'   the `report_label` will not be used in the report at all. Otherwise, if it
#'   is specified as an empty character (i.e. `character(0)`) in a user constructor,
#'   then a default, class-specific label will be created for this slot.
#'   Finally, for the remaining cases, a user can provide a custom label.
#'
#' @seealso [`StoppingList`], [`StoppingCohortsNearDose`], [`StoppingPatientsNearDose`],
#'   [`StoppingMinCohorts`], [`StoppingMinPatients`], [`StoppingTargetProb`],
#'   [`StoppingMTDdistribution`], [`StoppingTargetBiomarker`], [`StoppingHighestDose`]
#'   [`StoppingMTDCV`], [`StoppingLowestDoseHSRBeta`], [`StoppingSpecificDose`].
#'
#' @aliases Stopping
#' @export
#'
setClass(
  Class = "Stopping",
  slots = c(report_label = "character"),
  prototype = prototype(report_label = character(0))
)


# StoppingMissingDose ----

## class ----

#' `StoppingMissingDose`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`StoppingMissingDose`] is the class for stopping based on NA returned by
#'  next best dose.
#'
#' @aliases StoppingMissingDose
#' @export
#'
.StoppingMissingDose <- setClass(
  Class = "StoppingMissingDose",
  contains = "Stopping"
)

## constructor ----

#' @rdname StoppingMissingDose-class
#'
#' @example examples/Rules-class-StoppingMissingDose.R
#' @export
#'
StoppingMissingDose <- function(
    report_label = NA_character_) {

  report_label <- h_default_if_empty(
    as.character(report_label),
    paste("Stopped because of missing dose")
  )

  .StoppingMissingDose(report_label = report_label)

}

## default constructor ----

#' @rdname StoppingMissingDose-class
#' @note Typically, end users will not use the `.DefaultStoppingMissingDose()` function.
#' @export
#'
.DefaultStoppingMissingDose <- function() {
  StoppingMissingDose()
}

# StoppingCohortsNearDose ----

## class ----

#' `StoppingCohortsNearDose`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`StoppingCohortsNearDose`] is the class for stopping based on number of
#' cohorts near to next best dose.
#'
#'
#' @slot nCohorts (`number`)\cr number of required cohorts.
#' @slot percentage (`number`)\cr percentage (between and including 0 and 100)
#'   within the next best dose the cohorts must lie.
#'
#' @aliases StoppingCohortsNearDose
#' @export
#'
.StoppingCohortsNearDose <- setClass(
  Class = "StoppingCohortsNearDose",
  slots = c(
    nCohorts = "integer",
    percentage = "numeric"
  ),
  prototype = prototype(
    nCohorts = 2L,
    percentage = 50
  ),
  contains = "Stopping",
  validity = v_stopping_cohorts_near_dose
)

## constructor ----

#' @rdname StoppingCohortsNearDose-class
#'
#' @param nCohorts (`number`)\cr see slot definition.
#' @param percentage (`number`)\cr see slot definition.
#' @param report_label (`string` or `NA`)\cr see slot definition.
#'
#' @example examples/Rules-class-StoppingCohortsNearDose.R
#' @export
#'
StoppingCohortsNearDose <- function(nCohorts = 2L,
                                    percentage = 50,
                                    report_label = NA_character_) {
  nCohorts <- safeInteger(nCohorts)
  report_label <- h_default_if_empty(
    as.character(report_label),
    paste("\u2265", nCohorts, "cohorts dosed in", percentage, "% dose range around NBD")
  )

  .StoppingCohortsNearDose(
    nCohorts = safeInteger(nCohorts),
    percentage = percentage,
    report_label = report_label
  )
}

## default constructor ----

#' @rdname StoppingCohortsNearDose-class
#' @note Typically, end users will not use the `.DefaultStoppingCohortsNearDose()` function.
#' @export
.DefaultStoppingCohortsNearDose <- function() { # nolint
  StoppingCohortsNearDose(
    nCohorts = 3L,
    percentage = 0.2,
    report_label = NA_character_)
}


# StoppingPatientsNearDose ----

## class ----

#' `StoppingPatientsNearDose`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`StoppingPatientsNearDose`] is the class for stopping based on number of
#' patients near to next best dose.
#'
#' @slot nPatients (`number`)\cr number of required patients.
#' @slot percentage (`number`)\cr percentage (between and including 0 and 100)
#'   within the next best dose the patients must lie.
#'
#' @aliases StoppingPatientsNearDose
#' @export
#'
.StoppingPatientsNearDose <- setClass(
  Class = "StoppingPatientsNearDose",
  slots = c(
    nPatients = "integer",
    percentage = "numeric"
  ),
  prototype = prototype(
    nPatients = 10L,
    percentage = 50
  ),
  contains = "Stopping",
  validity = v_stopping_patients_near_dose
)

## constructor ----

#' @rdname StoppingPatientsNearDose-class
#'
#' @param nPatients (`number`)\cr see slot definition.
#' @param percentage (`number`)\cr see slot definition.
#' @param report_label (`string` or `NA`)\cr see slot definition.
#'
#' @example examples/Rules-class-StoppingPatientsNearDose.R
#' @export
#'
StoppingPatientsNearDose <- function(nPatients = 10L,
                                     percentage = 50,
                                     report_label = NA_character_) {
  nPatients <- safeInteger(nPatients)
  report_label <- h_default_if_empty(
    as.character(report_label),
    paste("\u2265", nPatients, "patients dosed in", percentage, "% dose range around NBD")
  )

  .StoppingPatientsNearDose(
    nPatients = nPatients,
    percentage = percentage,
    report_label = report_label
  )
}

## default constructor ----

#' @rdname StoppingPatientsNearDose-class
#' @note Typically, end users will not use the `.DefaultStoppingPatientsNearDose()` function.
#' @export
.DefaultStoppingPatientsNearDose <- function() { # nolint
  StoppingPatientsNearDose(
    nPatients = 9L,
    percentage = 20,
    report_label = NA_character_)
}

# StoppingMinCohorts ----

## class ----

#' `StoppingMinCohorts`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`StoppingMinCohorts`] is the class for stopping based on minimum number of
#' cohorts.
#'
#' @slot nCohorts (`number`)\cr minimum required number of cohorts.
#'
#' @aliases StoppingMinCohorts
#' @export
#'
.StoppingMinCohorts <- setClass(
  Class = "StoppingMinCohorts",
  slots = c(nCohorts = "integer"),
  prototype = prototype(nCohorts = 2L),
  contains = "Stopping",
  validity = v_stopping_min_cohorts
)

## constructor ----

#' @rdname StoppingMinCohorts-class
#'
#' @param nCohorts (`number`)\cr see slot definition.
#' @param report_label (`string` or `NA`)\cr see slot definition.
#'
#' @example examples/Rules-class-StoppingMinCohorts.R
#' @export
#'
StoppingMinCohorts <- function(nCohorts = 2L,
                               report_label = NA_character_) {
  nCohorts <- safeInteger(nCohorts)
  report_label <- h_default_if_empty(
    as.character(report_label),
    paste("\u2265", nCohorts, "cohorts dosed")
  )

  .StoppingMinCohorts(
    nCohorts = safeInteger(nCohorts),
    report_label = report_label
  )
}

## default constructor ----

#' @rdname StoppingMinCohorts-class
#' @note Typically, end users will not use the `.DefaultStoppingMinCohorts()` function.
#' @export
.DefaultStoppingMinCohorts <- function() {
  StoppingMinCohorts(
    nCohorts = 6L,
    report_label = NA_character_)
}

# StoppingMinPatients ----

## class ----

#' `StoppingMinPatients`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`StoppingMinPatients`] is the class for stopping based on minimum number of
#' patients
#'
#' @slot nPatients (`number`)\cr minimum allowed number of patients.
#'
#' @aliases StoppingMinPatients
#' @export
#'
.StoppingMinPatients <- setClass(
  Class = "StoppingMinPatients",
  slots = c(nPatients = "integer"),
  prototype = prototype(nPatients = 20L),
  contains = "Stopping",
  validity = v_stopping_min_patients
)

## constructor ----

#' @rdname StoppingMinPatients-class
#'
#' @param nPatients (`number`)\cr see slot definition.
#' @param report_label (`string` or `NA`)\cr see slot definition.
#'
#' @example examples/Rules-class-StoppingMinPatients.R
#' @export
#'
StoppingMinPatients <- function(nPatients = 20L,
                                report_label = NA_character_) {
  nPatients <- safeInteger(nPatients)
  report_label <- h_default_if_empty(
    as.character(report_label),
    paste("\u2265", nPatients, "patients dosed")
  )

  .StoppingMinPatients(
    nPatients = safeInteger(nPatients),
    report_label = report_label
  )
}

## default constructor ----

#' @rdname StoppingMinPatients-class
#' @note Typically, end users will not use the `.DefaultStoppingMinPatients()` function.
#' @export
.DefaultStoppingMinPatients <- function() {
  StoppingMinPatients(
    nPatients = 20L,
    report_label = NA_character_
    )
}

# StoppingTargetProb ----

## class ----

#' `StoppingTargetProb`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`StoppingTargetProb`] is the class for stopping based on the probability of
#' the DLT rate being in the target toxicity interval.
#'
#' @slot target (`number`)\cr the target toxicity interval, e.g. `c(0.2, 0.35)`.
#' @slot prob (`proportion`)\cr required target toxicity probability (except 0 or 1)
#'   for reaching sufficient precision.
#'
#' @aliases StoppingTargetProb
#' @export
#'
.StoppingTargetProb <- setClass(
  Class = "StoppingTargetProb",
  slots = c(
    target = "numeric",
    prob = "numeric"
  ),
  prototype = prototype(
    target = c(0.2, 0.35),
    prob = 0.4
  ),
  contains = "Stopping",
  validity = v_stopping_target_prob
)

## constructor ----

#' @rdname StoppingTargetProb-class
#'
#' @param target (`number`)\cr see slot definition.
#' @param prob (`proportion`)\cr see slot definition.
#' @param report_label (`string` or `NA`)\cr see slot definition.
#'
#' @example examples/Rules-class-StoppingTargetProb.R
#' @export
#'
StoppingTargetProb <- function(target = c(0.2, 0.35),
                               prob = 0.4,
                               report_label = NA_character_) {
  assert_numeric(target, len = 2)
  report_label <- h_default_if_empty(
    as.character(report_label),
    paste0("P(", target[1], " \u2264 prob(DLE | NBD) \u2264 ", target[2], ") \u2265 ", prob)
  )

  .StoppingTargetProb(
    target = target,
    prob = prob,
    report_label = report_label
  )
}

## default constructor ----

#' @rdname StoppingTargetProb-class
#' @note Typically, end users will not use the `.DefaultStoppingTargetProb()` function.
#' @export
.DefaultStoppingTargetProb <- function() {
  StoppingTargetProb(
    target = c(0.2, 0.35),
    prob = 0.5,
    report_label = NA_character_)
}

# StoppingMTDdistribution ----

## class ----

#' `StoppingMTDdistribution`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`StoppingMTDdistribution`] is the class for stopping based on the posterior
#' distribution of the MTD. It is used for the cases where the stopping occurs
#' when the probability of `MTD > thresh * next_dose` is greater than or equal
#' to `prob`, where the `next_dose` is the recommended next best dose.
#' Here, the MTD is defined as the dose that reaches a specific `target`
#' probability of the occurrence of a DLT.
#'
#' @slot target (`proportion`)\cr the target toxicity probability (except 0 or 1)
#'   defining the MTD.
#' @slot thresh (`proportion`)\cr the threshold (except 0 or 1) relative to the
#'   recommended next best dose.
#' @slot prob (`proportion`)\cr required minimum probability, except 0 or 1.
#'
#' @aliases StoppingMTDdistribution
#' @export
#'
.StoppingMTDdistribution <- setClass(
  Class = "StoppingMTDdistribution",
  slots = c(
    target = "numeric",
    thresh = "numeric",
    prob = "numeric"
  ),
  prototype = prototype(
    target = 0.33,
    thresh = 0.5,
    prob = 0.9
  ),
  contains = "Stopping",
  validity = v_stopping_mtd_distribution
)

## constructor ----

#' @rdname StoppingMTDdistribution-class
#'
#' @param target (`proportion`)\cr see slot definition.
#' @param thresh (`proportion`)\cr see slot definition.
#' @param prob (`proportion`)\cr see slot definition.
#' @param report_label (`string` or `NA`)\cr see slot definition.
#'
#' @example examples/Rules-class-StoppingMTDdistribution.R
#' @export
#'
StoppingMTDdistribution <- function(target = 0.33,
                                    thresh = 0.5,
                                    prob = 0.9,
                                    report_label = NA_character_) {
  report_label <- h_default_if_empty(
    as.character(report_label),
    paste0("P(MTD > ", thresh, " * NBD | P(DLE) = ", target, ") \u2265 ", prob)
  )

  .StoppingMTDdistribution(
    target = target,
    thresh = thresh,
    prob = prob,
    report_label = report_label
  )
}

## default constructor ----

#' @rdname StoppingMTDdistribution-class
#' @note Typically, end users will not use the `.DefaultStoppingMTDDistribution()` function.
#' @export
.DefaultStoppingMTDdistribution <- function() {
  StoppingMTDdistribution(
    target = 0.33,
    thresh = 0.5,
    prob = 0.9,
    report_label = NA_character_)
}

# StoppingMTDCV ----

## class ----

#' `StoppingMTDCV`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`StoppingMTDCV`] is a class for stopping rule based on precision of MTD
#' which is calculated as the coefficient of variation (CV) of the MTD.
#' Here, the MTD is defined as the dose that reaches a specific `target`
#' probability of the occurrence of a DLT.
#'
#' @slot target (`proportion`)\cr toxicity target of MTD (except 0 or 1).
#' @slot thresh_cv (`number`)\cr threshold (percentage > 0) for CV to be
#'   considered accurate enough to stop the trial. The stopping occurs when the
#'   CV is less than or equal to `tresh_cv`.
#'
#' @aliases StoppingMTDCV
#' @export
#'
.StoppingMTDCV <- setClass(
  Class = "StoppingMTDCV",
  slots = c(
    target = "numeric",
    thresh_cv = "numeric"
  ),
  prototype = prototype(
    target = 0.3,
    thresh_cv = 40
  ),
  contains = "Stopping",
  validity = v_stopping_mtd_cv
)

## constructor ----

#' @rdname StoppingMTDCV-class
#'
#' @param target (`proportion`)\cr see slot definition.
#' @param thresh_cv (`number`)\cr see slot definition.
#' @param report_label (`string` or `NA`)\cr see slot definition.
#'
#' @export
#' @example examples/Rules-class-StoppingMTDCV.R
#'
StoppingMTDCV <- function(target = 0.3,
                          thresh_cv = 40,
                          report_label = NA_character_) {
  report_label <- h_default_if_empty(
    as.character(report_label),
    paste("CV(MTD) >", target)
  )

  .StoppingMTDCV(
    target = target,
    thresh_cv = thresh_cv,
    report_label = report_label
  )
}

## default constructor ----

#' @rdname StoppingMTDCV-class
#' @note Typically, end users will not use the `.DefaultStoppingMTDCV()` function.
#'
#' @export
.DefaultStoppingMTDCV <- function() {
  StoppingMTDCV(
    target = 0.3,
    thresh_cv = 40,
    report_label = NA_character_
    )
}

# StoppingLowestDoseHSRBeta ----

## class ----

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
#'
#' @note This stopping rule is independent from the underlying model.
#'
#' @slot target (`proportion`)\cr the target toxicity.
#' @slot prob (`proportion`)\cr the threshold probability for the lowest dose
#'   being toxic.
#' @slot a (`number`)\cr shape parameter \eqn{a > 0} of probability distribution
#'   Beta (a,b).
#' @slot b (`number`)\cr shape parameter \eqn{b > 0} of probability distribution
#'   Beta (a,b).
#'
#' @aliases StoppingLowestDoseHSRBeta
#' @export
#'
.StoppingLowestDoseHSRBeta <- setClass(
  Class = "StoppingLowestDoseHSRBeta",
  slots = c(
    target = "numeric",
    prob = "numeric",
    a = "numeric",
    b = "numeric"
  ),
  prototype = prototype(
    target = 0.3,
    prob = 0.95,
    a = 1,
    b = 1
  ),
  contains = "Stopping",
  validity = v_increments_hsr_beta
)

## constructor ----

#' @rdname StoppingLowestDoseHSRBeta-class
#'
#' @param target (`proportion`)\cr see slot definition.
#' @param prob (`proportion`)\cr see slot definition.
#' @param a (`number`)\cr see slot definition.
#' @param b (`number`)\cr see slot definition.
#' @param report_label (`string` or `NA`)\cr see slot definition.
#'
#' @export
#' @example examples/Rules-class-StoppingLowestDoseHSRBeta.R
#'
StoppingLowestDoseHSRBeta <- function(target = 0.3,
                                      prob = 0.95,
                                      a = 1,
                                      b = 1,
                                      report_label = NA_character_) {
  report_label <- h_default_if_empty(
    as.character(report_label),
    paste0("P\u03B2(lowest dose > P(DLE) = ", target, ") > ", prob)
  )

  .StoppingLowestDoseHSRBeta(
    target = target,
    prob = prob,
    a = a,
    b = b,
    report_label = report_label
  )
}

## default constructor ----

#' @rdname StoppingLowestDoseHSRBeta-class
#' @note Typically, end users will not use the `.DefaultStoppingLowestDoseHSRBeta()` function.
#' @export
.DefaultStoppingLowestDoseHSRBeta <- function() { # nolint
  StoppingLowestDoseHSRBeta(
    target = 0.3,
    prob = 0.95,
    a = 1,
    b = 1,
    report_label = NA_character_)
}

# StoppingTargetBiomarker ----

## class ----

#' `StoppingTargetBiomarker`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`StoppingTargetBiomarker`] is a class for stopping based on probability of
#' target biomarker.
#'
#' @slot target (`numeric`)\cr the biomarker target range that needs to be
#'   reached. For example, `target = c(0.8, 1.0)` with `is_relative = TRUE`
#'   means that we target a dose with at least 80% of maximum biomarker level.
#' @slot is_relative (`flag`)\cr is target relative? If it so (default), then
#'   the `target` is interpreted relative to the maximum, so it must be a
#'   probability range. Otherwise, the `target` is interpreted as absolute
#'   biomarker range.
#' @slot prob (`proportion`)\cr required target probability (except 0 or 1) for
#'   reaching sufficient precision.
#'
#' @aliases StoppingTargetBiomarker
#' @export
#'
.StoppingTargetBiomarker <- setClass(
  Class = "StoppingTargetBiomarker",
  slots = c(
    target = "numeric",
    is_relative = "logical",
    prob = "numeric"
  ),
  prototype = prototype(
    target = c(0.9, 1),
    is_relative = TRUE,
    prob = 0.3
  ),
  contains = "Stopping",
  validity = v_stopping_target_biomarker
)

## constructor ----

#' @rdname StoppingTargetBiomarker-class
#'
#' @param target (`numeric`)\cr see slot definition.
#' @param prob (`proportion`)\cr see slot definition.
#' @param is_relative (`flag`)\cr see slot definition.
#' @param report_label (`string` or `NA`)\cr see slot definition.
#'
#' @export
#' @example examples/Rules-class-StoppingTargetBiomarker.R
#'
StoppingTargetBiomarker <- function(target = c(0.9, 1),
                                    prob = 0.3,
                                    is_relative = TRUE,
                                    report_label = NA_character_) {
  assert_numeric(target, len = 2)
  assert_flag(is_relative)

  report_label <- h_default_if_empty(
    as.character(report_label),
    paste0(
      "P(", target[1], " \u2264 ", "Biomarker \u2264 ", target[2], ") \u2265 ", prob,
      ifelse(is_relative, " (relative)", " (absolute)")
    )
  )

  .StoppingTargetBiomarker(
    target = target,
    is_relative = is_relative,
    prob = prob,
    report_label = report_label
  )
}

## default constructor ----

#' @rdname StoppingTargetBiomarker-class
#' @note Typically, end users will not use the `.DefaultStoppingTargetBiomarker()` function.
#' @export
.DefaultStoppingTargetBiomarker <- function() {
  StoppingTargetBiomarker(
    target = c(0.9, 1),
    prob = 0.5,
    is_relative = TRUE,
    report_label = NA_character_)
}

# StoppingSpecificDose ----

## class ----

#' `StoppingSpecificDose`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`StoppingSpecificDose`] is the class for testing a stopping rule at specific
#' dose of the dose grid and not at the next best dose.
#'
#' @slot rule (`Stopping`)\cr a stopping rule available in this package.
#' @slot dose (`positive_number`)\cr a dose that is defined as part of the dose
#'   grid of the data.
#'
#' @aliases StoppingSpecificDose
#' @export
#'
.StoppingSpecificDose <- setClass(
  Class = "StoppingSpecificDose",
  slots = c(
    rule = "Stopping",
    dose = "positive_number"
  ),
  contains = "Stopping"
)

## constructor ----

#' @rdname StoppingSpecificDose-class
#'
#' @param rule (`Stopping`)\cr see slot definition.
#' @param dose (`number`)\cr see slot definition.
#' @param report_label (`string` or `NA`) \cr see slot definition.
#'
#' @export
#' @example examples/Rules-class-StoppingSpecificDose.R
#'
StoppingSpecificDose <- function(rule = StoppingTargetProb(target = c(0, 0.3), prob = 0.8),
                                  dose = 80,
                                  report_label = NA_character_) {
  report_label <- h_default_if_empty(
    as.character(report_label),
    paste0("Dose ", dose, " used for testing a stopping rule")
  )

  .StoppingSpecificDose(
    rule = rule,
    dose = positive_number(dose),
    report_label = report_label
  )
}

## default constructor ----

#' @rdname StoppingSpecificDose-class
#' @note Typically, end users will not use the `.DefaultStoppingSpecificDose()` function.
#' @export
.DefaultStoppingSpecificDose <- function() {
  StoppingSpecificDose(
    rule = StoppingTargetProb(target = c(0, 0.3), prob = 0.8),
    dose = positive_number(80)
  )
}

# StoppingHighestDose ----

## class ----

#' `StoppingHighestDose`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`StoppingHighestDose`] is the class for stopping based on the highest dose.
#' That is, the stopping occurs when the highest dose is reached.
#'
#' @aliases StoppingHighestDose
#' @export
#'
.StoppingHighestDose <- setClass(
  Class = "StoppingHighestDose",
  contains = "Stopping"
)

## constructor ----

#' @rdname StoppingHighestDose-class
#' @param report_label (`string` or `NA`)\cr see slot definition.
#'
#' @export
#' @example examples/Rules-class-StoppingHighestDose.R
#'
StoppingHighestDose <- function(report_label = NA_character_) {
  report_label <- h_default_if_empty(
    as.character(report_label),
    "NBD is the highest dose"
  )

  .StoppingHighestDose(report_label = report_label)
}

## default constructor ----

#' @rdname StoppingHighestDose-class
#' @note Typically, end users will not use the `.DefaultStoppingHighestDose()` function.
#' @export
.DefaultStoppingHighestDose <- function() {
  StoppingHighestDose(report_label = NA_character_)
}

# StoppingTDCIRatio ----

## class ----

#' `StoppingTDCIRatio`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`StoppingTDCIRatio`] is the class for testing a stopping rule that is based
#' on a target ratio of the 95% credibility interval. Specifically, this is the
#' ratio of the upper to the lower bound of the 95% credibility interval's
#' estimate of the target dose (i.e. a dose that corresponds to a given target
#' probability of the occurrence of a DLT `prob_target`).
#'
#' @slot target_ratio (`numeric`)\cr target for the ratio of the 95% credibility
#'   interval's estimate, that is required to stop a trial.
#' @slot prob_target (`proportion`)\cr the target probability of the occurrence
#'   of a DLT.
#'
#' @aliases StoppingTDCIRatio
#' @export
#'
.StoppingTDCIRatio <- setClass(
  Class = "StoppingTDCIRatio",
  slots = c(
    target_ratio = "numeric",
    prob_target = "numeric"
  ),
  prototype = prototype(
    target_ratio = 5,
    prob_target = 0.3
  ),
  contains = "Stopping",
  validity = v_stopping_tdci_ratio
)

## constructor ----

#' @rdname StoppingTDCIRatio-class
#'
#' @param target_ratio (`numeric`)\cr see slot definition.
#' @param prob_target (`proportion`)\cr see slot definition.
#' @param report_label (`string` or `NA`)\cr see slot definition.
#'
#' @export
#' @example examples/Rules-class-StoppingTDCIRatio.R
#'
StoppingTDCIRatio <- function(target_ratio = 5,
                              prob_target = 0.3,
                              report_label = NA_character_) {
  report_label <- h_default_if_empty(
    as.character(report_label),
    paste("TD", target_ratio, "for", prob_target, "target prob")
  )

  .StoppingTDCIRatio(
    target_ratio = target_ratio,
    prob_target = prob_target,
    report_label = report_label
  )
}

## default constructor ----

#' @rdname StoppingTDCIRatio-class
#' @note Typically, end users will not use the `.DefaultStoppingTDCIRatio()` function.
#' @export
.DefaultStoppingTDCIRatio <- function() {
  StoppingTDCIRatio(
    target_ratio = 5,
    prob_target = 0.3,
    report_label = NA_character_)
}

# StoppingMaxGainCIRatio ----

## class ----

#' `StoppingMaxGainCIRatio`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`StoppingMaxGainCIRatio`] is the class for testing a stopping rule that is based
#' on a target ratio of the 95% credibility interval. Specifically, this is the
#' ratio of the upper to the lower bound of the 95% credibility interval's
#' estimate of the:
#' (1) target dose (i.e. a dose that corresponds to a given target
#' probability of the occurrence of a DLT `prob_target`), or
#' (2) max gain dose (i.e. a dose which gives the maximum gain),
#' depending on which one out of these two is smaller.
#'
#' @slot target_ratio (`numeric`)\cr target for the ratio of the 95% credibility
#'   interval's estimate, that is required to stop a trial.
#' @slot prob_target (`proportion`)\cr the target probability of the occurrence
#'   of a DLT.
#'
#' @aliases StoppingMaxGainCIRatio
#' @export
#'
.StoppingMaxGainCIRatio <- setClass(
  Class = "StoppingMaxGainCIRatio",
  slots = c(
    target_ratio = "numeric",
    prob_target = "numeric"
  ),
  prototype = prototype(
    target_ratio = 5,
    prob_target = 0.3
  ),
  contains = "Stopping",
  validity = v_stopping_tdci_ratio
)

## constructor ----

#' @rdname StoppingMaxGainCIRatio-class
#'
#' @param target_ratio (`numeric`)\cr see slot definition.
#' @param prob_target (`proportion`)\cr see slot definition.
#' @param report_label (`string` or `NA`)\cr see slot definition.
#'
#' @export
#' @example examples/Rules-class-StoppingMaxGainCIRatio.R
#'
StoppingMaxGainCIRatio <- function(target_ratio = 5,
                                  prob_target = 0.3,
                                  report_label = NA_character_) {
  report_label <- h_default_if_empty(
    as.character(report_label),
    paste("GStar", target_ratio, "for", prob_target, "target prob")
  )

  .StoppingMaxGainCIRatio(
    target_ratio = target_ratio,
    prob_target = prob_target,
    report_label = report_label
  )
}


## default constructor ----

#' @rdname StoppingMaxGainCIRatio-class
#' @examples
#' .DefaultStoppingMaxGainCIRatio()
#' @export
.DefaultStoppingMaxGainCIRatio <- function() {
  StoppingMaxGainCIRatio(
    target_ratio = 5,
    prob_target = 0.3,
    report_label = NA_character_)
}



# StoppingList ----

## class ----

#' `StoppingList`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`StoppingList`] is the class for testing a stopping rule that consists of
#' many single stopping rules that are in turn the objects of class `Stopping`.
#' The `summary` slot stores a function that takes a logical vector of the size
#' of `stop_list` and returns a single logical value. For example, if the function
#' `all` is specified as a `summary` function, then that all stopping rules
#' defined in `stop_list` must be satisfied in order the result of this rule to
#' be `TRUE`.
#'
#' @slot stop_list (`list`)\cr list of stopping rules.
#' @slot summary (`function`)\cr a summary function to combine the results of
#'   the stopping rules into a single result.
#'
#' @aliases StoppingList
#' @export
#'
.StoppingList <- setClass(
  Class = "StoppingList",
  slots = c(
    stop_list = "list",
    summary = "function"
  ),
  prototype = prototype(
    stop_list = list(StoppingMinPatients(50), StoppingMinCohorts(5)),
    summary = all
  ),
  contains = "Stopping",
  validity = v_stopping_list
)

## constructor ----

#' @rdname StoppingList-class
#'
#' @param stop_list (`list`)\cr see slot definition.
#' @param summary (`function`)\cr see slot definition.
#'
#' @export
#' @example examples/Rules-class-StoppingList.R
#'
StoppingList <- function(stop_list, summary) {
  .StoppingList(
    stop_list = stop_list,
    summary = summary
  )
}

## default constructor ----

#' @rdname StoppingList-class
#' @note Typically, end users will not use the `.DefaultStoppingList()` function.
#' @export
.DefaultStoppingList <- function() {
  StoppingList(
    stop_list = c(
      StoppingMinCohorts(nCohorts = 3L),
      StoppingTargetProb(target = c(0.2, 0.35), prob = 0.5),
      StoppingMinPatients(nPatients = 20L)
    ),
    summary = any
  )
}

# StoppingAll ----

## class ----

#' `StoppingAll`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`StoppingAll`] is the class for testing a stopping rule that consists of
#' many single stopping rules that are in turn the objects of class `Stopping`.
#' All single stopping rules must be satisfied in order the result of this rule
#' to be `TRUE`.
#'
#' @slot stop_list (`list`)\cr list of stopping rules.
#' @slot report_label label for reporting
#' @aliases StoppingAll
#' @export
#'
.StoppingAll <- setClass(
  Class = "StoppingAll",
  slots = c(
    stop_list = "list"
    ),
  prototype = prototype(
    stop_list = list(
      StoppingMinPatients(50),
      StoppingMinCohorts(5)
    )
  ),
  contains = "Stopping",
  validity = v_stopping_all
)

## constructor ----

#' @rdname StoppingAll-class
#'
#' @param stop_list (`list`)\cr see slot definition.
#' @param report_label (`string`) \cr see slot definition.
#' @export
#' @example examples/Rules-class-StoppingAll.R
#'
StoppingAll <- function(stop_list, report_label = character(0)) {
  .StoppingAll(
    stop_list = stop_list,
    report_label = report_label
  )
}
## default constructor ----

#' @rdname StoppingAll-class
#' @note Typically, end users will not use the `.DefaultStoppingAll()` function.
#' @export
.DefaultStoppingAll <- function() {
  StoppingAll(
    stop_list = c(
      StoppingMinCohorts(nCohorts = 3L),
      StoppingTargetProb(target = c(0.2, 0.35), prob = 0.5),
      StoppingMinPatients(nPatients = 20L)
    )
  )
}

# StoppingAny ----

## class ----

#' `StoppingAny`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`StoppingAny`] is the class for testing a stopping rule that consists of
#' many single stopping rules that are in turn the objects of class `Stopping`.
#' At least one single stopping rule must be satisfied in order the result of
#' this rule to be `TRUE`.
#'
#' @slot stop_list (`list`)\cr list of stopping rules.
#' @slot report_label label for reporting
#'
#' @aliases StoppingAny
#' @export
#'
.StoppingAny <- setClass(
  Class = "StoppingAny",
  slots = c(
    stop_list = "list"
  ),
  prototype = prototype(
    stop_list = list(StoppingMinPatients(50), StoppingMinCohorts(5))
  ),
  contains = "Stopping",
  validity = v_stopping_all
)

## constructor ----

#' @rdname StoppingAny-class
#'
#' @param stop_list (`list`)\cr see slot definition.
#' @param report_label (`string`)\cr see slot definition.
#'
#' @export
#' @example examples/Rules-class-StoppingAny.R
#'
StoppingAny <- function(stop_list, report_label = character(0)) {
  .StoppingAny(
    stop_list = stop_list,
    report_label = report_label
  )
}

## default constructor ----

#' @rdname StoppingAny-class
#' @note Typically, end users will not use the `.DefaultStoppingAny()` function.
#' @export
.DefaultStoppingAny <- function() {
  StoppingAny(
    stop_list = c(
      StoppingMinCohorts(nCohorts = 3L),
      StoppingTargetProb(target = c(0.2, 0.35), prob = 0.5),
      StoppingMinPatients(nPatients = 20L)
    )
  )
}


# CohortSize ----

## class ----

#' `CohortSize`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`CohortSize`] is a class for cohort sizes.
#'
#' @seealso [`CohortSizeRange`], [`CohortSizeDLT`], [`CohortSizeConst`],
#'   [`CohortSizeParts`], [`CohortSizeMin`], [`CohortSizeMin`].
#'
#' @aliases CohortSize
#' @export
#'
setClass(
  Class = "CohortSize"
)

# CohortSizeRange ----

## class ----

#' `CohortSizeRange`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`CohortSizeRange`] is the class for cohort size based on dose range.
#'
#' @slot intervals (`numeric`)\cr a vector with the left bounds of the relevant
#'   dose intervals.
#' @slot cohort_size (`integer`)\cr an integer vector with the cohort sizes
#'   corresponding to the elements of `intervals`.
#'
#' @aliases CohortSizeRange
#' @export
#'
.CohortSizeRange <- setClass(
  Class = "CohortSizeRange",
  slots = c(
    intervals = "numeric",
    cohort_size = "integer"
  ),
  prototype = prototype(
    intervals = c(0, 20),
    cohort_size = c(1L, 3L)
  ),
  contains = "CohortSize",
  validity = v_cohort_size_range
)

## constructor ----

#' @rdname CohortSizeRange-class
#'
#' @param intervals (`numeric`)\cr see slot definition.
#' @param cohort_size (`numeric`)\cr see slot definition.
#'
#' @export
#' @example examples/Rules-class-CohortSizeRange.R
#'
CohortSizeRange <- function(intervals, cohort_size) {
  .CohortSizeRange(
    intervals = intervals,
    cohort_size = safeInteger(cohort_size)
  )
}

## default constructor ----

#' @rdname CohortSizeRange-class
#' @note Typically, end users will not use the `.DefaultCohortSizeRange()` function.
#' @export
.DefaultCohortSizeRange <- function() {
  CohortSizeRange(intervals = c(0L, 30L), cohort_size = c(1L, 3L))
}

# CohortSizeDLT ----

## class ----

#' `CohortSizeDLT`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`CohortSizeDLT`] is the class for cohort size based on number of DLTs.
#'
#' @slot dlt_intervals (`integer`)\cr a vector with the left bounds of the
#'   relevant DLT intervals.
#' @slot cohort_size (`integer`)\cr a vector with the cohort sizes corresponding
#'   to the elements of `dlt_intervals`.
#'
#' @aliases CohortSizeDLT
#' @export
#'
.CohortSizeDLT <- setClass(
  Class = "CohortSizeDLT",
  slots = c(
    dlt_intervals = "integer",
    cohort_size = "integer"
  ),
  prototype = prototype(
    dlt_intervals = c(0L, 1L),
    cohort_size = c(1L, 3L)
  ),
  contains = "CohortSize",
  validity = v_cohort_size_dlt
)

## constructor ----

#' @rdname CohortSizeDLT-class
#'
#' @param dlt_intervals (`numeric`)\cr see slot definition.
#' @param cohort_size (`numeric`)\cr see slot definition.
#'
#' @export
#' @example examples/Rules-class-CohortSizeDLT.R
#'
CohortSizeDLT <- function(dlt_intervals, cohort_size) {
  .CohortSizeDLT(
    dlt_intervals = safeInteger(dlt_intervals),
    cohort_size = safeInteger(cohort_size)
  )
}

## default constructor ----

#' @rdname CohortSizeDLT-class
#' @note Typically, end users will not use the `.DefaultCohortSizeDLT()` function.
#' @export
.DefaultCohortSizeDLT <- function() {
  CohortSizeDLT(dlt_intervals = c(0L, 1L), cohort_size = c(1L, 3L))
}


# CohortSizeConst ----

## class ----

#' `CohortSizeConst`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`CohortSizeConst`] is the class for fixed and constant size of cohort.
#'
#' @slot size (`integer`)\cr cohort size.
#'
#' @aliases CohortSizeConst
#' @export
#'
.CohortSizeConst <- setClass(
  Class = "CohortSizeConst",
  slots = c(size = "integer"),
  prototype = prototype(size = 3L),
  contains = "CohortSize",
  validity = v_cohort_size_const
)

## constructor ----

#' @rdname CohortSizeConst-class
#'
#' @param size (`number`)\cr see slot definition.
#'
#' @export
#' @example examples/Rules-class-CohortSizeConst.R
#'
CohortSizeConst <- function(size) {
  .CohortSizeConst(size = safeInteger(size))
}

## default constructor ----

#' @rdname CohortSizeConst-class
#' @note Typically, end users will not use the `.DefaultCohortSizeConst()` function.
#' @export
.DefaultCohortSizeConst <- function() {
  CohortSizeConst(size = 3L)
}

# CohortSizeParts ----

## class ----

#' `CohortSizeParts`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`CohortSizeParts`] is the class for cohort size that changes for the second
#' part of the dose escalation. It works only in conjunction with [`DataParts`]
#' objects.
#'
#' @slot sizes (`integer`)\cr a vector of length two with two sizes, one for
#'   part 1, and one for part 2 respectively.
#'
#' @aliases CohortSizeParts
#' @export
#'
.CohortSizeParts <- setClass(
  Class = "CohortSizeParts",
  slots = c(sizes = "integer"),
  prototype = prototype(sizes = c(1L, 3L)),
  contains = "CohortSize",
  validity = v_cohort_size_parts
)

## constructor ----

#' @rdname CohortSizeParts-class
#'
#' @param sizes (`numeric`)\cr see slot definition.
#'
#' @export
#' @example examples/Rules-class-CohortSizeParts.R
#'
CohortSizeParts <- function(sizes) {
  .CohortSizeParts(sizes = safeInteger(sizes))
}

## default constructor ----

#' @rdname CohortSizeParts-class
#' @note Typically, end users will not use the `.DefaultCohortSizeParts()` function.
#' @export
.DefaultCohortSizeParts <- function() {
  CohortSizeParts(sizes = c(1L, 3L))
}

# CohortSizeMax ----

## class ----

#' `CohortSizeMax`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`CohortSizeMax`] is the class for cohort size that is based on maximum of
#' multiple cohort size rules. The `cohort_size_list` slot stores a set of cohort
#' size rules, which are again the objects of class [`CohortSize`]. The maximum
#' of these individual cohort sizes is taken to give the final cohort size.
#'
#' @slot cohort_size_list (`list`)\cr a list of cohort size rules, i.e. objects
#' of class [`CohortSize`].
#'
#' @aliases CohortSizeMax
#' @export
#'
.CohortSizeMax <- setClass(
  Class = "CohortSizeMax",
  slots = c(cohort_size_list = "list"),
  prototype = prototype(
    cohort_size_list = list(
      CohortSizeRange(intervals = c(0, 30), cohort_size = c(1, 3)),
      CohortSizeDLT(dlt_intervals = c(0, 1), cohort_size = c(1, 3))
    )
  ),
  contains = "CohortSize",
  validity = v_cohort_size_max
)

## default constructor ----

#' @rdname CohortSizeMax-class
#' @note Typically, end users will not use the `.DefaultCohortSizeMax()` function.
#'
#' @export
.DefaultCohortSizeMax <- function() {
  CohortSizeMax(
    cohort_size_list = list(
      CohortSizeRange(intervals = c(0, 10), cohort_size = c(1L, 3L)),
      CohortSizeDLT(dlt_intervals = c(0L, 1L), cohort_size = c(1L, 3L))
    )
  )
}

## constructor ----

#' @rdname CohortSizeMax-class
#'
#' @param cohort_size_list (`list`)\cr see slot definition.
#'
#' @export
#' @example examples/Rules-class-CohortSizeMax.R
#'
CohortSizeMax <- function(cohort_size_list) {
  .CohortSizeMax(cohort_size_list = cohort_size_list)
}

# CohortSizeMin ----

## class ----

#' `CohortSizeMin`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`CohortSizeMin`] is the class for cohort size that is based on minimum of
#' multiple cohort size rules. The `cohort_size_list` slot stores a set of cohort
#' size rules, which are again the objects of class [`CohortSize`]. The minimum
#' of these individual cohort sizes is taken to give the final cohort size.
#'
#' @slot cohort_size_list (`list`)\cr a list of cohort size rules, i.e. objects
#' of class [`CohortSize`].
#'
#' @aliases CohortSizeMin
#' @export
#'
.CohortSizeMin <- setClass(
  Class = "CohortSizeMin",
  slots = c(cohort_size_list = "list"),
  prototype = prototype(
    cohort_size_list =
      list(
        CohortSizeRange(intervals = c(0, 30), cohort_size = c(1, 3)),
        CohortSizeDLT(dlt_intervals = c(0, 1), cohort_size = c(1, 3))
      )
  ),
  contains = "CohortSize",
  validity = v_cohort_size_max
)

## constructor ----

#' @rdname CohortSizeMin-class
#'
#' @param cohort_size_list (`list`)\cr see slot definition.
#'
#' @export
#' @example examples/Rules-class-CohortSizeMin.R
#'
CohortSizeMin <- function(cohort_size_list) {
  .CohortSizeMin(cohort_size_list = cohort_size_list)
}

## default constructor ----

#' @rdname CohortSizeMin-class
#' @note Typically, end users will not use the `.DefaultCohortSizeMin()` function.
#' @export
.DefaultCohortSizeMin <- function() {
  CohortSizeMin(
    cohort_size_list = list(
      CohortSizeRange(intervals = c(0, 10), cohort_size = c(1L, 3L)),
      CohortSizeDLT(dlt_intervals = c(0L, 1L), cohort_size = c(1L, 3L))
    )
  )
}

# SafetyWindow ----

## class ----

#' `SafetyWindow`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`SafetyWindow`] is a class for safety window.
#'
#' @seealso [`SafetyWindowSize`], [`SafetyWindowConst`].
#'
#' @aliases SafetyWindow
#' @export
#'
setClass(
  Class = "SafetyWindow"
)

# SafetyWindowSize ----

## class ----

#' `SafetyWindowSize`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`SafetyWindowSize`] is the class for safety window length based on cohort
#' size. This class is used to decide the rolling rule from the clinical
#' perspective.
#'
#' @slot gap (`list`)\cr observed period of the previous patient before
#'   the next patient can be dosed. This is used as follows. If for instance,
#'   the cohort size is 4 and we want to specify three time intervals between
#'   these four consecutive patients, i.e. 7 units of time between the 1st and
#'   the 2nd patient, 5 units between the 2nd and the 3rd one, and finally 3
#'   units between the 3rd and the 4th one, then,
#'   `gap` = `list(c(7L, 5L, 3L))`. Sometimes, we want that the interval
#'   only between the 1st and 2nd patient should be increased for the
#'   safety consideration and the rest time intervals should remain constant,
#'   regardless of what the cohort size is. Then, `gap` = `list(c(7L, 3L))`
#'   and the the package will automatically repeat the last element of the vector
#'   for the remaining time intervals.
#' @slot size (`integer`)\cr a vector with the left bounds of the
#'   relevant cohort size intervals. This is used as follows. For instance, when
#'   we want to change the `gap` based on the cohort size, i.e. the time
#'   interval between the 1st and 2nd patient = 9 units of time and the rest
#'   time intervals are of 5 units of time when the cohort size is equal to or
#'   larger than 4. And the time interval between the 1st and 2nd patient = 7 units
#'   of time and the rest time intervals are 3 units of time when the cohort size
#'   is smaller than 4, then we specify `size = c(0L, 4L)`. This means,
#'   the right bound of the intervals are exclusive to the interval, and the
#'   last interval goes from the last value until infinity.
#' @slot follow (`count`)\cr the period of time that each patient in the
#'   cohort needs to be followed before the next cohort opens.
#' @slot follow_min (`count`)\cr at least one patient in the cohort needs
#'   to be followed at the minimal follow up time.
#'
#' @aliases SafetyWindowSize
#' @export
#'
.SafetyWindowSize <- setClass(
  Class = "SafetyWindowSize",
  slots = c(
    gap = "list",
    size = "integer",
    follow = "integer",
    follow_min = "integer"
  ),
  prototype = prototype(
    gap = list(1:2, 1:2),
    size = c(1L, 3L),
    follow = 1L,
    follow_min = 1L
  ),
  contains = "SafetyWindow",
  validity = v_safety_window_size
)

## constructor ----

#' @rdname SafetyWindowSize-class
#'
#' @param gap see slot definition.
#' @param size see slot definition.
#' @param follow see slot definition.
#' @param follow_min see slot definition.
#'
#' @export
#' @example examples/Rules-class-SafetyWindowSize.R
#'
SafetyWindowSize <- function(gap,
                             size,
                             follow,
                             follow_min) {
  if (follow > follow_min) {
    warning("The value of follow_min is typically larger than the value of follow")
  }
  .SafetyWindowSize(
    gap = lapply(gap, safeInteger),
    size = safeInteger(size),
    follow = safeInteger(follow),
    follow_min = safeInteger(follow_min)
  )
}

# SafetyWindowConst ----

## class ----

#' `SafetyWindowConst`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`SafetyWindowConst`] is the class for safety window length and it is used
#' when the `gap` should be kept constant.
#'
#' @slot gap (`integer`)\cr a vector, the constant gap between patients.
#' @slot follow (`count`)\cr how long to follow each patient. The period of time
#'   that each patient in the cohort needs to be followed before the next cohort
#'   opens.
#' @slot follow_min (`count`)\cr minimum follow up. At least one patient in the
#'   cohort needs to be followed at the minimal follow up time.
#'
#' @aliases SafetyWindowConst
#' @export
#'
.SafetyWindowConst <- setClass(
  Class = "SafetyWindowConst",
  slots = c(
    gap = "integer",
    follow = "integer",
    follow_min = "integer"
  ),
  prototype = prototype(
    gap = 0L,
    follow = 1L,
    follow_min = 1L
  ),
  contains = "SafetyWindow",
  validity = v_safety_window_const
)

## constructor ----

#' @rdname SafetyWindowConst-class
#'
#' @param gap see slot definition.
#' @param follow see slot definition.
#' @param follow_min see slot definition.
#'
#' @export
#' @example examples/Rules-class-SafetyWindowConst.R
#'
SafetyWindowConst <- function(gap,
                              follow,
                              follow_min) {
  if (follow > follow_min) {
    warning("the value of follow_min is typically larger than the value of follow")
  }
  .SafetyWindowConst(
    gap = safeInteger(gap),
    follow = safeInteger(follow),
    follow_min = safeInteger(follow_min)
  )
}
