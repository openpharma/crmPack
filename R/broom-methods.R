# if (!requireNamespace("broom", quietly=TRUE)) {
#' Conversion of crmPack objects to data.frames/tibbles
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A method that converts a crmPack S4 object to a tibble or list of tibbles,
#' in the style of `broom::tidy()`.
#'
#' @param object (`DataDual`)\cr the object to convert
#' @param ... not used.
#' @return (`tbl_df` or named list [of `tbl_df`])
#' @export
#'
setGeneric(
  name = "tidy",
  def = function(object, ...) {
    standardGeneric("tidy")
  },
  valueClass = c("tbl_df", "tbl", "data.frame", "list")
)
# }

# tidy-NextBestMTD ----
# tidy-NextBestMinDist ----

#' @rdname tidy
#' @aliases tidy-NextBest
#' @example examples/NextBest-method-tidy.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(object = "NextBest"),
  definition = function(object, ...) {
    rv <- tibble::tibble(target = object@target)
    class(rv) <- c(paste0("tbl_", class(object)), class(rv))
    return(rv)
  }
)

# tidy-NextBestInfTheory ----

#' @rdname tidy
#' @aliases tidy-NextBestInfTheory
#' @example examples/NextBestInfTheory-method-tidy.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(object = "NextBestInfTheory"),
  definition = function(object, ...) {
    rv <- callNextMethod()
    rv <- rv %>% tibble::add_column(asymmetry = object@asymmetry)
    class(rv) <- c(paste0("tbl_", class(object)), class(rv))
    return(rv)
  }
)

# tidy-NextBestNCRM ----

#' @rdname tidy
#' @aliases tidy-NextBestNCRM
#' @example examples/NextBestNCRM-method-tidy.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(object = "NextBestNCRM"),
  definition = function(object, ...) {
    rv <- tibble::tibble(
      Range = c("Underdose", "Target", "Overdose"),
      min = c(0, object@target[1], object@target[2]),
      max = c(object@target[1], object@overdose[1], 1),
      max_prob = c(NA, NA, object@max_overdose_prob)
    )
    class(rv) <- c(paste0("tbl_", class(object)), class(rv))
    return(rv)
  }
)

# tidy-NextBestNCRMLoss ----

#' @rdname tidy
#' @aliases tidy-NextBestNCRMLoss
#' @example examples/NextBestNCRMLoss-method-tidy.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(object = "NextBestNCRMLoss"),
  definition = function(object, ...) {
    rv <- callNextMethod()
    if (rv %>% nrow() == 3 && length(object@losses) == 4) {
      rv <- rv %>%
        tibble::add_row(Range = "Unacceptable", min = object@unacceptable[1], max = 1) %>%
        dplyr::mutate(max = ifelse(Range == "Overdose", object@unacceptable[1], max))
    }
    rv <- rv %>% tibble::add_column(losses = object@losses)
    class(rv) <- c(paste0("tbl_", class(object)), class(rv))
    return(rv)
  }
)

# tidy-NextBestTDsamples ----

#' @rdname tidy
#' @aliases tidy-NextBestTDsamples
#' @example examples/NextBestTDsamples-method-tidy.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(object = "NextBestTD"),
  definition = function(object, ...) {
    rv <- tibble::tibble(
      prob_target_drt = object@prob_target_drt,
      prob_target_eot = object@prob_target_eot
    )
    class(rv) <- c(paste0("tbl_", class(object)), class(rv))
    return(rv)
  }
)

# tidy-NextBestTDsamples ----

#' @rdname tidy
#' @aliases tidy-NextBestTDsamples
#' @example examples/NextBestTDsamples-method-tidy.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(object = "NextBestTDsamples"),
  definition = function(object, ...) {
    rv <- tibble::tibble(
      prob_target_drt = object@prob_target_drt,
      prob_target_eot = object@prob_target_eot
    )
    class(rv) <- c(paste0("tbl_", class(object)), class(rv))
    return(rv)
  }
)

# tidy-NextBestMaxGain ----

#' @rdname tidy
#' @aliases tidy-NextBestMaxGain
#' @example examples/NextBestMaxGain-method-tidy.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(object = "NextBestMaxGain"),
  definition = function(object, ...) {
    rv <- tibble::tibble(
      prob_target_drt = object@prob_target_drt,
      prob_target_eot = object@prob_target_eot
    )
    class(rv) <- c(paste0("tbl_", class(object)), class(rv))
    return(rv)
  }
)
# tidy-NextBestMaxGainSamples ----

#' @rdname tidy
#' @aliases tidy-NextBestMaxGainSamples
#' @example examples/NextBestMaxGainSamples-method-tidy.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(object = "NextBestMaxGainSamples"),
  definition = function(object, ...) {
    rv <- tibble::tibble(
      prob_target_drt = object@prob_target_drt,
      prob_target_eot = object@prob_target_eot
    )
    class(rv) <- c(paste0("tbl_", class(object)), class(rv))
    return(rv)
  }
)

# tidy-NextBestDualEndpoint ----

#' @rdname tidy
#' @aliases tidy-NextBestDualEndpoint
#' @example examples/NextBestDualEndpoint-method-tidy.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(object = "NextBestDualEndpoint"),
  definition = function(object, ...) {
    rv <- tibble::tibble(
      Parameter = c("Biomarker", "Overdose"),
      min = c(object@target[1], object@overdose[1]),
      max = c(object@target[2], object@overdose[2]),
      max_prob = c(NA, object@max_overdose_prob),
      target_thresh = c(object@target_thresh, NA),
      target_relative = c(object@target_relative, NA)
    )
    class(rv) <- c(paste0("tbl_", class(object)), class(rv))
    return(rv)
  }
)

# tidy-NextBestThreePlusThree ----

#' @rdname tidy
#' @aliases tidy-NextBestThreePlusThree
#' @example examples/NextBestThreePlusThree-method-tidy.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(object = "NextBestThreePlusThree"),
  definition = function(object, ...) {
    rv <- tibble::tibble()
    class(rv) <- c(paste0("tbl_", class(object)), class(rv))
    return(rv)
  }
)

# tidy-CohortSize ----

#' @rdname tidy
#' @aliases tidy-CohortSize
#' @example examples/CohortSize-method-tidy.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(object = "CohortSize"),
  definition = function(object, ...) {
    row_count <- max(sapply(slotNames(object), function(name) length(slot(object, name))))
    rv <- tibble::tibble(.rows = row_count)
    for (slot in slotNames(object)) {
      rv <- rv %>% tibble::add_column(!!slot := slot(object, slot))
    }
    class(rv) <- c(paste0("tbl_", class(object)), class(rv))
    return(rv)
  }
)


# tidy-CohortSizeMax ----

#' @rdname tidy
#' @aliases tidy-CohortSizeMax
#' @example examples/CohortSizeMax-method-tidy.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(object = "CohortSizeMax"),
  definition = function(object, ...) {
    rv <- lapply(object@cohort_size_list, tidy) %>%
      dplyr::bind_rows(.id = "Index")
    class(rv) <- c(paste0("tbl_", class(object)), class(rv))
    return(rv)
  }
)

# tidy-CohortSizeMin ----

#' @rdname tidy
#' @aliases tidy-CohortSizeMin
#' @example examples/CohortSizeMin-method-tidy.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(object = "CohortSizeMin"),
  definition = function(object, ...) {
    rv <- lapply(object@cohort_size_list, tidy) %>%
      dplyr::bind_rows(.id = "Index")
    class(rv) <- c(paste0("tbl_", class(object)), class(rv))
    return(rv)
  }
)

# tidy-CohortSizeParts ----

#' @rdname tidy
#' @aliases tidy-CohortSizeParts
#' @example examples/CohortSizeParts-method-tidy.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(object = "CohortSizeParts"),
  definition = function(object, ...) {
    rv <- tibble::tibble(
      part = seq_along(object@sizes),
      sizes = object@sizes
    )
    class(rv) <- c(paste0("tbl_", class(object)), class(rv))
    return(rv)
  }
)

# tidy Data ----

#' @rdname tidy
#' @aliases tidy-Data
#' @example examples/Data-method-tidy.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(object = "Data"),
  definition = function(object, ...) {
    rv <- tibble::tibble(
      ID = object@ID,
      cohort = object@cohort,
      x = object@x,
      y = object@y,
      nGrid = object@nGrid,
      xLevel = object@xLevel,
      nObs = object@nObs,
      placebo = object@placebo
    )
    grid_tibble <- tibble::tibble(x = object@doseGrid) %>%
      tibble::add_column(xLevel = seq_along(nrow(.)))
    if (rv %>% nrow() == 0) {
      rv <- rv %>%
        tibble::add_column(doseGrid = list()) %>%
        tibble::add_row(doseGrid = list(grid_tibble))
    } else {
      rv <- rv %>% tibble::add_column(doseGrid = list(grid_tibble))
    }
    class(rv) <- c(paste0("tbl_", class(object)), class(rv))
    return(rv)
  }
)

# tidy DataDA ----

#' @rdname tidy
#' @aliases tidy-DataDA
#' @example examples/DataDA-method-tidy.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(object = "DataDA"),
  definition = function(object, ...) {
    rv <- callNextMethod()
    rv <- rv %>%
      tibble::add_column(t0 = object@t0, .after = 5) %>%
      tibble::add_column(Tmax = object@Tmax, .after = 5) %>%
      tibble::add_column(u = object@u, .after = 5)
    class(rv) <- c(paste0("tbl_", class(object)), class(rv))
    return(rv)
  }
)

# tidy DataDual ----

#' @rdname tidy
#' @aliases tidy-DataDual
#' @example examples/DataDual-method-tidy.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(object = "DataDual"),
  definition = function(object, ...) {
    rv <- callNextMethod()
    rv <- rv %>%
      tibble::add_column(w = object@w, .after = 5)
    class(rv) <- c(paste0("tbl_", class(object)), class(rv))
    return(rv)
  }
)

# tidy DataMixture ----

#' @rdname tidy
#' @aliases tidy-DataMixture
#' @example examples/DataMixture-method-tidy.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(object = "DataMixture"),
  definition = function(object, ...) {
    rv <- callNextMethod()
    rv <- rv %>%
      tibble::add_column(
        share = list(
          tibble::tibble(
            xshare = object@xshare,
            yshare = object@yshare
          )
        )
      )
    class(rv) <- c(paste0("tbl_", class(object)), class(rv))
    return(rv)
  }
)

# tidy DataParts ----

#' @rdname tidy
#' @aliases tidy-DataParts
#' @example examples/DataParts-method-tidy.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(object = "DataParts"),
  definition = function(object, ...) {
    rv <- callNextMethod() %>%
      add_column(part = object@part) %>%
      add_column(nextPart = object@nextPart)
    rv <- rv %>%
      tibble::add_column(
        part1Ladder = list(
          rv$doseGrid[[1]] %>% filter(x %in% object@part1Ladder)
        )
      )
    class(rv) <- c(paste0("tbl_", class(object)), class(rv))
    return(rv)
  }
)
# tidy Design ----

#' @rdname tidy
#' @aliases tidy-Design
#' @example examples/Design-method-tidy.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(object = "Design"),
  definition = function(object, ...) {
    rv <- callNextMethod()
    rv$model <- object@model %>% tidy()
    rv$stopping <- object@stopping %>% tidy()
    rv$increments <- object@increments %>% tidy()
    if (!is.null(object@PLcohortSize)) {
      if (is.integer(object@PLcohortSize)) {
        rv$PLcohortSize <- CohortSizeConst(cohortSizeobject@PLcohortSize) %>% tidy()
      } else {
        rv$PLcohortSize <- object@PLcohortSize %>% tidy()
      }
    }
    class(rv) <- c(paste0("tbl_", class(object)), class(rv))
    return(rv)
  }
)
# tidy RuleDesign ----

#' @rdname tidy
#' @aliases tidy-RuleDesign
#' @example examples/RuleDesign-method-tidy.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(object = "RuleDesign"),
  definition = function(object, ...) {
    rv <- list(
      "nextBest" = object@nextBest %>% tidy(),
      "cohortSize" = object@nextBest %>% tidy(),
      "data" = object@nextBest %>% tidy(),
      "startingDose" = tibble::tibble(startingDose = object@startingDose)
    )
    class(rv) <- c(paste0("tbl_", class(object)), class(rv))
    return(rv)
  }
)

# tidy-Increments ----

#' @rdname tidy
#' @aliases tidy-Increments
#' @example examples/Increments-method-tidy.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(object = "Increments"),
  definition = function(object, interval_name = NA, ...) {
    row_count <- max(sapply(slotNames(object), function(name) length(slot(object, name))))
    rv <- tibble::tibble(.rows = row_count)
    for (slot in slotNames(object)) {
      rv <- rv %>% tibble::add_column(!!slot := slot(object, slot))
    }
    if (!is.na(interval_name)) {
      rv <- rv %>%
        dplyr::rename(min = {{ interval_name }}) %>%
        dplyr::mutate(
          max = dplyr::lead(min),
          max = ifelse(is.infinite(max), Inf, max)
        ) %>%
        dplyr::select(min, max, increments)
    }
    class(rv) <- c(paste0("tbl_", class(object)), class(rv))
    return(rv)
  }
)

# tidy IncrementsMin ----

#' @rdname tidy
#' @aliases tidy-IncrementsMin
#' @example examples/IncrementsMin-method-tidy.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(object = "IncrementsMin"),
  definition = function(object, ...) {
    rv <- lapply(object@increments_list, tidy) %>%
      dplyr::bind_rows(.id = "Index")
    class(rv) <- c(paste0("tbl_", class(object)), class(rv))
    return(rv)
  }
)

# tidy ModelParamsNormal
#' @rdname tidy
#' @aliases tidy ModelParamsNormal
#' @example examples/ModelParamsNormal-method-tidy.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(object = "ModelParamsNormal"),
  definition = function(object, param_names = c("alpha0", "alpha1"), ...) {
    rv <- list(
      "mean" = tibble::tibble(Mean = object@mean),
      "cov" = tibble::as_tibble(object@cov, .name_repair = function(x) param_names),
      "prec" = tibble::as_tibble(object@prec, .name_repair = function(x) param_names)
    )
    if (length(param_names) > 0) {
      rv$mean <- rv$mean %>%
        tibble::add_column(Parameter = param_names, .before = 1)
      rv$cov <- rv$cov %>%
        tibble::add_column(Parameter = param_names, .before = 1)
      rv$prec <- rv$prec %>%
        tibble::add_column(Parameter = param_names, .before = 1)
    }
    class(rv) <- c(paste0("tbl_", class(object)), class(rv))
    return(rv)
  }
)

# tidy Model ----

#' @rdname tidy
#' @aliases tidy-GeneralModel
#' @example examples/GeneralModel-method-tidy.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(object = "GeneralModel"),
  definition = function(object, param_names = NA, ...) {
    rv <- list(
      "params" = object@params %>% tidy(),
      "ref_dose" = tibble::tibble(ref_dose = object@ref_dose),
      "datanames" = tibble::tibble(Parameter = object@datanames),
      "datanames_prior" = tibble::tibble(Parameter = object@datanames_prior),
      "sample" = tibble::tibble(Parameter = object@sample)
    )
    class(rv) <- c(paste0("tbl_", class(object)), class(rv))
    return(rv)
  }
)

# tidy Model ----

#' @rdname tidy
#' @aliases tidy-LogisticNormalMixture
#' @example examples/LogisticNormalMixture-method-tidy.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(object = "LogisticNormalMixture"),
  definition = function(object, param_names = NA, ...) {
    rv <- list(
      "comp1" = object@comp1 %>% tidy(),
      "comp2" = object@comp2 %>% tidy(),
      "ref_dose" = tibble::tibble(ref_dose = object@ref_dose),
      "datanames" = tibble::tibble(Parameter = object@datanames),
      "datanames_prior" = tibble::tibble(Parameter = object@datanames_prior),
      "sample" = tibble::tibble(Parameter = object@sample),
      "weightpar" = tibble::tibble(Parameter = names(object@weightpar), weightpar = object@weightpar)
    )
    class(rv) <- c(paste0("tbl_", class(object)), class(rv))
    return(rv)
  }
)

# tidy Model ----

#' @rdname tidy
#' @aliases tidy-LogisticNormalFixedMixture
#' @example examples/LogisticNormalFixedMixture-method-tidy.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(object = "LogisticNormalFixedMixture"),
  definition = function(object, ...) {
    rv <- list(
      "components" = list(
        "comp1" = object@components$comp1 %>% tidy(),
        "comp2" = object@components$comp2 %>% tidy()
      ),
      "ref_dose" = tibble::tibble(ref_dose = object@ref_dose),
      "datanames" = tibble::tibble(Parameter = object@datanames),
      "datanames_prior" = tibble::tibble(Parameter = object@datanames_prior),
      "sample" = tibble::tibble(Parameter = object@sample),
      "weights" = tibble::tibble(Parameter = names(object@weights), weightpar = object@weights)
    )
    class(rv) <- c(paste0("tbl_", class(object)), class(rv))
    return(rv)
  }
) # tidy Model ----

#' @rdname tidy
#' @aliases tidy-LogisticKadaneBetaGamma
#' @example examples/LogisticKadaneBetaGamma-method-tidy.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(object = "LogisticKadaneBetaGamma"),
  definition = function(object, ...) {
    rv <- callNextMethod() %>%
      tibble::add_column(
        beta = object@beta,
        shape = object@shape,
        rate = object@rate
      )
    class(rv) <- c(paste0("tbl_", class(object)), class(rv))
    return(rv)
  }
)

# tidy DualEndpoint ----

#' @rdname tidy
#' @aliases tidy-DualEndpoint
#' @example examples/DualEndpoint-method-tidy.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(object = "DualEndpoint"),
  definition = function(object, ...) {
    rv <- list(
      "betaZ_params" = object@betaZ_params %>% tidy(),
      "ref_dose" = tibble::tibble(ref_dose = object@ref_dose),
      "use_log_dose" = tibble::tibble(use_log_dose = object@use_log_dose),
      "rho" = tibble::tibble(Parameter = names(object@rho), rho = object@rho),
      "sigma2W" = tibble::tibble(Parameter = names(object@sigma2W), sigma2W = object@sigma2W),
      "use_fixed" = tibble::tibble(Parameter = names(object@use_fixed), rho = object@use_fixed)
    )
    class(rv) <- c(paste0("tbl_", class(object)), class(rv))
    return(rv)
  }
)

# tidy LogisticKadane ----

#' @rdname tidy
#' @aliases tidy-LogisticKadane
#' @example examples/LogisticKadane-method-tidy.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(object = "LogisticKadane"),
  definition = function(object, param_names = NA, ...) {
    rv <- tibble::tibble(
      theta = object@theta,
      xmin = object@xmin,
      xmax = object@xmax
    )
    class(rv) <- c(paste0("tbl_", class(object)), class(rv))
    return(rv)
  }
)

# tidy DALogisticLogNormal ----

#' @rdname tidy
#' @aliases tidy-DALogisticLogNormal
#' @example examples/DALogisticLogNormal-method-tidy.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(object = "DALogisticLogNormal"),
  definition = function(object, param_names = NA, ...) {
    rv <- callNextMethod()
    rv$npiece <- tibble::tibble(npiece = object@npiece)
    rv$l <- tibble::tibble(l = object@l)
    rv$c_par <- tibble::tibble(c_par = object@c_par)
    rv$cond_pem <- tibble::tibble(cond_pem = object@cond_pem)
    class(rv) <- c(paste0("tbl_", class(object)), class(rv))
    return(rv)
  }
)
# tidy TITELogisticLogNormal ----

#' @rdname tidy
#' @aliases tidy-TITELogisticLogNormal
#' @example examples/TITELogisticLogNormal-method-tidy.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(object = "TITELogisticLogNormal"),
  definition = function(object, param_names = NA, ...) {
    rv <- callNextMethod()
    rv$weight_method <- tibble::tibble(weight_method = object@weight_method)
    class(rv) <- c(paste0("tbl_", class(object)), class(rv))
    return(rv)
  }
)

# tidy FractionalCRM ----

#' @rdname tidy
#' @aliases tidy-FractionalCRM
#' @example examples/FractionalCRM-method-tidy.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(object = "FractionalCRM"),
  definition = function(object, param_names = NA, ...) {
    rv <- list(
      "skel_probs" = tibble::tibble(skel_probs = object@skel_probs),
      "sigma2" = tibble::tibble(sigma2 = object@sigma2),
      "datanames" = tibble::tibble(Parameter = object@datanames),
      "datanames_prior" = tibble::tibble(Parameter = object@datanames_prior),
      "sample" = tibble::tibble(Parameter = object@sample)
    )
    class(rv) <- c(paste0("tbl_", class(object)), class(rv))
    return(rv)
  }
)

# tidy OneParExpNormalPrior ----

#' @rdname tidy
#' @aliases tidy-OneParExpNormalPrior
#' @example examples/OneParExpNormalPrior-method-tidy.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(object = "OneParExpNormalPrior"),
  definition = function(object, param_names = NA, ...) {
    rv <- list(
      "skel_probs" = tibble::tibble(skel_probs = object@skel_probs),
      "sigma2" = tibble::tibble(sigma2 = object@sigma2),
      "datanames" = tibble::tibble(Parameter = object@datanames),
      "datanames_prior" = tibble::tibble(Parameter = object@datanames_prior),
      "sample" = tibble::tibble(Parameter = object@sample)
    )
    class(rv) <- c(paste0("tbl_", class(object)), class(rv))
    return(rv)
  }
)

# tidy Stopping ----

#' @rdname tidy
#' @aliases tidy-Stopping
#' @example examples/Stopping-method-tidy.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(object = "Stopping"),
  definition = function(object, ...) {
    row_count <- max(sapply(slotNames(object), function(name) length(slot(object, name))))
    rv <- tibble::tibble(.rows = row_count)
    for (slot in slotNames(object)) {
      if (!is.function(slot(object, slot))) {
        rv <- rv %>% tibble::add_column(!!slot := slot(object, slot))
      }
    }
    class(rv) <- c(paste0("tbl_", class(object)), class(rv))
    return(rv)
  }
)

# tidy StoppingAll ----

#' @rdname tidy
#' @aliases tidy-StoppingAll
#' @example examples/StoppingAll-method-tidy.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(object = "StoppingAll"),
  definition = function(object, ...) {
    rv <- lapply(object@stop_list, tidy) %>%
      dplyr::bind_rows(.id = "Index")
    class(rv) <- c(paste0("tbl_", class(object)), class(rv))
    return(rv)
  }
)

# tidy StoppingAny ----

#' @rdname tidy
#' @aliases tidy-StoppingAny
#' @example examples/StoppingAny-method-tidy.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(object = "StoppingAny"),
  definition = function(object, ...) {
    rv <- lapply(object@stop_list, tidy) %>%
      dplyr::bind_rows(.id = "Index")
    class(rv) <- c(paste0("tbl_", class(object)), class(rv))
    return(rv)
  }
)

# tidy StoppingHighestDose ----

#' @rdname tidy
#' @aliases tidy-StoppingHighestDose
#' @example examples/StoppingHighestDose-method-tidy.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(object = "StoppingHighestDose"),
  definition = function(object, ...) {
    # StoppingHighestDose has no slots
    rv <- tibble::tibble()
    class(rv) <- c(paste0("tbl_", class(object)), class(rv))
    return(rv)
  }
)

# tidy StoppingAny ----

#' @rdname tidy
#' @aliases tidy-StoppingList
#' @example examples/StoppingList-method-tidy.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(object = "StoppingList"),
  definition = function(object, ...) {
    rv <- lapply(object@stop_list, tidy) %>%
      dplyr::bind_rows(.id = "Index")
    tibble::tibble()
    return(rv)
  }
)

# tidy Samples ----

#' @rdname tidy
#' @aliases tidy-Samples
#' @example examples/Samples-method-tidy.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(object = "Samples"),
  definition = function(object, ...) {
    rv <- lapply(
      names(object@data),
      function(x) {
        z <- tibble::tibble(unlist(object@data[x]))
        names(z) <- x
        z
      }
    ) %>%
      dplyr::bind_cols() %>%
      dplyr::bind_cols(object@options %>% tidy())
    return(rv)
  }
)

# tidy McmcOptions ----

#' @rdname tidy
#' @aliases tidy-McmcOptions
#' @example examples/McmcOptions-method-tidy.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(object = "McmcOptions"),
  definition = function(object, ...) {
    tibble(
      iterations = object@iterations,
      burnin = object@burnin,
      step = object@step,
      rng_kind = object@rng_kind,
      rng_seed = object@rng_seed
    )
  }
)

