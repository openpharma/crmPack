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

# tidy-CohortSize ----

#' @rdname tidy
#' @aliases tidy-CohortSize
#' @example examples/CohortSize-method-tidy.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(object = "CohortSize"),
  definition = function(object, ...) {
    rowCount <- max(sapply(slotNames(object), function(name) length(slot(object, name))))
    rv <- tibble::tibble(.rows=rowCount)
    for(slot in slotNames(object)) {
      rv <- rv %>% tibble::add_column(!! slot := slot(object, slot))
    }
    rv <- rv %>% tibble::add_column(Class=class(object), .before=1)
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
    return(
      lapply(object@cohortSizeList, tidy) %>%
        dplyr::bind_rows(.id="Index") %>%
        dplyr::rename(ElementClass=Class) %>%
        tibble::add_column(Class="CohortSizeMax", .before=1)
    )
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
    return(
      lapply(object@cohortSizeList, tidy) %>%
        dplyr::bind_rows(.id="Index") %>%
        dplyr::rename(ElementClass=Class) %>%
        tibble::add_column(Class="CohortSizeMin", .before=1)
    )
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
    return(
      tibble::tibble(
        Class="CohortSizeParts",
        part=1:length(object@sizes),
        sizes=object@sizes
      )
    )
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
            Class="Data",
            ID=object@ID,
            cohort=object@cohort,
            x=object@x,
            y=object@y,
            nGrid=object@nGrid,
            xLevel=object@xLevel,
            nObs=object@nObs,
            placebo=object@placebo
          )
    gridTibble <- tibble::tibble(x=object@doseGrid) %>%
                    tibble::add_column(xLevel=1:nrow(.))
    if (rv %>% nrow() == 0) {
      rv <- rv %>%
              tibble::add_column(doseGrid=list()) %>%
              tibble::add_row(doseGrid=list(gridTibble))
    } else {
      rv <- rv %>% tibble::add_column(doseGrid=list(gridTibble))
    }
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
            tibble::add_column(t0=object@t0, .after=5) %>%
            tibble::add_column(Tmax=object@Tmax, .after=5) %>%
            tibble::add_column(u=object@u, .after=5) %>%
            dplyr::mutate(Class="DataDA", .before=1)
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
            tibble::add_column(w=object@w, .after=5) %>%
            dplyr::mutate(Class="DataDual", .before=1)
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
              share=list(
                tibble::tibble(
                  xshare=object@xshare,
                  yshare=object@yshare
                )
                )
              ) %>%
              dplyr::mutate(Class="DataMixture", .before=1)
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
            add_column(part=object@part) %>%
            add_column(nextpart=object@nextPart)
    rv <- rv %>%
            tibble::add_column(
              part1Ladder=list(
                rv$doseGrid[[1]] %>% filter(x %in% object@part1Ladder)
              )
            ) %>%
            dplyr::mutate(Class="DataParts", .before=1)
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
        rv$PLcohortSize <- object@PLcohortSize %>%  tidy()
      }
    }
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
      "nextBest"=object@nextBest %>%  tidy(),
      "cohortSize"=object@nextBest %>%  tidy(),
      "data"=object@nextBest %>%  tidy(),
      "startingDose"=tibble::tibble(startingDose=object@startingDose)
    )
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
  definition = function(object, ...) {
    rowCount <- max(sapply(slotNames(object), function(name) length(slot(object, name))))
    rv <- tibble::tibble(.rows=rowCount)
    for(slot in slotNames(object)) {
      rv <- rv %>% tibble::add_column(!! slot := slot(object, slot))
    }
    rv <- rv %>% tibble::add_column(Class=class(object), .before=1)
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
  rv <- lapply(object@increments_list,  tidy) %>%
          dplyr::bind_rows(.id="Index") %>%
          dplyr::rename(ElementClass=Class) %>%
          tibble::add_column(Class="CohortSizeMax", .before=1)
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
  definition = function(object, paramNames=c("alpha0", "alpha1"), ...) {
    rv <- list(
            "mean"=tibble::tibble(Mean=object@mean),
            "cov"=tibble::as_tibble(object@cov, .name_repair=function(x) paramNames),
            "prec"=tibble::as_tibble(object@prec, .name_repair=function(x) paramNames)
          )
    if(length(paramNames) > 0) {
      rv$mean <- rv$mean %>%
                   tibble::add_column(Parameter=paramNames, .before=1)
      rv$cov <- rv$cov %>%
                  tibble::add_column(Parameter=paramNames, .before=1)
      rv$prec <- rv$prec %>%
                   tibble::add_column(Parameter=paramNames, .before=1)
    }
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
  definition = function(object, paramNames=NA, ...) {
    rv <- list(
             "params"=object@params %>% tidy(),
             "ref_dose"=tibble::tibble(ref_dose=object@ref_dose),
             "datanames"=tibble::tibble(Parameter=object@datanames),
             "datanames_prior"=tibble::tibble(Parameter=object@datanames_prior),
             "sample"=tibble::tibble(Parameter=object@sample)
    )
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
  definition = function(object, paramNames=NA, ...) {
    rv <- list(
             "comp1"=object@comp1 %>% tidy(),
             "comp2"=object@comp2 %>% tidy(),
             "ref_dose"=tibble::tibble(ref_dose=object@ref_dose),
             "datanames"=tibble::tibble(Parameter=object@datanames),
             "datanames_prior"=tibble::tibble(Parameter=object@datanames_prior),
             "sample"=tibble::tibble(Parameter=object@sample),
             "weightpar"=tibble::tibble(Parameter=names(object@weightpar), weightpar=object@weightpar)
    )
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
             "components"=list(
               "comp1"=object@components$comp1 %>% tidy(),
               "comp2"=object@components$comp2 %>% tidy()
             ),
             "ref_dose"=tibble::tibble(ref_dose=object@ref_dose),
             "datanames"=tibble::tibble(Parameter=object@datanames),
             "datanames_prior"=tibble::tibble(Parameter=object@datanames_prior),
             "sample"=tibble::tibble(Parameter=object@sample),
             "weights"=tibble::tibble(Parameter=names(object@weights), weightpar=object@weights)
    )
    return(rv)
  }
)# tidy Model ----

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
              beta=object@beta,
              shape=object@shape,
              rate=object@rate
            )
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
             "betaZ_params"=object@betaZ_params %>% tidy(),
             "ref_dose"=tibble::tibble(ref_dose=object@ref_dose),
             "use_log_dose"=tibble::tibble(use_log_dose=object@use_log_dose),
             "rho"=tibble::tibble(Parameter=names(object@rho), rho=object@rho),
             "sigma2W"=tibble::tibble(Parameter=names(object@sigma2W), sigma2W=object@sigma2W),
             "use_fixed"=tibble::tibble(Parameter=names(object@use_fixed), rho=object@use_fixed)
          )
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
  definition = function(object, paramNames=NA, ...) {
    tibble::tibble(
      theta=object@theta,
      xmin=object@xmin,
      xmax=object@xmax
    )
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
  definition = function(object, paramNames=NA, ...) {
    rv <- callNextMethod()
    rv$npiece <- tibble::tibble(npiece=object@npiece)
    rv$l <- tibble::tibble(l=object@l)
    rv$c_par <- tibble::tibble(c_par=object@c_par)
    rv$cond_pem <- tibble::tibble(cond_pem=object@cond_pem)
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
    rowCount <- max(sapply(slotNames(object), function(name) length(slot(object, name))))
    rv <- tibble::tibble(.rows=rowCount)
    for(slot in slotNames(object)) {
      if (!is.function(slot(object, slot))) {
        rv <- rv %>% tibble::add_column(!! slot := slot(object, slot))
      }
    }
    rv <- rv %>% tibble::add_column(Class=class(object), .before=1)
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
  rv <- lapply(object@stopList,  tidy) %>%
          dplyr::bind_rows(.id="Index") %>%
          dplyr::rename(ElementClass=Class) %>%
          tibble::add_column(Class="StoppingAll", .before=1)
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
    rv <- lapply(object@stopList,  tidy) %>%
            dplyr::bind_rows(.id="Index") %>%
            dplyr::rename(ElementClass=Class) %>%
            tibble::add_column(Class="StoppingAny", .before=1)
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
    return(tibble::tibble())
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
    rv <- lapply(object@stopList,  tidy) %>%
            dplyr::bind_rows(.id="Index") %>%
            dplyr::rename(ElementClass=Class) %>%
            tibble::add_column(Class="StoppingList", .before=1)
    return(rv)
  }
)



