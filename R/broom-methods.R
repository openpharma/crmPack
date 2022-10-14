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
  valueClass = c("tbl_df", "tbl", "data.frame")
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



