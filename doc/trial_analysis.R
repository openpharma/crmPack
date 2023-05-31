## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 5
)

## ----checkPacakges, echo=FALSE------------------------------------------------
notFound <- which(
              !sapply(c("kableExtra", "tibble", "magrittr", "dplyr",
                        "tidyr", "stringr", "kableExtra", "knitr"),
                      requireNamespace,
                      quietly = TRUE)
            )
cantRun <- length(notFound) > 0

## ---- eval=cantRun, results="asis", echo=FALSE--------------------------------
#  cat(
#    "The following packages are required to run this vignette but are not installed:",
#    paste0(names(notFound), collapse = ", "),
#    ".  Please install them and try again."
#  )
#  knitr::knit_exit()

## ----setup, echo=FALSE--------------------------------------------------------
suppressPackageStartupMessages({
  library(crmPack)
  library(tibble)
  library(magrittr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(kableExtra)
})

## ---- echo=FALSE--------------------------------------------------------------
doseGrid <- c(1, 3, 9, 20, 30, 45, 60, 80, 100)
empty_data <- Data(doseGrid = doseGrid)

model <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 56
)

my_increments <- IncrementsRelative(
  intervals = c(0, 30),
  increments = c(1, 0.5)
)

my_next_best <- NextBestNCRM(
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)

my_size <- maxSize(
  CohortSizeRange(intervals = c(0, 30), cohort_size = c(1, 3)),
  CohortSizeDLT(dlt_intervals = c(0, 1), cohort_size = c(1, 3))
)

my_stopping <- (StoppingMinCohorts(nCohorts = 3) &
  StoppingTargetProb(target = c(0.2, 0.35), prob = 0.5)) |
  StoppingMinPatients(nPatients = 20)


design <- Design(
  model = model,
  nextBest = my_next_best,
  stopping = my_stopping,
  increments = my_increments,
  cohortSize = my_size,
  data = empty_data,
  startingDose = 3
)

## -----------------------------------------------------------------------------
firstFour <- Data(
  x = c(1, 3, 9, 20),
  y = c(0, 0, 0, 1),
  ID = 1:4,
  cohort = 1:4,
  doseGrid = doseGrid
)

## -----------------------------------------------------------------------------
plot(firstFour)

## -----------------------------------------------------------------------------
plot(firstFour) + theme_light()

## -----------------------------------------------------------------------------
vignetteMcmcOptions <- McmcOptions(burnin = 100, step = 2, samples = 1000)
postSamples <- mcmc(
  data = firstFour,
  model = model,
  options = vignetteMcmcOptions
)

## -----------------------------------------------------------------------------
plot(postSamples, model, firstFour)

## -----------------------------------------------------------------------------
nextBest(
  my_next_best,
  doselimit = 100,
  samples = postSamples,
  model = model,
  data = empty_data
)$plot

## -----------------------------------------------------------------------------
tabulatePosterior <- function(mcmcSamples, observedData) {
  as_tibble(
    nextBest(
      my_next_best,
      doselimit = 100,
      samples = mcmcSamples,
      model = model,
      data = observedData
    )$probs
  ) %>%
    left_join(
      tibble(
        dose = observedData@x,
        WithDLT = observedData@y
      ) %>%
        group_by(dose) %>%
        summarise(
          Treated = n(),
          WithDLT = sum(WithDLT),
          .groups = "drop"
        ),
      by = "dose"
    ) %>%
    replace_na(list(Treated = 0, WithDLT = 0)) %>%
    select(dose, Treated, WithDLT, target, overdose) %>%
    kableExtra::kable(
      col.names = c("Dose", "Treated", "With DLT", "Target range", "Overdose range"),
      table.attr = "style='width:60%;'",
      digits = c(0, 0, 0, 3, 3)
    ) %>%
    kableExtra::add_header_above(c(" " = 1, "Participants" = 2, "Probability that dose is in " = 2))
}

tabulatePosterior(postSamples, firstFour)

## -----------------------------------------------------------------------------
nextMaxDose <- maxDose(my_increments, firstFour)
nextMaxDose

doseRecommendation <- nextBest(
  my_next_best,
  doselimit = nextMaxDose,
  samples = postSamples,
  model = model,
  data = firstFour
)
doseRecommendation$value

## -----------------------------------------------------------------------------
stopTrial(
  my_stopping,
  dose = doseRecommendation$value,
  postSamples,
  model,
  firstFour
)

## -----------------------------------------------------------------------------
firstFullCohort <- Data(
  x = c(1, 3, 9, 20, 20, 20, 20),
  y = c(0, 0, 0, 1, 0, 0, 0),
  ID = 1:7,
  cohort = c(1:4, rep(5, 3)),
  doseGrid = doseGrid
)

## -----------------------------------------------------------------------------
postSamples1 <- mcmc(
  data = firstFullCohort,
  model = model,
  options = vignetteMcmcOptions
)

## -----------------------------------------------------------------------------
tabulatePosterior(postSamples1, firstFullCohort)

## ---- error=TRUE--------------------------------------------------------------
nextMaxDose <- maxDose(my_increments, firstFullCohort)
nextMaxDose

doseRecommendation <- nextBest(
  my_next_best,
  doselimit = nextMaxDose,
  samples = postSamples1,
  model = model,
  data = firstFullCohort
)
doseRecommendation$value

x <- stopTrial(
  my_stopping,
  dose = doseRecommendation$value,
  postSamples1,
  model,
  firstFullCohort
)
attributes(x) <- NULL
x

## -----------------------------------------------------------------------------
secondFullCohort <- Data(
  x = c(1, 3, 9, 20, 20, 20, 20, 30, 30, 30),
  y = c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0),
  ID = 1:10,
  cohort = c(1:4, rep(5, 3), rep(6, 3)),
  doseGrid = doseGrid
)

## -----------------------------------------------------------------------------
postSamples2 <- mcmc(
  data = secondFullCohort,
  model = model,
  options = vignetteMcmcOptions
)

## -----------------------------------------------------------------------------
tabulatePosterior(postSamples2, secondFullCohort)

## -----------------------------------------------------------------------------
nextMaxDose <- maxDose(my_increments, secondFullCohort)
nextMaxDose

doseRecommendation <- nextBest(
  my_next_best,
  doselimit = nextMaxDose,
  samples = postSamples2,
  model = model,
  data = secondFullCohort
)
doseRecommendation$value

x <- stopTrial(
  my_stopping,
  dose = doseRecommendation$value,
  postSamples2,
  model,
  secondFullCohort
)
attributes(x) <- NULL
x

## -----------------------------------------------------------------------------
thirdFullCohort <- Data(
  x = c(1, 3, 9, rep(20, 4), rep(30, 6)),
  y = c(0, 0, 0, 1, rep(0, 9)),
  ID = 1:13,
  cohort = c(1:4, rep(5, 3), rep(6, 3), rep(7, 3)),
  doseGrid = doseGrid
)

## -----------------------------------------------------------------------------
postSamples3 <- mcmc(
  data = thirdFullCohort,
  model = model,
  options = vignetteMcmcOptions
)

## -----------------------------------------------------------------------------
tabulatePosterior(postSamples3, thirdFullCohort)

## -----------------------------------------------------------------------------
nextMaxDose <- maxDose(my_increments, thirdFullCohort)
nextMaxDose

doseRecommendation <- nextBest(
  my_next_best,
  doselimit = nextMaxDose,
  samples = postSamples3,
  model = model,
  data = thirdFullCohort
)
doseRecommendation$value

x <- stopTrial(
  my_stopping,
  dose = doseRecommendation$value,
  postSamples3,
  model,
  thirdFullCohort
)
attributes(x) <- NULL
x

## -----------------------------------------------------------------------------
fourthFullCohort <- Data(
  x = c(1, 3, 9, rep(20, 4), rep(30, 6), rep(45, 3)),
  y = c(0, 0, 0, 1, rep(0, 12)),
  ID = 1:16,
  cohort = c(1:4, rep(5:8, each = 3)),
  doseGrid = doseGrid
)

## -----------------------------------------------------------------------------
postSamples4 <- mcmc(
  data = fourthFullCohort,
  model = model,
  options = vignetteMcmcOptions
)

## -----------------------------------------------------------------------------
tabulatePosterior(postSamples4, fourthFullCohort)

## -----------------------------------------------------------------------------
nextMaxDose <- maxDose(my_increments, fourthFullCohort)
nextMaxDose

doseRecommendation <- nextBest(
  my_next_best,
  doselimit = nextMaxDose,
  samples = postSamples4,
  model = model,
  data = fourthFullCohort
)
doseRecommendation$value

x <- stopTrial(
  my_stopping,
  dose = doseRecommendation$value,
  postSamples4,
  model,
  fourthFullCohort
)
attributes(x) <- NULL
x

## -----------------------------------------------------------------------------
fifthFullCohort <- Data(
  x = c(1, 3, 9, rep(20, 4), rep(30, 6), rep(45, 6)),
  y = c(0, 0, 0, 1, rep(0, 13), 1, 1),
  ID = 1:19,
  cohort = c(1:4, rep(5:9, each = 3)),
  doseGrid = doseGrid
)

## -----------------------------------------------------------------------------
postSamples5 <- mcmc(
  data = fifthFullCohort,
  model = model,
  options = vignetteMcmcOptions
)

## -----------------------------------------------------------------------------
tabulatePosterior(postSamples5, fifthFullCohort)

## -----------------------------------------------------------------------------
nextMaxDose <- maxDose(my_increments, fifthFullCohort)
nextMaxDose

doseRecommendation <- nextBest(
  my_next_best,
  doselimit = nextMaxDose,
  samples = postSamples5,
  model = model,
  data = fifthFullCohort
)
doseRecommendation$value

x <- stopTrial(
  my_stopping,
  dose = doseRecommendation$value,
  postSamples5,
  model,
  fifthFullCohort
)
x

## -----------------------------------------------------------------------------
plot(fifthFullCohort)
plot(postSamples5, model, fifthFullCohort)

## -----------------------------------------------------------------------------
doseRecommendation$plot

## ---- error=TRUE--------------------------------------------------------------
slotNames(model)

fullSamples <- tibble(
  Alpha = postSamples5@data$alpha0,
  Beta = postSamples5@data$alpha1
) %>%
  expand(nesting(Alpha, Beta), Dose = doseGrid) %>%
  rowwise() %>%
  mutate(P = probFunction(model, alpha0 = Alpha, alpha1 = Beta)(dose = Dose)) %>%
  ungroup()

fullSummary <- fullSamples %>%
  group_by(Dose) %>%
  summarise(
    Mean = mean(P),
    Median = median(P),
    Q = list(quantile(P, probs = c(0.05, 0.1, 0.25, 0.75, 0.9, 0.95), na.rm = TRUE))
  ) %>%
  unnest_wider(
    col = Q,
    names_repair = function(.x) {
      ifelse(
        str_detect(.x, "\\d+%"),
        sprintf("Q%02.0f", as.numeric(str_remove_all(.x, "%"))),
        .x
      )
    }
  )

fullSummary %>%
  kableExtra::kable(
    col.names = c("Dose", "Mean", "Median", "5th", "10th", "25th", "75th", "90th", "95th"),
    digits = c(0, rep(3, 8))
  ) %>%
  add_header_above(c(" " = 3, "Quantiles" = 6)) %>%
  add_header_above(c(" " = 1, "P(Toxicity)" = 8))

fullSamples %>%
  filter(Dose > 9) %>%
  ggplot() +
  geom_density(aes(x = P, color = as.factor(Dose))) +
  theme_light() +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(
    title = "Posterior PDFs for doses > 9",
    colour = "Dose"
  )

## ---- error=TRUE--------------------------------------------------------------
fullSummary %>%
  ggplot(aes(x = Dose)) +
  geom_ribbon(aes(ymin = Q05, ymax = Q95), fill = "steelblue", alpha = 0.25) +
  geom_ribbon(aes(ymin = Q10, ymax = Q90), fill = "steelblue", alpha = 0.25) +
  geom_ribbon(aes(ymin = Q25, ymax = Q75), fill = "steelblue", alpha = 0.25) +
  geom_line(aes(y = Mean), colour = "black") +
  geom_line(aes(y = Median), colour = "blue") +
  theme_light() +
  labs(
    title = "Posterior Dose toxicity curve",
    colour = "Dose",
    y = "P(Toxicity)"
  )

