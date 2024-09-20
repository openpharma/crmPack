model <- LogisticLogNormalOrdinal(
  mean = c(0.25, 0.15, 0.5),
  cov = matrix(c(1.5, 0, 0, 0, 2, 0, 0, 0, 1), nrow = 3),
  ref_dose = 30
)

emptyData <- DataOrdinal(
  doseGrid = doseGrid,
  yCategories = c("No tox" = 0L, "DLAE" = 1L, "CRS" = 2L)
)

#For warning regarding tox, see issue #748 https://github.com/openpharma/crmPack/issues/748
suppressWarnings({samples <- mcmc(emptyData, model, .DefaultMcmcOptions())})
toxIncrements <- IncrementsMaxToxProb(probs = c("DLAE" = 0.2, "CRS" = 0.05))
maxDose(toxIncrements, emptyData, model, samples)
