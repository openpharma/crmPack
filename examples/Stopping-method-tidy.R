StoppingCohortsNearDose(nCohorts = 3, percentage = 0.2) %>% tidy()
StoppingGstarCIRatio(targetRatio = 5, targetEndOfTrial = 0.3) %>% tidy()
StoppingMinCohorts(nCohorts = 6) %>% tidy()
StoppingMinPatients(nPatients = 20) %>% tidy()
StoppingMTDdistribution(target = 0.33, thresh = 0.5, prob = 0.9) %>% tidy()
StoppingPatientsNearDose(nPatients = 9, percentage = 0.2) %>% tidy()
StoppingTargetBiomarker(target = c(0.9, 1), prob = 0.5) %>% tidy()
StoppingTargetProb(target = c(0.2, 0.35), prob = 0.5) %>% tidy()
StoppingTDCIRatio(targetRatio = 5, targetEndOfTrial = 0.3) %>% tidy()

#
#
# StoppingTargetProb(target=c(0.2, 0.35), prob=0.5) %>%
#   tidy() %>%
#   tibble::add_column(limit=c("min", "max")) %>%
#   tidyr::pivot_wider(names_from=limit, values_from=target)
