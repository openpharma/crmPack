library(tidyverse)

#  [6] "NextBestMaxGain"
# [11] "NextBestMaxGainSamples"


LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 50
) %>% tidy()

LogisticNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 50
) %>% tidy()

ProbitLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2)
) %>% tidy()

# LogisticKadane(theta = 0.33, xmin = 1, xmax = 200) %>%  tidy()

ProbitLogNormalRel(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2)
) %>% tidy()

LogisticLogNormalSub(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 50
)

DualEndpointBeta(
  mean = c(0, 1),
  cov = matrix(c(1, 0, 0, 1), nrow = 2),
  ref_dose = 10,
  use_log_dose = TRUE,
  sigma2W = c(a = 0.1, b = 0.1),
  rho = c(a = 1, b = 1),
  E0 = c(0, 100),
  Emax = c(0, 500),
  delta1 = c(0, 5),
  mode = c(1, 15),
  ref_dose_beta = 1000
) %>% tidy()


DualEndpointEmax(
  mean = c(0, 1),
  cov = matrix(c(1, 0, 0, 1), nrow = 2),
  sigma2W = c(a = 0.1, b = 0.1),
  rho = c(a = 1, b = 1),
  E0 = c(0, 100),
  Emax = c(0, 500),
  ED50 = c(10, 200),
  ref_dose_emax = 1000
) %>% tidy()

DualEndpointRW(
  mean = c(0, 1),
  cov = matrix(c(1, 0, 0, 1), nrow = 2),
  sigma2W = c(a = 0.1, b = 0.1),
  rho = c(a = 1, b = 1),
  sigma2betaW = 0.01,
  rw1 = TRUE
) %>% tidy()

# LogisticNormalMixture(
#   comp1 = ModelParamsNormal(
#     mean = c(-0.85, 1),
#     cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2)
#   ),
#   comp2 = ModelParamsNormal(
#     mean = c(1, 1.5),
#     cov = matrix(c(1.2, -0.45, -0.45, 0.6), nrow = 2)
#   ),
#   weightpar = c(a = 1, b = 1),
#   ref_dose = 50
# ) %>%
# tidy()

# LogisticNormalFixedMixture(
#   components = list(
#     comp1 = ModelParamsNormal(
#       mean = c(-0.85, 1),
#       cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2)
#     ),
#     comp2 = ModelParamsNormal(
#       mean = c(1, 1.5),
#       cov = matrix(c(1.2, -0.45, -0.45, 0.6), nrow = 2)
#     )
#   ),
#   weights = c(0.3, 0.7),
#   ref_dose = 50
# ) %>%
# tidy()

LogisticLogNormalMixture(
  share_weight = 0.1,
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 50
) %>%
  tidy()

# LogisticKadaneBetaGamma(
#   theta = 0.3,
#   xmin = 0,
#   xmax = 7,
#   alpha = 1,
#   beta = 19,
#   shape = 0.5625,
#   rate = 0.125
# ) %>% tidy()

# DALogisticLogNormal(
#   mean = c(-0.85, 1),
#   cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
#   ref_dose = 56,
#   npiece = 10,
#   l = as.numeric(
#         t(
#           apply(
#             as.matrix(c(1:10), 1, 10),
#             2,
#             function(k) 10 / (60 * (10 - k + 0.5))
#           )
#         )
#       ),
#   c_par = 2
# ) %>%  tidy()


# TITELogisticLogNormal(
#   mean = c(0, 1),
#   cov = diag(2),
#   ref_dose = 1,
#   weight_method = "adaptive"
# ) %>% tidy()

# FractionalCRM(
#   skel_probs = c(0.1, 0.2, 0.3, 0.4),
#   dose_grid = c(10, 30, 50, 100),
#   sigma2 = 2
# ) %>% tidy()

# OneParExpNormalPrior(
#   skel_probs = seq(from = 0.1, to = 0.9, length = 5),
#   dose_grid = 1:5,
#   sigma2 = 2
# ) %>%  tidy()
