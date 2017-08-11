model <- ProbitLogNormal(mu = c(-0.85, 1),
                           Sigma = matrix(c(1, -0.5, -0.5, 1), nrow = 2))

## we can also specify a reference dose, and use a log transformation of
## standardized dose in the model:
model <- ProbitLogNormal(mu = c(-0.85, 1),
                         Sigma = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
                         refDose = 7.2,
                         useLogDose=TRUE)


