npiece <- 10
Tmax <- 60

lambda_prior <- function(k)
{
  npiece / (Tmax * (npiece - k + 0.5))
}

model <- DALogisticLogNormal(mean=c(-0.85,1),
                           cov=matrix(c(1, -0.5, -0.5, 1),
                                      nrow=2),
                           ref_dose=56,
                           npiece=npiece,
                           l=as.numeric(t(apply(as.matrix(c(1:npiece), 1, npiece),
                                                2,
                                                lambda_prior))),
                           C_par=2)
