npiece <- 10
Tmax <- 60

lambda_prior <- function(k)
{
  npiece / (Tmax * (npiece - k + 0.5))
}

## todo_with_reply: need to explain why "l" is defined like that.
## reply: using the same assumption in Liu, Yin, Yuan 2013, it is assumed that a priori toxicity occurs 
## uniformly throughout the assessment period (0, Tmax), which represents a neutral prior opinion
## between early-onset and late-onset toxicity. Under this assumption, the hazard at the middle of 
## the kth partition is caluclated as the lambda_prior function;

model <- DALogisticLogNormal(mean=c(-0.85,1),
                           cov=matrix(c(1, -0.5, -0.5, 1),
                                      nrow=2),
                           refDose=56,
                           l=as.numeric(t(apply(as.matrix(c(1:npiece), 1, npiece),
                                                2,
                                                lambda_prior))),
                           C_par=2)