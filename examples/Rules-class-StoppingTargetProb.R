
# As example, here is the rule for stopping the study if the posterior
# probability that [0.2 =< Prob(DLT | dose) <= 0.35] for the next best dose
# is above 0.5.

my_stopping <- StoppingTargetProb(target = c(0.2, 0.35), prob = 0.5)
