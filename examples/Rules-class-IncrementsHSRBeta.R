
# Limit the escalation with a hard safety criteria to the doses that are below
# the first dose that is toxic with a probability of 0.95.
my_increments <- IncrementsHSRBeta(target = 0.3, prob = 0.95)
