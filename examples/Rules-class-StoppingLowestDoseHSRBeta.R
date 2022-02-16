
#   Stopping the study if the first dose is toxic with more than 90%
#   probability based on a Beta posterior distribution with Beta(1,1) prior.

my_stopping <- StoppingLowestDoseHSRBeta(target = 0.3,
                                         prob = 0.9)


#   Stopping the study if the first dose is toxic with more than 90%
#   probability based on a Beta posterior distribution with Beta(0.5,0.5) prior.

my_stopping <- StoppingLowestDoseHSRBeta(target = 0.3,
                                         prob = 0.9,
                                         a = 0.5,
                                         b = 0.5)
