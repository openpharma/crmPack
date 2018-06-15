
# In the example below, the target toxiciy interval [0.2, 0.35] while the 
# overdose interval is (0.35,1]. Finally we would like to constrain the probability
# of overdosing below 25%.
myNextBest <- NextBestNCRM(target=c(0.2, 0.35),
                           overdose=c(0.35, 1),
                           maxOverdoseProb=0.25)


