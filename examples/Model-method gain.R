##Obtain the gain value for a given dose, a pseudo DLE model, a DLE sample, a pseudo efficacy model
## and an efficacy sample
##The DLE model must be from 'ModelTox' class (DLEmodel slot)
##The efficacy model must be from 'ModelEff' class (Effmodel slot)
## The DLE and efficayc samples must be from 'Samples' class (DLEsamples and Effsamples slot)
## Given a dose level 75,
gain(dose=75,DLEmodel=DLEmodel,DLEsamples=DLEsamples,Effmodel=Effmodel,Effsamples=Effsamples)