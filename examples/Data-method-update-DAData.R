thisData <- DataDA(x=c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
                   y=c(0, 0, 1, 1, 0, 0, 1, 0),
                   doseGrid=
                     c(0.1, 0.5, 1.5, 3, 6,
                       seq(from=10, to=80, by=2)),
                   u=c(42,30,15,5,20,25,30,60),
                   t0=c(0,-15,-30,-40,-55,-70,-75,-85),
                   Tmax=60)


## todo: update this example with new function call structure 
factDLTs <- thisData@y
factSurv <- thisData@u
factT0   <- thisData@t0
thisT0   <- 5
thisDose <- 20
thisDLTs <- 0
thisSurv <- 20

tempData <- update(object=thisData,
                   factDLTs= c(factDLTs,thisDLTs),  ####the y will be updated according to u 
                   factSurv= c(factSurv,thisSurv),
                   factT0= c(factT0,thisT0),
                   thisDose= thisDose,
                   trialtime= 8)  ####this is the globle timeline for a trial 
