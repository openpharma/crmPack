# Define the dose-grid and PEM parameters
emptydata <- DataDA(doseGrid=c(0.1, 0.5,1, 1.5, 3, 6,
                               seq(from=10, to=80, by=2)),Tmax=60)
# Initialize the mDA-CRM model 
npiece_=10
Tmax_=60

lambda_prior<-function(k){
  npiece_/(Tmax_*(npiece_-k+0.5))
}

model<-DALogisticLogNormal(mean=c(-0.85,1),
                           cov=matrix(c(1,-0.5,-0.5,1),nrow=2),
                           refDose=56,
                           npiece=npiece_,
                           l=as.numeric(t(apply(as.matrix(c(1:npiece_),1,npiece_),2,lambda_prior))),
                           C_par=2)
# Choose the rule for dose increments
myIncrements <- IncrementsRelative(intervals=c(0,20),
                                   increments=c(1,0.33))
# Choose the rule for selecting the next dose 
nextMaxDose <- maxDose(myIncrements,data=emptydata)

myNextBest <- NextBestNCRM(target=c(0.2,0.35),
                           overdose=c(0.35,1),
                           maxOverdoseProb=0.25)

# Choose the rule for the cohort-size 
mySize1 <- CohortSizeRange(intervals=c(0, 30),
                           cohortSize=c(1, 3))
mySize2 <- CohortSizeDLT(DLTintervals=c(0, 1),
                         cohortSize=c(1, 3))
mySize <- maxSize(mySize1, mySize2)

# Choose the rule for stopping
myStopping1 <- StoppingTargetProb(target=c(0.2, 0.35),
                                  prob=0.5)
myStopping2 <- StoppingMinPatients(nPatients=50)

myStopping <- (myStopping1 | myStopping2)

# Choose the safety window 
mysafetywindow=SafetyWindowConst(c(6,2),7,7)

# Initialize the design
design <- DADesign(model=model,
                   increments=myIncrements,
                   nextBest=myNextBest,
                   stopping=myStopping,
                   cohortSize=mySize,
                   data=emptydata,
                   safetyWindow=mysafetywindow,
                   startingDose=3)

## set up truth curves

myTruth<-function(dose){
  
  model@prob(dose,alpha0=2,alpha1=3)
  
}

curve(myTruth(x), from=0, to=100, ylim=c(0, 1))



onset=15

exp_cond.cdf<-function(x){
  
  1-(pexp(x,1/onset,lower.tail=FALSE)-pexp(28,1/onset,lower.tail=FALSE))/pexp(28,1/onset)
  
}


#set up simulation settings
options <- McmcOptions(burnin=100,
                       step=1,
                       samples=2000)

mySims <- simulate(design,
                   args=NULL,
                   truthTox=myTruth,
                   truthSurv=exp_cond.cdf,#piece_exp_cond.cdf,
                   trueTmax=80,
                   nsim=2,
                   seed=819,
                   mcmcOptions=options,
                   firstSeparate=TRUE,
                   deescalate=FALSE,
                   parallel=TRUE,
                   nCores=2)
