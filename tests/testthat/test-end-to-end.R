

test_that("Posterior summaries for probabilities of DLT (CRM)",{


  # define a new class inheriting from the general model class Model:

  .PowerNormal <- setClass(Class = "PowerNormal", contains = "Model",
                           representation(skeletonFun = "function",
                                          skeletonProbs = "numeric",
                                          mean = "numeric",
                                          variance = "numeric"))

  # create a convenient initialization function

  PowerNormal <- function(skeletonProbs, doseGrid, mean, variance)
  {
    skeletonFun <- approxfun(x = doseGrid, y = skeletonProbs, rule = 2)
    invSkeletonFun <- approxfun(x = skeletonProbs, y = doseGrid, rule = 1)

    .PowerNormal(
      skeletonFun = skeletonFun, skeletonProbs = skeletonProbs,
      mean = mean,
      variance=variance,
      datamodel = function(){
        for (i in 1:nObs)
        {
          y[i] ~ dbern(p[i])
          p[i] <- skeletonProbs[xLevel[i]]^exp(theta)
        }},
      datanames = c("nObs", "y", "xLevel"),
      prob = function(dose, theta){ skeletonFun(dose)^exp(theta) },
      dose = function(prob, theta){ invSkeletonFun(prob^(1 / exp(theta))) },
      priormodel = function(){ theta ~ dnorm(mean,1/variance) },
      modelspecs = function(){ list(skeletonProbs = skeletonProbs,
                                    mean = mean, variance=variance) },
      init = function(){ list(theta = 1) }, sample = "theta")
  }

  mcmcOptions <- McmcOptions(burnin = 50000, step = 2, samples = 1000000)


  ## One-paramter model
  ## (A) Posterior summaries (original skeleton)
  emptyData <- Data(doseGrid = c(1,2.5,5,10,15,20,25,30,
                                 40,50,75,100,150, 200, 250))
  data.obs <- Data(x= c(rep(1,3), rep(2.5,4), rep(5,5), rep(10,4), rep(25,2)),
                   y=c(rep(0,3), rep(0,4), rep(0,5), rep(0,4), rep(1,2)),
                   cohort=c(rep(1,3), rep(2,4), rep(3,5), rep(4,4), rep(7,2)),
                   doseGrid=c(1,2.5,5,10,15, 20,25, 30,40,50, 75,100,150, 200, 250),
                   ID = 1:18)

  model_power_A <- PowerNormal(skeletonProbs= c(0.01, 0.015, 0.020, 0.025, 0.03,
                                                0.04, 0.05, 0.10, 0.17, 0.30,
                                                0.45,0.70, 0.80, 0.90, 0.95),
                               doseGrid= c(1,2.5,5,10,15,20,25,30,40,50,75,
                                           100,150, 200, 250),
                               mean= 0,
                               variance=1.34^2)

  expect_warning(priorSamples <-  mcmc(data = emptyData,
                                       model = model_power_A,
                                       options = mcmcOptions))

  ## NCRM rule with loss function
  ncrm_loss <- NextBestNCRM_loss(target = c(0.2, 0.35),
                                 overdose = c(0.35, 0.6, 1),
                                 maxOverdoseProb = 0.25,
                                 losses <- c(1, 0, 1, 2))

  increments_no <- IncrementsRelative(intervals=250,
                                      increments= 2)

  postSamples_A <- mcmc(data.obs, model_power_A, mcmcOptions)

  doseRec_loss_A <- expect_warning(nextBest(ncrm_loss,
                                            doselimit = maxDose(increments_no, data.obs),
                                            postSamples_A, model_power_A, data.obs))



  ## (A) Actual table I
  pat.info <- rbind("No. of patients" = c(3,4,5,4,"-","-",2, "-", "-", "-"),
                    "No. of DLTs" = c(0,0,0,0,"-","-",2, "-", "-", "-"))
  colnames(pat.info) <- c(1,2.5,5,10,15,20,25,30,40,50)

  tab1A.act <- round(rbind("Skeleton (CRM)" = c(0.01, 0.015, 0.020, 0.025,
                                                0.03, 0.04, 0.05, 0.10, 0.17,
                                                0.30, 0.45, 0.70, 0.80, 0.90, 0.95)[1:10],
                           t(doseRec_loss_A[[4]])[c(7:8),1:10]),3)
  colnames(tab1A.act) <- colnames(doseRec_loss_A[[3]])[1:10]

  ## (B) Actual table I
  model_power_B <- PowerNormal(skeletonProbs=c(0.063, 0.125, 0.188, 0.250,
                                               0.313, 0.375, 0.438, 0.500, 0.563, 0.625),
                               doseGrid= c(1,2.5,5,10,15,20,25,30,40,50),
                               mean= 0,
                               variance=1.34^2)
  postSamples_B <- mcmc(data.obs, model_power_B, mcmcOptions)
  doseRec_loss_B <- expect_warning(nextBest(ncrm_loss,
                                            doselimit = maxDose(increments_no, data.obs),
                                            postSamples_B, model_power_B, data.obs))

  tab1B.act <- round(rbind("Skeleton (CRM)" = c(0.063, 0.125, 0.188, 0.250,
                                                0.313, 0.375, 0.438, 0.500, 0.563, 0.625),
                           t(doseRec_loss_B[[4]])[c(7:8),1:10]),3)
  colnames(tab1B.act) <- colnames(doseRec_loss_B[[3]])[1:10]

  ## (A)+ (B) Actual table I
  tab1.act<- list("Posterior summaries (original skeleton)" =tab1A.act,
                  "Posterior summaries (equidistant skeleton)"=tab1B.act)


  ## Expected table I (Neuenschwander et al.)
  tab1.exp <- list()
  tab1.names <- list(c("Skeleton (CRM)", "Mean", "Std. dev."),
                     c(1,2.5,5,10,15,20,25,30,40,50))

  tab1 <- read.table("testdata/NeuenschwanderTable1.txt", sep="\t", header=TRUE)
  #tab1.exp$"Patient info" <- pat.info
  tab1.exp$"Posterior summaries (original skeleton)" <- apply(as.matrix(tab1[4:6,-1]), 2, as.numeric)
  tab1.exp$"Posterior summaries (equidistant skeleton)" <- apply(as.matrix(tab1[8:10,-1] ), 2, as.numeric)
  colnames(tab1.exp[[1]]) <- colnames(tab1.exp[[2]]) <- tab1.names[[2]]
  rownames(tab1.exp[[1]]) <- rownames(tab1.exp[[2]]) <- tab1.names[[1]]



  ## test Posterior summaries for probabilities of DLT (CRM)

  expect_equal(tab1.act$"Posterior summaries (original skeleton)",
               tab1.exp$"Posterior summaries (original skeleton)",
               tolerance=.01, scale=1,check.attributes=FALSE)
  expect_equal(tab1.act$"Posterior summaries (equidistant skeleton)",
               tab1.exp$"Posterior summaries (equidistant skeleton)",
               tolerance=.01, scale=1,check.attributes=FALSE)

})
