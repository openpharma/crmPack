# StoppingMTDCV-method ----

#Sample data to test Stopping Rule MTD precisely estimated: CV(MTD) <= 30% 
data1 <- Data(
  x=c(1.5, 2.5, 3.5, 4.5, 6, 6, 6),
  y=c(  0,   0,   0,   0, 0, 0, 1),
  cohort=c(  1,   2,   3,   4, 5, 5, 5),
  doseGrid=c(1.5, 2.5, 3.5, 4.5, 6, 7),
  ID=1:7)

#model that leads to a CV of 25% given the data and the options
model1 <- LogisticKadane(0.3, xmin = 1.5, xmax = 7)

options1 <- McmcOptions(burnin = 1000, step = 1, samples = 10000)

set.seed(94)

samples1 <- mcmc(data1, model1, options1)

test_that("StoppingMTDCV works correctly if CV is below threshold", {
  stopping <- StoppingMTDCV(target=0.3, thresh=30) 
  dose <- 7
  samples <- samples1
  model <- model1
  data <- data1
  result <- stopTrial(
    stopping = stopping,
    dose = dose,
    samples = samples,
    model = model,
    data = data
  )
  #expect_valid(result,"stopTrial") ????
  expect_equal(result, TRUE)  # CV is 25% <= 30%.
})

test_that("StoppingMTDCV works correctly if CV is above threshold", {
  stopping <- StoppingMTDCV(target=0.3, thresh=20) 
  dose <- 7
  samples <- samples1
  model <- model1
  data <- data1
  result <- stopTrial(
    stopping = stopping,
    dose = dose,
    samples = samples,
    model = model,
    data = data
  )
  #expect_valid(result,"stopTrial") ????
  expect_equal(result, FALSE)  # CV is 25% >= 20%.
})
