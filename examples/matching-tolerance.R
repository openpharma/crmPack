myDose <- c(rep(0.030, 6), rep(0.050, 3), rep(0.075, 4), rep(0.1, 9), rep(0.15, 7))
doseGrid <- seq(from = .025, to = .15, by = .005)

myDose %in% doseGrid
matchTolerance(myDose, doseGrid)
myDose %~% doseGrid

matchTolerance(c(myDose, 500), doseGrid)
c(myDose, 500) %~% doseGrid
