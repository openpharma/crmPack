
# Create the data
data <- Data(x=c(0.1, 0.5, 1.5, 3, 6, 8, 8, 8),
             y=c(0, 0, 0, 0, 0, 0, 1, 0),
             cohort=c(0, 1, 2, 3, 4, 5, 5, 5),
             doseGrid=
               c(0.1, 0.5, 1.5, 3, 6, 8,
                 seq(from=10, to=80, by=2)))


# In this example we define a rule for dose increments which would allow:
# maximum skip one dose level, that is 2 dose levels higher is maximum
# increment
myIncrements <- IncrementsNumDoseLevels(maxLevels=2)

# Based on the rule above, we then calculate the maximum dose allowed
nextMaxDose <- maxDose(myIncrements,
                       data=data)

