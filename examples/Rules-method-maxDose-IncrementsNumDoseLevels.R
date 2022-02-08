# Create the data
data <- Data(x = c(0.1, 0.5, 1.5, 3, 6, 8, 8, 8, 12, 12, 12, 16, 16, 16, 10, 10, 10),
             y = c(0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 0, 1, 0),
             cohort = c(0, 1, 2, 3, 4, 5, 5, 5, 6, 6, 6, 7, 7, 7, 8, 8, 8),
             doseGrid =
               c(0.1, 0.5, 1.5, 3, 6, 8,
               seq(from = 10, to = 80, by = 2)
                )
            )


# In this first example we define a rule for dose increments which would allow:
# Maximum skip one dose level, that is 2 dose levels higher than the last dose
# given. Maximum increment is explicitly defined as:
myIncrements1 <- IncrementsNumDoseLevels(maxLevels = 2, basisLevel = "lastGiven")
# Since the default method is based on the last dose given, maximum increment
# can also be defined as:
myIncrements1 <- IncrementsNumDoseLevels(maxLevels = 2)

# Based on the rule above, we then calculate the maximum dose allowed
nextMaxDose1 <- maxDose(myIncrements1, data = data)

# In this second example we define a rule for dose increments which would allow:
# Maximum skip one dose level, that is 2 dose levels higher than the max dose
# given. Maximum increment is explicitly defined as:
myIncrements2 <- IncrementsNumDoseLevels(maxLevels = 2, basisLevel = "maxGiven")

# Based on the rule above, we then calculate the maximum dose allowed
nextMaxDose2 <- maxDose(myIncrements2, data = data)
