
# As example, here we are combining 2 different increment rules. 

# The first rule is the following: 
#      maximum doubling the dose if no DLTs were observed at the current dose
#      or maximum increasing the dose by 1.33 if 1 or 2 DLTs were observed at the current dose
#      or maximum increasing the dose by 1.22 if 3 or more DLTs were observed

# The second rule is the following: 
#   maximum doubling the dose if the current dose is <20
#   OR only maximum increasing the dose by 1.33 if the current dose is >=20


myIncrements1 <- IncrementsRelativeDLT(DLTintervals = c(0, 1, 3),
                                      increments = c(1, 0.33, 0.2))

myIncrements2 <- IncrementsRelative(intervals=c(0, 20),
                                    increments=c(1, 0.33))

# Now we combine the 2 rules
combIncrement <- IncrementMin(IncrementsList=
                                list(myIncrements1,myIncrements2))


