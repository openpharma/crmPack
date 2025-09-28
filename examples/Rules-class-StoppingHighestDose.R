# The following stopping rule is met when:
# - next proposed dose is highest dose, and
# - there are already at least 3 patients on that dose, and
# - we are sure that this dose is safe, e.g. the probability to be in (0%, 20%)
# interval of the DLT rate is above 50%.
my_stopping <- StoppingHighestDose() &
  StoppingPatientsNearDose(nPatients = 3, percentage = 0) &
  StoppingTargetProb(target = c(0, 0.2), prob = 0.5)

# We note that this rule would then need to be combined with the other standard
# stopping rules, when the MTD is found based on being near e.g. a 30% DLT
# probability or having reached maximal sample size, in the manner of:
# stop_rule <- stop_high | stop_low | stop_sample_size # nolintr.
