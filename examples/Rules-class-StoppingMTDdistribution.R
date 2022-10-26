
# As example, here is the rule for stopping the study if there is at least 0.9
# probability that MTD > 0.5 * next_dose. Here MTD is defined as the dose for
# which prob(DLT) = 0.33
my_stopping <- StoppingMTDdistribution(
  target = 0.33,
  thresh = 0.5,
  prob = 0.9
)
