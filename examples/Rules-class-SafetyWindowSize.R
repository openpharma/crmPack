# As example, here is the rule for: 
#   having patientGap as (7,3,3,3,...) for cohort size <4
#   and having patientGap as (9,5,5,5...) for cohort size >=4

myWindowLength <- SafetyWindowSize(patientGap = list(c(7,3),c(9,5)),
                                   sizeIntervals = c(1,4),
                                   patientFollow = 7,
                                   patientFollowMin = 14)
