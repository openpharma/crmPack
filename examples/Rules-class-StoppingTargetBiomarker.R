# Stopping the study if there is at least 0.5 probability that the biomarker
# (efficacy) is within the biomarker target range of [0.9, 1.0] (relative to the
# maximum for the biomarker).

my_stopping <- StoppingTargetBiomarker(target = c(0.9, 1), prob = 0.5)
