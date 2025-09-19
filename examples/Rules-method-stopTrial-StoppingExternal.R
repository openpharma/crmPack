my_rule <- StoppingExternal(report_label = "Based on combo stop")

stopTrial(my_rule, 5, .DefaultSamples(), .DefaultModelLogNormal(), .DefaultData(), external = TRUE)
