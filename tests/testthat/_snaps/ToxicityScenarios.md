# toxicity markdown table output is stable

    Code
      cat(paste(report_table(toxicity_test_scenarios(), digits = 2), collapse = "\n"))
    Output
      |Scenario | Dose 5| Dose 10| Dose 20|
      |:--------|------:|-------:|-------:|
      |Z low    |   0.05|    0.10|    0.20|
      |A high   |   0.10|    0.20|    0.40|

