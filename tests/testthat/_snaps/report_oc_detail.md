# detail renderers preserve grouping, pairing and stored formatting

    Code
      cat(paste(table, collapse = "\n"))
    Output
      Table: Per-dose results
      
      |Scenario   |Statistic          |     1|     2|     4| SUM| STOP|  MAX|
      |:----------|:------------------|-----:|-----:|-----:|---:|----:|----:|
      |Scenario 1 |Patients with DLTs |   0.3|   0.7|   0.0| 1.0|    —|    —|
      |Scenario 1 |Patients at dose   |   0.7|   1.0|   0.0| 1.7|    —|    —|
      |Scenario 1 |Dose selected:     |  33.3|  33.3|   0.0|   —| 66.7| 66.7|
      |Scenario 1 |True P(DLT):       | 0.100| 0.200| 0.400|   —|    —|    —|
      |Scenario 2 |Patients with DLTs |   0.3|   0.7|   0.0| 1.0|    —|    —|
      |Scenario 2 |Patients at dose   |   0.7|   1.0|   0.0| 1.7|    —|    —|
      |Scenario 2 |Dose selected:     |   0.0|   0.0| 100.0|   —| 66.7| 66.7|
      |Scenario 2 |True P(DLT):       | 0.100| 0.200| 0.400|   —|    —|    —|

