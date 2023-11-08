# nextBest-NextBestNCRM returns expected values of the objects

    Code
      result$probs
    Output
            dose target overdose
       [1,]   25   0.00     0.00
       [2,]   50   0.00     0.00
       [3,]   75   0.50     0.25
       [4,]  100   0.25     0.50
       [5,]  125   0.25     0.75
       [6,]  150   0.00     1.00
       [7,]  175   0.00     1.00
       [8,]  200   0.00     1.00
       [9,]  225   0.00     1.00
      [10,]  250   0.00     1.00
      [11,]  275   0.00     1.00
      [12,]  300   0.00     1.00

# nextBest-NextBestNCRM returns expected values of the objects (no doselimit)

    Code
      result$probs
    Output
            dose target overdose
       [1,]   25   0.00     0.00
       [2,]   50   0.00     0.00
       [3,]   75   0.50     0.25
       [4,]  100   0.25     0.50
       [5,]  125   0.25     0.75
       [6,]  150   0.00     1.00
       [7,]  175   0.00     1.00
       [8,]  200   0.00     1.00
       [9,]  225   0.00     1.00
      [10,]  250   0.00     1.00
      [11,]  275   0.00     1.00
      [12,]  300   0.00     1.00

# nextBest-NextBestNCRM-DataParts returns expected values of the objects

    Code
      result$probs
    Output
            dose target overdose
       [1,]   25   0.00     0.00
       [2,]   50   0.00     0.00
       [3,]   75   0.50     0.25
       [4,]  100   0.25     0.50
       [5,]  125   0.25     0.75
       [6,]  150   0.00     1.00
       [7,]  175   0.00     1.00
       [8,]  200   0.00     1.00
       [9,]  225   0.00     1.00
      [10,]  250   0.00     1.00
      [11,]  275   0.00     1.00
      [12,]  300   0.00     1.00

# nextBest-NextBestNCRM-DataParts returns expected values of the objects (no doselimit)

    Code
      result$probs
    Output
            dose target overdose
       [1,]   25   0.00     0.00
       [2,]   50   0.00     0.00
       [3,]   75   0.50     0.25
       [4,]  100   0.25     0.50
       [5,]  125   0.25     0.75
       [6,]  150   0.00     1.00
       [7,]  175   0.00     1.00
       [8,]  200   0.00     1.00
       [9,]  225   0.00     1.00
      [10,]  250   0.00     1.00
      [11,]  275   0.00     1.00
      [12,]  300   0.00     1.00

# nextBest-NextBestNCRMLoss returns expected values of the objects

    Code
      result$probs
    Output
          dose underdosing target overdose       mean    std_dev posterior_loss
      25    25        1.00   0.00     0.00 0.02399772 0.02468099           1.00
      50    50        1.00   0.00     0.00 0.10786611 0.06384384           1.00
      75    75        0.25   0.50     0.25 0.28084217 0.16237037           0.75
      100  100        0.25   0.25     0.50 0.45071815 0.25848085           1.25
      125  125        0.00   0.25     0.75 0.56989233 0.26271358           1.50
      150  150        0.00   0.00     1.00 0.65868827 0.22931946           2.00
      175  175        0.00   0.00     1.00 0.72753310 0.19117925           2.00
      200  200        0.00   0.00     1.00 0.78065042 0.15910226           2.00
      225  225        0.00   0.00     1.00 0.82128990 0.13465035           2.00
      250  250        0.00   0.00     1.00 0.85237854 0.11638492           2.00
      275  275        0.00   0.00     1.00 0.87633603 0.10248763           2.00
      300  300        0.00   0.00     1.00 0.89501585 0.09155455           2.00

# nextBest-NextBestNCRMLoss returns expected values of the objects (loss function of 4 elements)

    Code
      result$probs
    Output
          dose underdosing target excessive unacceptable       mean    std_dev
      25    25        1.00   0.00      0.00         0.00 0.02399772 0.02468099
      50    50        1.00   0.00      0.00         0.00 0.10786611 0.06384384
      75    75        0.25   0.50      0.25         0.00 0.28084217 0.16237037
      100  100        0.25   0.25      0.25         0.25 0.45071815 0.25848085
      125  125        0.00   0.25      0.25         0.50 0.56989233 0.26271358
      150  150        0.00   0.00      0.50         0.50 0.65868827 0.22931946
      175  175        0.00   0.00      0.50         0.50 0.72753310 0.19117925
      200  200        0.00   0.00      0.00         1.00 0.78065042 0.15910226
      225  225        0.00   0.00      0.00         1.00 0.82128990 0.13465035
      250  250        0.00   0.00      0.00         1.00 0.85237854 0.11638492
      275  275        0.00   0.00      0.00         1.00 0.87633603 0.10248763
      300  300        0.00   0.00      0.00         1.00 0.89501585 0.09155455
          posterior_loss
      25            1.00
      50            1.00
      75            0.50
      100           1.00
      125           1.25
      150           1.50
      175           1.50
      200           2.00
      225           2.00
      250           2.00
      275           2.00
      300           2.00

# nextBest-NextBestNCRMLoss returns expected values of the objects (no doselimit)

    Code
      result$probs
    Output
          dose underdosing target overdose       mean    std_dev posterior_loss
      25    25        1.00   0.00     0.00 0.02399772 0.02468099           1.00
      50    50        1.00   0.00     0.00 0.10786611 0.06384384           1.00
      75    75        0.25   0.50     0.25 0.28084217 0.16237037           0.75
      100  100        0.25   0.25     0.50 0.45071815 0.25848085           1.25
      125  125        0.00   0.25     0.75 0.56989233 0.26271358           1.50
      150  150        0.00   0.00     1.00 0.65868827 0.22931946           2.00
      175  175        0.00   0.00     1.00 0.72753310 0.19117925           2.00
      200  200        0.00   0.00     1.00 0.78065042 0.15910226           2.00
      225  225        0.00   0.00     1.00 0.82128990 0.13465035           2.00
      250  250        0.00   0.00     1.00 0.85237854 0.11638492           2.00
      275  275        0.00   0.00     1.00 0.87633603 0.10248763           2.00
      300  300        0.00   0.00     1.00 0.89501585 0.09155455           2.00

# nextBest-NextBestDualEndpoint returns expected elements

    Code
      result$probs
    Output
            dose target overdose
       [1,]   25   0.50      0.0
       [2,]   50   0.00      0.0
       [3,]   75   0.00      0.0
       [4,]  100   0.00      0.5
       [5,]  125   0.25      1.0
       [6,]  150   0.00      1.0
       [7,]  175   0.25      1.0
       [8,]  200   0.00      1.0
       [9,]  225   0.00      1.0
      [10,]  250   0.00      1.0
      [11,]  275   0.00      1.0
      [12,]  300   0.00      1.0

# nextBest-NextBestDualEndpoint returns expected elements (with Emax param)

    Code
      result$probs
    Output
            dose target overdose
       [1,]   25      0     0.00
       [2,]   50      0     0.00
       [3,]   75      0     0.25
       [4,]  100      0     0.75
       [5,]  125      0     1.00
       [6,]  150      0     1.00
       [7,]  175      0     1.00
       [8,]  200      0     1.00
       [9,]  225      0     1.00
      [10,]  250      0     1.00
      [11,]  275      0     1.00
      [12,]  300      0     1.00

# nextBest-NextBestDualEndpoint returns expected elements (absolute target)

    Code
      result$probs
    Output
            dose target overdose
       [1,]   25      0     0.00
       [2,]   50      0     0.00
       [3,]   75      0     0.00
       [4,]  100      0     0.25
       [5,]  125      0     1.00
       [6,]  150      0     1.00
       [7,]  175      0     1.00
       [8,]  200      0     1.00
       [9,]  225      0     1.00
      [10,]  250      0     1.00
      [11,]  275      0     1.00
      [12,]  300      0     1.00

# nextBest-NextBestDualEndpoint returns expected elements (absolute target, no doselimit)

    Code
      result$probs
    Output
            dose target overdose
       [1,]   25      0     0.00
       [2,]   50      0     0.00
       [3,]   75      0     0.00
       [4,]  100      0     0.25
       [5,]  125      0     1.00
       [6,]  150      0     1.00
       [7,]  175      0     1.00
       [8,]  200      0     1.00
       [9,]  225      0     1.00
      [10,]  250      0     1.00
      [11,]  275      0     1.00
      [12,]  300      0     1.00

# nextBest-NextBestMinDist returns expected values and plot

    Code
      result$probs
    Output
            dose   dlt_prob
       [1,]   25 0.02399772
       [2,]   50 0.10786611
       [3,]   75 0.28084217
       [4,]  100 0.45071815
       [5,]  125 0.56989233
       [6,]  150 0.65868827
       [7,]  175 0.72753310
       [8,]  200 0.78065042
       [9,]  225 0.82128990
      [10,]  250 0.85237854
      [11,]  275 0.87633603
      [12,]  300 0.89501585

# nextBest-NextBestMinDist returns expected values and plot (with placebo)

    Code
      result$probs
    Output
               dose     dlt_prob
       [1,]   0.001 1.783989e-07
       [2,]  25.000 4.216923e-01
       [3,]  50.000 6.523699e-01
       [4,]  75.000 7.600665e-01
       [5,] 100.000 8.225213e-01
       [6,] 125.000 8.624275e-01
       [7,] 150.000 8.895790e-01
       [8,] 175.000 9.089476e-01
       [9,] 200.000 9.232921e-01
      [10,] 225.000 9.342444e-01
      [11,] 250.000 9.428199e-01
      [12,] 275.000 9.496774e-01
      [13,] 300.000 9.552602e-01

# nextBest-NextBestMinDist returns expected values and plot (no doselimit)

    Code
      result$probs
    Output
            dose   dlt_prob
       [1,]   25 0.02399772
       [2,]   50 0.10786611
       [3,]   75 0.28084217
       [4,]  100 0.45071815
       [5,]  125 0.56989233
       [6,]  150 0.65868827
       [7,]  175 0.72753310
       [8,]  200 0.78065042
       [9,]  225 0.82128990
      [10,]  250 0.85237854
      [11,]  275 0.87633603
      [12,]  300 0.89501585

# nextBest-NextBestProbMTDLTE returns correct next dose and plot

    Code
      result$allocation
    Output
          dose allocation
      25    25       0.00
      50    50       0.25
      75    75       0.25
      100  100       0.00
      125  125       0.50
      150  150       0.00
      175  175       0.00
      200  200       0.00
      225  225       0.00
      250  250       0.00
      275  275       0.00
      300  300       0.00

# nextBest-NextBestProbMTDLTE returns correct next dose and plot (with placebo)

    Code
      result$allocation
    Output
          dose allocation
      25    25       0.75
      50    50       0.00
      75    75       0.25
      100  100       0.00
      125  125       0.00
      150  150       0.00
      175  175       0.00
      200  200       0.00
      225  225       0.00
      250  250       0.00
      275  275       0.00
      300  300       0.00

# nextBest-NextBestProbMTDLTE returns correct next dose and plot (no doselimit)

    Code
      result$allocation
    Output
          dose allocation
      25    25       0.00
      50    50       0.25
      75    75       0.25
      100  100       0.00
      125  125       0.50
      150  150       0.00
      175  175       0.00
      200  200       0.00
      225  225       0.00
      250  250       0.00
      275  275       0.00
      300  300       0.00

# nextBest-NextBestProbMTDMinDist returns correct next dose and plot

    Code
      result$allocation
    Output
          dose allocation
      25    25       0.00
      50    50       0.00
      75    75       0.25
      100  100       0.25
      125  125       0.50
      150  150       0.00
      175  175       0.00
      200  200       0.00
      225  225       0.00
      250  250       0.00
      275  275       0.00
      300  300       0.00

# nextBest-NextBestProbMTDMinDist returns correct next dose and plot (with placebo)

    Code
      result$allocation
    Output
          dose allocation
      25    25       0.75
      50    50       0.25
      75    75       0.00
      100  100       0.00
      125  125       0.00
      150  150       0.00
      175  175       0.00
      200  200       0.00
      225  225       0.00
      250  250       0.00
      275  275       0.00
      300  300       0.00

# nextBest-NextBestProbMTDMinDist returns correct next dose and plot (no doselimit)

    Code
      result$allocation
    Output
          dose allocation
      25    25       0.25
      50    50       0.00
      75    75       0.25
      100  100       0.25
      125  125       0.25
      150  150       0.00
      175  175       0.00
      200  200       0.00
      225  225       0.00
      250  250       0.00
      275  275       0.00
      300  300       0.00

# tidy-IncrementsRelative works correctly

    structure(list(min = c(0, 20), max = c(20, Inf), increment = c(1, 
    0.33)), row.names = c(NA, -2L), class = c("tbl_IncrementsRelative", 
    "tbl_df", "tbl", "data.frame"))

---

    structure(list(min = c(0, 20), max = c(20, Inf), increment = c(1, 
    0.33)), row.names = c(NA, -2L), class = c("tbl_IncrementsRelative", 
    "tbl_df", "tbl", "data.frame"))

# tidy-CohortSizeDLT works correctly

    structure(list(min = c(0, 1), max = c(1, Inf), cohort_size = c(1L, 
    3L)), row.names = c(NA, -2L), class = c("tbl_CohortSizeDLT", 
    "tbl_df", "tbl", "data.frame"))

# tidy-CohortSizeMin works correctly

    structure(list(structure(list(min = c(0, 10), max = c(10, Inf
    ), cohort_size = c(1L, 3L)), row.names = c(NA, -2L), class = c("tbl_CohortSizeRange", 
    "tbl_df", "tbl", "data.frame")), structure(list(min = c(0, 1), 
        max = c(1, Inf), cohort_size = c(1L, 3L)), row.names = c(NA, 
    -2L), class = c("tbl_CohortSizeDLT", "tbl_df", "tbl", "data.frame"
    ))), class = c("tbl_CohortSizeMin", "tbl_CohortSizeMin", "list"
    ))

# tidy-CohortSizeMax works correctly

    structure(list(structure(list(min = c(0, 10), max = c(10, Inf
    ), cohort_size = c(1L, 3L)), row.names = c(NA, -2L), class = c("tbl_CohortSizeRange", 
    "tbl_df", "tbl", "data.frame")), structure(list(min = c(0, 1), 
        max = c(1, Inf), cohort_size = c(1L, 3L)), row.names = c(NA, 
    -2L), class = c("tbl_CohortSizeDLT", "tbl_df", "tbl", "data.frame"
    ))), class = c("tbl_CohortSizeMax", "tbl_CohortSizeMax", "list"
    ))

# tidy-CohortSizeRange works correctly

    structure(list(min = c(0, 30), max = c(30, Inf), cohort_size = c(1L, 
    3L)), row.names = c(NA, -2L), class = c("tbl_CohortSizeRange", 
    "tbl_df", "tbl", "data.frame"))

# tidy-CohortSizeParts works correctly

    WAoAAAACAAQDAQACAwAAAAMTAAAAAgAAAA0AAAACAAAAAQAAAAIAAAANAAAAAgAAAAEAAAAD
    AAAEAgAAAAEABAAJAAAABWNsYXNzAAAAEAAAAAQABAAJAAAAE3RibF9Db2hvcnRTaXplUGFy
    dHMABAAJAAAABnRibF9kZgAEAAkAAAADdGJsAAQACQAAAApkYXRhLmZyYW1lAAAEAgAAAAEA
    BAAJAAAACXJvdy5uYW1lcwAAAA0AAAACgAAAAP////4AAAQCAAAAAQAEAAkAAAAFbmFtZXMA
    AAAQAAAAAgAEAAkAAAAEcGFydAAEAAkAAAALY29ob3J0X3NpemUAAAD+

# tidy-IncrementsMin works correctly

    structure(list(structure(list(intervals = c(0L, 1L, 3L), increments = c(1, 
    0.33, 0.2)), class = c("tbl_IncrementsRelativeDLT", "tbl_df", 
    "tbl", "data.frame"), row.names = c(NA, -3L)), structure(list(
        min = c(0, 20), max = c(20, Inf), increment = c(1, 0.33)), row.names = c(NA, 
    -2L), class = c("tbl_IncrementsRelative", "tbl_df", "tbl", "data.frame"
    ))), class = c("tbl_IncrementsMin", "tbl_IncrementsMin", "list"
    ))

# tidy-IncrementsRelativeParts works correctly

    structure(list(dlt_start = structure(list(dlt_start = 0L), class = c("tbl_df", 
    "tbl", "data.frame"), row.names = c(NA, -1L)), clean_start = structure(list(
        clean_start = 1L), class = c("tbl_df", "tbl", "data.frame"
    ), row.names = c(NA, -1L)), intervals = structure(list(intervals = c(0, 
    2)), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, 
    -2L)), increments = structure(list(increments = c(2, 1)), class = c("tbl_df", 
    "tbl", "data.frame"), row.names = c(NA, -2L))), class = c("tbl_IncrementsRelativeParts", 
    "list"))

# tidy-NextBestNCRM works correctly

    structure(list(Range = c("Underdose", "Target", "Overdose"), 
        min = c(0, 0.2, 0.35), max = c(0.2, 0.35, 1), max_prob = c(NA, 
        NA, 0.25)), row.names = c(NA, -3L), class = c("tbl_NextBestNCRM", 
    "tbl_df", "tbl", "data.frame"))

# tidy-NextBestNCRMLoss works correctly

    structure(list(unacceptable = structure(list(unacceptable = c(0.6, 
    1)), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, 
    -2L)), losses = structure(list(losses = c(1, 0, 1, 2)), class = c("tbl_df", 
    "tbl", "data.frame"), row.names = c(NA, -4L)), target = structure(list(
        target = c(0.2, 0.35)), class = c("tbl_df", "tbl", "data.frame"
    ), row.names = c(NA, -2L)), overdose = structure(list(overdose = c(0.35, 
    0.6)), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, 
    -2L)), max_overdose_prob = structure(list(max_overdose_prob = 0.25), class = c("tbl_df", 
    "tbl", "data.frame"), row.names = c(NA, -1L))), class = c("tbl_NextBestNCRMLoss", 
    "list"))

