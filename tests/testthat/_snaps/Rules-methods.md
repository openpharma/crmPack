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

# nextBest-NextBestProbMTDLTE returns correct next dose and plot (with placebo and max option)

    Code
      result$allocation
    Output
          dose allocation
      25    25       0.50
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
      300  300       0.25

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

