# simulate produces consistent results with placebo data

    Code
      result
    Output
      An object of class 'Simulations' containing 1 simulated trials.
      Please use 'summary()' to obtain more information.

# simulate produces consistent results with sentinel patients

    Code
      result
    Output
      An object of class 'Simulations' containing 1 simulated trials.
      Please use 'summary()' to obtain more information.

# Test if simulate generate the expected output.

    Code
      sim
    Output
      An object of class 'Simulations' containing 1 simulated trials.
      Please use 'summary()' to obtain more information.

# Backfilling works in a simple design

    Code
      result
    Output
      An object of class 'Simulations' containing 10 simulated trials.
      Please use 'summary()' to obtain more information.

# NextBestInfTheory produces consistent results for empty data

    Code
      result@mean_fit
    Output
      $truth
       [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
      
      $average
       [1] 0.9856020 0.9906587 0.9934492 0.9951504 0.9962639 0.9970324 0.9975852
       [8] 0.9979962 0.9983101 0.9985553 0.9987506 0.9989086 0.9990382 0.9991460
      [15] 0.9992365 0.9993133 0.9993790
      
      $lower
       [1] 0.9856020 0.9906587 0.9934492 0.9951504 0.9962639 0.9970324 0.9975852
       [8] 0.9979962 0.9983101 0.9985553 0.9987506 0.9989086 0.9990382 0.9991460
      [15] 0.9992365 0.9993133 0.9993790
      
      $upper
       [1] 0.9856020 0.9906587 0.9934492 0.9951504 0.9962639 0.9970324 0.9975852
       [8] 0.9979962 0.9983101 0.9985553 0.9987506 0.9989086 0.9990382 0.9991460
      [15] 0.9992365 0.9993133 0.9993790
      

# NextBestInfTheory produces consistent results with a dataset

    Code
      result@mean_fit
    Output
      $truth
       [1] 1 1 1 1 1 1 1 1 1 1 1 1
      
      $average
       [1] 0.1789688 0.2222795 0.2507550 0.2723040 0.2897450 0.3044386 0.3171541
       [8] 0.3283720 0.3384141 0.3475067 0.3558159 0.3634667
      
      $lower
       [1] 0.1789688 0.2222795 0.2507550 0.2723040 0.2897450 0.3044386 0.3171541
       [8] 0.3283720 0.3384141 0.3475067 0.3558159 0.3634667
      
      $upper
       [1] 0.1789688 0.2222795 0.2507550 0.2723040 0.2897450 0.3044386 0.3171541
       [8] 0.3283720 0.3384141 0.3475067 0.3558159 0.3634667
      

# simulate-RuleDesign produces consistent results

    Code
      result
    Output
      An object of class 'GeneralSimulations' containing 1 simulated trials.
      Please use 'summary()' to obtain more information.

---

    Code
      result
    Output
         dose DLTs nextDose  stop increment
      1     1    0        3 FALSE       200
      2     1    1        1 FALSE         0
      3     1    2       NA  TRUE        NA
      4     1    3       NA  TRUE        NA
      5     3    0        5 FALSE        67
      6     3    1        3 FALSE         0
      7     3    2        1 FALSE       -67
      8     3    3        1 FALSE       -67
      9     5    0       10 FALSE       100
      10    5    1        5 FALSE         0
      11    5    2        3 FALSE       -40
      12    5    3        3 FALSE       -40
      13   10    0       15 FALSE        50
      14   10    1       10 FALSE         0
      15   10    2        5 FALSE       -50
      16   10    3        5 FALSE       -50
      17   15    0       20 FALSE        33
      18   15    1       15 FALSE         0
      19   15    2       10 FALSE       -33
      20   15    3       10 FALSE       -33
      21   20    0       25 FALSE        25
      22   20    1       20 FALSE         0
      23   20    2       15 FALSE       -25
      24   20    3       15 FALSE       -25
      25   25    0       40 FALSE        60
      26   25    1       25 FALSE         0
      27   25    2       20 FALSE       -20
      28   25    3       20 FALSE       -20
      29   40    0       50 FALSE        25
      30   40    1       40 FALSE         0
      31   40    2       25 FALSE       -38
      32   40    3       25 FALSE       -38
      33   50    0       80 FALSE        60
      34   50    1       50 FALSE         0
      35   50    2       40 FALSE       -20
      36   50    3       40 FALSE       -20
      37   80    0      100 FALSE        25
      38   80    1       80 FALSE         0
      39   80    2       50 FALSE       -38
      40   80    3       50 FALSE       -38

# simulate-DualDesign produces consistent results

    Code
      result
    Output
      An object of class 'DualSimulations' containing 1 simulated trials.
      Please use 'summary()' to obtain more information.

---

    Code
      result
    Output
      An object of class 'DualSimulations' containing 1 simulated trials.
      Please use 'summary()' to obtain more information.

---

    Code
      result
    Output
      An object of class 'DualSimulations' containing 1 simulated trials.
      Please use 'summary()' to obtain more information.

# simulate-TDSamplesDesign produces consistent results

    Code
      result
    Output
      An object of class 'PseudoSimulations' containing 1 simulated trials.
      Please use 'summary()' to obtain more information.

# simulate-TDSamplesDesign produces consistent results with placebo patients

    Code
      result
    Output
      An object of class 'PseudoSimulations' containing 1 simulated trials.
      Please use 'summary()' to obtain more information.

# simulate-TDDesign produces consistent results

    Code
      result
    Output
      An object of class 'PseudoSimulations' containing 1 simulated trials.
      Please use 'summary()' to obtain more information.

# simulate-TDDesign with sentinel patient and placebo patients produces consistent results

    Code
      result
    Output
      An object of class 'PseudoSimulations' containing 1 simulated trials.
      Please use 'summary()' to obtain more information.

# simulate-DualResponsesDesign produces consistent results

    Code
      result
    Output
      An object of class 'PseudoDualSimulations' containing 1 simulated trials.
      Please use 'summary()' to obtain more information.

# simulate-DualResponsesDesign with sentinel patient and placebo patients produces consistent results

    Code
      result
    Output
      An object of class 'PseudoDualSimulations' containing 1 simulated trials.
      Please use 'summary()' to obtain more information.

# simulate-DualResponsesSamplesDesign produces consistent results

    Code
      result
    Output
      An object of class 'PseudoDualSimulations' containing 1 simulated trials.
      Please use 'summary()' to obtain more information.

# simulate-DualResponsesSamplesDesign with sentinel patient and placebo dose produces consistent results

    Code
      result
    Output
      An object of class 'PseudoDualSimulations' containing 1 simulated trials.
      Please use 'summary()' to obtain more information.

# simulate-DualResponsesSamplesDesign with EffFlexi model produces consistent results

    Code
      result
    Output
      An object of class 'PseudoDualFlexiSimulations' containing 1 simulated trials.
      Please use 'summary()' to obtain more information.

# simulate for DADesign works consistently

    Code
      mySims
    Output
      An object of class 'DASimulations' containing 2 simulated trials.
      Please use 'summary()' to obtain more information.

# simulate for DADesign with placebo and deescalation works consistently

    Code
      mySims
    Output
      An object of class 'DASimulations' containing 2 simulated trials.
      Please use 'summary()' to obtain more information.

# examine produces consistent results

    Code
      result
    Output
         dose DLTs nextDose  stop increment
      1     3    0      1.0 FALSE       -67
      2     3    1      3.0 FALSE         0
      3     3    2      1.0 FALSE       -67
      4     3    3       NA  TRUE        NA
      5     1    0      1.0 FALSE         0
      6     1    1      1.0 FALSE         0
      7     1    2      1.0 FALSE         0
      8     1    3      1.0 FALSE         0
      9     1    0      1.0 FALSE         0
      10    1    1      1.0 FALSE         0
      11    1    2      1.0 FALSE         0
      12    1    3      0.1  TRUE       -90
      13    1    0      1.0 FALSE         0
      14    1    1      1.0 FALSE         0
      15    1    2      1.0  TRUE         0
      16    1    3      1.0 FALSE         0
      17    1    0      1.0 FALSE         0
      18    1    1      1.0 FALSE         0
      19    1    2      1.0 FALSE         0
      20    1    3      1.0 FALSE         0
      21    1    0      1.0 FALSE         0
      22    1    1      1.0 FALSE         0
      23    1    2      1.0 FALSE         0
      24    1    3      1.0 FALSE         0
      25    1    0      1.0  TRUE         0
      26    1    1      1.0  TRUE         0
      27    1    2      1.0  TRUE         0
      28    1    3      1.0  TRUE         0

# examine produces consistent results with placebo data

    Code
      result
    Output
         dose DLTs nextDose  stop increment
      1     3    0        1 FALSE       -67
      2     3    1        3 FALSE         0
      3     3    2        1 FALSE       -67
      4     3    3       NA  TRUE        NA
      5     1    0        1 FALSE         0
      6     1    1        1 FALSE         0
      7     1    2        1 FALSE         0
      8     1    3        1 FALSE         0
      9     1    0        1 FALSE         0
      10    1    1        1 FALSE         0
      11    1    2        1  TRUE         0
      12    1    3        1 FALSE         0
      13    1    0        1 FALSE         0
      14    1    1        1 FALSE         0
      15    1    2        1  TRUE         0
      16    1    3        1 FALSE         0
      17    1    0        1  TRUE         0
      18    1    1        1  TRUE         0
      19    1    2        1  TRUE         0
      20    1    3        1  TRUE         0

# tidy-DualDesign works correctly

    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

