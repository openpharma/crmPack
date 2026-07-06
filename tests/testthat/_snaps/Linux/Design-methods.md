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

