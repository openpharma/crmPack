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
    BAIAAAP/AAAAEAAAAAEABAAJAAAADXRhcmdldF90aHJlc2gAAAD+AAAEAgAAA/8AAAAQAAAA
    BQAEAAkAAAAGdGFyZ2V0AAQACQAAAAhvdmVyZG9zZQAEAAkAAAARbWF4X292ZXJkb3NlX3By
    b2IABAAJAAAAD3RhcmdldF9yZWxhdGl2ZQAEAAkAAAANdGFyZ2V0X3RocmVzaAAABAIAAAH/
    AAAAEAAAAAIABAAJAAAAGHRibF9OZXh0QmVzdER1YWxFbmRwb2ludAAEAAkAAAAEbGlzdAAA
    AP4AAAMTAAAAAgAAAxMAAAADAAAADgAAAAIAAAAAAAAAAEA+AAAAAAAAAAAADgAAAAJAPgAA
    AAAAAH/wAAAAAAAAAAAADQAAAAIAAAABAAAAAwAABAIAAAP/AAAAEAAAAAMABAAJAAAAA21p
    bgAEAAkAAAADbWF4AAQACQAAAAtjb2hvcnRfc2l6ZQAABAIAAAL/AAAADQAAAAKAAAAA////
    /gAABAIAAAH/AAAAEAAAAAQABAAJAAAAE3RibF9Db2hvcnRTaXplUmFuZ2UABAAJAAAABnRi
    bF9kZgAEAAkAAAADdGJsAAQACQAAAApkYXRhLmZyYW1lAAAA/gAAAxMAAAADAAAADgAAAAIA
    AAAAAAAAAD/wAAAAAAAAAAAADgAAAAI/8AAAAAAAAH/wAAAAAAAAAAAADQAAAAIAAAABAAAA
    AwAABAIAAAP/AAAAEAAAAAMABAAJAAAAA21pbgAEAAkAAAADbWF4AAQACQAAAAtjb2hvcnRf
    c2l6ZQAABAIAAAL/AAAADQAAAAKAAAAA/////gAABAIAAAH/AAAAEAAAAAQABAAJAAAAEXRi
    bF9Db2hvcnRTaXplRExUAAQACQAAAAZ0YmxfZGYABAAJAAAAA3RibAAEAAkAAAAKZGF0YS5m
    cmFtZQAAAP4AAAQCAAAB/wAAABAAAAADAAQACQAAABF0YmxfQ29ob3J0U2l6ZU1heAAEAAkA
    AAARdGJsX0NvaG9ydFNpemVNYXgABAAJAAAABGxpc3QAAAD+AAADEwAAAAEAAAAOAAAAAUAI
    AAAAAAAAAAAEAgAAAf8AAAAQAAAAAwAEAAkAAAAGdGJsX2RmAAQACQAAAAN0YmwABAAJAAAA
    CmRhdGEuZnJhbWUAAAQCAAAC/wAAAA0AAAACgAAAAP////8AAAQCAAAD/wAAABAAAAABAAQA
    CQAAAAxzdGFydGluZ0Rvc2UAAAD+AAAEAgAAA/8AAAAQAAAACQAEAAkAAAAFbW9kZWwABAAJ
    AAAABGRhdGEABAAJAAAACHN0b3BwaW5nAAQACQAAAAppbmNyZW1lbnRzAAQACQAAAA5wbF9j
    b2hvcnRfc2l6ZQAEAAkAAAAIYmFja2ZpbGwABAAJAAAACG5leHRCZXN0AAQACQAAAAtjb2hv
    cnRfc2l6ZQAEAAkAAAAMc3RhcnRpbmdEb3NlAAAEAgAAAf8AAAAQAAAAAgAEAAkAAAAOdGJs
    X0R1YWxEZXNpZ24ABAAJAAAABGxpc3QAAAD+

