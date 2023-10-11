# simulate produces consistent results with placebo data

    Code
      result
    Output
      An object of class "Simulations"
      Slot "fit":
      [[1]]
              middle       lower       upper
      1  0.002380255 0.000638856 0.007082031
      2  0.014222189 0.008233734 0.030391015
      3  0.036277884 0.027439857 0.060140558
      4  0.056826691 0.047465309 0.082102421
      5  0.104528948 0.097325308 0.123978777
      6  0.147987480 0.144820674 0.156537858
      7  0.187713911 0.183810054 0.189159784
      8  0.224084458 0.207488656 0.230231052
      9  0.316307005 0.264631442 0.335446102
      10 0.366443100 0.295038058 0.392889412
      11 0.479544075 0.364837154 0.522028120
      12 0.533826996 0.400048833 0.583374464
      
      
      Slot "stop_report":
           <NA>  <NA> <NA>  <NA> <NA>
      [1,] TRUE FALSE TRUE FALSE TRUE
      
      Slot "stop_reasons":
      [[1]]
      [[1]][[1]]
      [[1]][[1]][[1]]
      [1] "Number of cohorts is 5 and thus reached the prespecified minimum number 3"
      
      [[1]][[1]][[2]]
      [1] "Probability for target toxicity is 0 % for dose 1 and thus below the required 50 %"
      
      
      [[1]][[2]]
      [1] "Number of patients is 20 and thus reached the prespecified minimum number 20"
      
      
      
      Slot "data":
      [[1]]
      An object of class "Data"
      Slot "x":
       [1] 0.1 3.0 3.0 3.0 0.1 1.0 1.0 1.0 0.1 1.0 1.0 1.0 0.1 1.0 1.0 1.0 0.1 1.0 1.0
      [20] 1.0
      
      Slot "y":
       [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      
      Slot "doseGrid":
       [1]   0.1   1.0   3.0   5.0  10.0  15.0  20.0  25.0  40.0  50.0  80.0 100.0
      
      Slot "nGrid":
      [1] 12
      
      Slot "xLevel":
       [1] 1 3 3 3 1 2 2 2 1 2 2 2 1 2 2 2 1 2 2 2
      
      Slot "placebo":
      [1] TRUE
      
      Slot "ID":
       [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
      
      Slot "cohort":
       [1] 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5
      
      Slot "nObs":
      [1] 20
      
      
      
      Slot "doses":
      [1] 1
      
      Slot "seed":
      [1] 819
      

# simulate produces consistent results with sentinel patients

    Code
      result
    Output
      An object of class "Simulations"
      Slot "fit":
      [[1]]
              middle        lower       upper
      1  0.002395946 0.0006466696 0.007118992
      2  0.014321117 0.0083103333 0.030550234
      3  0.036517541 0.0276538514 0.060449502
      4  0.057182146 0.0477992422 0.082515987
      5  0.105108928 0.0978987592 0.124576383
      6  0.148728222 0.1455668979 0.157263798
      7  0.188570939 0.1846333002 0.190029324
      8  0.225025224 0.2083888362 0.231186850
      9  0.317375596 0.2656888966 0.336518818
      10 0.367536873 0.2961634016 0.393971492
      11 0.480600330 0.3660795717 0.523015426
      12 0.534826616 0.4013310812 0.584269407
      
      
      Slot "stop_report":
           <NA>  <NA> <NA>  <NA> <NA>
      [1,] TRUE FALSE TRUE FALSE TRUE
      
      Slot "stop_reasons":
      [[1]]
      [[1]][[1]]
      [[1]][[1]][[1]]
      [1] "Number of cohorts is 7 and thus reached the prespecified minimum number 3"
      
      [[1]][[1]][[2]]
      [1] "Probability for target toxicity is 0 % for dose 1 and thus below the required 50 %"
      
      
      [[1]][[2]]
      [1] "Number of patients is 21 and thus reached the prespecified minimum number 20"
      
      
      
      Slot "data":
      [[1]]
      An object of class "Data"
      Slot "x":
       [1] 3 3 3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
      
      Slot "y":
       [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      
      Slot "doseGrid":
       [1]   0.1   1.0   3.0   5.0  10.0  15.0  20.0  25.0  40.0  50.0  80.0 100.0
      
      Slot "nGrid":
      [1] 12
      
      Slot "xLevel":
       [1] 3 3 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
      
      Slot "placebo":
      [1] FALSE
      
      Slot "ID":
       [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
      
      Slot "cohort":
       [1] 1 1 1 2 2 2 3 3 3 4 4 4 5 5 5 6 6 6 7 7 7
      
      Slot "nObs":
      [1] 21
      
      
      
      Slot "doses":
      [1] 1
      
      Slot "seed":
      [1] 819
      

# simulate-RuleDesign produces consistent results

    Code
      result
    Output
      An object of class "GeneralSimulations"
      Slot "data":
      [[1]]
      An object of class "Data"
      Slot "x":
       [1]  1  1  1  3  3  3  5  5  5 10 10 10 15 15 15 20 20 20 25 25 25 25 25 25 40
      [26] 40 40 50 50 50 80 80 80 50 50 50
      
      Slot "y":
       [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 1 0 0 0
      
      Slot "doseGrid":
       [1]   1   3   5  10  15  20  25  40  50  80 100
      
      Slot "nGrid":
      [1] 11
      
      Slot "xLevel":
       [1]  1  1  1  2  2  2  3  3  3  4  4  4  5  5  5  6  6  6  7  7  7  7  7  7  8
      [26]  8  8  9  9  9 10 10 10  9  9  9
      
      Slot "placebo":
      [1] FALSE
      
      Slot "ID":
       [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
      [26] 26 27 28 29 30 31 32 33 34 35 36
      
      Slot "cohort":
       [1]  1  1  1  2  2  2  3  3  3  4  4  4  5  5  5  6  6  6  7  7  7  8  8  8  9
      [26]  9  9 10 10 10 11 11 11 12 12 12
      
      Slot "nObs":
      [1] 36
      
      
      
      Slot "doses":
      [1] 50
      
      Slot "seed":
      [1] 819
      

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
      An object of class "DualSimulations"
      Slot "rho_est":
      [1] 0.07991541
      
      Slot "sigma2w_est":
      [1] 0.03177778
      
      Slot "fit_biomarker":
      [[1]]
         middleBiomarker lowerBiomarker upperBiomarker
      1        0.2434966     0.13294431      0.3701356
      2        0.2325369     0.07576239      0.3763400
      3        0.2284404    -0.07077652      0.5703187
      4        0.2522588    -0.36780215      0.7826113
      5        0.2364434    -0.49574988      0.9677055
      6        0.2198298    -0.58828157      0.9583787
      7        0.2185295    -0.71740079      1.1194549
      8        0.2034642    -0.98358012      1.3289780
      9        0.2058875    -1.12465077      1.4500549
      10       0.1976203    -1.48546712      1.7065673
      11       0.1905852    -1.80896626      2.0767703
      
      
      Slot "fit":
      [[1]]
             middle        lower      upper
      1  0.01093716 0.0009962211 0.04095732
      2  0.22473302 0.0526930358 0.68081948
      3  0.43571994 0.0935359919 0.97409485
      4  0.67981427 0.1808020000 0.99996023
      5  0.77864284 0.2500773718 0.99999984
      6  0.82773116 0.3066616729 1.00000000
      7  0.85641581 0.3540839546 1.00000000
      8  0.89924516 0.4608241419 1.00000000
      9  0.91428153 0.5130270841 1.00000000
      10 0.93939515 0.6211946840 1.00000000
      11 0.94903699 0.6699001621 1.00000000
      
      
      Slot "stop_report":
           [,1]
      [1,] TRUE
      
      Slot "stop_reasons":
      [[1]]
      [[1]][[1]]
      [1] "Probability for target biomarker is 21 % for dose 1 and thus below the required 50 %"
      
      [[1]][[2]]
      [1] "Number of patients is 12 and thus reached the prespecified minimum number 10"
      
      
      
      Slot "data":
      [[1]]
      An object of class "DataDual"
      Slot "w":
       [1] 0.2557299 0.1150998 0.3181927 0.2531184 0.1632822 0.3616207 0.2672235
       [8] 0.1000139 0.1305151 0.2393188 0.3006751 0.2951640
      
      Slot "x":
       [1] 3 3 3 1 1 1 1 1 1 1 1 1
      
      Slot "y":
       [1] 0 0 0 0 0 0 0 0 0 0 0 0
      
      Slot "doseGrid":
       [1]   1   3   5  10  15  20  25  40  50  80 100
      
      Slot "nGrid":
      [1] 11
      
      Slot "xLevel":
       [1] 2 2 2 1 1 1 1 1 1 1 1 1
      
      Slot "placebo":
      [1] FALSE
      
      Slot "ID":
       [1]  1  2  3  4  5  6  7  8  9 10 11 12
      
      Slot "cohort":
       [1] 1 1 1 2 2 2 3 3 3 4 4 4
      
      Slot "nObs":
      [1] 12
      
      
      
      Slot "doses":
      [1] 1
      
      Slot "seed":
      [1] 3
      

# simulate-TDSamplesDesign produces consistent results

    Code
      result
    Output
      An object of class "PseudoSimulations"
      Slot "fit":
      [[1]]
             middle      lower     upper
      1  0.08583626 0.02314306 0.1615121
      2  0.13700263 0.05461354 0.2354364
      3  0.17983349 0.08156022 0.2883433
      4  0.21717502 0.10142184 0.3323753
      5  0.25027602 0.12585805 0.3964506
      6  0.27989888 0.14829847 0.4402591
      7  0.30659033 0.15645905 0.4676890
      8  0.33077494 0.16457914 0.5086352
      9  0.35279588 0.16964809 0.5446742
      10 0.37293640 0.17428698 0.5765315
      11 0.39143316 0.17856908 0.6048191
      12 0.40848551 0.18254982 0.6300498
      
      
      Slot "FinalTDtargetDuringTrialEstimates":
      [1] 169.4013
      
      Slot "FinalTDtargetEndOfTrialEstimates":
      [1] 131.4969
      
      Slot "FinalTDtargetDuringTrialAtDoseGrid":
      [1] 150
      
      Slot "FinalTDtargetEndOfTrialAtDoseGrid":
      [1] 125
      
      Slot "FinalTDEOTCIs":
      [[1]]
      [[1]]$lower
      [1] 81.58873
      
      [[1]]$upper
      [1] 1429.96
      
      
      
      Slot "FinalTDEOTRatios":
      [1] 17.52644
      
      Slot "FinalCIs":
      [[1]]
      [[1]]$lower
      [1] 81.58873
      
      [[1]]$upper
      [1] 1429.96
      
      
      
      Slot "FinalRatios":
      [1] 17.52644
      
      Slot "stopReasons":
      [[1]]
      [1] "Number of patients is 36 and thus reached the prespecified minimum number 36"
      
      
      Slot "data":
      [[1]]
      An object of class "Data"
      Slot "x":
       [1]  25  25  25  50  50  50  75  75  75 100 100 100 125 125 125 300 300 300  75
      [20]  75  75 125 125 125 150 150 150 150 150 150 225 225 225 150 150 150
      
      Slot "y":
       [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 0 0 0 0 1 0 0 0 0 0 1 1 0 0 0 0
      
      Slot "doseGrid":
       [1]  25  50  75 100 125 150 175 200 225 250 275 300
      
      Slot "nGrid":
      [1] 12
      
      Slot "xLevel":
       [1]  1  1  1  2  2  2  3  3  3  4  4  4  5  5  5 12 12 12  3  3  3  5  5  5  6
      [26]  6  6  6  6  6  9  9  9  6  6  6
      
      Slot "placebo":
      [1] FALSE
      
      Slot "ID":
       [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
      [26] 26 27 28 29 30 31 32 33 34 35 36
      
      Slot "cohort":
       [1]  1  1  1  2  2  2  3  3  3  4  4  4  5  5  5  6  6  6  7  7  7  8  8  8  9
      [26]  9  9 10 10 10 11 11 11 12 12 12
      
      Slot "nObs":
      [1] 36
      
      
      
      Slot "doses":
      [1] 125
      
      Slot "seed":
      [1] 819
      

# simulate-TDDesign produces consistent results

    Code
      result
    Output
      An object of class "PseudoSimulations"
      Slot "fit":
      [[1]]
      [[1]]$phi1
      [1] -9.791609
      
      [[1]]$phi2
      [1] 1.826935
      
      [[1]]$probDLE
       [1] 0.01962880 0.06632269 0.12967442 0.20128792 0.27476182 0.34581054
       [7] 0.41195661 0.47204678 0.52579017 0.57340058 0.61535170 0.65222382
      
      
      
      Slot "FinalTDtargetDuringTrialEstimates":
      [1] 151.5239
      
      Slot "FinalTDtargetEndOfTrialEstimates":
      [1] 133.7273
      
      Slot "FinalTDtargetDuringTrialAtDoseGrid":
      [1] 150
      
      Slot "FinalTDtargetEndOfTrialAtDoseGrid":
      [1] 125
      
      Slot "FinalTDEOTCIs":
      [[1]]
      [[1]]$lower
      [1] 90.1733
      
      [[1]]$upper
      [1] 198.318
      
      
      
      Slot "FinalTDEOTRatios":
      [1] 2.199298
      
      Slot "FinalCIs":
      [[1]]
      [[1]]$lower
      [1] 90.1733
      
      [[1]]$upper
      [1] 198.318
      
      
      
      Slot "FinalRatios":
      [1] 2.199298
      
      Slot "stopReasons":
      [[1]]
      [1] "Number of patients is 36 and thus reached the prespecified minimum number 36"
      
      
      Slot "data":
      [[1]]
      An object of class "Data"
      Slot "x":
       [1]  50  50  50 100 100 100 200 200 200  75  75  75 100 100 100 125 125 125 150
      [20] 150 150 150 150 150 125 125 125 150 150 150 150 150 150 125 125 125
      
      Slot "y":
       [1] 0 0 0 0 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 1 1 0 1 0 0 0 0 0 0 1 1 1 0 0 0
      
      Slot "doseGrid":
       [1]  25  50  75 100 125 150 175 200 225 250 275 300
      
      Slot "nGrid":
      [1] 12
      
      Slot "xLevel":
       [1] 2 2 2 4 4 4 8 8 8 3 3 3 4 4 4 5 5 5 6 6 6 6 6 6 5 5 5 6 6 6 6 6 6 5 5 5
      
      Slot "placebo":
      [1] FALSE
      
      Slot "ID":
       [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
      [26] 26 27 28 29 30 31 32 33 34 35 36
      
      Slot "cohort":
       [1]  1  1  1  2  2  2  3  3  3  4  4  4  5  5  5  6  6  6  7  7  7  8  8  8  9
      [26]  9  9 10 10 10 11 11 11 12 12 12
      
      Slot "nObs":
      [1] 36
      
      
      
      Slot "doses":
      [1] 125
      
      Slot "seed":
      [1] 819
      

# simulate-DualResponsesDesign produces consistent results

    Code
      result
    Output
      An object of class "PseudoDualSimulations"
      Slot "fitEff":
      [[1]]
      [[1]]$theta1
      [1] -4.20662
      
      [[1]]$theta2
      [1] 3.302767
      
      [[1]]$ExpEff
       [1] -0.3455793  0.2985344  0.6242505  0.8372982  0.9935774  1.1159959
       [7]  1.2160729  1.3003781  1.3729953  1.4366277  1.4931529  1.5439249
      
      
      
      Slot "FinalGstarEstimates":
      [1] 146.2479
      
      Slot "FinalGstarAtDoseGrid":
      [1] 125
      
      Slot "FinalGstarCIs":
      [[1]]
      [[1]]$lower
      [1] 75.03531
      
      [[1]]$upper
      [1] 285.0453
      
      
      
      Slot "FinalGstarRatios":
      [1] 3.798815
      
      Slot "FinalOptimalDose":
      [1] 137.5996
      
      Slot "FinalOptimalDoseAtDoseGrid":
      [1] 125
      
      Slot "sigma2est":
      [1] 0.1616952
      
      Slot "fit":
      [[1]]
      [[1]]$phi1
      [1] -9.998377
      
      [[1]]$phi2
      [1] 1.858333
      
      [[1]]$probDLE
       [1] 0.01769477 0.06131043 0.12184797 0.19147776 0.26390666 0.33471252
       [7] 0.40119608 0.46198895 0.51662937 0.56521068 0.60812916 0.64591963
      
      
      
      Slot "FinalTDtargetDuringTrialEstimates":
      [1] 155.5827
      
      Slot "FinalTDtargetEndOfTrialEstimates":
      [1] 137.5996
      
      Slot "FinalTDtargetDuringTrialAtDoseGrid":
      [1] 150
      
      Slot "FinalTDtargetEndOfTrialAtDoseGrid":
      [1] 125
      
      Slot "FinalTDEOTCIs":
      [[1]]
      [[1]]$lower
      [1] 92.64601
      
      [[1]]$upper
      [1] 204.3655
      
      
      
      Slot "FinalTDEOTRatios":
      [1] 2.205875
      
      Slot "FinalCIs":
      [[1]]
      [[1]]$lower
      [1] 92.64601
      
      [[1]]$upper
      [1] 204.3655
      
      
      
      Slot "FinalRatios":
      [1] 2.205875
      
      Slot "stopReasons":
      [[1]]
      [1] "Number of patients is 36 and thus reached the prespecified minimum number 36"
      
      
      Slot "data":
      [[1]]
      An object of class "DataDual"
      Slot "w":
       [1] -0.6588212 -0.6574900 -0.6767438  0.4201327  0.1657673  0.6398290
       [7]  1.0794245  1.0493776  0.9825006  1.6985308  1.7758098  1.5194703
      [13]  1.1747540  0.7687320  0.5822763  0.8930588  1.0287153  0.9338591
      [19]  1.1158580  1.0035394  1.0517295  1.5133967  1.2179947  1.0877295
      [25]  1.6171994  1.2565239  1.0710927  0.9289380  0.8863580  0.8356701
      [31]  0.7950787  0.6793477  0.7302074  0.7626097  0.8732485  0.8272182
      
      Slot "x":
       [1]  25  25  25  75  75  75 125 125 125 250 250 250 100 100 100 125 125 125 150
      [20] 150 150 150 150 150 175 175 175 125 125 125 125 125 125 125 125 125
      
      Slot "y":
       [1] 0 0 0 0 0 0 0 0 0 1 1 1 0 0 0 0 0 0 0 1 0 0 0 0 1 1 1 1 0 0 1 0 0 0 0 0
      
      Slot "doseGrid":
       [1]  25  50  75 100 125 150 175 200 225 250 275 300
      
      Slot "nGrid":
      [1] 12
      
      Slot "xLevel":
       [1]  1  1  1  3  3  3  5  5  5 10 10 10  4  4  4  5  5  5  6  6  6  6  6  6  7
      [26]  7  7  5  5  5  5  5  5  5  5  5
      
      Slot "placebo":
      [1] FALSE
      
      Slot "ID":
       [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
      [26] 26 27 28 29 30 31 32 33 34 35 36
      
      Slot "cohort":
       [1]  1  1  1  2  2  2  3  3  3  4  4  4  5  5  5  6  6  6  7  7  7  8  8  8  9
      [26]  9  9 10 10 10 11 11 11 12 12 12
      
      Slot "nObs":
      [1] 36
      
      
      
      Slot "doses":
      [1] 125
      
      Slot "seed":
      [1] 819
      

# simulate-DualResponsesSamplesDesign produces consistent results

    Code
      result
    Output
      An object of class "PseudoDualSimulations"
      Slot "fitEff":
      [[1]]
             middle      lower      upper
      1  -0.3079474 -0.8521410 0.08196096
      2   0.3736494  0.1411489 0.55468787
      3   0.7183200  0.5077815 0.88538501
      4   0.9437657  0.7214829 1.15179652
      5   1.1091393  0.8738198 1.41328216
      6   1.2386817  0.9809589 1.61908158
      7   1.3445825  1.0555782 1.78343868
      8   1.4337937  1.1087482 1.89651747
      9   1.5106368  1.1545466 1.99391914
      10  1.5779722  1.1946786 2.07926954
      11  1.6377868  1.2303281 2.15508685
      12  1.6915134  1.2623492 2.22318756
      
      
      Slot "FinalGstarEstimates":
      [1] 300
      
      Slot "FinalGstarAtDoseGrid":
      [1] 225
      
      Slot "FinalGstarCIs":
      [[1]]
      [[1]]$lower
      [1] 300
      
      [[1]]$upper
      [1] 300
      
      
      
      Slot "FinalGstarRatios":
      [1] 1
      
      Slot "FinalOptimalDose":
      [1] 62.78087
      
      Slot "FinalOptimalDoseAtDoseGrid":
      [1] 50
      
      Slot "sigma2est":
      [1] 0.2648646
      
      Slot "fit":
      [[1]]
            middle      lower     upper
      1  0.1627044 0.07680748 0.2713562
      2  0.2061350 0.09015426 0.3239337
      3  0.2352925 0.09889961 0.3570465
      4  0.2575780 0.10555513 0.3819918
      5  0.2757086 0.11098665 0.4045258
      6  0.2910230 0.11560460 0.4232458
      7  0.3042901 0.11963849 0.4392495
      8  0.3159958 0.12323056 0.4532166
      9  0.3264687 0.12647556 0.4655986
      10 0.3359419 0.12943997 0.4767113
      11 0.3445874 0.13217237 0.4867840
      12 0.3525358 0.13470940 0.4960901
      
      
      Slot "FinalTDtargetDuringTrialEstimates":
      [1] 95.95589
      
      Slot "FinalTDtargetEndOfTrialEstimates":
      [1] 62.78087
      
      Slot "FinalTDtargetDuringTrialAtDoseGrid":
      [1] 75
      
      Slot "FinalTDtargetEndOfTrialAtDoseGrid":
      [1] 50
      
      Slot "FinalTDEOTCIs":
      [[1]]
      [[1]]$lower
      [1] 62.78087
      
      [[1]]$upper
      [1] 62.78087
      
      
      
      Slot "FinalTDEOTRatios":
      [1] 1
      
      Slot "FinalCIs":
      [[1]]
      [[1]]$lower
      [1] 62.78087
      
      [[1]]$upper
      [1] 62.78087
      
      
      
      Slot "FinalRatios":
      [1] 1
      
      Slot "stopReasons":
      [[1]]
      [1] "Number of patients is 12 and thus reached the prespecified minimum number 10"
      
      
      Slot "data":
      [[1]]
      An object of class "DataDual"
      Slot "w":
       [1] -0.6588212 -0.6574900 -0.6767438  0.5106132  0.5020599  0.5558647
       [7]  1.0105606  0.8984411  1.2273833  0.6454706  0.5818376  0.4685371
      
      Slot "x":
       [1]  25  25  25  75  75  75 125 125 125  75  75  75
      
      Slot "y":
       [1] 0 0 0 0 0 0 0 0 0 0 0 0
      
      Slot "doseGrid":
       [1]  25  50  75 100 125 150 175 200 225 250 275 300
      
      Slot "nGrid":
      [1] 12
      
      Slot "xLevel":
       [1] 1 1 1 3 3 3 5 5 5 3 3 3
      
      Slot "placebo":
      [1] FALSE
      
      Slot "ID":
       [1]  1  2  3  4  5  6  7  8  9 10 11 12
      
      Slot "cohort":
       [1] 1 1 1 2 2 2 3 3 3 4 4 4
      
      Slot "nObs":
      [1] 12
      
      
      
      Slot "doses":
      [1] 50
      
      Slot "seed":
      [1] 819
      

# simulate-DADesign producs consistent results

    Code
      result
    Output
      An object of class "DASimulations"
      Slot "trialduration":
      [1]  92 102
      
      Slot "fit":
      [[1]]
              middle        lower      upper
      1  0.004295391 1.244080e-06 0.03397437
      2  0.012179874 4.479996e-05 0.07132948
      3  0.019891979 2.096678e-04 0.09708732
      4  0.026918223 5.170274e-04 0.11575861
      5  0.046553819 2.415536e-03 0.16383458
      6  0.083833057 1.120714e-02 0.24115394
      7  0.132106465 2.918661e-02 0.31221605
      8  0.155735462 3.970419e-02 0.34025823
      9  0.178969746 5.134981e-02 0.36496765
      10 0.201754985 6.398343e-02 0.38696830
      11 0.224039304 7.747479e-02 0.40676806
      12 0.245776886 9.170221e-02 0.42474208
      13 0.266929606 1.065521e-01 0.44943530
      14 0.287467620 1.219185e-01 0.48181337
      15 0.307369311 1.377034e-01 0.51174029
      16 0.326620816 1.538159e-01 0.53937816
      17 0.345215301 1.701728e-01 0.56489605
      18 0.363152099 1.866979e-01 0.58846177
      19 0.380435807 2.033222e-01 0.61023685
      20 0.397075393 2.199833e-01 0.63037365
      21 0.413083363 2.366252e-01 0.64901395
      22 0.428475004 2.531982e-01 0.66628839
      23 0.443267723 2.696581e-01 0.68231655
      24 0.457480467 2.859664e-01 0.69720740
      25 0.471133239 3.020862e-01 0.71105988
      26 0.484246691 3.179131e-01 0.72396368
      27 0.496841790 3.336383e-01 0.73599996
      28 0.508939546 3.490769e-01 0.74724212
      29 0.520560799 3.583465e-01 0.75775654
      30 0.531726050 3.635700e-01 0.76760325
      31 0.542455331 3.703076e-01 0.77683656
      32 0.552768109 3.768652e-01 0.78550564
      33 0.562683218 3.832510e-01 0.79365505
      34 0.572218812 3.894728e-01 0.80132519
      35 0.581392333 3.931278e-01 0.80855275
      36 0.590220500 3.965823e-01 0.81537108
      37 0.598719302 3.999466e-01 0.82181051
      38 0.606904005 4.032252e-01 0.82789870
      39 0.614789163 4.064223e-01 0.83366086
      40 0.622388635 4.095417e-01 0.83912003
      41 0.629715604 4.125872e-01 0.84429726
      42 0.636782605 4.155620e-01 0.84921185
      
      [[2]]
              middle        lower      upper
      1  0.004911478 8.087168e-08 0.05528729
      2  0.012549333 5.301817e-06 0.11394485
      3  0.019758284 3.212131e-05 0.15290884
      4  0.026281283 9.213620e-05 0.18040663
      5  0.044580013 5.579665e-04 0.23605045
      6  0.080050074 3.715401e-03 0.30252744
      7  0.127620452 1.404093e-02 0.35770890
      8  0.151589899 2.143298e-02 0.39190695
      9  0.175591096 2.968381e-02 0.41946431
      10 0.199531587 4.148828e-02 0.43463638
      11 0.223311872 5.386560e-02 0.44785496
      12 0.246830819 6.783903e-02 0.45974264
      13 0.269990000 8.332033e-02 0.47053627
      14 0.292697413 1.002761e-01 0.48041440
      15 0.314870521 1.185218e-01 0.49734178
      16 0.336438469 1.379099e-01 0.51751677
      17 0.357343410 1.557747e-01 0.53766160
      18 0.377540952 1.659874e-01 0.55409937
      19 0.396999864 1.688536e-01 0.57079934
      20 0.415701182 1.716082e-01 0.58639676
      21 0.433636933 1.742613e-01 0.60099090
      22 0.450808626 1.768218e-01 0.61467020
      23 0.467225674 1.797946e-01 0.63460692
      24 0.482903851 1.921902e-01 0.65659243
      25 0.497863858 2.046432e-01 0.67699334
      26 0.512130047 2.171268e-01 0.69591573
      27 0.525729316 2.296159e-01 0.71346433
      28 0.538690185 2.420877e-01 0.72974052
      29 0.551042041 2.545209e-01 0.74484107
      30 0.562814531 2.668964e-01 0.75885728
      31 0.574037091 2.791964e-01 0.77187463
      32 0.584738589 2.914051e-01 0.78397263
      33 0.594947062 3.035532e-01 0.79616701
      34 0.604689530 3.156578e-01 0.80935856
      35 0.613991872 3.276321e-01 0.82155223
      36 0.622878748 3.388754e-01 0.83279009
      37 0.631373569 3.433570e-01 0.84315387
      38 0.639498479 3.477404e-01 0.85271875
      39 0.647274378 3.520297e-01 0.85955014
      40 0.654720947 3.562287e-01 0.86348846
      41 0.661856689 3.603410e-01 0.86723469
      42 0.668698980 3.643700e-01 0.87118442
      
      
      Slot "stop_report":
           [,1]
      [1,] TRUE
      [2,] TRUE
      
      Slot "stop_reasons":
      [[1]]
      [[1]][[1]]
      [1] "Probability for target toxicity is 62 % for dose 22 and thus above the required 50 %"
      
      [[1]][[2]]
      [1] "Number of patients is 17 and thus below the prespecified minimum number 50"
      
      
      [[2]]
      [[2]][[1]]
      [1] "Probability for target toxicity is 60 % for dose 22 and thus above the required 50 %"
      
      [[2]][[2]]
      [1] "Number of patients is 18 and thus below the prespecified minimum number 50"
      
      
      
      Slot "data":
      [[1]]
      An object of class "DataDA"
      Slot "u":
       [1] 60 60 60 60  4 60 54 52 45  4 15 19 24  8 15  7  7
      
      Slot "t0":
       [1]  0  7 14 21 28 32 38 40 47 53 55 62 68 70 77 83 85
      
      Slot "Tmax":
      [1] 60
      
      Slot "x":
       [1]  3  6 12 24 30 18 18 18 36 36 36 30 30 30 28 28 28
      
      Slot "y":
       [1] 0 0 0 0 1 0 0 0 0 1 1 1 0 1 0 1 0
      
      Slot "doseGrid":
       [1]  0.1  0.5  1.0  1.5  3.0  6.0 10.0 12.0 14.0 16.0 18.0 20.0 22.0 24.0 26.0
      [16] 28.0 30.0 32.0 34.0 36.0 38.0 40.0 42.0 44.0 46.0 48.0 50.0 52.0 54.0 56.0
      [31] 58.0 60.0 62.0 64.0 66.0 68.0 70.0 72.0 74.0 76.0 78.0 80.0
      
      Slot "nGrid":
      [1] 42
      
      Slot "xLevel":
       [1]  5  6  8 14 17 11 11 11 20 20 20 17 17 17 16 16 16
      
      Slot "placebo":
      [1] FALSE
      
      Slot "ID":
       [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
      
      Slot "cohort":
       [1] 1 2 3 4 5 6 6 6 7 7 7 8 8 8 9 9 9
      
      Slot "nObs":
      [1] 17
      
      
      [[2]]
      An object of class "DataDA"
      Slot "u":
       [1] 60 60 60  8 13 60 60 59 52 46 44 37 31 29 14 11 14  7
      
      Slot "t0":
       [1]  0  7 14 21 28 35 41 43 50 56 58 65 71 73 80 86 88 95
      
      Slot "Tmax":
      [1] 60
      
      Slot "x":
       [1]  3  6 12 22 28 12 12 12 16 16 16 10 10 10 20 20 20 20
      
      Slot "y":
       [1] 0 0 0 1 1 0 0 0 0 0 0 0 0 0 1 1 0 0
      
      Slot "doseGrid":
       [1]  0.1  0.5  1.0  1.5  3.0  6.0 10.0 12.0 14.0 16.0 18.0 20.0 22.0 24.0 26.0
      [16] 28.0 30.0 32.0 34.0 36.0 38.0 40.0 42.0 44.0 46.0 48.0 50.0 52.0 54.0 56.0
      [31] 58.0 60.0 62.0 64.0 66.0 68.0 70.0 72.0 74.0 76.0 78.0 80.0
      
      Slot "nGrid":
      [1] 42
      
      Slot "xLevel":
       [1]  5  6  8 13 16  8  8  8 10 10 10  7  7  7 12 12 12 12
      
      Slot "placebo":
      [1] FALSE
      
      Slot "ID":
       [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18
      
      Slot "cohort":
       [1]  1  2  3  4  5  6  6  6  7  7  7  8  8  8  9  9  9 10
      
      Slot "nObs":
      [1] 18
      
      
      
      Slot "doses":
      [1] 22 22
      
      Slot "seed":
      [1] 819
      

# NextBestInfTheory produces consistent results for empty data

    Code
      result@meanFit
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
      result@meanFit
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
      

# examine produces consistent results

    Code
      result
    Output
         dose DLTs nextDose  stop increment
      1     3    0      1.0 FALSE       -67
      2     3    1      3.0 FALSE         0
      3     3    2      1.0 FALSE       -67
      4     3    3       NA FALSE        NA
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
      4     3    3       NA FALSE        NA
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

