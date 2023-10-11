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
      
      
      
      Slot "additional_stats":
      [[1]]
      list()
      
      
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
      
      
      
      Slot "additional_stats":
      [[1]]
      list()
      
      
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
      
      
      
      Slot "additional_stats":
      [[1]]
      list()
      
      
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
      

# Test if simulate generate the expected output.

    Code
      sim
    Output
      An object of class "Simulations"
      Slot "fit":
      [[1]]
            middle     lower     upper
      1  0.3185617 0.1610002 0.4380715
      2  0.3115668 0.2305102 0.4260590
      3  0.3123386 0.2023495 0.4190715
      4  0.3148504 0.1839781 0.4141332
      5  0.3177190 0.1706377 0.4250526
      6  0.3205237 0.1603176 0.4615404
      7  0.3231301 0.1519912 0.4927203
      8  0.3255041 0.1450683 0.5197231
      9  0.3276489 0.1391811 0.5433694
      10 0.3295813 0.1340857 0.5642717
      11 0.3313223 0.1296131 0.5828979
      12 0.3328926 0.1256414 0.5996128
      
      
      Slot "stop_report":
           <NA>
      [1,] TRUE
      
      Slot "stop_reasons":
      [[1]]
      [1] "Number of patients is 15 and thus reached the prespecified minimum number 5"
      
      
      Slot "additional_stats":
      [[1]]
      list()
      
      
      Slot "data":
      [[1]]
      An object of class "Data"
      Slot "x":
       [1]  25  25  25  25  50  50  50  50 100 100 100 100  25  25  25
      
      Slot "y":
       [1] 0 0 0 0 0 0 0 0 0 0 1 0 1 1 1
      
      Slot "doseGrid":
       [1]  25  50  75 100 125 150 175 200 225 250 275 300
      
      Slot "nGrid":
      [1] 12
      
      Slot "xLevel":
       [1] 1 1 1 1 2 2 2 2 4 4 4 4 1 1 1
      
      Slot "placebo":
      [1] FALSE
      
      Slot "ID":
       [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15
      
      Slot "cohort":
       [1] 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4
      
      Slot "nObs":
      [1] 15
      
      
      
      Slot "doses":
      [1] NA
      
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

