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
      

# simulate-DualDesign produces consistent results with firstSeparate

    Code
      result
    Output
      An object of class "DualSimulations"
      Slot "rho_est":
      [1] -0.15486300 -0.10955726 -0.01177171
      
      Slot "sigma2w_est":
      [1] 0.03502396 0.03618350 0.03277689
      
      Slot "fit_biomarker":
      [[1]]
         middleBiomarker lowerBiomarker upperBiomarker
      1        0.2086917     0.09332589      0.3341649
      2        0.2378809     0.03928196      0.4847109
      3        0.2338422    -0.11648992      0.5718531
      4        0.2354693    -0.32916647      0.7948879
      5        0.1989655    -0.48425686      0.8857304
      6        0.2173203    -0.59718586      1.0175460
      7        0.2249437    -0.72145708      1.0567596
      8        0.1987182    -0.93817336      1.3636867
      9        0.2037979    -0.92876046      1.5383155
      10       0.2063992    -1.28868524      1.7150746
      11       0.2483740    -1.63966269      1.9443200
      
      [[2]]
         middleBiomarker lowerBiomarker upperBiomarker
      1        0.2066799     0.05434646      0.3357103
      2        0.2595252     0.02671844      0.4551710
      3        0.2683451    -0.09688182      0.6226785
      4        0.2883525    -0.24133905      0.9485969
      5        0.2847174    -0.42396836      0.9194391
      6        0.2737519    -0.60734199      1.1155737
      7        0.2682734    -0.70517510      1.2028347
      8        0.2758864    -0.97191185      1.5384185
      9        0.2309859    -1.14205020      1.6380261
      10       0.2857383    -1.37715127      1.9852789
      11       0.2666181    -1.61032455      2.1791798
      
      [[3]]
         middleBiomarker lowerBiomarker upperBiomarker
      1        0.2024137     0.07600110      0.3053696
      2        0.2360812     0.02297166      0.4747025
      3        0.2425879    -0.13771673      0.6525277
      4        0.2317937    -0.28803079      0.7767033
      5        0.2322719    -0.42225103      0.9610503
      6        0.2444870    -0.48402769      1.0924026
      7        0.2503818    -0.62562319      1.2071096
      8        0.2238244    -0.97050063      1.4815096
      9        0.2169614    -1.08387556      1.5532608
      10       0.2394941    -1.43514219      2.0076785
      11       0.2333254    -1.72093764      2.2332546
      
      
      Slot "fit":
      [[1]]
             middle        lower     upper
      1  0.05192373 0.0001197865 0.1666527
      2  0.35002888 0.0556533046 0.7086394
      3  0.60682498 0.0824755643 0.9852298
      4  0.80109118 0.1232876667 0.9999942
      5  0.85631897 0.1528649620 1.0000000
      6  0.88337406 0.1764951600 1.0000000
      7  0.89969301 0.1963330372 1.0000000
      8  0.92454816 0.2423067601 1.0000000
      9  0.93305137 0.2660218395 1.0000000
      10 0.94640116 0.3195353377 1.0000000
      11 0.95119809 0.3464265031 1.0000000
      
      [[2]]
             middle        lower     upper
      1  0.03835437 1.224655e-05 0.1612162
      2  0.39400269 8.368400e-02 0.9243805
      3  0.64705481 1.224422e-01 0.9997330
      4  0.81133001 1.928981e-01 1.0000000
      5  0.86198471 2.437306e-01 1.0000000
      6  0.88924224 2.837735e-01 1.0000000
      7  0.90643601 3.168568e-01 1.0000000
      8  0.93393303 3.912473e-01 1.0000000
      9  0.94380146 4.282198e-01 1.0000000
      10 0.95978882 5.077903e-01 1.0000000
      11 0.96562275 5.456546e-01 1.0000000
      
      [[3]]
             middle        lower     upper
      1  0.02276437 1.234448e-05 0.1049693
      2  0.38324243 2.508884e-02 0.8558803
      3  0.63101395 5.588557e-02 0.9908926
      4  0.78623656 1.376991e-01 0.9999953
      5  0.83929625 2.122938e-01 1.0000000
      6  0.87025751 2.772105e-01 1.0000000
      7  0.89118558 3.334801e-01 1.0000000
      8  0.92688566 4.635153e-01 1.0000000
      9  0.94007507 5.276020e-01 1.0000000
      10 0.96104539 6.583579e-01 1.0000000
      11 0.96830637 7.152575e-01 1.0000000
      
      
      Slot "stop_report":
           [,1]
      [1,] TRUE
      [2,] TRUE
      [3,] TRUE
      
      Slot "stop_reasons":
      [[1]]
      [[1]][[1]]
      [1] "Probability for target biomarker is 20 % for dose 1 and thus below the required 50 %"
      
      [[1]][[2]]
      [1] "Number of patients is 10 and thus reached the prespecified minimum number 10"
      
      
      [[2]]
      [[2]][[1]]
      [1] "Probability for target biomarker is 18 % for dose 1 and thus below the required 50 %"
      
      [[2]][[2]]
      [1] "Number of patients is 10 and thus reached the prespecified minimum number 10"
      
      
      [[3]]
      [[3]][[1]]
      [1] "Probability for target biomarker is 19 % for dose 1 and thus below the required 50 %"
      
      [[3]][[2]]
      [1] "Number of patients is 10 and thus reached the prespecified minimum number 10"
      
      
      
      Slot "data":
      [[1]]
      An object of class "DataDual"
      Slot "w":
       [1] 0.2557299 0.1150870 0.3181799 0.2531184 0.1632822 0.3616207 0.2672235
       [8] 0.1000139 0.1305151 0.2393188
      
      Slot "x":
       [1] 3 1 1 1 1 1 1 1 1 1
      
      Slot "y":
       [1] 0 0 0 0 0 0 0 0 0 0
      
      Slot "doseGrid":
       [1]   1   3   5  10  15  20  25  40  50  80 100
      
      Slot "nGrid":
      [1] 11
      
      Slot "xLevel":
       [1] 2 1 1 1 1 1 1 1 1 1
      
      Slot "placebo":
      [1] FALSE
      
      Slot "ID":
       [1]  1  2  3  4  5  6  7  8  9 10
      
      Slot "cohort":
       [1]  1  2  3  4  5  6  7  8  9 10
      
      Slot "nObs":
      [1] 10
      
      
      [[2]]
      An object of class "DataDual"
      Slot "w":
       [1] 0.32953447 0.08107771 0.30196261 0.29559547 0.29927986 0.21919932
       [7] 0.27619227 0.14093015 0.08342380 0.14010511
      
      Slot "x":
       [1] 3 1 1 1 1 1 1 1 1 1
      
      Slot "y":
       [1] 0 0 0 0 0 0 0 0 0 0
      
      Slot "doseGrid":
       [1]   1   3   5  10  15  20  25  40  50  80 100
      
      Slot "nGrid":
      [1] 11
      
      Slot "xLevel":
       [1] 2 1 1 1 1 1 1 1 1 1
      
      Slot "placebo":
      [1] FALSE
      
      Slot "ID":
       [1]  1  2  3  4  5  6  7  8  9 10
      
      Slot "cohort":
       [1]  1  2  3  4  5  6  7  8  9 10
      
      Slot "nObs":
      [1] 10
      
      
      [[3]]
      An object of class "DataDual"
      Slot "w":
       [1] 0.30414108 0.11340543 0.28113626 0.07531698 0.22455967 0.27182019
       [7] 0.13609741 0.13674535 0.21145309 0.33199249
      
      Slot "x":
       [1] 3 1 1 1 1 1 1 1 1 1
      
      Slot "y":
       [1] 0 0 0 0 0 0 0 0 0 0
      
      Slot "doseGrid":
       [1]   1   3   5  10  15  20  25  40  50  80 100
      
      Slot "nGrid":
      [1] 11
      
      Slot "xLevel":
       [1] 2 1 1 1 1 1 1 1 1 1
      
      Slot "placebo":
      [1] FALSE
      
      Slot "ID":
       [1]  1  2  3  4  5  6  7  8  9 10
      
      Slot "cohort":
       [1]  1  2  3  4  5  6  7  8  9 10
      
      Slot "nObs":
      [1] 10
      
      
      
      Slot "doses":
      [1] 1 1 1
      
      Slot "seed":
      [1] 3
      

# simulate-DualDesign produces consistent results wih placebo data

    Code
      result
    Output
      An object of class "DualSimulations"
      Slot "rho_est":
      [1] -0.000371335
      
      Slot "sigma2w_est":
      [1] 0.0312664
      
      Slot "fit_biomarker":
      [[1]]
         middleBiomarker lowerBiomarker upperBiomarker
      1        0.2146516     0.09823235      0.3196912
      2        0.2573415     0.00426307      0.4984764
      3        0.2466649    -0.12245781      0.5761141
      4        0.2576503    -0.31594732      0.9222295
      5        0.2469613    -0.45105610      0.9446578
      6        0.2508922    -0.52678549      1.1664346
      7        0.2416638    -0.77509135      1.2583016
      8        0.2638054    -1.09308026      1.3620701
      9        0.2388120    -1.44073683      1.5834733
      10       0.2712078    -1.62682487      2.0700636
      11       0.2866754    -1.99871015      2.1883864
      
      
      Slot "fit":
      [[1]]
             middle        lower      upper
      1  0.01297601 2.500743e-06 0.05362108
      2  0.44807294 1.466051e-02 0.92308168
      3  0.69609283 4.524826e-02 0.99952661
      4  0.82305320 1.034355e-01 1.00000000
      5  0.86090313 1.561639e-01 1.00000000
      6  0.88217059 2.028039e-01 1.00000000
      7  0.89657414 2.441436e-01 1.00000000
      8  0.92169439 3.441808e-01 1.00000000
      9  0.93142377 3.964805e-01 1.00000000
      10 0.94832292 5.117626e-01 1.00000000
      11 0.95499775 5.667480e-01 1.00000000
      
      
      Slot "stop_report":
           [,1]
      [1,] TRUE
      
      Slot "stop_reasons":
      [[1]]
      [[1]][[1]]
      [1] "Probability for target biomarker is 16 % for dose 1 and thus below the required 50 %"
      
      [[1]][[2]]
      [1] "Number of patients is 10 and thus reached the prespecified minimum number 10"
      
      
      
      Slot "data":
      [[1]]
      An object of class "DataDual"
      Slot "w":
       [1] 0.2557299 0.1150870 0.3181799 0.2531184 0.1632822 0.3616207 0.2672235
       [8] 0.1000139 0.1305151 0.2393188
      
      Slot "x":
       [1] 3 1 1 1 1 1 1 1 1 1
      
      Slot "y":
       [1] 0 0 0 0 0 0 0 0 0 0
      
      Slot "doseGrid":
       [1]   1   3   5  10  15  20  25  40  50  80 100
      
      Slot "nGrid":
      [1] 11
      
      Slot "xLevel":
       [1] 2 1 1 1 1 1 1 1 1 1
      
      Slot "placebo":
      [1] FALSE
      
      Slot "ID":
       [1]  1  2  3  4  5  6  7  8  9 10
      
      Slot "cohort":
       [1]  1  2  3  4  5  6  7  8  9 10
      
      Slot "nObs":
      [1] 10
      
      
      
      Slot "doses":
      [1] 1
      
      Slot "seed":
      [1] 3
      

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

