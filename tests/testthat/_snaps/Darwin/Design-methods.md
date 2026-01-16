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
           <NA>  <NA> ≥ 3 cohorts dosed P(0.2 ≤ prob(DLE | NBD) ≤ 0.35) ≥ 0.5
      [1,] TRUE FALSE              TRUE                                 FALSE
           ≥ 20 patients dosed Stopped because of missing dose
      [1,]                TRUE                           FALSE
      
      Slot "stop_reasons":
      [[1]]
      [[1]][[1]]
      [[1]][[1]][[1]]
      [1] "Number of cohorts is 5 and thus reached the prespecified minimum number 3"
      
      [[1]][[1]][[2]]
      [1] "Probability for target toxicity is 0 % for dose 1 and thus below the required 50 %"
      
      
      [[1]][[2]]
      [1] "Number of patients is 20 and thus reached the prespecified minimum number 20"
      
      [[1]][[3]]
      [1] "Next dose is available at the dose grid."
      
      
      
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
      
      Slot "backfilled":
       [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
      [13] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
      
      Slot "response":
       [1] 0 1 1 1 0 1 0 1 1 1 1 1 1 0 1 1 0 1 1 1
      
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
           <NA>  <NA> ≥ 3 cohorts dosed P(0.2 ≤ prob(DLE | NBD) ≤ 0.35) ≥ 0.5
      [1,] TRUE FALSE              TRUE                                 FALSE
           ≥ 20 patients dosed Stopped because of missing dose
      [1,]                TRUE                           FALSE
      
      Slot "stop_reasons":
      [[1]]
      [[1]][[1]]
      [[1]][[1]][[1]]
      [1] "Number of cohorts is 7 and thus reached the prespecified minimum number 3"
      
      [[1]][[1]][[2]]
      [1] "Probability for target toxicity is 0 % for dose 1 and thus below the required 50 %"
      
      
      [[1]][[2]]
      [1] "Number of patients is 21 and thus reached the prespecified minimum number 20"
      
      [[1]][[3]]
      [1] "Next dose is available at the dose grid."
      
      
      
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
      
      Slot "backfilled":
       [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
      [13] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
      
      Slot "response":
       [1] 1 1 1 0 0 1 1 1 1 1 1 1 0 1 1 1 1 1 1 0 1
      
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
           ≥ 5 patients dosed
      [1,]               TRUE
      
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
      
      Slot "backfilled":
       [1] FALSE FALSE  TRUE  TRUE FALSE FALSE  TRUE  TRUE FALSE FALSE  TRUE  TRUE
      [13] FALSE FALSE FALSE
      
      Slot "response":
       [1]  0  1  0 NA  0  1 NA  0  0  1  1 NA  1  1  1
      
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
      
      Slot "backfilled":
       [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
      [13] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
      [25] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
      
      Slot "response":
       [1] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
      [26] NA NA NA NA NA NA NA NA NA NA NA
      
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
           <NA> P(0.9 ≤ Biomarker ≤ 1) ≥ 0.5 (relative) ≥ 10 patients dosed
      [1,] TRUE                                   FALSE                TRUE
           Stopped because of missing dose
      [1,]                           FALSE
      
      Slot "stop_reasons":
      [[1]]
      [[1]][[1]]
      [1] "Probability for target biomarker is 21 % for dose 1 and thus below the required 50 %"
      
      [[1]][[2]]
      [1] "Number of patients is 12 and thus reached the prespecified minimum number 10"
      
      [[1]][[3]]
      [1] "Next dose is available at the dose grid."
      
      
      
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
      
      Slot "backfilled":
       [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
      
      Slot "response":
       [1] NA NA NA NA NA NA NA NA NA NA NA NA
      
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
      

---

    Code
      result
    Output
      An object of class "DualSimulations"
      Slot "rho_est":
      [1] 0.06611618
      
      Slot "sigma2w_est":
      [1] 0.09181379
      
      Slot "fit_biomarker":
      [[1]]
         middleBiomarker lowerBiomarker upperBiomarker
      1        0.2279948     -0.3024101      0.6737112
      2        0.2274759     -0.2189952      0.7034534
      3        0.2294547     -0.2809549      0.8129385
      4        0.2267946     -0.4990013      0.9702282
      5        0.2353311     -0.5084695      1.1986096
      6        0.2226399     -0.6489695      1.1434165
      7        0.2077775     -0.9357929      1.2670528
      8        0.2033259     -1.2744141      1.4465307
      9        0.2468064     -1.2324341      1.6871920
      10       0.2060423     -1.7461333      1.9678736
      11       0.2028092     -1.4860999      2.1509038
      
      
      Slot "fit":
      [[1]]
             middle        lower     upper
      1  0.03424327 0.0005114312 0.1441135
      2  0.37482637 0.0789682953 0.6274737
      3  0.64417886 0.1691032466 0.9529461
      4  0.81627361 0.2731796477 0.9999059
      5  0.86725607 0.3461037917 0.9999996
      6  0.89330581 0.4017832069 1.0000000
      7  0.90964652 0.4464586351 1.0000000
      8  0.93648214 0.5421121210 1.0000000
      9  0.94652849 0.5870163938 1.0000000
      10 0.96331486 0.6773304054 1.0000000
      11 0.96951461 0.7171402854 1.0000000
      
      
      Slot "stop_report":
           <NA> P(0.9 ≤ Biomarker ≤ 1) ≥ 0.5 (relative) ≥ 10 patients dosed
      [1,] TRUE                                   FALSE               FALSE
           Stopped because of missing dose
      [1,]                            TRUE
      
      Slot "stop_reasons":
      [[1]]
      [[1]][[1]]
      [1] "Probability for target biomarker is 0 % for dose NA and thus below the required 50 %"
      
      [[1]][[2]]
      [1] "Number of patients is 4 and thus below the prespecified minimum number 10"
      
      [[1]][[3]]
      [1] "Next dose is NA , i.e., no active dose is safe enough according to the NextBest rule."
      
      
      
      Slot "additional_stats":
      [[1]]
      list()
      
      
      Slot "data":
      [[1]]
      An object of class "DataDual"
      Slot "w":
      [1] 0.2531184 0.2557299 0.1150998 0.3181927
      
      Slot "x":
      [1] 1 3 3 3
      
      Slot "y":
      [1] 0 0 0 0
      
      Slot "doseGrid":
       [1]   1   3   5  10  15  20  25  40  50  80 100
      
      Slot "nGrid":
      [1] 11
      
      Slot "xLevel":
      [1] 1 2 2 2
      
      Slot "placebo":
      [1] TRUE
      
      Slot "backfilled":
      [1] FALSE FALSE FALSE FALSE
      
      Slot "response":
      [1] NA NA NA NA
      
      Slot "ID":
      [1] 1 2 3 4
      
      Slot "cohort":
      [1] 1 1 1 1
      
      Slot "nObs":
      [1] 4
      
      
      
      Slot "doses":
      [1] NA
      
      Slot "seed":
      [1] 3
      

---

    Code
      result
    Output
      An object of class "DualSimulations"
      Slot "rho_est":
      [1] 0.06611618
      
      Slot "sigma2w_est":
      [1] 0.09181379
      
      Slot "fit_biomarker":
      [[1]]
         middleBiomarker lowerBiomarker upperBiomarker
      1        0.2279948     -0.3024101      0.6737112
      2        0.2274759     -0.2189952      0.7034534
      3        0.2294547     -0.2809549      0.8129385
      4        0.2267946     -0.4990013      0.9702282
      5        0.2353311     -0.5084695      1.1986096
      6        0.2226399     -0.6489695      1.1434165
      7        0.2077775     -0.9357929      1.2670528
      8        0.2033259     -1.2744141      1.4465307
      9        0.2468064     -1.2324341      1.6871920
      10       0.2060423     -1.7461333      1.9678736
      11       0.2028092     -1.4860999      2.1509038
      
      
      Slot "fit":
      [[1]]
             middle        lower     upper
      1  0.03424327 0.0005114312 0.1441135
      2  0.37482637 0.0789682953 0.6274737
      3  0.64417886 0.1691032466 0.9529461
      4  0.81627361 0.2731796477 0.9999059
      5  0.86725607 0.3461037917 0.9999996
      6  0.89330581 0.4017832069 1.0000000
      7  0.90964652 0.4464586351 1.0000000
      8  0.93648214 0.5421121210 1.0000000
      9  0.94652849 0.5870163938 1.0000000
      10 0.96331486 0.6773304054 1.0000000
      11 0.96951461 0.7171402854 1.0000000
      
      
      Slot "stop_report":
           <NA> P(0.9 ≤ Biomarker ≤ 1) ≥ 0.5 (relative) ≥ 10 patients dosed
      [1,] TRUE                                   FALSE               FALSE
           Stopped because of missing dose
      [1,]                            TRUE
      
      Slot "stop_reasons":
      [[1]]
      [[1]][[1]]
      [1] "Probability for target biomarker is 0 % for dose NA and thus below the required 50 %"
      
      [[1]][[2]]
      [1] "Number of patients is 4 and thus below the prespecified minimum number 10"
      
      [[1]][[3]]
      [1] "Next dose is NA , i.e., no active dose is safe enough according to the NextBest rule."
      
      
      
      Slot "additional_stats":
      [[1]]
      list()
      
      
      Slot "data":
      [[1]]
      An object of class "DataDual"
      Slot "w":
      [1] 0.2531184 0.2557299 0.1150998 0.3181927
      
      Slot "x":
      [1] 1 3 3 3
      
      Slot "y":
      [1] 0 0 0 0
      
      Slot "doseGrid":
       [1]   1   3   5  10  15  20  25  40  50  80 100
      
      Slot "nGrid":
      [1] 11
      
      Slot "xLevel":
      [1] 1 2 2 2
      
      Slot "placebo":
      [1] TRUE
      
      Slot "backfilled":
      [1] FALSE FALSE FALSE FALSE
      
      Slot "response":
      [1] NA NA NA NA
      
      Slot "ID":
      [1] 1 2 3 4
      
      Slot "cohort":
      [1] 1 1 1 1
      
      Slot "nObs":
      [1] 4
      
      
      
      Slot "doses":
      [1] NA
      
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
      
      
      Slot "final_td_target_during_trial_estimates":
      [1] 169.4013
      
      Slot "final_td_target_end_of_trial_estimates":
      [1] 131.4969
      
      Slot "final_td_target_during_trial_at_dose_grid":
      [1] 150
      
      Slot "final_td_target_end_of_trial_at_dose_grid":
      [1] 125
      
      Slot "final_tdeot_cis":
      [[1]]
      [[1]]$lower
      [1] 81.58873
      
      [[1]]$upper
      [1] 1429.96
      
      
      
      Slot "final_tdeot_ratios":
      [1] 17.52644
      
      Slot "final_cis":
      [[1]]
      [[1]]$lower
      [1] 81.58873
      
      [[1]]$upper
      [1] 1429.96
      
      
      
      Slot "final_ratios":
      [1] 17.52644
      
      Slot "stop_report":
           ≥ 36 patients dosed
      [1,]                TRUE
      
      Slot "stop_reasons":
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
      
      Slot "backfilled":
       [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
      [13] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
      [25] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
      
      Slot "response":
       [1] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
      [26] NA NA NA NA NA NA NA NA NA NA NA
      
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
      

# simulate-TDSamplesDesign produces consistent results with placebo patients

    Code
      result
    Output
      An object of class "PseudoSimulations"
      Slot "fit":
      [[1]]
              middle        lower      upper
      1  0.009540806 3.607127e-05 0.04069487
      2  0.102980560 1.874472e-02 0.30954840
      3  0.186235828 4.344215e-02 0.42693586
      4  0.259900212 6.203562e-02 0.50078208
      5  0.323303150 8.921700e-02 0.61587549
      6  0.377101391 1.129564e-01 0.70247253
      7  0.422599236 1.304361e-01 0.76113638
      8  0.461213181 1.434409e-01 0.80414643
      9  0.494218512 1.532713e-01 0.83656681
      10 0.522673979 1.623943e-01 0.86151458
      11 0.547427318 1.709216e-01 0.88095706
      12 0.569147802 1.841623e-01 0.89646578
      13 0.588362449 1.981568e-01 0.90903544
      
      
      Slot "final_td_target_during_trial_estimates":
      [1] 90.54898
      
      Slot "final_td_target_end_of_trial_estimates":
      [1] 72.99006
      
      Slot "final_td_target_during_trial_at_dose_grid":
      [1] 1
      
      Slot "final_td_target_end_of_trial_at_dose_grid":
      [1] 1
      
      Slot "final_tdeot_cis":
      [[1]]
      [[1]]$lower
      [1] 23.50865
      
      [[1]]$upper
      [1] 683.7909
      
      
      
      Slot "final_tdeot_ratios":
      [1] 29.08678
      
      Slot "final_cis":
      [[1]]
      [[1]]$lower
      [1] 23.50865
      
      [[1]]$upper
      [1] 683.7909
      
      
      
      Slot "final_ratios":
      [1] 29.08678
      
      Slot "stop_report":
           ≥ 36 patients dosed
      [1,]                TRUE
      
      Slot "stop_reasons":
      [[1]]
      [1] "Number of patients is 36 and thus reached the prespecified minimum number 36"
      
      
      Slot "data":
      [[1]]
      An object of class "Data"
      Slot "x":
       [1] 100 100 100   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1
      [20]   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1
      
      Slot "y":
       [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      
      Slot "doseGrid":
       [1]   1  25  50  75 100 125 150 175 200 225 250 275 300
      
      Slot "nGrid":
      [1] 13
      
      Slot "xLevel":
       [1] 5 5 5 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
      
      Slot "placebo":
      [1] TRUE
      
      Slot "backfilled":
       [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
      [13] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
      [25] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
      
      Slot "response":
       [1] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
      [26] NA NA NA NA NA NA NA NA NA NA NA
      
      Slot "ID":
       [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
      [26] 26 27 28 29 30 31 32 33 34 35 36
      
      Slot "cohort":
       [1] 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 6 6 6 6 7 7 7 7 8 8 8 8 9 9 9 9
      
      Slot "nObs":
      [1] 36
      
      
      
      Slot "doses":
      [1] 1
      
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
      
      
      
      Slot "final_td_target_during_trial_estimates":
      [1] 151.5239
      
      Slot "final_td_target_end_of_trial_estimates":
      [1] 133.7273
      
      Slot "final_td_target_during_trial_at_dose_grid":
      [1] 150
      
      Slot "final_td_target_end_of_trial_at_dose_grid":
      [1] 125
      
      Slot "final_tdeot_cis":
      [[1]]
      [[1]]$lower
      [1] 90.1733
      
      [[1]]$upper
      [1] 198.318
      
      
      
      Slot "final_tdeot_ratios":
      [1] 2.199298
      
      Slot "final_cis":
      [[1]]
      [[1]]$lower
      [1] 90.1733
      
      [[1]]$upper
      [1] 198.318
      
      
      
      Slot "final_ratios":
      [1] 2.199298
      
      Slot "stop_report":
           ≥ 36 patients dosed
      [1,]                TRUE
      
      Slot "stop_reasons":
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
      
      Slot "backfilled":
       [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
      [13] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
      [25] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
      
      Slot "response":
       [1] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
      [26] NA NA NA NA NA NA NA NA NA NA NA
      
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
      

# simulate-TDDesign with sentinel patient and placebo patients produces consistent results

    Code
      result
    Output
      An object of class "PseudoSimulations"
      Slot "fit":
      [[1]]
      [[1]]$phi1
      [1] -7.711824
      
      [[1]]$phi2
      [1] 1.357813
      
      [[1]]$probDLE
       [1] 0.01009723 0.03418471 0.08317039 0.13593342 0.18864078 0.23941686
       [7] 0.28734359 0.33203109 0.37338964 0.41149825 0.44652645 0.47868743
      [13] 0.50821006
      
      
      
      Slot "final_td_target_during_trial_estimates":
      [1] 185.6169
      
      Slot "final_td_target_end_of_trial_estimates":
      [1] 156.8951
      
      Slot "final_td_target_during_trial_at_dose_grid":
      [1] 175
      
      Slot "final_td_target_end_of_trial_at_dose_grid":
      [1] 150
      
      Slot "final_tdeot_cis":
      [[1]]
      [[1]]$lower
      [1] 83.13214
      
      [[1]]$upper
      [1] 296.1077
      
      
      
      Slot "final_tdeot_ratios":
      [1] 3.561892
      
      Slot "final_cis":
      [[1]]
      [[1]]$lower
      [1] 83.13214
      
      [[1]]$upper
      [1] 296.1077
      
      
      
      Slot "final_ratios":
      [1] 3.561892
      
      Slot "stop_report":
           ≥ 36 patients dosed
      [1,]                TRUE
      
      Slot "stop_reasons":
      [[1]]
      [1] "Number of patients is 36 and thus reached the prespecified minimum number 36"
      
      
      Slot "data":
      [[1]]
      An object of class "Data"
      Slot "x":
       [1]  10  50  50  50  10 100 100 100  10 175  10 125 125 125  10 175  10 125 125
      [20] 125  10 175 175 175  10 175  10 150  10 125 125 125  10 150 150 150
      
      Slot "y":
       [1] 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 0 1 0 0 1 0 1 0 0 0 0 0 0 0 0
      
      Slot "doseGrid":
       [1]  10  25  50  75 100 125 150 175 200 225 250 275 300
      
      Slot "nGrid":
      [1] 13
      
      Slot "xLevel":
       [1] 1 3 3 3 1 5 5 5 1 8 1 6 6 6 1 8 1 6 6 6 1 8 8 8 1 8 1 7 1 6 6 6 1 7 7 7
      
      Slot "placebo":
      [1] TRUE
      
      Slot "backfilled":
       [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
      [13] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
      [25] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
      
      Slot "response":
       [1] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
      [26] NA NA NA NA NA NA NA NA NA NA NA
      
      Slot "ID":
       [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
      [26] 26 27 28 29 30 31 32 33 34 35 36
      
      Slot "cohort":
       [1]  1  1  1  1  2  2  2  2  3  3  4  4  4  4  5  5  6  6  6  6  7  7  7  7  8
      [26]  8  9  9 10 10 10 10 11 11 11 11
      
      Slot "nObs":
      [1] 36
      
      
      
      Slot "doses":
      [1] 150
      
      Slot "seed":
      [1] 819
      

# simulate-DualResponsesDesign produces consistent results

    Code
      result
    Output
      An object of class "PseudoDualSimulations"
      Slot "fit_eff":
      [[1]]
      [[1]]$theta1
      [1] -4.20662
      
      [[1]]$theta2
      [1] 3.302767
      
      [[1]]$ExpEff
       [1] -1.4520053 -0.3455793  0.2985344  0.6242505  0.8372982  0.9935774
       [7]  1.1159959  1.2160729  1.3003781  1.3729953  1.4366277  1.4931529
      [13]  1.5439249
      
      
      
      Slot "final_gstar_estimates":
      [1] 146.2479
      
      Slot "final_gstar_at_dose_grid":
      [1] 125
      
      Slot "final_gstar_cis":
      [[1]]
      [[1]]$lower
      [1] 75.03529
      
      [[1]]$upper
      [1] 285.0452
      
      
      
      Slot "final_gstar_ratios":
      [1] 3.798815
      
      Slot "final_optimal_dose":
      [1] 137.5996
      
      Slot "final_optimal_dose_at_dose_grid":
      [1] 125
      
      Slot "sigma2_est":
      [1] 0.1616952
      
      Slot "fit":
      [[1]]
      [[1]]$phi1
      [1] -9.998377
      
      [[1]]$phi2
      [1] 1.858333
      
      [[1]]$probDLE
       [1] 0.003270923 0.017694765 0.061310429 0.121847969 0.191477765 0.263906664
       [7] 0.334712519 0.401196078 0.461988954 0.516629367 0.565210679 0.608129165
      [13] 0.645919625
      
      
      
      Slot "final_td_target_during_trial_estimates":
      [1] 155.5827
      
      Slot "final_td_target_end_of_trial_estimates":
      [1] 137.5996
      
      Slot "final_td_target_during_trial_at_dose_grid":
      [1] 150
      
      Slot "final_td_target_end_of_trial_at_dose_grid":
      [1] 125
      
      Slot "final_tdeot_cis":
      [[1]]
      [[1]]$lower
      [1] 92.64601
      
      [[1]]$upper
      [1] 204.3655
      
      
      
      Slot "final_tdeot_ratios":
      [1] 2.205875
      
      Slot "final_cis":
      [[1]]
      [[1]]$lower
      [1] 92.64601
      
      [[1]]$upper
      [1] 204.3655
      
      
      
      Slot "final_ratios":
      [1] 2.205875
      
      Slot "stop_report":
           ≥ 36 patients dosed
      [1,]                TRUE
      
      Slot "stop_reasons":
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
       [1]  10  25  50  75 100 125 150 175 200 225 250 275 300
      
      Slot "nGrid":
      [1] 13
      
      Slot "xLevel":
       [1]  2  2  2  4  4  4  6  6  6 11 11 11  5  5  5  6  6  6  7  7  7  7  7  7  8
      [26]  8  8  6  6  6  6  6  6  6  6  6
      
      Slot "placebo":
      [1] FALSE
      
      Slot "backfilled":
       [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
      [13] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
      [25] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
      
      Slot "response":
       [1] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
      [26] NA NA NA NA NA NA NA NA NA NA NA
      
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
      

# simulate-DualResponsesDesign with sentinel patient and placebo patients produces consistent results

    Code
      result
    Output
      An object of class "PseudoDualSimulations"
      Slot "fit_eff":
      [[1]]
      [[1]]$theta1
      [1] -4.846946
      
      [[1]]$theta2
      [1] 3.706834
      
      [[1]]$ExpEff
       [1] -1.7553268 -0.5135385  0.2093774  0.5749421  0.8140546  0.9894533
       [7]  1.1268487  1.2391693  1.3337885  1.4152899  1.4867072  1.5501478
      [13]  1.6071314
      
      
      
      Slot "final_gstar_estimates":
      [1] 178.1122
      
      Slot "final_gstar_at_dose_grid":
      [1] 175
      
      Slot "final_gstar_cis":
      [[1]]
      [[1]]$lower
      [1] 81.92621
      
      [[1]]$upper
      [1] 387.2257
      
      
      
      Slot "final_gstar_ratios":
      [1] 4.726518
      
      Slot "final_optimal_dose":
      [1] 130.793
      
      Slot "final_optimal_dose_at_dose_grid":
      [1] 125
      
      Slot "sigma2_est":
      [1] 0.1346881
      
      Slot "fit":
      [[1]]
      [[1]]$phi1
      [1] -7.811573
      
      [[1]]$phi2
      [1] 1.428975
      
      [[1]]$probDLE
       [1] 0.01075856 0.03872109 0.09784603 0.16219439 0.22602393 0.28658390
       [7] 0.34265147 0.39383431 0.44018530 0.48198255 0.51960407 0.55345799
      [13] 0.58394503
      
      
      
      Slot "final_td_target_during_trial_estimates":
      [1] 153.4465
      
      Slot "final_td_target_end_of_trial_estimates":
      [1] 130.793
      
      Slot "final_td_target_during_trial_at_dose_grid":
      [1] 150
      
      Slot "final_td_target_end_of_trial_at_dose_grid":
      [1] 125
      
      Slot "final_tdeot_cis":
      [[1]]
      [[1]]$lower
      [1] 70.93066
      
      [[1]]$upper
      [1] 241.1765
      
      
      
      Slot "final_tdeot_ratios":
      [1] 3.400173
      
      Slot "final_cis":
      [[1]]
      [[1]]$lower
      [1] 70.93066
      
      [[1]]$upper
      [1] 241.1765
      
      
      
      Slot "final_ratios":
      [1] 3.400173
      
      Slot "stop_report":
           ≥ 36 patients dosed
      [1,]                TRUE
      
      Slot "stop_reasons":
      [[1]]
      [1] "Number of patients is 39 and thus reached the prespecified minimum number 36"
      
      
      Slot "data":
      [[1]]
      An object of class "DataDual"
      Slot "w":
       [1] -1.8023458 -1.6707191 -0.6793428 -0.4501283 -0.6735681 -2.0136468
       [7] -1.7535448  0.6398290  0.6408798  0.5740029 -1.6755309  1.1168030
      [13] -2.0237164 -1.7082040  0.9423434  0.7059016  0.3463026 -1.5019231
      [19] -1.8509383  0.9338591  0.8681372  0.9163273 -1.7526336 -1.8799615
      [25]  1.0931826  1.3178810  1.5655401 -1.8859884  0.9314330 -1.5938204
      [31] -2.0051962  0.8356701  0.6793477  0.7302074 -1.8777428 -1.7138563
      [37]  0.7856746  0.8318014  0.8677019
      
      Slot "x":
       [1]  10  10  25  25  25  10  10  75  75  75  10 125  10  10 100 100 100  10  10
      [20] 125 125 125  10  10 200 200 200  10 150  10  10 125 125 125  10  10 125 125
      [39] 125
      
      Slot "y":
       [1] 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 1 0 0 0 0 1 0 0 0 0
      [39] 0
      
      Slot "doseGrid":
       [1]  10  25  50  75 100 125 150 175 200 225 250 275 300
      
      Slot "nGrid":
      [1] 13
      
      Slot "xLevel":
       [1] 1 1 2 2 2 1 1 4 4 4 1 6 1 1 5 5 5 1 1 6 6 6 1 1 9 9 9 1 7 1 1 6 6 6 1 1 6 6
      [39] 6
      
      Slot "placebo":
      [1] TRUE
      
      Slot "backfilled":
       [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
      [13] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
      [25] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
      [37] FALSE FALSE FALSE
      
      Slot "response":
       [1] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
      [26] NA NA NA NA NA NA NA NA NA NA NA NA NA NA
      
      Slot "ID":
       [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
      [26] 26 27 28 29 30 31 32 33 34 35 36 37 38 39
      
      Slot "cohort":
       [1] 1 1 1 1 1 2 2 2 2 2 3 3 4 4 4 4 4 5 5 5 5 5 6 6 6 6 6 7 7 8 8 8 8 8 9 9 9 9
      [39] 9
      
      Slot "nObs":
      [1] 39
      
      
      
      Slot "doses":
      [1] 125
      
      Slot "seed":
      [1] 819
      

# simulate-DualResponsesSamplesDesign produces consistent results

    Code
      result
    Output
      An object of class "PseudoDualSimulations"
      Slot "fit_eff":
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
      
      
      Slot "final_gstar_estimates":
      [1] 300
      
      Slot "final_gstar_at_dose_grid":
      [1] 225
      
      Slot "final_gstar_cis":
      [[1]]
      [[1]]$lower
      [1] 300
      
      [[1]]$upper
      [1] 300
      
      
      
      Slot "final_gstar_ratios":
      [1] 1
      
      Slot "final_optimal_dose":
      [1] 62.78087
      
      Slot "final_optimal_dose_at_dose_grid":
      [1] 50
      
      Slot "sigma2_est":
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
      
      
      Slot "final_td_target_during_trial_estimates":
      [1] 95.95589
      
      Slot "final_td_target_end_of_trial_estimates":
      [1] 62.78087
      
      Slot "final_td_target_during_trial_at_dose_grid":
      [1] 75
      
      Slot "final_td_target_end_of_trial_at_dose_grid":
      [1] 50
      
      Slot "final_tdeot_cis":
      [[1]]
      [[1]]$lower
      [1] 62.78087
      
      [[1]]$upper
      [1] 62.78087
      
      
      
      Slot "final_tdeot_ratios":
      [1] 1
      
      Slot "final_cis":
      [[1]]
      [[1]]$lower
      [1] 62.78087
      
      [[1]]$upper
      [1] 62.78087
      
      
      
      Slot "final_ratios":
      [1] 1
      
      Slot "stop_report":
           ≥ 10 patients dosed
      [1,]                TRUE
      
      Slot "stop_reasons":
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
      
      Slot "backfilled":
       [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
      
      Slot "response":
       [1] NA NA NA NA NA NA NA NA NA NA NA NA
      
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
      

# simulate-DualResponsesSamplesDesign with sentinel patient and placebo dose produces consistent results

    Code
      result
    Output
      An object of class "PseudoDualSimulations"
      Slot "fit_eff":
      [[1]]
             middle      lower      upper
      1  -1.7927307 -2.1988188 -1.3773094
      2  -0.3611421 -0.5568119 -0.1536857
      3   0.4722673  0.1194780  0.7475906
      4   0.8937066  0.4437650  1.2278149
      5   1.1693660  0.6573611  1.5405356
      6   1.3715734  0.8181365  1.7800246
      7   1.5299689  0.9440770  1.9717442
      8   1.6594571  1.0470332  2.1297198
      9   1.7685383  1.1337639  2.2657935
      10  1.8624967  1.2084704  2.3830021
      11  1.9448298  1.2739336  2.4857088
      12  2.0179669  1.3320850  2.5769438
      13  2.0836601  1.3843178  2.6588930
      
      
      Slot "final_gstar_estimates":
      [1] 300
      
      Slot "final_gstar_at_dose_grid":
      [1] 150
      
      Slot "final_gstar_cis":
      [[1]]
      [[1]]$lower
      [1] 300
      
      [[1]]$upper
      [1] 300
      
      
      
      Slot "final_gstar_ratios":
      [1] 1
      
      Slot "final_optimal_dose":
      [1] 0.8047576
      
      Slot "final_optimal_dose_at_dose_grid":
      [1] NA
      
      Slot "sigma2_est":
      [1] 0.2497116
      
      Slot "fit":
      [[1]]
            middle      lower     upper
      1  0.2489895 0.04548953 0.3794378
      2  0.2369822 0.05652638 0.3859259
      3  0.2294370 0.06650004 0.4120926
      4  0.2255699 0.07306946 0.4534294
      5  0.2230419 0.07808783 0.4831215
      6  0.2211926 0.08219558 0.5061188
      7  0.2197479 0.08569689 0.5247686
      8  0.2185697 0.08876208 0.5403732
      9  0.2175793 0.08666471 0.5537305
      10 0.2167276 0.08462670 0.5653645
      11 0.2159825 0.08284060 0.5756378
      12 0.2153214 0.08125448 0.5848112
      13 0.2147282 0.07983070 0.5930786
      
      
      Slot "final_td_target_during_trial_estimates":
      [1] 0.1912854
      
      Slot "final_td_target_end_of_trial_estimates":
      [1] 0.8047576
      
      Slot "final_td_target_during_trial_at_dose_grid":
      [1] NA
      
      Slot "final_td_target_end_of_trial_at_dose_grid":
      [1] NA
      
      Slot "final_tdeot_cis":
      [[1]]
      [[1]]$lower
      [1] 0.8047576
      
      [[1]]$upper
      [1] 0.8047576
      
      
      
      Slot "final_tdeot_ratios":
      [1] 1
      
      Slot "final_cis":
      [[1]]
      [[1]]$lower
      [1] 0.8047576
      
      [[1]]$upper
      [1] 0.8047576
      
      
      
      Slot "final_ratios":
      [1] 1
      
      Slot "stop_report":
           ≥ 10 patients dosed
      [1,]                TRUE
      
      Slot "stop_reasons":
      [[1]]
      [1] "Number of patients is 12 and thus reached the prespecified minimum number 10"
      
      
      Slot "data":
      [[1]]
      An object of class "DataDual"
      Slot "w":
       [1] -1.89734159 -0.65882124 -0.65749004 -0.67674379 -1.50644098 -0.54922839
       [7] -0.35100774 -0.53904983 -1.69648885  0.45862411 -0.04987216  0.27220601
      
      Slot "x":
       [1] 10 25 25 25 10 25 25 25 10 50 50 50
      
      Slot "y":
       [1] 0 0 0 0 0 0 0 0 0 0 0 0
      
      Slot "doseGrid":
       [1]  10  25  50  75 100 125 150 175 200 225 250 275 300
      
      Slot "nGrid":
      [1] 13
      
      Slot "xLevel":
       [1] 1 2 2 2 1 2 2 2 1 3 3 3
      
      Slot "placebo":
      [1] TRUE
      
      Slot "backfilled":
       [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
      
      Slot "response":
       [1] NA NA NA NA NA NA NA NA NA NA NA NA
      
      Slot "ID":
       [1]  1  2  3  4  5  6  7  8  9 10 11 12
      
      Slot "cohort":
       [1] 1 1 1 1 2 2 2 2 3 3 3 3
      
      Slot "nObs":
      [1] 12
      
      
      
      Slot "doses":
      [1] NA
      
      Slot "seed":
      [1] 819
      

# simulate-DualResponsesSamplesDesign with EffFlexi model produces consistent results

    Code
      result
    Output
      An object of class "PseudoDualFlexiSimulations"
      Slot "sigma2_beta_w_est":
      [1] 0.01
      
      Slot "fit_eff":
      [[1]]
           middle    lower    upper
      1  20.88591 20.83710 20.94205
      2  20.85839 20.69785 21.01192
      3  20.83216 20.59051 21.08175
      4  20.81341 20.49147 21.07048
      5  20.79872 20.49617 21.09368
      6  20.81479 20.41175 21.12648
      7  20.83060 20.43636 21.19958
      8  20.83387 20.39517 21.21415
      9  20.84242 20.29638 21.27499
      10 20.84877 20.25829 21.28232
      11 20.83598 20.31379 21.35704
      12 20.81260 20.25121 21.30070
      
      
      Slot "final_gstar_estimates":
      [1] 125
      
      Slot "final_gstar_at_dose_grid":
      [1] 75
      
      Slot "final_gstar_cis":
      [[1]]
      [[1]]$lower
      [1] 25
      
      [[1]]$upper
      [1] 300
      
      
      
      Slot "final_gstar_ratios":
      [1] 12
      
      Slot "final_optimal_dose":
      [1] 13.04757
      
      Slot "final_optimal_dose_at_dose_grid":
      [1] NA
      
      Slot "sigma2_est":
      [1] 93.41502
      
      Slot "fit":
      [[1]]
            middle      lower     upper
      1  0.2196747 0.10468249 0.3730683
      2  0.2252279 0.09963802 0.4135189
      3  0.2299009 0.08707117 0.4377731
      4  0.2338269 0.07904299 0.4551712
      5  0.2372058 0.07328804 0.4687442
      6  0.2401740 0.06887499 0.4798695
      7  0.2428232 0.06533712 0.4892919
      8  0.2452175 0.06240985 0.4974603
      9  0.2474033 0.05992987 0.5046666
      10 0.2494150 0.05778993 0.5111113
      11 0.2512791 0.05591609 0.5169381
      12 0.2530165 0.05425546 0.5222536
      
      
      Slot "final_td_target_during_trial_estimates":
      [1] 5.184515
      
      Slot "final_td_target_end_of_trial_estimates":
      [1] 13.04757
      
      Slot "final_td_target_during_trial_at_dose_grid":
      [1] NA
      
      Slot "final_td_target_end_of_trial_at_dose_grid":
      [1] NA
      
      Slot "final_tdeot_cis":
      [[1]]
      [[1]]$lower
      [1] 13.04757
      
      [[1]]$upper
      [1] 13.04757
      
      
      
      Slot "final_tdeot_ratios":
      [1] 1
      
      Slot "final_cis":
      [[1]]
      [[1]]$lower
      [1] 13.04757
      
      [[1]]$upper
      [1] 13.04757
      
      
      
      Slot "final_ratios":
      [1] 1
      
      Slot "stop_report":
           <NA> ≥ 10 patients dosed Stopped because of missing dose
      [1,] TRUE               FALSE                            TRUE
      
      Slot "stop_reasons":
      [[1]]
      [[1]][[1]]
      [1] "Number of patients is 9 and thus below the prespecified minimum number 10"
      
      [[1]][[2]]
      [1] "Next dose is NA , i.e., no active dose is safe enough according to the NextBest rule."
      
      
      
      Slot "data":
      [[1]]
      An object of class "DataDual"
      Slot "w":
      [1] 24.50389 24.50984 24.42373 25.73899 25.01408 26.29193 25.37579 25.21809
      [9] 24.85937
      
      Slot "x":
      [1] 25 25 25 25 25 25 25 25 25
      
      Slot "y":
      [1] 0 0 0 0 0 0 0 0 0
      
      Slot "doseGrid":
       [1]  25  50  75 100 125 150 175 200 225 250 275 300
      
      Slot "nGrid":
      [1] 12
      
      Slot "xLevel":
      [1] 1 1 1 1 1 1 1 1 1
      
      Slot "placebo":
      [1] FALSE
      
      Slot "backfilled":
      [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
      
      Slot "response":
      [1] NA NA NA NA NA NA NA NA NA
      
      Slot "ID":
      [1] 1 2 3 4 5 6 7 8 9
      
      Slot "cohort":
      [1] 1 1 1 2 2 2 3 3 3
      
      Slot "nObs":
      [1] 9
      
      
      
      Slot "doses":
      [1] NA
      
      Slot "seed":
      [1] 819
      

# simulate for DADesign works consistently

    Code
      mySims
    Output
      An object of class "DASimulations"
      Slot "trial_duration":
      [1] 65 35
      
      Slot "fit":
      [[1]]
              middle        lower      upper
      1  0.006795534 1.177486e-05 0.01945189
      2  0.016945935 2.312255e-04 0.04273146
      3  0.025459654 8.331333e-04 0.05966942
      4  0.032496252 1.762380e-03 0.07237336
      5  0.050158979 6.324853e-03 0.10011871
      6  0.080506270 2.243328e-02 0.13997143
      7  0.119130934 5.575416e-02 0.19343845
      8  0.138557745 7.641254e-02 0.21597155
      9  0.158136865 9.913061e-02 0.23646621
      10 0.177821395 1.234807e-01 0.25527596
      11 0.197531630 1.490640e-01 0.27266191
      12 0.217174737 1.755153e-01 0.28882433
      13 0.236656297 2.025072e-01 0.30392125
      14 0.255887168 2.297513e-01 0.31808021
      15 0.274787409 2.476521e-01 0.33216385
      16 0.293288270 2.560464e-01 0.34625575
      17 0.311332903 2.640769e-01 0.35967433
      18 0.328876212 2.717687e-01 0.37246915
      19 0.345884166 2.791435e-01 0.38468374
      20 0.362332795 2.862206e-01 0.39635689
      21 0.378207024 2.927292e-01 0.41107380
      22 0.393499456 2.982505e-01 0.43422599
      23 0.408209186 3.035548e-01 0.45651956
      24 0.422340678 3.086591e-01 0.47794064
      25 0.435902750 3.135779e-01 0.49848655
      26 0.448907662 3.183247e-01 0.51816360
      27 0.461370331 3.229112e-01 0.53698527
      28 0.473307645 3.273478e-01 0.55497056
      29 0.484737896 3.316443e-01 0.57214268
      30 0.495680294 3.358092e-01 0.58852787
      31 0.506154580 3.398504e-01 0.60415450
      32 0.516180702 3.437750e-01 0.61905232
      33 0.525778560 3.475894e-01 0.63325183
      34 0.534967806 3.512998e-01 0.64678374
      35 0.543767687 3.549117e-01 0.65967868
      36 0.552196932 3.584299e-01 0.67196679
      37 0.560273659 3.618593e-01 0.68367757
      38 0.568015322 3.652042e-01 0.69483966
      39 0.575438670 3.684686e-01 0.70548073
      40 0.582559725 3.716561e-01 0.71562741
      41 0.589393772 3.747704e-01 0.72530523
      42 0.595955363 3.778146e-01 0.73453856
      
      [[2]]
              middle        lower      upper
      1  0.002959326 0.0001925712 0.00793995
      2  0.013365051 0.0014466415 0.03592053
      3  0.025253718 0.0034996585 0.06733241
      4  0.036346880 0.0058834275 0.09603331
      5  0.066240215 0.0143202414 0.17029006
      6  0.115871316 0.0346568373 0.28310937
      7  0.169149601 0.0655834157 0.38913298
      8  0.192143876 0.0819108109 0.43009479
      9  0.213337669 0.0985396051 0.46537752
      10 0.233036988 0.1153262149 0.49612700
      11 0.251469522 0.1321587865 0.52319332
      12 0.268809270 0.1489490184 0.54722230
      13 0.285192282 0.1656267332 0.56871433
      14 0.300727105 0.1796005364 0.58806370
      15 0.315501911 0.1921591817 0.60558570
      16 0.329589487 0.2043772720 0.62153563
      17 0.343050802 0.2162654345 0.63612262
      18 0.355937607 0.2278342205 0.64951973
      19 0.368294358 0.2390940332 0.66187144
      20 0.380159656 0.2500550763 0.67329942
      21 0.391567351 0.2607273172 0.68390685
      22 0.402547387 0.2711204612 0.69378190
      23 0.413126465 0.2810438832 0.70320039
      24 0.423328558 0.2905334519 0.71220108
      25 0.433175334 0.2997764153 0.72066252
      26 0.442686483 0.3087822518 0.72863350
      27 0.451879993 0.3175599584 0.73615695
      28 0.460772367 0.3261180819 0.74327075
      29 0.469378810 0.3344647465 0.75000849
      30 0.477713379 0.3426076801 0.75640008
      31 0.485789115 0.3505542383 0.76247226
      32 0.493618148 0.3583114267 0.76824899
      33 0.501211791 0.3658859214 0.77375181
      34 0.508580622 0.3732840885 0.77900019
      35 0.515734547 0.3805120020 0.78401173
      36 0.522682866 0.3875754598 0.78880240
      37 0.529434323 0.3944800001 0.79338674
      38 0.535997151 0.4012309151 0.79777799
      39 0.542379113 0.4078332651 0.80198828
      40 0.548587542 0.4142918906 0.80602871
      41 0.554629367 0.4206114244 0.80990947
      42 0.560511150 0.4267963025 0.81363993
      
      
      Slot "stop_report":
           <NA> P(0.2 ≤ prob(DLE | NBD) ≤ 0.35) ≥ 0.5 ≥ 50 patients dosed
      [1,] TRUE                                  TRUE               FALSE
      [2,] TRUE                                  TRUE               FALSE
      
      Slot "stop_reasons":
      [[1]]
      [[1]][[1]]
      [1] "Probability for target toxicity is 100 % for dose 22 and thus above the required 50 %"
      
      [[1]][[2]]
      [1] "Number of patients is 12 and thus below the prespecified minimum number 50"
      
      
      [[2]]
      [[2]][[1]]
      [1] "Probability for target toxicity is 75 % for dose 28 and thus above the required 50 %"
      
      [[2]][[2]]
      [1] "Number of patients is 5 and thus below the prespecified minimum number 50"
      
      
      
      Slot "additional_stats":
      [[1]]
      list()
      
      [[2]]
      list()
      
      
      Slot "data":
      [[1]]
      An object of class "DataDA"
      Slot "u":
       [1] 60 58 51 44  4 33 27 25  3 15  9  7
      
      Slot "t0":
       [1]  0  7 14 21 28 32 38 40 47 50 56 58
      
      Slot "Tmax":
      [1] 60
      
      Slot "x":
       [1]  3  6 12 24 30 10 10 10 20 18 18 18
      
      Slot "y":
       [1] 0 0 0 0 1 0 0 0 1 0 0 0
      
      Slot "doseGrid":
       [1]  0.1  0.5  1.0  1.5  3.0  6.0 10.0 12.0 14.0 16.0 18.0 20.0 22.0 24.0 26.0
      [16] 28.0 30.0 32.0 34.0 36.0 38.0 40.0 42.0 44.0 46.0 48.0 50.0 52.0 54.0 56.0
      [31] 58.0 60.0 62.0 64.0 66.0 68.0 70.0 72.0 74.0 76.0 78.0 80.0
      
      Slot "nGrid":
      [1] 42
      
      Slot "xLevel":
       [1]  5  6  8 14 17  7  7  7 12 11 11 11
      
      Slot "placebo":
      [1] FALSE
      
      Slot "backfilled":
       [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
      
      Slot "response":
       [1] NA NA NA NA NA NA NA NA NA NA NA NA
      
      Slot "ID":
       [1]  1  2  3  4  5  6  7  8  9 10 11 12
      
      Slot "cohort":
       [1] 1 2 3 4 5 6 6 6 7 8 8 8
      
      Slot "nObs":
      [1] 12
      
      
      [[2]]
      An object of class "DataDA"
      Slot "u":
      [1] 35 28 21  8  7
      
      Slot "t0":
      [1]  0  7 14 21 28
      
      Slot "Tmax":
      [1] 60
      
      Slot "x":
      [1]  3  6 12 24 30
      
      Slot "y":
      [1] 0 0 0 1 0
      
      Slot "doseGrid":
       [1]  0.1  0.5  1.0  1.5  3.0  6.0 10.0 12.0 14.0 16.0 18.0 20.0 22.0 24.0 26.0
      [16] 28.0 30.0 32.0 34.0 36.0 38.0 40.0 42.0 44.0 46.0 48.0 50.0 52.0 54.0 56.0
      [31] 58.0 60.0 62.0 64.0 66.0 68.0 70.0 72.0 74.0 76.0 78.0 80.0
      
      Slot "nGrid":
      [1] 42
      
      Slot "xLevel":
      [1]  5  6  8 14 17
      
      Slot "placebo":
      [1] FALSE
      
      Slot "backfilled":
      [1] FALSE FALSE FALSE FALSE FALSE
      
      Slot "response":
      [1] NA NA NA NA NA
      
      Slot "ID":
      [1] 1 2 3 4 5
      
      Slot "cohort":
      [1] 1 2 3 4 5
      
      Slot "nObs":
      [1] 5
      
      
      
      Slot "doses":
      [1] 22 28
      
      Slot "seed":
      [1] 819
      

# simulate for DADesign with placebo and deescalation works consistently

    Code
      mySims
    Output
      An object of class "DASimulations"
      Slot "trial_duration":
      [1] 67 50
      
      Slot "fit":
      [[1]]
               middle        lower        upper
      1  0.0001762200 7.553063e-08 0.0006516032
      2  0.0009347775 3.886709e-06 0.0034394168
      3  0.0019258584 2.123517e-05 0.0070246090
      4  0.0029507331 5.735124e-05 0.0106511232
      5  0.0062162637 3.135140e-04 0.0216006312
      6  0.0137048782 1.712589e-03 0.0433706774
      7  0.0260722843 5.966773e-03 0.0717091671
      8  0.0334177344 9.298499e-03 0.0855368419
      9  0.0415928555 1.351089e-02 0.0991341688
      10 0.0506203153 1.864532e-02 0.1125045356
      11 0.0605083899 2.473076e-02 0.1256526322
      12 0.0712515880 3.178434e-02 0.1385833180
      13 0.0828314556 3.981178e-02 0.1513010058
      14 0.0952175797 4.880784e-02 0.1638093445
      15 0.1083687882 5.875691e-02 0.1761110859
      16 0.1222345179 6.963359e-02 0.1882080731
      17 0.1367563121 8.140351e-02 0.2001013074
      18 0.1518693986 9.402415e-02 0.2117910655
      19 0.1675042944 1.074458e-01 0.2232770462
      20 0.1835883878 1.216127e-01 0.2345585309
      21 0.2000474488 1.364638e-01 0.2456345454
      22 0.2168070295 1.519342e-01 0.2565040135
      23 0.2337937217 1.679561e-01 0.2671658962
      24 0.2509362522 1.844600e-01 0.2776193122
      25 0.2681664020 2.007264e-01 0.2958685894
      26 0.2854197474 2.170537e-01 0.3173660875
      27 0.3026362246 2.336455e-01 0.3387464339
      28 0.3197605308 2.504411e-01 0.3599286364
      29 0.3367423735 2.673813e-01 0.3808409075
      30 0.3535365862 2.844092e-01 0.4014205873
      31 0.3701031292 3.014709e-01 0.4216138868
      32 0.3864069946 3.185155e-01 0.4413754893
      33 0.4024180339 3.354956e-01 0.4606680474
      34 0.4181107259 3.523672e-01 0.4794616052
      35 0.4334639010 3.690904e-01 0.4977329774
      36 0.4484604357 3.772825e-01 0.5154651061
      37 0.4630869288 3.851044e-01 0.5326464168
      38 0.4773333709 3.927669e-01 0.5492701884
      39 0.4911928139 4.002723e-01 0.5653339489
      40 0.5046610480 4.076230e-01 0.5808389043
      41 0.5177362909 4.148213e-01 0.5957894091
      42 0.5304188921 4.218699e-01 0.6101924774
      
      [[2]]
              middle        lower      upper
      1  0.004441762 1.126733e-05 0.01612135
      2  0.012169947 1.190495e-04 0.04237589
      3  0.018991501 3.587037e-04 0.06358200
      4  0.024796962 7.092231e-04 0.08024352
      5  0.039844027 2.441078e-03 0.11819951
      6  0.066437075 9.156885e-03 0.17111332
      7  0.100327706 2.526108e-02 0.22158159
      8  0.117257358 3.641191e-02 0.24216566
      9  0.134282263 4.956632e-02 0.26065130
      10 0.151416122 6.462851e-02 0.27746562
      11 0.168647098 8.146937e-02 0.29290844
      12 0.185947519 9.993222e-02 0.30719981
      13 0.203279698 1.198390e-01 0.32050695
      14 0.220599852 1.409967e-01 0.33296048
      15 0.237860986 1.632036e-01 0.34466478
      16 0.255015133 1.862555e-01 0.35570484
      17 0.272015120 2.099506e-01 0.36615101
      18 0.288815958 2.340947e-01 0.37606230
      19 0.305375898 2.585045e-01 0.38548880
      20 0.321657184 2.830106e-01 0.39447352
      21 0.337626550 3.074594e-01 0.40305367
      22 0.353255474 3.258279e-01 0.41126175
      23 0.368520246 3.409600e-01 0.41944888
      24 0.383401871 3.552446e-01 0.42777280
      25 0.397885854 3.691399e-01 0.43579337
      26 0.411961893 3.826507e-01 0.44352440
      27 0.425623516 3.957830e-01 0.45097881
      28 0.438867679 4.085437e-01 0.47135923
      29 0.451694359 4.209406e-01 0.49133025
      30 0.464106143 4.329819e-01 0.51059629
      31 0.476107838 4.446764e-01 0.52914666
      32 0.487706103 4.560331e-01 0.54697822
      33 0.498909111 4.670612e-01 0.56409409
      34 0.509726243 4.777701e-01 0.58050262
      35 0.520167816 4.880846e-01 0.59630081
      36 0.530244846 4.978019e-01 0.61171694
      37 0.539968841 5.031360e-01 0.62646155
      38 0.549351624 5.078740e-01 0.64055540
      39 0.558405182 5.124803e-01 0.65402071
      40 0.567141544 5.169610e-01 0.66688062
      41 0.575572676 5.213218e-01 0.67915887
      42 0.583710397 5.255679e-01 0.69087941
      
      
      Slot "stop_report":
           <NA> P(0.2 ≤ prob(DLE | NBD) ≤ 0.35) ≥ 0.5 ≥ 50 patients dosed
      [1,] TRUE                                  TRUE               FALSE
      [2,] TRUE                                  TRUE               FALSE
      
      Slot "stop_reasons":
      [[1]]
      [[1]][[1]]
      [1] "Probability for target toxicity is 75 % for dose 38 and thus above the required 50 %"
      
      [[1]][[2]]
      [1] "Number of patients is 12 and thus below the prespecified minimum number 50"
      
      
      [[2]]
      [[2]][[1]]
      [1] "Probability for target toxicity is 75 % for dose 30 and thus above the required 50 %"
      
      [[2]][[2]]
      [1] "Number of patients is 8 and thus below the prespecified minimum number 50"
      
      
      
      Slot "additional_stats":
      [[1]]
      list()
      
      [[2]]
      list()
      
      
      Slot "data":
      [[1]]
      An object of class "DataDA"
      Slot "u":
       [1] 60 60 54 54 41 41 28 28 15 15  9  2
      
      Slot "t0":
       [1]  0  0 13 13 26 26 39 39 52 52 58 60
      
      Slot "Tmax":
      [1] 60
      
      Slot "x":
       [1]  0.1  3.0  0.1  6.0  0.1 12.0  0.1 24.0  0.1 30.0 30.0 30.0
      
      Slot "y":
       [1] 0 0 0 0 0 0 0 0 0 0 0 1
      
      Slot "doseGrid":
       [1]  0.1  0.5  1.0  1.5  3.0  6.0 10.0 12.0 14.0 16.0 18.0 20.0 22.0 24.0 26.0
      [16] 28.0 30.0 32.0 34.0 36.0 38.0 40.0 42.0 44.0 46.0 48.0 50.0 52.0 54.0 56.0
      [31] 58.0 60.0 62.0 64.0 66.0 68.0 70.0 72.0 74.0 76.0 78.0 80.0
      
      Slot "nGrid":
      [1] 42
      
      Slot "xLevel":
       [1]  1  5  1  6  1  8  1 14  1 17 17 17
      
      Slot "placebo":
      [1] TRUE
      
      Slot "backfilled":
       [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
      
      Slot "response":
       [1] NA NA NA NA NA NA NA NA NA NA NA NA
      
      Slot "ID":
       [1]  1  2  3  4  5  6  7  8  9 10 11 12
      
      Slot "cohort":
       [1]  1  2  3  4  5  6  7  8  9 10 10 10
      
      Slot "nObs":
      [1] 12
      
      
      [[2]]
      An object of class "DataDA"
      Slot "u":
      [1] 50 50 37 37 24 24 11  5
      
      Slot "t0":
      [1]  0  0 13 13 26 26 39 39
      
      Slot "Tmax":
      [1] 60
      
      Slot "x":
      [1]  0.1  3.0  0.1  6.0  0.1 12.0  0.1 24.0
      
      Slot "y":
      [1] 0 0 0 0 0 0 0 1
      
      Slot "doseGrid":
       [1]  0.1  0.5  1.0  1.5  3.0  6.0 10.0 12.0 14.0 16.0 18.0 20.0 22.0 24.0 26.0
      [16] 28.0 30.0 32.0 34.0 36.0 38.0 40.0 42.0 44.0 46.0 48.0 50.0 52.0 54.0 56.0
      [31] 58.0 60.0 62.0 64.0 66.0 68.0 70.0 72.0 74.0 76.0 78.0 80.0
      
      Slot "nGrid":
      [1] 42
      
      Slot "xLevel":
      [1]  1  5  1  6  1  8  1 14
      
      Slot "placebo":
      [1] TRUE
      
      Slot "backfilled":
      [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
      
      Slot "response":
      [1] NA NA NA NA NA NA NA NA
      
      Slot "ID":
      [1] 1 2 3 4 5 6 7 8
      
      Slot "cohort":
      [1] 1 2 3 4 5 6 7 8
      
      Slot "nObs":
      [1] 8
      
      
      
      Slot "doses":
      [1] 38 30
      
      Slot "seed":
      [1] 819
      

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
    AAKAAAAA/////wAABAIAAAP/AAAAEAAAAAEABAAJAAAADXRhcmdldF90aHJlc2gAAAD+AAAE
    AgAAA/8AAAAQAAAABQAEAAkAAAAGdGFyZ2V0AAQACQAAAAhvdmVyZG9zZQAEAAkAAAARbWF4
    X292ZXJkb3NlX3Byb2IABAAJAAAAD3RhcmdldF9yZWxhdGl2ZQAEAAkAAAANdGFyZ2V0X3Ro
    cmVzaAAABAIAAAH/AAAAEAAAAAIABAAJAAAAGHRibF9OZXh0QmVzdER1YWxFbmRwb2ludAAE
    AAkAAAAEbGlzdAAAAP4AAAMTAAAAAgAAAxMAAAADAAAADgAAAAIAAAAAAAAAAEA+AAAAAAAA
    AAAADgAAAAJAPgAAAAAAAH/wAAAAAAAAAAAADQAAAAIAAAABAAAAAwAABAIAAAP/AAAAEAAA
    AAMABAAJAAAAA21pbgAEAAkAAAADbWF4AAQACQAAAAtjb2hvcnRfc2l6ZQAABAIAAAL/AAAA
    DQAAAAKAAAAA/////gAABAIAAAH/AAAAEAAAAAQABAAJAAAAE3RibF9Db2hvcnRTaXplUmFu
    Z2UABAAJAAAABnRibF9kZgAEAAkAAAADdGJsAAQACQAAAApkYXRhLmZyYW1lAAAA/gAAAxMA
    AAADAAAADgAAAAIAAAAAAAAAAD/wAAAAAAAAAAAADgAAAAI/8AAAAAAAAH/wAAAAAAAAAAAA
    DQAAAAIAAAABAAAAAwAABAIAAAP/AAAAEAAAAAMABAAJAAAAA21pbgAEAAkAAAADbWF4AAQA
    CQAAAAtjb2hvcnRfc2l6ZQAABAIAAAL/AAAADQAAAAKAAAAA/////gAABAIAAAH/AAAAEAAA
    AAQABAAJAAAAEXRibF9Db2hvcnRTaXplRExUAAQACQAAAAZ0YmxfZGYABAAJAAAAA3RibAAE
    AAkAAAAKZGF0YS5mcmFtZQAAAP4AAAQCAAAB/wAAABAAAAADAAQACQAAABF0YmxfQ29ob3J0
    U2l6ZU1heAAEAAkAAAARdGJsX0NvaG9ydFNpemVNYXgABAAJAAAABGxpc3QAAAD+AAADEwAA
    AAEAAAAOAAAAAUAIAAAAAAAAAAAEAgAAAf8AAAAQAAAAAwAEAAkAAAAGdGJsX2RmAAQACQAA
    AAN0YmwABAAJAAAACmRhdGEuZnJhbWUAAAQCAAAC/wAAAA0AAAACgAAAAP////8AAAQCAAAD
    /wAAABAAAAABAAQACQAAAAxzdGFydGluZ0Rvc2UAAAD+AAAEAgAAA/8AAAAQAAAACQAEAAkA
    AAAFbW9kZWwABAAJAAAABGRhdGEABAAJAAAACHN0b3BwaW5nAAQACQAAAAppbmNyZW1lbnRz
    AAQACQAAAA5wbF9jb2hvcnRfc2l6ZQAEAAkAAAAIYmFja2ZpbGwABAAJAAAACG5leHRCZXN0
    AAQACQAAAAtjb2hvcnRfc2l6ZQAEAAkAAAAMc3RhcnRpbmdEb3NlAAAEAgAAAf8AAAAQAAAA
    AgAEAAkAAAAOdGJsX0R1YWxEZXNpZ24ABAAJAAAABGxpc3QAAAD+

