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
      

# tidy-DualDesign works correctly

    Code
      result
    Output
      $model
      $sigma2betaW
      # A tibble: 1 x 1
        sigma2betaW
              <dbl>
      1        0.01
      
      $rw1
      # A tibble: 1 x 1
        rw1  
        <lgl>
      1 TRUE 
      
      $betaZ_params
      # A tibble: 2 x 3
         mean cov[,1]  [,2] prec[,1]  [,2]
        <dbl>   <dbl> <dbl>    <dbl> <dbl>
      1     0       1     0        1     0
      2     1       0     1        0     1
      
      $ref_dose
      # A tibble: 1 x 1
        ref_dose  
        <pstv_nmb>
      1 1         
      
      $use_log_dose
      # A tibble: 1 x 1
        use_log_dose
        <lgl>       
      1 FALSE       
      
      $sigma2W
      # A tibble: 2 x 1
        sigma2W
          <dbl>
      1     0.1
      2     0.1
      
      $rho
      # A tibble: 2 x 1
          rho
        <dbl>
      1     1
      2     1
      
      $use_fixed
      # A tibble: 3 x 1
        use_fixed
        <lgl>    
      1 FALSE    
      2 FALSE    
      3 TRUE     
      
      $datanames
      # A tibble: 5 x 1
        datanames
        <chr>    
      1 nObs     
      2 w        
      3 x        
      4 xLevel   
      5 y        
      
      $datanames_prior
      # A tibble: 2 x 1
        datanames_prior
        <chr>          
      1 nGrid          
      2 doseGrid       
      
      $sample
      # A tibble: 5 x 1
        sample
        <chr> 
      1 betaZ 
      2 precW 
      3 rho   
      4 betaW 
      5 delta 
      
      attr(,"class")
      [1] "tbl_DualEndpointRW" "list"              
      
      $data
      $w
      # A tibble: 0 x 1
      # ... with 1 variable: w <dbl>
      
      $x
      # A tibble: 0 x 1
      # ... with 1 variable: x <dbl>
      
      $y
      # A tibble: 0 x 1
      # ... with 1 variable: y <int>
      
      $doseGrid
      # A tibble: 11 x 1
         doseGrid
            <dbl>
       1        1
       2        3
       3        5
       4       10
       5       15
       6       20
       7       25
       8       40
       9       50
      10       80
      11      100
      
      $nGrid
      # A tibble: 1 x 1
        nGrid
        <int>
      1    11
      
      $xLevel
      # A tibble: 0 x 1
      # ... with 1 variable: xLevel <int>
      
      $placebo
      # A tibble: 1 x 1
        placebo
        <lgl>  
      1 FALSE  
      
      $ID
      # A tibble: 0 x 1
      # ... with 1 variable: ID <int>
      
      $cohort
      # A tibble: 0 x 1
      # ... with 1 variable: cohort <int>
      
      $nObs
      # A tibble: 1 x 1
         nObs
        <int>
      1     0
      
      attr(,"class")
      [1] "tbl_DataDual" "list"        
      
      $stopping
      $stop_list
      $stop_list[[1]]
      $target
      # A tibble: 2 x 1
        target
         <dbl>
      1    0.9
      2    1  
      
      $is_relative
      # A tibble: 1 x 1
        is_relative
        <lgl>      
      1 TRUE       
      
      $prob
      # A tibble: 1 x 1
         prob
        <dbl>
      1   0.5
      
      $report_label
      # A tibble: 1 x 1
        report_label
        <chr>       
      1 <NA>        
      
      attr(,"class")
      [1] "tbl_StoppingTargetBiomarker" "list"                       
      
      $stop_list[[2]]
      # A tibble: 1 x 2
        nPatients report_label
            <int> <chr>       
      1        40 <NA>        
      
      
      $report_label
      # A tibble: 1 x 1
        report_label
        <chr>       
      1 <NA>        
      
      attr(,"class")
      [1] "tbl_StoppingAny" "list"           
      
      $increments
      # A tibble: 2 x 3
          min   max increment
        <dbl> <dbl>     <dbl>
      1     0    20      1   
      2    20   Inf      0.33
      
      $pl_cohort_size
      # A tibble: 1 x 1
         size
        <int>
      1     0
      
      $nextBest
      $target
      # A tibble: 2 x 1
        target
         <dbl>
      1    0.9
      2    1  
      
      $overdose
      # A tibble: 2 x 1
        overdose
           <dbl>
      1     0.35
      2     1   
      
      $max_overdose_prob
      # A tibble: 1 x 1
        max_overdose_prob
                    <dbl>
      1              0.25
      
      $target_relative
      # A tibble: 1 x 1
        target_relative
        <lgl>          
      1 TRUE           
      
      $target_thresh
      # A tibble: 1 x 1
        target_thresh
                <dbl>
      1          0.01
      
      attr(,"class")
      [1] "tbl_NextBestDualEndpoint" "list"                    
      
      $cohort_size
      [[1]]
      # A tibble: 2 x 3
          min   max cohort_size
        <dbl> <dbl>       <int>
      1     0    30           1
      2    30   Inf           3
      
      [[2]]
      # A tibble: 2 x 3
          min   max cohort_size
        <dbl> <dbl>       <int>
      1     0     1           1
      2     1   Inf           3
      
      attr(,"class")
      [1] "tbl_CohortSizeMax" "tbl_CohortSizeMax" "list"             
      
      $startingDose
      # A tibble: 1 x 1
        startingDose
               <dbl>
      1            3
      
      attr(,"class")
      [1] "tbl_DualDesign" "list"          

