# summary-Simulations works correctly

    Code
      result
    Output
      Summary of 1 simulations
      
      Target toxicity interval was 20, 35 %
      Target dose interval corresponding to this was NA, NA 
      Intervals are corresponding to 10 and 90 % quantiles
      
      Number of patients overall : mean 13 (13, 13) 
      Number of patients treated above target tox interval : mean 13 (13, 13) 
      Proportions of DLTs in the trials : mean 23 % (23 %, 23 %) 
      Mean toxicity risks for the patients on active : mean 100 % (100 %, 100 %) 
      Doses selected as MTD : mean 25 (25, 25) 
      True toxicity at doses selected : mean 100 % (100 %, 100 %) 
      Proportion of trials selecting target MTD: 0 %
      Dose most often selected as MTD: 25 
      Observed toxicity rate at dose most often selected: 33 %
      Fitted toxicity rate at dose most often selected : mean 26 % (26 %, 26 %) 
      Stop reason triggered:
       ≥ 3 cohorts dosed :  100 %
       P(0.2 ≤ prob(DLE | NBD) ≤ 0.35) ≥ 0.5 :  100 %
       ≥ 20 patients dosed :  0 %

# summary-DualSimulations works correctly

    Code
      result
    Output
      Summary of 1 simulations
      
      Target toxicity interval was 20, 35 %
      Target dose interval corresponding to this was NA, NA 
      Intervals are corresponding to 10 and 90 % quantiles
      
      Number of patients overall : mean 12 (12, 12) 
      Number of patients treated above target tox interval : mean 0 (0, 0) 
      Proportions of DLTs in the trials : mean 0 % (0 %, 0 %) 
      Mean toxicity risks for the patients on active : mean 0 % (0 %, 0 %) 
      Doses selected as MTD : mean 1 (1, 1) 
      True toxicity at doses selected : mean 0 % (0 %, 0 %) 
      Proportion of trials selecting target MTD: 0 %
      Dose most often selected as MTD: 1 
      Observed toxicity rate at dose most often selected: 0 %
      Fitted toxicity rate at dose most often selected : mean 8 % (8 %, 8 %) 
      Stop reason triggered:
       P(0.9 ≤ Biomarker ≤ 1) ≥ 0.5 (relative) :  0 %
       ≥ 10 patients dosed :  100 %
       Stopped because of missing dose :  0 %
      Fitted biomarker level at dose most often selected : mean 0.2 (0.2, 0.2) 

# summary-PseudoSimulations works correctly

    Code
      result
    Output
      Summary of 2 simulations
      
      Target probability of DLE p(DLE) used at the end of a trial was 30 %
      The dose level corresponds to the target p(DLE) used at the end of a trial, TDEOT, was 152.6195 
      TDEOT at dose Grid was 150 
      Target p(DLE) used during a trial was 35 %
      The dose level corresponds to the target p(DLE) used during a trial, TDDT, was 155.972 
      TDDT at dose Grid was 150 
      Number of patients overall : mean 6 (6, 6) 
      Number of patients treated above the target p(DLE) used at the end of a trial : mean 0 (0, 0) 
      Number of patients treated above the target p(DLE) used during a trial : mean 0 (0, 0) 
      Proportions of observed DLT in the trials : mean 0 % (0 %, 0 %) 
      Mean toxicity risks for the patients : mean 0 % (0 %, 0 %) 
      Doses selected as TDEOT : mean 100 (100, 100) 
      True toxicity at TDEOT : mean 1 % (1 %, 1 %) 
      Proportion of trials selecting the TDEOT: 0 %
      Proportion of trials selecting the TDDT: 0 %
      Dose most often selected as TDEOT: 100 
      Observed toxicity rate at dose most often selected: NaN %
      Fitted probabilities of DLE at dose most often selected : mean 28 % (28 %, 28 %) 
      The summary table of the final TDEOT across all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
         111.3   111.3   111.3   111.3   111.3   111.3  
      The summary table of the final ratios of the TDEOT across all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
         32.13   32.13   32.13   32.13   32.13   32.13  
      The summary table of the final TDDT across all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
         146.1   146.1   146.1   146.1   146.1   146.1  
      The summary table of dose levels, the optimal dose
       to recommend for subsequent study across all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
         111.3   111.3   111.3   111.3   111.3   111.3  
      The summary table of the final ratios of the optimal dose for stopping across
                        all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
         32.13   32.13   32.13   32.13   32.13   32.13  
      
      Stop reason triggered:
       ≥ 6 patients dosed :  100 %

