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

# show-GeneralSimulationsSummary works correctly

    Code
      show(simSummary)
    Output
      Summary of 2 simulations
      
      Target toxicity interval was 20, 35 %
      Target dose interval corresponding to this was 19.6, 21.6 
      Intervals are corresponding to 10 and 90 % quantiles
      
      Number of patients overall : mean 6 (6, 6) 
      Number of patients treated above target tox interval : mean 0 (0, 0) 
      Proportions of DLTs in the trials : mean 0 % (0 %, 0 %) 
      Mean toxicity risks for the patients on active : mean 0 % (0 %, 0 %) 
      Doses selected as MTD : mean 10 (10, 10) 
      True toxicity at doses selected : mean 0 % (0 %, 0 %) 
      Proportion of trials selecting target MTD: 0 %
      Dose most often selected as MTD: 10 
      Observed toxicity rate at dose most often selected: NaN %
      Fitted toxicity rate at dose most often selected : mean 6 % (6 %, 6 %) 
      Stop reason triggered:
       ≥ 6 patients dosed :  100 %

# show-SimulationsSummary works correctly

    Code
      show(simSummary)
    Output
      Summary of 2 simulations
      
      Target toxicity interval was 20, 35 %
      Target dose interval corresponding to this was 19.6, 21.6 
      Intervals are corresponding to 10 and 90 % quantiles
      
      Number of patients overall : mean 6 (6, 6) 
      Number of patients treated above target tox interval : mean 0 (0, 0) 
      Proportions of DLTs in the trials : mean 0 % (0 %, 0 %) 
      Mean toxicity risks for the patients on active : mean 0 % (0 %, 0 %) 
      Doses selected as MTD : mean 10 (10, 10) 
      True toxicity at doses selected : mean 0 % (0 %, 0 %) 
      Proportion of trials selecting target MTD: 0 %
      Dose most often selected as MTD: 10 
      Observed toxicity rate at dose most often selected: NaN %
      Fitted toxicity rate at dose most often selected : mean 6 % (6 %, 6 %) 
      Stop reason triggered:
       ≥ 6 patients dosed :  100 %

# show-DualSimulationsSummary works correctly

    Code
      show(simSummary)
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

# show-PseudoSimulationsSummary works correctly

    Code
      show(pseudo_summary)
    Output
      Summary of 1 simulations
      
      Target probability of DLE p(DLE) used at the end of a trial was 30 %
      The dose level corresponds to the target p(DLE) used at the end of a trial, TDEOT, was 43.05404 
      TDEOT at dose Grid was 25 
      Target p(DLE) used during a trial was 35 %
      The dose level corresponds to the target p(DLE) used during a trial, TDDT, was 47.61922 
      TDDT at dose Grid was 25 
      Number of patients overall : mean 12 (12, 12) 
      Number of patients treated above the target p(DLE) used at the end of a trial : mean 6 (6, 6) 
      Number of patients treated above the target p(DLE) used during a trial : mean 6 (6, 6) 
      Proportions of observed DLT in the trials : mean 0 % (0 %, 0 %) 
      Mean toxicity risks for the patients : mean 34 % (34 %, 34 %) 
      Doses selected as TDEOT : mean 75 (75, 75) 
      True toxicity at TDEOT : mean 68 % (68 %, 68 %) 
      Proportion of trials selecting the TDEOT: 0 %
      Proportion of trials selecting the TDDT: 0 %
      Dose most often selected as TDEOT: 75 
      Observed toxicity rate at dose most often selected: 0 %
      Fitted probabilities of DLE at dose most often selected : mean 21 % (21 %, 21 %) 
      The summary table of the final TDEOT across all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
         86.99   86.99   86.99   86.99   86.99   86.99  
      The summary table of the final ratios of the TDEOT across all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
         47654   47654   47654   47654   47654   47654  
      The summary table of the final TDDT across all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
         105.5   105.5   105.5   105.5   105.5   105.5  
      The summary table of dose levels, the optimal dose
       to recommend for subsequent study across all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
         86.99   86.99   86.99   86.99   86.99   86.99  
      The summary table of the final ratios of the optimal dose for stopping across
                        all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
         47654   47654   47654   47654   47654   47654  
      
      Stop reason triggered:
       ≥ 12 patients dosed :  100 %

# show-PseudoSimulationsSummary produces expected output format

    Code
      show(pseudo_summary)
    Output
      Summary of 1 simulations
      
      Target probability of DLE p(DLE) used at the end of a trial was 30 %
      The dose level corresponds to the target p(DLE) used at the end of a trial, TDEOT, was 43.05404 
      TDEOT at dose Grid was 25 
      Target p(DLE) used during a trial was 35 %
      The dose level corresponds to the target p(DLE) used during a trial, TDDT, was 47.61922 
      TDDT at dose Grid was 25 
      Number of patients overall : mean 12 (12, 12) 
      Number of patients treated above the target p(DLE) used at the end of a trial : mean 6 (6, 6) 
      Number of patients treated above the target p(DLE) used during a trial : mean 6 (6, 6) 
      Proportions of observed DLT in the trials : mean 0 % (0 %, 0 %) 
      Mean toxicity risks for the patients : mean 34 % (34 %, 34 %) 
      Doses selected as TDEOT : mean 75 (75, 75) 
      True toxicity at TDEOT : mean 68 % (68 %, 68 %) 
      Proportion of trials selecting the TDEOT: 0 %
      Proportion of trials selecting the TDDT: 0 %
      Dose most often selected as TDEOT: 75 
      Observed toxicity rate at dose most often selected: 0 %
      Fitted probabilities of DLE at dose most often selected : mean 21 % (21 %, 21 %) 
      The summary table of the final TDEOT across all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
         86.99   86.99   86.99   86.99   86.99   86.99  
      The summary table of the final ratios of the TDEOT across all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
         47654   47654   47654   47654   47654   47654  
      The summary table of the final TDDT across all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
         105.5   105.5   105.5   105.5   105.5   105.5  
      The summary table of dose levels, the optimal dose
       to recommend for subsequent study across all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
         86.99   86.99   86.99   86.99   86.99   86.99  
      The summary table of the final ratios of the optimal dose for stopping across
                        all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
         47654   47654   47654   47654   47654   47654  
      
      Stop reason triggered:
       ≥ 12 patients dosed :  100 %

# summary-PseudoDualSimulations works correctly

    Code
      result
    Output
      Summary of 1 simulations
      
      Target probability of DLE p(DLE) used at the end of a trial was 30 %
      The dose level corresponds to the target p(DLE) used at the end of a trial, TDEOT, was 43.05404 
      TDEOT at dose Grid was 25 
      Target p(DLE) used during a trial was 35 %
      The dose level corresponds to the target p(DLE) used during a trial, TDDT, was 47.61922 
      TDDT at dose Grid was 25 
      Number of patients overall : mean 12 (12, 12) 
      Number of patients treated above the target p(DLE) used at the end of a trial : mean 9 (9, 9) 
      Number of patients treated above the target p(DLE) used during a trial : mean 9 (9, 9) 
      Proportions of observed DLT in the trials : mean 25 % (25 %, 25 %) 
      Mean toxicity risks for the patients : mean 70 % (70 %, 70 %) 
      Doses selected as TDEOT : mean 100 (100, 100) 
      True toxicity at TDEOT : mean 88 % (88 %, 88 %) 
      Proportion of trials selecting the TDEOT: 0 %
      Proportion of trials selecting the TDDT: 0 %
      Dose most often selected as TDEOT: 100 
      Observed toxicity rate at dose most often selected: NaN %
      Fitted probabilities of DLE at dose most often selected : mean 30 % (30 %, 30 %) 
      The summary table of the final TDEOT across all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
         100.6   100.6   100.6   100.6   100.6   100.6  
      The summary table of the final ratios of the TDEOT across all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
          6.24    6.24    6.24    6.24    6.24    6.24  
      The summary table of the final TDDT across all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
         119.5   119.5   119.5   119.5   119.5   119.5  
      The summary table of dose levels, the optimal dose
       to recommend for subsequent study across all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
         100.6   100.6   100.6   100.6   100.6   100.6  
      The summary table of the final ratios of the optimal dose for stopping across
                        all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
          6.24    6.24    6.24    6.24    6.24    6.24  
      
      Stop reason triggered:
       ≥ 12 patients dosed :  100 %
      Target Gstar, the dose which gives the maximum gain value was 36.0307 
      Target Gstar at dose Grid was 25 
      The summary table of the final Gstar across all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
         151.9   151.9   151.9   151.9   151.9   151.9  
      The summary table of the final ratios of the Gstar across all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
         7.505   7.505   7.505   7.505   7.505   7.505  
      The summary table of dose levels, the optimal dose
       to recommend for subsequent study across all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
         100.6   100.6   100.6   100.6   100.6   100.6  
      The summary table of the final ratios of the optimal dose for stopping across
              all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
          6.24    6.24    6.24    6.24    6.24    6.24  
      Fitted expected efficacy level at dose most often selected : mean 1 (1, 1) 
      Stop reason triggered:
       ≥ 12 patients dosed :  100 %

---

    Code
      result_custom
    Output
      Summary of 1 simulations
      
      Target probability of DLE p(DLE) used at the end of a trial was 25 %
      The dose level corresponds to the target p(DLE) used at the end of a trial, TDEOT, was 38.02775 
      TDEOT at dose Grid was 25 
      Target p(DLE) used during a trial was 30 %
      The dose level corresponds to the target p(DLE) used during a trial, TDDT, was 43.05404 
      TDDT at dose Grid was 25 
      Number of patients overall : mean 12 (12, 12) 
      Number of patients treated above the target p(DLE) used at the end of a trial : mean 9 (9, 9) 
      Number of patients treated above the target p(DLE) used during a trial : mean 9 (9, 9) 
      Proportions of observed DLT in the trials : mean 25 % (25 %, 25 %) 
      Mean toxicity risks for the patients : mean 70 % (70 %, 70 %) 
      Doses selected as TDEOT : mean 100 (100, 100) 
      True toxicity at TDEOT : mean 88 % (88 %, 88 %) 
      Proportion of trials selecting the TDEOT: 0 %
      Proportion of trials selecting the TDDT: 0 %
      Dose most often selected as TDEOT: 100 
      Observed toxicity rate at dose most often selected: NaN %
      Fitted probabilities of DLE at dose most often selected : mean 30 % (30 %, 30 %) 
      The summary table of the final TDEOT across all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
         100.6   100.6   100.6   100.6   100.6   100.6  
      The summary table of the final ratios of the TDEOT across all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
          6.24    6.24    6.24    6.24    6.24    6.24  
      The summary table of the final TDDT across all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
         119.5   119.5   119.5   119.5   119.5   119.5  
      The summary table of dose levels, the optimal dose
       to recommend for subsequent study across all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
         100.6   100.6   100.6   100.6   100.6   100.6  
      The summary table of the final ratios of the optimal dose for stopping across
                        all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
          6.24    6.24    6.24    6.24    6.24    6.24  
      
      Stop reason triggered:
       ≥ 12 patients dosed :  100 %
      Target Gstar, the dose which gives the maximum gain value was 36.0307 
      Target Gstar at dose Grid was 25 
      The summary table of the final Gstar across all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
         151.9   151.9   151.9   151.9   151.9   151.9  
      The summary table of the final ratios of the Gstar across all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
         7.505   7.505   7.505   7.505   7.505   7.505  
      The summary table of dose levels, the optimal dose
       to recommend for subsequent study across all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
         100.6   100.6   100.6   100.6   100.6   100.6  
      The summary table of the final ratios of the optimal dose for stopping across
              all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
          6.24    6.24    6.24    6.24    6.24    6.24  
      Fitted expected efficacy level at dose most often selected : mean 1 (1, 1) 
      Stop reason triggered:
       ≥ 12 patients dosed :  100 %

# summary-PseudoDualSimulations produces expected structure

    Code
      result
    Output
      Summary of 1 simulations
      
      Target probability of DLE p(DLE) used at the end of a trial was 30 %
      The dose level corresponds to the target p(DLE) used at the end of a trial, TDEOT, was 43.05404 
      TDEOT at dose Grid was 25 
      Target p(DLE) used during a trial was 35 %
      The dose level corresponds to the target p(DLE) used during a trial, TDDT, was 47.61922 
      TDDT at dose Grid was 25 
      Number of patients overall : mean 12 (12, 12) 
      Number of patients treated above the target p(DLE) used at the end of a trial : mean 9 (9, 9) 
      Number of patients treated above the target p(DLE) used during a trial : mean 9 (9, 9) 
      Proportions of observed DLT in the trials : mean 25 % (25 %, 25 %) 
      Mean toxicity risks for the patients : mean 70 % (70 %, 70 %) 
      Doses selected as TDEOT : mean 100 (100, 100) 
      True toxicity at TDEOT : mean 88 % (88 %, 88 %) 
      Proportion of trials selecting the TDEOT: 0 %
      Proportion of trials selecting the TDDT: 0 %
      Dose most often selected as TDEOT: 100 
      Observed toxicity rate at dose most often selected: NaN %
      Fitted probabilities of DLE at dose most often selected : mean 30 % (30 %, 30 %) 
      The summary table of the final TDEOT across all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
         100.6   100.6   100.6   100.6   100.6   100.6  
      The summary table of the final ratios of the TDEOT across all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
          6.24    6.24    6.24    6.24    6.24    6.24  
      The summary table of the final TDDT across all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
         119.5   119.5   119.5   119.5   119.5   119.5  
      The summary table of dose levels, the optimal dose
       to recommend for subsequent study across all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
         100.6   100.6   100.6   100.6   100.6   100.6  
      The summary table of the final ratios of the optimal dose for stopping across
                        all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
          6.24    6.24    6.24    6.24    6.24    6.24  
      
      Stop reason triggered:
       ≥ 12 patients dosed :  100 %
      Target Gstar, the dose which gives the maximum gain value was 36.0307 
      Target Gstar at dose Grid was 25 
      The summary table of the final Gstar across all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
         151.9   151.9   151.9   151.9   151.9   151.9  
      The summary table of the final ratios of the Gstar across all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
         7.505   7.505   7.505   7.505   7.505   7.505  
      The summary table of dose levels, the optimal dose
       to recommend for subsequent study across all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
         100.6   100.6   100.6   100.6   100.6   100.6  
      The summary table of the final ratios of the optimal dose for stopping across
              all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
          6.24    6.24    6.24    6.24    6.24    6.24  
      Fitted expected efficacy level at dose most often selected : mean 1 (1, 1) 
      Stop reason triggered:
       ≥ 12 patients dosed :  100 %

# summary-PseudoDualFlexiSimulations works correctly

    Code
      result
    Output
      Summary of 1 simulations
      
      Target probability of DLE p(DLE) used at the end of a trial was 30 %
      The dose level corresponds to the target p(DLE) used at the end of a trial, TDEOT, was 43.05404 
      TDEOT at dose Grid was 25 
      Target p(DLE) used during a trial was 35 %
      The dose level corresponds to the target p(DLE) used during a trial, TDDT, was 47.61922 
      TDDT at dose Grid was 25 
      Number of patients overall : mean 12 (12, 12) 
      Number of patients treated above the target p(DLE) used at the end of a trial : mean 9 (9, 9) 
      Number of patients treated above the target p(DLE) used during a trial : mean 9 (9, 9) 
      Proportions of observed DLT in the trials : mean 33 % (33 %, 33 %) 
      Mean toxicity risks for the patients : mean 70 % (70 %, 70 %) 
      Doses selected as TDEOT : mean 0 (0, 0) 
      True toxicity at TDEOT : mean 5 % (5 %, 5 %) 
      Proportion of trials selecting the TDEOT: 0 %
      Proportion of trials selecting the TDDT: 0 %
      Dose most often selected as TDEOT: 0 
      Observed toxicity rate at dose most often selected: NaN %
      Fitted probabilities of DLE at dose most often selected : mean NA % (NA %, NA %) 
      The summary table of the final TDEOT across all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
         14.37   14.37   14.37   14.37   14.37   14.37  
      The summary table of the final ratios of the TDEOT across all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
             1       1       1       1       1       1  
      The summary table of the final TDDT across all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
            25      25      25      25      25      25  
      The summary table of dose levels, the optimal dose
       to recommend for subsequent study across all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
         14.37   14.37   14.37   14.37   14.37   14.37  
      The summary table of the final ratios of the optimal dose for stopping across
                        all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
             1       1       1       1       1       1  
      
      Stop reason triggered:
       ≥ 10 patients dosed :  100 %
      Target Gstar, the dose which gives the maximum gain value was 36.0307 
      Target Gstar at dose Grid was 25 
      The summary table of the final Gstar across all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
           250     250     250     250     250     250  
      The summary table of the final ratios of the Gstar across all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
         1.661   1.661   1.661   1.661   1.661   1.661  
      The summary table of dose levels, the optimal dose
       to recommend for subsequent study across all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
         14.37   14.37   14.37   14.37   14.37   14.37  
      The summary table of the final ratios of the optimal dose for stopping across
              all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
             1       1       1       1       1       1  
      Fitted expected efficacy level at dose most often selected : mean NA (NA, NA) 
      Stop reason triggered:
       ≥ 10 patients dosed :  100 %

---

    Code
      result_custom
    Output
      Summary of 1 simulations
      
      Target probability of DLE p(DLE) used at the end of a trial was 25 %
      The dose level corresponds to the target p(DLE) used at the end of a trial, TDEOT, was 38.02775 
      TDEOT at dose Grid was 25 
      Target p(DLE) used during a trial was 30 %
      The dose level corresponds to the target p(DLE) used during a trial, TDDT, was 43.05404 
      TDDT at dose Grid was 25 
      Number of patients overall : mean 12 (12, 12) 
      Number of patients treated above the target p(DLE) used at the end of a trial : mean 9 (9, 9) 
      Number of patients treated above the target p(DLE) used during a trial : mean 9 (9, 9) 
      Proportions of observed DLT in the trials : mean 33 % (33 %, 33 %) 
      Mean toxicity risks for the patients : mean 70 % (70 %, 70 %) 
      Doses selected as TDEOT : mean 0 (0, 0) 
      True toxicity at TDEOT : mean 5 % (5 %, 5 %) 
      Proportion of trials selecting the TDEOT: 0 %
      Proportion of trials selecting the TDDT: 0 %
      Dose most often selected as TDEOT: 0 
      Observed toxicity rate at dose most often selected: NaN %
      Fitted probabilities of DLE at dose most often selected : mean NA % (NA %, NA %) 
      The summary table of the final TDEOT across all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
         14.37   14.37   14.37   14.37   14.37   14.37  
      The summary table of the final ratios of the TDEOT across all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
             1       1       1       1       1       1  
      The summary table of the final TDDT across all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
            25      25      25      25      25      25  
      The summary table of dose levels, the optimal dose
       to recommend for subsequent study across all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
         14.37   14.37   14.37   14.37   14.37   14.37  
      The summary table of the final ratios of the optimal dose for stopping across
                        all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
             1       1       1       1       1       1  
      
      Stop reason triggered:
       ≥ 10 patients dosed :  100 %
      Target Gstar, the dose which gives the maximum gain value was 36.0307 
      Target Gstar at dose Grid was 25 
      The summary table of the final Gstar across all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
           250     250     250     250     250     250  
      The summary table of the final ratios of the Gstar across all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
         1.661   1.661   1.661   1.661   1.661   1.661  
      The summary table of dose levels, the optimal dose
       to recommend for subsequent study across all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
         14.37   14.37   14.37   14.37   14.37   14.37  
      The summary table of the final ratios of the optimal dose for stopping across
              all simulations
          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
             1       1       1       1       1       1  
      Fitted expected efficacy level at dose most often selected : mean NA (NA, NA) 
      Stop reason triggered:
       ≥ 10 patients dosed :  100 %

