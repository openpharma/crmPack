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
      Doses selected as MTD : mean 5 (5, 5) 
      True toxicity at doses selected : mean 0 % (0 %, 0 %) 
      Proportion of trials selecting target MTD: 0 %
      Dose most often selected as MTD: 5 
      Observed toxicity rate at dose most often selected: 0 %
      Fitted toxicity rate at dose most often selected : mean 3 % (1 %, 5 %) 
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
      Fitted toxicity rate at dose most often selected : mean 1 % (0 %, 1 %) 
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

