# knit_print.GeneralSimulations works correctly

    Code
      knit_print(.DefaultGeneralSimulations(), asis = FALSE)
    Output
      [1] "### Simulation Results\n\n- **Number of simulations:** 2\n- **Random seed:** 123\n- **Dose grid size:** 3\n- **Final recommended doses:** 1.00, 2.00\n\n"

# knit_print.DualSimulations works correctly

    Code
      knit_print(.DefaultDualSimulations(), asis = FALSE)
    Output
      [1] "### Simulation Results\n\n- **Number of simulations:** 2\n- **Random seed:** 123\n- **Dose grid size:** 2\n- **Final recommended doses:** 1.00, 2.00\n\n- **Stopping reasons:** A (50.0%), B (50.0%)\n\n- **Rho estimates:** Mean = 0.300, Range = [0.250, 0.350]\n- **Sigma2W estimates:** Mean = 0.200, Range = [0.150, 0.250]\n\n"

# knit_print.PseudoSimulations works correctly

    Code
      knit_print(x, asis = FALSE)
    Output
      [1] "### Simulation Results\n\n- **Number of simulations:** 2\n- **Random seed:** 123\n- **Dose grid size:** 2\n- **Final recommended doses:** 1.00, 2.00\n\n- **TD target during trial:** Mean = 62.50\n- **TD target end of trial:** Mean = 70.00\n- **Stopping reasons:** A (50.0%), B (50.0%)\n\n"

# knit_print.PseudoDualSimulations works correctly

    Code
      knit_print(x, asis = FALSE)
    Output
      [1] "### Simulation Results\n\n- **Number of simulations:** 2\n- **Random seed:** 123\n- **Dose grid size:** 2\n- **Final recommended doses:** 1.00, 2.00\n\n- **TD target during trial:** Mean = 62.50\n- **TD target end of trial:** Mean = 70.00\n- **Stopping reasons:** A (50.0%), B (50.0%)\n\n- **Gstar estimates:** Mean = 105.00, Range = [100.00, 110.00]\n- **Optimal dose:** Mean = 105.00\n\n"

# knit_print.PseudoDualFlexiSimulations works correctly

    Code
      knit_print(x, asis = FALSE)
    Output
      [1] "### Simulation Results\n\n- **Number of simulations:** 2\n- **Random seed:** 123\n- **Dose grid size:** 2\n- **Final recommended doses:** 1.00, 2.00\n\n- **TD target during trial:** Mean = 62.50\n- **TD target end of trial:** Mean = 70.00\n- **Stopping reasons:** A (50.0%), B (50.0%)\n\n- **Gstar estimates:** Mean = 105.00, Range = [100.00, 110.00]\n- **Optimal dose:** Mean = 105.00\n\n- **Sigma2 beta W estimates:** Mean = 0.013, Range = [0.010, 0.015]\n\n"

# knit_print.DualSimulationsSummary works correctly

    Code
      knit_print(.DefaultDualSimulationsSummary(), asis = FALSE)
    Output
      [1] "### Simulation Summary (1 simulations)\n\n**Target toxicity interval:** [20.0%, 35.0%]\n\n**Target dose interval:** [NA, NA]\n\n**Dose most often selected as MTD:** 1 (observed toxicity rate: 0.0%)\n\n**Proportion selecting target MTD:** 0.0%\n\n**Number of patients:** 12\n\n**Patients treated above target:** 0\n\n**Fitted toxicity at dose most selected:** 0.069\n\n**Stopping rules triggered:**  NA (100.0%), P(0.9 ≤ Biomarker ≤ 1) ≥ 0.5 (relative) (0.0%), ≥ 10 patients dosed (100.0%), Stopped because of missing dose (0.0%) \n\n**Biomarker fit at dose most selected:** 0.237\n\n"

# knit_print.PseudoSimulationsSummary works correctly

    Code
      knit_print(.DefaultPseudoSimulationsSummary(), asis = FALSE)
    Output
      [1] "### Simulation Summary (1 simulations)\n\n**Target probability of DLE at end of trial:** 30.0%\n\n**TDEOT:** 152.62 (at dose grid: 150.00)\n\n**Target probability of DLE during trial:** 35.0%\n\n**TDDT:** 155.97 (at dose grid: 150.00)\n\n**Dose most often selected:** 75 (observed toxicity rate: 0.0%)\n\n**Proportion selecting TDEOT:** 0.0%\n\n**Proportion selecting TDDT:** 0.0%\n\n**Number of patients:** 12\n\n**Patients treated above TDEOT:** 0\n\n**Patients treated above TDDT:** 0\n\n\nTable: TDEOT Summary\n\n|Statistic |Value |\n|:---------|:-----|\n|Min.      |86.99 |\n|1st Qu.   |86.99 |\n|Median    |86.99 |\n|Mean      |86.99 |\n|3rd Qu.   |86.99 |\n|Max.      |86.99 |\n\n\nTable: TDDT Summary\n\n|Statistic |Value  |\n|:---------|:------|\n|Min.      |105.46 |\n|1st Qu.   |105.46 |\n|Median    |105.46 |\n|Mean      |105.46 |\n|3rd Qu.   |105.46 |\n|Max.      |105.46 |\n\n"

# knit_print.PseudoDualSimulationsSummary works correctly

    Code
      knit_print(.DefaultPseudoDualSimulationsSummary(), asis = FALSE)
    Output
      [1] "### Simulation Summary (1 simulations)\n\n**Target probability of DLE at end of trial:** 30.0%\n\n**TDEOT:** 152.62 (at dose grid: 150.00)\n\n**Target probability of DLE during trial:** 35.0%\n\n**TDDT:** 155.97 (at dose grid: 150.00)\n\n**Dose most often selected:** 100 (observed toxicity rate: NaN%)\n\n**Proportion selecting TDEOT:** 0.0%\n\n**Proportion selecting TDDT:** 0.0%\n\n**Number of patients:** 12\n\n**Patients treated above TDEOT:** 3\n\n**Patients treated above TDDT:** 3\n\n\nTable: TDEOT Summary\n\n|Statistic |Value  |\n|:---------|:------|\n|Min.      |100.64 |\n|1st Qu.   |100.64 |\n|Median    |100.64 |\n|Mean      |100.64 |\n|3rd Qu.   |100.64 |\n|Max.      |100.64 |\n\n\nTable: TDDT Summary\n\n|Statistic |Value  |\n|:---------|:------|\n|Min.      |119.54 |\n|1st Qu.   |119.54 |\n|Median    |119.54 |\n|Mean      |119.54 |\n|3rd Qu.   |119.54 |\n|Max.      |119.54 |\n\n**Target Gstar:** 130.01 (at dose grid: 125.00)\n\n**Efficacy fit at dose most selected:** 0.977\n\n\nTable: Gstar Summary\n\n|Statistic |Value  |\n|:---------|:------|\n|Min.      |151.91 |\n|1st Qu.   |151.91 |\n|Median    |151.91 |\n|Mean      |151.91 |\n|3rd Qu.   |151.91 |\n|Max.      |151.91 |\n\n"

