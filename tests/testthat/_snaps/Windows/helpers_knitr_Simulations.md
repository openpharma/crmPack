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

