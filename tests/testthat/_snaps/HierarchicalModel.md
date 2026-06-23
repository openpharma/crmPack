# hierarchical pool lookup helpers work as expected

    Code
      pooled_map
    Output
      $`my_mono::alpha0`
      [1] "mono_intercept"
      
      $`my_combo::alpha0[1]`
      [1] "mono_intercept"
      
      $`my_mono::alpha1`
      [1] "mono_slope"
      
      $`my_combo::alpha1[1]`
      [1] "mono_slope"
      

# hierarchical compiler helpers produce readable compiled functions

    Code
      cat(body_datamodel)
    Output
      {
          for (i in 1:nObs_my_mono) {
              logit(p_my_mono[i]) <- alpha0_my_mono + alpha1_my_mono * 
                  log(x_my_mono[i]/ref_dose_my_mono)
              y_my_mono[i] ~ dbern(p_my_mono[i])
          }
          for (i in 1:nObs_my_combo) {
              x_drug1_my_combo[i] <- x_my_combo[i, 1L]
          }
          for (i in 1:nObs_my_combo) {
              logit(p_drug1_my_combo[i]) <- alpha0_drug1_my_combo + 
                  alpha1_drug1_my_combo * log(x_drug1_my_combo[i]/ref_dose_drug1_my_combo)
              p_single_my_combo[i, 1L] <- p_drug1_my_combo[i]
          }
          for (i in 1:nObs_my_combo) {
              x_drug2_my_combo[i] <- x_my_combo[i, 2L]
          }
          for (i in 1:nObs_my_combo) {
              logit(p_drug2_my_combo[i]) <- alpha0_drug2_my_combo + 
                  alpha1_drug2_my_combo * log(x_drug2_my_combo[i]/ref_dose_drug2_my_combo)
              p_single_my_combo[i, 2L] <- p_drug2_my_combo[i]
          }
          for (i in 1:nObs_my_combo) {
              combo_interaction_my_combo[i] <- x_drug1_my_combo[i]/ref_dose_drug1_my_combo * 
                  (x_drug2_my_combo[i]/ref_dose_drug2_my_combo)
          }
          for (i in 1:nObs_my_combo) {
              p0_my_combo[i] <- p_single_my_combo[i, 1] + p_single_my_combo[i, 
                  2] - p_single_my_combo[i, 1] * p_single_my_combo[i, 
                  2]
              logit(p_my_combo[i]) <- log(p0_my_combo[i]/(1 - p0_my_combo[i])) + 
                  eta_my_combo * combo_interaction_my_combo[i]
              y_my_combo[i] ~ dbern(p_my_combo[i])
          }
      }

---

    Code
      cat(body_priormodel)
    Output
      {
          alpha0_my_mono <- theta_my_mono[1]
          alpha1_my_mono <- exp(theta_my_mono[2])
          alpha0_drug1_my_combo <- theta_drug1_my_combo[1]
          alpha1_drug1_my_combo <- exp(theta_drug1_my_combo[2])
          theta_drug2_my_combo ~ dmnorm(mean_drug2_my_combo, prec_drug2_my_combo)
          alpha0_drug2_my_combo <- theta_drug2_my_combo[1]
          alpha1_drug2_my_combo <- exp(theta_drug2_my_combo[2])
          alpha0_my_combo[1L] <- alpha0_drug1_my_combo
          alpha0_my_combo[2L] <- alpha0_drug2_my_combo
          alpha1_my_combo[1L] <- alpha1_drug1_my_combo
          alpha1_my_combo[2L] <- alpha1_drug2_my_combo
          eta_my_combo ~ dnorm(gamma_my_combo, tau_my_combo)
          theta_my_mono[1] ~ dnorm(mu_mono_intercept, pow(tau_mono_intercept, 
              -2))
          theta_drug1_my_combo[1] ~ dnorm(mu_mono_intercept, pow(tau_mono_intercept, 
              -2))
          mu_mono_intercept ~ dnorm(logit(0.25), pow(2.5, -2))
          tau_mono_intercept ~ dlnorm(log(0.5), pow(kappa_hier, -2))
          theta_my_mono[2] ~ dnorm(mu_mono_slope, pow(tau_mono_slope, 
              -2))
          theta_drug1_my_combo[2] ~ dnorm(mu_mono_slope, pow(tau_mono_slope, 
              -2))
          mu_mono_slope ~ dnorm(0, pow(0.7, -2))
          tau_mono_slope ~ dlnorm(log(0.25), pow(kappa_hier, -2))
      }

# hierarchical modelspecs and init compilers return expected fields

    list(kappa_hier = 0.35364652069385, ref_dose_my_mono = 10, ref_dose_drug1_my_combo = 10, 
        mean_drug2_my_combo = c(-0.7, 0.8), prec_drug2_my_combo = structure(c(1, 
        0.333333333333333, 0.333333333333333, 1.22222222222222), dim = c(2L, 
        2L)), ref_dose_drug2_my_combo = 20, gamma_my_combo = 0, tau_my_combo = 1)

---

    list(kappa_hier = 0.35364652069385, mean_drug2_my_combo = c(-0.7, 
    0.8), prec_drug2_my_combo = structure(c(1, 0.333333333333333, 
    0.333333333333333, 1.22222222222222), dim = c(2L, 2L)), gamma_my_combo = 0, 
        tau_my_combo = 1)

---

    list(theta_my_mono = c(0, 1), theta_drug1_my_combo = c(0, 1), 
        theta_drug2_my_combo = c(0, 1), eta_my_combo = 0, mu_mono_intercept = 0, 
        tau_mono_intercept = 0.5, mu_mono_slope = 0, tau_mono_slope = 0.5)

# h_mcmc_get_hierarchical_data flattens arm data for JAGS

    list(kappa_hier = 0.35364652069385, ref_dose_my_mono = 10, ref_dose_drug1_my_combo = 10, 
        mean_drug2_my_combo = c(-0.7, 0.8), prec_drug2_my_combo = structure(c(1, 
        0.333333333333333, 0.333333333333333, 1.22222222222222), dim = c(2L, 
        2L)), ref_dose_drug2_my_combo = 20, gamma_my_combo = 0, tau_my_combo = 1, 
        nObs_my_mono = 4L, y_my_mono = c(0L, 0L, 0L, 1L), x_my_mono = c(10, 
        10, 20, 20), nObs_my_combo = 4L, y_my_combo = c(0L, 0L, 0L, 
        1L), x_my_combo = structure(c(10, 10, 20, 20, 20, 40, 20, 
        40), dim = c(4L, 2L), dimnames = list(NULL, c("drug1", "drug2"
        ))))

