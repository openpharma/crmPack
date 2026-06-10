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
              for (j in 1:2) {
                  logit(p_single_my_combo[i, j]) <- alpha0_my_combo[j] + 
                      alpha1_my_combo[j] * log(x_my_combo[i, j]/ref_dose_my_combo[j])
              }
              p0_my_combo[i] <- p_single_my_combo[i, 1] + p_single_my_combo[i, 
                  2] - p_single_my_combo[i, 1] * p_single_my_combo[i, 
                  2]
              logit(p_my_combo[i]) <- log(p0_my_combo[i]/(1 - p0_my_combo[i])) + 
                  eta_my_combo * (x_my_combo[i, 1]/ref_dose_my_combo[1]) * 
                      (x_my_combo[i, 2]/ref_dose_my_combo[2])
              y_my_combo[i] ~ dbern(p_my_combo[i])
          }
      }

---

    Code
      cat(body_priormodel)
    Output
      {
          theta_my_combo[1:2, 2] ~ dmnorm(prior_mean_my_combo[1:2, 
              2], prior_prec_my_combo[1:2, 1:2, 2])
          eta_my_combo ~ dnorm(gamma_my_combo, tau_my_combo)
          theta_my_mono[1] ~ dnorm(mu_mono_intercept, pow(tau_mono_intercept, 
              -2))
          theta_my_combo[1, 1] ~ dnorm(mu_mono_intercept, pow(tau_mono_intercept, 
              -2))
          mu_mono_intercept ~ dnorm(logit(0.25), pow(2.5, -2))
          tau_mono_intercept ~ dlnorm(log(0.5), pow(kappa_hier, -2))
          theta_my_mono[2] ~ dnorm(mu_mono_slope, pow(tau_mono_slope, 
              -2))
          theta_my_combo[2, 1] ~ dnorm(mu_mono_slope, pow(tau_mono_slope, 
              -2))
          mu_mono_slope ~ dnorm(0, pow(0.7, -2))
          tau_mono_slope ~ dlnorm(log(0.25), pow(kappa_hier, -2))
          alpha0_my_mono <- theta_my_mono[1]
          alpha1_my_mono <- exp(theta_my_mono[2])
          alpha0_my_combo[1] <- theta_my_combo[1, 1]
          alpha1_my_combo[1] <- exp(theta_my_combo[2, 1])
          alpha0_my_combo[2] <- theta_my_combo[1, 2]
          alpha1_my_combo[2] <- exp(theta_my_combo[2, 2])
      }

# hierarchical modelspecs and init compilers return expected fields

    list(kappa_hier = 0.35364652069385, ref_dose_my_mono = 10, prior_mean_my_combo = structure(c(-0.85, 
    1, -0.7, 0.8), dim = c(2L, 2L), dimnames = list(NULL, c("drug1", 
    "drug2"))), prior_prec_my_combo = structure(c(1.33333333333333, 
    0.666666666666667, 0.666666666666667, 1.33333333333333, 1, 0.333333333333333, 
    0.333333333333333, 1.22222222222222), dim = c(2L, 2L, 2L)), gamma_my_combo = 0, 
        tau_my_combo = 1, ref_dose_my_combo = c(10, 20))

---

    list(kappa_hier = 0.35364652069385, prior_mean_my_combo = structure(c(-0.85, 
    1, -0.7, 0.8), dim = c(2L, 2L), dimnames = list(NULL, c("drug1", 
    "drug2"))), prior_prec_my_combo = structure(c(1.33333333333333, 
    0.666666666666667, 0.666666666666667, 1.33333333333333, 1, 0.333333333333333, 
    0.333333333333333, 1.22222222222222), dim = c(2L, 2L, 2L)), gamma_my_combo = 0, 
        tau_my_combo = 1)

---

    list(theta_my_mono = c(0, 1), theta_my_combo = structure(c(0, 
    1, 0, 1), dim = c(2L, 2L)), eta_my_combo = 0, mu_mono_intercept = 0, 
        tau_mono_intercept = 0.5, mu_mono_slope = 0, tau_mono_slope = 0.5)

# h_mcmc_get_hierarchical_data flattens arm data for JAGS

    list(kappa_hier = 0.35364652069385, ref_dose_my_mono = 10, prior_mean_my_combo = structure(c(-0.85, 
    1, -0.7, 0.8), dim = c(2L, 2L), dimnames = list(NULL, c("drug1", 
    "drug2"))), prior_prec_my_combo = structure(c(1.33333333333333, 
    0.666666666666667, 0.666666666666667, 1.33333333333333, 1, 0.333333333333333, 
    0.333333333333333, 1.22222222222222), dim = c(2L, 2L, 2L)), gamma_my_combo = 0, 
        tau_my_combo = 1, ref_dose_my_combo = c(10, 20), nObs_my_mono = 4L, 
        y_my_mono = c(0L, 0L, 0L, 1L), x_my_mono = c(10, 10, 20, 
        20), nObs_my_combo = 4L, y_my_combo = c(0L, 0L, 0L, 1L), 
        x_my_combo = structure(c(10, 10, 20, 20, 20, 40, 20, 40), dim = c(4L, 
        2L), dimnames = list(NULL, c("drug1", "drug2"))))

