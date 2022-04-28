# v_model_logistic_kadane_beta_gamma returns message for non-scalars

    Code
      v_model_logistic_kadane_beta_gamma(object)
    Output
      [1] "Beta distribution shape parameter alpha must be a positive scalar"
      [2] "Beta distribution shape parameter beta must be a positive scalar" 
      [3] "Gamma distribution shape parameter must be a positive scalar"     
      [4] "Gamma distribution rate parameter must be a positive scalar"      

# v_model_logistic_normal_fixed_mix returns message for non-valid ModelParamsNormal comp

    Code
      v_model_logistic_normal_fixed_mix(object)
    Output
      [1] "components must be a list with valid ModelParamsNormal S4 class objects, mean must have length 2 and no missing values are allowed, cov must be 2x2 positive-definite matrix without any missing values"

# v_model_dual_endpoint returns message for wrong use_fixed

    Code
      v_model_dual_endpoint(object)
    Output
      [1] "use_fixed must be a named logical vector that contains name 'sigma2W'"                                
      [2] "use_fixed must be a named logical vector that contains name 'rho'"                                    
      [3] "sigma2W must be a named numerical vector of length two with positive finite values and names 'a', 'b'"
      [4] "rho must be a named numerical vector of length two with positive finite values and names 'a', 'b'"    

# v_model_dual_endpoint_beta returns message for wrong use_fixed

    Code
      v_model_dual_endpoint_beta(object)
    Output
      [1] "use_fixed must be a named logical vector that contains name 'E0'"                                          
      [2] "E0 must be a numerical vector of length two with non-negative, finite, unique and sorted (asc.) values"    
      [3] "use_fixed must be a named logical vector that contains name 'Emax'"                                        
      [4] "Emax must be a numerical vector of length two with non-negative, finite, unique and sorted (asc.) values"  
      [5] "use_fixed must be a named logical vector that contains name 'delta1'"                                      
      [6] "delta1 must be a numerical vector of length two with non-negative, finite, unique and sorted (asc.) values"
      [7] "use_fixed must be a named logical vector that contains name 'mode'"                                        
      [8] "mode must be a numerical vector of length two with non-negative, finite, unique and sorted (asc.) values"  

# v_model_dual_endpoint_beta returns message for wrong fixed parameters

    Code
      v_model_dual_endpoint_beta(object)
    Output
      [1] "delta1 must be a positive and finite numerical scalar"
      [2] "mode must be a positive and finite numerical scalar"  

# v_model_dual_endpoint_beta returns message for wrong parameters

    Code
      v_model_dual_endpoint_beta(object)
    Output
      [1] "E0 must be a numerical vector of length two with non-negative, finite, unique and sorted (asc.) values"  
      [2] "Emax must be a numerical vector of length two with non-negative, finite, unique and sorted (asc.) values"
      [3] "delta1 must be a positive and finite numerical scalar"                                                   
      [4] "mode must be a positive and finite numerical scalar"                                                     

# v_model_dual_endpoint_emax returns message for wrong fixed parameters

    Code
      v_model_dual_endpoint_emax(object)
    Output
      [1] "E0 must be a positive and finite numerical scalar"  
      [2] "Emax must be a positive and finite numerical scalar"
      [3] "ED50 must be a positive and finite numerical scalar"

# v_model_dual_endpoint_emax returns message for wrong parameters

    Code
      v_model_dual_endpoint_emax(object)
    Output
      [1] "E0 must be a numerical vector of length two with non-negative, finite, unique and sorted (asc.) values"  
      [2] "Emax must be a numerical vector of length two with non-negative, finite, unique and sorted (asc.) values"
      [3] "ED50 must be a numerical vector of length two with non-negative, finite, unique and sorted (asc.) values"

# v_model_logistic_indep_beta returns message for wrong DLE parameters

    Code
      v_model_logistic_indep_beta(object)
    Output
      [1] "binDLE must be a finite numerical vector of minimum length 2, without missing values" 
      [2] "DLEdose must be a finite numerical vector of minimum length 2, without missing values"
      [3] "DLEweights must be an integer vector of minimum length 2, without missing values"     

---

    Code
      v_model_logistic_indep_beta(object)
    Output
      [1] "binDLE must be a finite numerical vector of minimum length 2, without missing values"

# v_model_logistic_indep_beta returns message for wrong DLE parameters (diff len)

    Code
      v_model_logistic_indep_beta(object)
    Output
      [1] "DLEdose must be a finite numerical vector of minimum length 2, without missing values"

---

    Code
      v_model_logistic_indep_beta(object)
    Output
      [1] "DLEweights must be an integer vector of minimum length 2, without missing values"

---

    Code
      v_model_logistic_indep_beta(object)
    Output
      [1] "DLEdose must be a finite numerical vector of minimum length 2, without missing values"
      [2] "DLEweights must be an integer vector of minimum length 2, without missing values"     

