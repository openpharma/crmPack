# Approximate works correctly

    Code
      slot(actual$model, slot_name)
    Output
      An object of class "ModelParamsNormal"
      Slot "mean":
                  meanAlpha.(Intercept) meanBeta.I(log(dosegrid/refDose)) 
                             -0.5273931                         1.0892889 
      
      Slot "cov":
           [,1] [,2]
      [1,]    1    0
      [2,]    0    1
      
      Slot "prec":
           [,1] [,2]
      [1,]    1    0
      [2,]    0    1
      

---

    Code
      slot(actual$model, slot_name)
    Output
      An object of class "positive_number"
      [1] 40.05

---

    Code
      slot(actual$model, slot_name)
    Output
      [1] "nObs" "y"    "x"   

---

    Code
      slot(actual$model, slot_name)
    Output
      character(0)

---

    Code
      slot(actual$model, slot_name)
    Output
      [1] "alpha0" "alpha1"

