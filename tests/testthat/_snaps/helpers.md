# h_write_model works as expected

    Code
      readLines(model_file)
    Output
      [1] "model"                     "{"                        
      [3] "    alpha0 <- mean(1:10)"  "    alpha1 <- 6.00000E+05"
      [5] "}"                        

# h_write_model works as expected for truncation

    Code
      readLines(model_file)
    Output
      [1] "model"                       "{"                          
      [3] "    alpha0 <- dnorm(4) I(4)" "    alpha1 <- 6.00000E+05"  
      [5] "}"                          

