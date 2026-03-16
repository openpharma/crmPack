# knit_print works ok for LogisticNormalMixture

    Code
      cat(result)
    Output
      A mixture of two logistic log normal models will describe the relationship between dose and toxicity: $$ p(Tox | d) = f(X = 1 | \theta, d) = \frac{e^{\alpha + \beta \cdot log(d/d^*)}}{1 + e^{\alpha + \beta \cdot log(d/d^*)}} $$\n where d* denotes a reference dose.
      
      
       The prior for &theta; is given by
       $$ \theta = \begin{bmatrix} \alpha \\ log(\beta) \end{bmatrix} \sim w \cdot N \left(\begin{bmatrix}-0.85 \\ 1\end{bmatrix} , \begin{bmatrix} 1 & -0.5 \\ -0.5 & 1\end{bmatrix} \right) + (1 - w) \cdot N \left(\begin{bmatrix}0.85 \\ 1\end{bmatrix} , \begin{bmatrix} 1 & -0.5 \\ -0.5 & 1\end{bmatrix} \right) $$ 
      
       and the prior for w is given by 
      
        $$ w \sim Beta(1, 1) $$ 
      
        The reference dose will be 50.00.
      

