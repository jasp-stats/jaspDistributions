# Main comparison table works

    Code
      table
    Output
           aic      bic      log_lik   n_par name              w_aic         w_bic        
      [1,] 325.5636 330.7739 -160.7818 2     "Gamma"           0.9998956     0.9996162    
      [2,] 343.8987 346.5038 -170.9493 1     "Exponential"     0.0001043618  0.0003838181 
      [3,] 371.6945 374.2996 -184.8472 1     "Normal"          9.610808e-11  3.53463e-10  
      [4,] 829.5608 829.5608 -414.7804 0     "Standard normal" 3.617031e-110 4.893752e-109

# Per distribution output works

    Code
      table
    Output
           estimate key     label           name   
      [1,] 1.901867 "alpha" "\\(\\alpha\\)" "shape"
      [2,] 1.068929 "theta" "\\(\\theta\\)" "scale"

---

    Code
      table
    Output
      [[1]]
      [[1]]$p_value
      [1] 0.97
      
      [[1]]$statistic
      [1] 0.0516236
      
      [[1]]$test
      [1] "Kolmogorov-Smirnov"
      
      
      [[2]]
      [[2]]$p_value
      [1] 0.93
      
      [[2]]$statistic
      [1] 0.04502064
      
      [[2]]$test
      [1] "Cramér–von Mises"
      
      
      [[3]]
      [[3]]$p_value
      [1] 0.89
      
      [[3]]$statistic
      [1] 0.3633734
      
      [[3]]$test
      [1] "Anderson-Darling"
      
      

