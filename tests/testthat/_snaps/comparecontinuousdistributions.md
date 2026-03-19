# Main comparison table works

    Code
      table
    Output
           aic      bic      log_lik   n_par name              w_aic         w_bic        
      [1,] 325.7672 330.9775 -160.8836 2     "Gamma"           1             1            
      [2,] 408.5922 411.1973 -203.2961 1     "Exponential"     1.034627e-18  3.806181e-18 
      [3,] 582.5582 585.1634 -290.2791 1     "Normal"          1.731956e-56  6.37151e-56  
      [4,] 829.5608 829.5608 -414.7804 0     "Standard normal" 4.005086e-110 5.420294e-109

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
      
      

