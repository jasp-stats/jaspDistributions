# Main comparison table works

    Code
      table
    Output
           aic      bic      log_lik   n_par
      [1,] 325.5636 330.7739 -160.7818 2    
      [2,] 343.8987 346.5038 -170.9493 1    
      [3,] 371.6945 374.2996 -184.8472 1    
      [4,] 829.5608 829.5608 -414.7804 0    
           name                                                                      
      [1,] "\\(\\text{Gamma}(\\widehat{\\alpha} = 1.9, \\widehat{\\theta} = 1.07)\\)"
      [2,] "\\(\\text{Exponential}(\\widehat{\\lambda} = 0.492)\\)"                  
      [3,] "\\(\\text{Normal}(\\widehat{\\mu} = 2.03, \\sigma = 1.4)\\)"             
      [4,] "\\(\\text{Standard normal}()\\)"                                         
           w_aic         w_bic        
      [1,] 0.9998956     0.9996162    
      [2,] 0.0001043618  0.0003838181 
      [3,] 9.610808e-11  3.53463e-10  
      [4,] 3.617031e-110 4.893752e-109

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
      
      

