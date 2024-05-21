#--------------------------------------------
## Study 1 Simulation Replication Function ##
#--------------------------------------------

#--------------
# To accompany:
# Shear, B. R., & Zumbo, B. D. (2013). False positives in multiple regression:
# Unanticipated consequences of measurement error in the predictor variables.
# Educational and Psychological Measurement, 73(5), 733–756. doi:10.1177/0013164413487738
#--------------

Study1FixedRep <- function(relX, relU, relY, corxu, r.sq, n, replics, var.names){
  # utility function that runs each condition and returns the results.
  # INPUTS:
  # relX - reliability of X (tested predictor).
  # relU - reliability of U (covariate).
  # relY - reliabiliy of Y.
  # corxu - correlation between (true) predictors.
  # r.sq - true model R-square.
  # n - sample size.
  # replics - number of simulation replications.
  # var.names - "pval", "bx", "bu", "bx.se", "bu.se", 
  #             "obs.rsq", "x.sr", "obs.sigma",
  #             "Bx", "Bu", "Bx.se", "Bu.se"
  # !!NOTE!!: var.names is likely the one to be most careful with.
  # OUTPUTS:
  # returns the currently used input parameters, the number of significant tests for X,
  # and the mean and SD of various model estimates (e.g., coefficients, R-square, change in R-square).
  
  SetError <- function(true.var, reli) { 
    # Function to compute necessary error variance for a given true score variance.
    if(reli > 0) {temp.err <- ((true.var/reli) - true.var)} else {
      temp.err <- 0 }
    return(temp.err)
  }
  
  sigma2 <- SetError(1, r.sq)                # Set the variance of sigma corresponding to current R-square.
  varTy <- (1 + sigma2)                      # Determine the true score variance of Y.
  varEx <- SetError(1, relX)                 # Determine measurement error variance of X.
  varEu <- SetError(1, relU)                 # Determine measurement error variance of U.
  varEy <- SetError(varTy, relY)             # Determine measurement error variance of Y.
  
  temp.res <- matrix(rep(NA, times=(replics*length(var.names))), ncol=length(var.names))  # A matrix that will hold results along the way. 
  colnames(temp.res) <- var.names                                                         # Label the columns.
  
  a1 <- sqrt(1 - corxu)  # Used to generate true scores.
  a2 <- sqrt(corxu)      # Used to generate true scores.
  
  # for-loop that generates a sample and estimates model.
  # Runs once for each replication desired.
  for (i in 1:replics) {
    cc <- rnorm(n)                         # Used to generate correlated random variables.  
    Tx <- a1*rnorm(n) + a2*cc       		   # True scores for x.
    Tu <- a1*rnorm(n) + a2*cc 					   # True scores for u.
    Ty <- Tu + rnorm(n, 0, sqrt(sigma2))   # generate true y values from true u values.
    
    X <- Tx + rnorm(n, 0, sqrt(varEx))	   # observed score X.
    U <- Tu + rnorm(n, 0, sqrt(varEu)) 	   # observed score U.
    Y <- Ty + rnorm(n, 0, sqrt(varEy))     # observed score Y.
    
    rr <- summary(lm(Y ~ U)) 							                 # reduced model with only U.
    ff <- summary(lm(Y ~ U + X))					                 # full model with U and X.
    ff.std <- summary(lm(scale(Y) ~ scale(U) + scale(X)))  # model to obtain standardized coefficients.
    
    temp.res[i,"pval"] <- ff$coef[3,4]                     # p-value for t-test of X.
    temp.res[i,"bx"] <- ff$coef[3,1]                       # estimated coefficient of X (bx).
    temp.res[i,"bu"] <- ff$coef[2,1]                       # estimated coefficient of U (bu).
    temp.res[i,"bx.se"] <- ff$coef[3,2]                    # estimated SE of bx.
    temp.res[i,"bu.se"] <- ff$coef[2,2]                    # estimated SE of bu.
    temp.res[i,"obs.rsq"] <- ff$r.squared                  # observed R-squared of full model.
    temp.res[i,"x.sr"] <- (ff$r.squared - rr$r.squared)    # change in R-square from adding X.
    temp.res[i,"obs.sigma"] <- ff$sigma                    # estimated var(sigma) in full model.
    temp.res[i,"Bx"] <- ff.std$coef[3,1]                   # estimated standardized coef. of X (Bx).
    temp.res[i,"Bu"] <- ff.std$coef[2,1]                   # estimated standardized coef. of U (Bu).
    temp.res[i,"Bx.se"] <- ff.std$coef[3,2]                # estimated SE of Bx.
    temp.res[i,"Bu.se"] <- ff.std$coef[2,2]                # estimated SE of Bu.
  }
  
  return(c(relX, relU, relY, corxu, r.sq, n, sum(temp.res[,"pval"] < 0.05),
           apply(temp.res[,-1], 2, mean),
           apply(temp.res[,-1], 2, sd))
  )
}
