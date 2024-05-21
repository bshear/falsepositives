#--------------------------------
## Useful Regression Functions ##
#--------------------------------

#--------------
# To accompany:
# Shear, B. R., & Zumbo, B. D. (2013). False positives in multiple regression:
# Unanticipated consequences of measurement error in the predictor variables.
# Educational and Psychological Measurement, 73(5), 733–756. doi:10.1177/0013164413487738
#--------------

#--------------------------------
lmCustomSummary <- function(mod){
  require(HH)
  # function to return a nice summary table of results
  # takes in a lm object "mod" with x=TRUE and y=TRUE
  # model and data are unstandardized (but could be centered)
  r_square <- summary(mod)$r.squared
  b <- as.numeric(coefficients(mod))
  stdzrs <- as.numeric(c(apply(mod$x[,-1], 2, sd))/sd(mod$y))
  B <- c(NA, as.numeric(coefficients(mod)[-1]*stdzrs))
  Beta2 <- c(as.numeric(coefficients(lm(scale(mod$y) ~ scale(mod$x[ ,-1])))))
  SE <- as.numeric(summary(mod)$coef[ ,"Std. Error"])
  VIF <- c(NA, as.numeric(vif(mod)))
  t <- as.numeric(summary(mod)$coef[ ,"t value"])
  p <- as.numeric(summary(mod)$coef[ ,"Pr(>|t|)"])
  r <- c(NA, round(as.numeric(cor(cbind(mod$y, mod$x[ ,-1]))[-1,1]), 4))
  Pratt <- round((B*r)/r_square, 4)
  
  return(data.frame(Var = c(names(mod$coefficients)),
                    b = b,
                    Beta = B,
                    Beta2 = Beta2,
                    SE = SE,
                    t = t,
                    p = p,
                    VIF = VIF,
                    r = r,
                    Pratt = Pratt,
                    R2 = c(r_square, rep(NA, times = (length(b)-1)))
  )
  )
  
}
