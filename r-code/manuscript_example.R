#---------------------------------------------------
## Generate Exxample Data for Table 1 and Table 2 ##
#---------------------------------------------------

#--------------
# To accompany:
# Shear, B. R., & Zumbo, B. D. (2013). False positives in multiple regression:
# Unanticipated consequences of measurement error in the predictor variables.
# Educational and Psychological Measurement, 73(5), 733–756. doi:10.1177/0013164413487738
#--------------

source("r-code/regression_functions.R")
set.seed(456)

  B0 <- 1
  B1 <- 1
  B2 <- 0
  corxu <- 0.5
  relX <- 0.8
  relW <- 0.8
  relY <- 0.8
  r.sq <- 0.5
  n <- 500
  
  var.tx <- 1
  var.tw <- 1
  
  set.error <- function(true.var, reli) { 
    if(reli > 0) {temp.err <- ((true.var/reli) - true.var)} else {
      temp.err <- 0 }
    return(temp.err)
  }
  
  sigma2 <- set.error(1, r.sq)
  var.ty <- var.tw + sigma2  
  var.ex <- set.error(var.tx, relX)
  var.ew <- set.error(var.tw, relW)
  var.ey <- set.error(var.ty, relY)
  
  sigma2

  a1 <- sqrt(1 - corxu)  						   # Used to generate true scores.
  a2 <- sqrt(corxu)     						   # Used to generate true scores.
  
    cc <- rnorm(n)  
    Tx <- a1*rnorm(n) + a2*cc 					   # True scores for x
    Tw <- a1*rnorm(n) + a2*cc 					   # True scores for u
    Ty <- B0 + B1*Tw + rnorm(n, 0, sqrt(sigma2))           # generate true y values from true u values
    
    X <- Tx + rnorm(n, 0, sqrt(var.ex))	           # observed score X
    W <- Tw + rnorm(n, 0, sqrt(var.ew)) 	           # observed score U
    Y <- Ty + rnorm(n, 0, sqrt(var.ey))             # observed score Y

  true.mod <- lm(Ty ~ Tw + Tx, x = T, y = T)
  obs.mod <- lm(Y ~ W + X, x = T, y = T)
  
  summary(true.mod)
  summary(obs.mod)
  lmCustomSummary(true.mod)
  lmCustomSummary(obs.mod)

  # compute semi-partials
  # sr2 for Tw
  summary(lm(Ty ~ Tw + Tx))$r.squared - summary(lm(Ty ~ Tx))$r.squared

  # sr2 for Tx
  summary(lm(Ty ~ Tw + Tx))$r.squared - summary(lm(Ty ~ Tw))$r.squared

  # sr2 for W
  summary(lm(Y ~ W + X))$r.squared - summary(lm(Y ~ X))$r.squared

  # sr2 for X
  summary(lm(Y ~ W + X))$r.squared - summary(lm(Y ~ W))$r.squared
