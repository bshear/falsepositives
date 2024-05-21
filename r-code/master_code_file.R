#---------------------
## Master Code File ##
#---------------------

#--------------
# To accompany:
# Shear, B. R., & Zumbo, B. D. (2013). False positives in multiple regression:
# Unanticipated consequences of measurement error in the predictor variables.
# Educational and Psychological Measurement, 73(5), 733–756. doi:10.1177/0013164413487738
#--------------

# This file calls and runs the necessary code contained in the additional R files to generate data and analyze results.

library(HH)
library(psych)
library(ggplot2)

sink(file = 'results/sim-session-info.txt')
Sys.time()
sessionInfo()
sink()

# Generate data for Study 1 ----------
source("r-code/study1_run_sim.R")
rm(list = ls())

# Analyze data for Study 1 -----------
source("r-code/study1_analysis.R")
rm(list = ls())

# Generate data for Study 2 ----------
source("r-code/study2_run_sim.R")

# Analyze data for Study 2 -----------
source("r-code/study2_analysis.R")


