#----------------------------
## Analyze Study 1 Results ##
#----------------------------

#--------------
# To accompany:
# Shear, B. R., & Zumbo, B. D. (2013). False positives in multiple regression:
# Unanticipated consequences of measurement error in the predictor variables.
# Educational and Psychological Measurement, 73(5), 733–756. doi:10.1177/0013164413487738
#--------------

require(psych)

# Read in data ----------------------------------------------------
study1.dat <- read.csv(file = "data/study1_simulation_results.csv")
study1.dat$Type1Rate <- study1.dat$Type1/1000

# Descriptives -------------------------
hist(study1.dat$Type1Rate, col = "grey", breaks = 30)
describe(study1.dat)

## Tables of results for Study 1 ---------------------------------------------------------------------------------
with(data = subset(study1.dat, study1.dat$RelX == 0.8 & study1.dat$RelU == 0.8 & study1.dat$RelY == 0.8), expr = {
  print("Type I Error Rates for Table 3:");
  print(ftable(xtabs(Type1Rate ~ Corxu + Sample.N + Rsq)));
  print("Mean Delta R-square (X) for Table 5:");
  print(ftable(xtabs(round(mean_x.sr, 3) ~ Corxu + Sample.N + Rsq)));
  print("Mean Bx for Table 6:");
  print(ftable(xtabs(round(mean_Bx, 3) ~ Corxu + Sample.N + Rsq)));
}
)

with(data = subset(study1.dat, study1.dat$RelX == 1.0 & study1.dat$RelU == 0.8 & study1.dat$RelY == 1.0), expr = {
  print("Type I Error Rates for Table 4:");
  print(ftable(xtabs(Type1Rate ~ Corxu + Sample.N + Rsq)));
}
)
