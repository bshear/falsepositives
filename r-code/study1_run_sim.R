#---------------------------
## Run Study 1 Simulation ##
#---------------------------

#--------------
# To accompany:
# Shear, B. R., & Zumbo, B. D. (2013). False positives in multiple regression:
# Unanticipated consequences of measurement error in the predictor variables.
# Educational and Psychological Measurement, 73(5), 733–756. doi:10.1177/0013164413487738
#--------------

# load replication function
source("r-code/study1_sim_replication.R")

## Simulation parameters:
relX <- c(0.8, 1.0)
relU <- c(0.8, 1.0)
relY <- c(0.8, 1.0)
rsq <- c(0.01961, 0.1304, 0.2539, 0.5)
corxu <- c(0.0, 0.1, 0.3, 0.5, 0.7)
N <- c(50, 100, 500, 1000)
cond <- expand.grid(relX=relX, relU=relU, relY=relY, corxu=corxu, rsq = rsq, N=N)
reps <- 1000

# list of things we will record in each replication.
# this MUST match the expected inputs in the replication function.
dv.names <- c("pval",
              "bx", "bu", "bx.se", "bu.se",
              "obs.rsq", "x.sr", "obs.sigma",
              "Bx", "Bu", "Bx.se", "Bu.se")

# create results matrix.
res.names <- c("Condition", "RelX", "RelU", "RelY", "Corxu", "Rsq", "Sample.N",
               "Type1",
               c(paste("mean_",dv.names[-1],sep="")),
               c(paste("sd_",dv.names[-1],sep=""))
)
results.mat <- matrix(-99, nrow = 0, ncol=length(res.names))
colnames(results.mat) <- res.names

## can set seed
sim.seed <- 9090
set.seed(sim.seed)

## in order to track length.
start.time <- Sys.time()
print("Beginning simulation for study 1")
print(start.time)

pb <- txtProgressBar(min = 0, max = nrow(cond), style = 3)

## Run simulation:
for (r in 1:length(cond[,1])){
  results.mat <- rbind(results.mat, c(r, Study1FixedRep(relX=cond[r,"relX"],
                                                        relU=cond[r,"relU"],
                                                        relY=cond[r,"relY"],
                                                        corxu=cond[r,"corxu"],
                                                        r.sq=cond[r,"rsq"],
                                                        n=cond[r,"N"],
                                                        replics=reps,
                                                        var.names=dv.names
  )
  )
  )
  setTxtProgressBar(pb, r)
}

close(pb)

write.csv(results.mat, file = "data/study1_simulation_results.csv", row.names = F)

# Optional clean-up:
# rm(results.mat, relX, relU, relY, corxu, rsq, N, reps, dv.names, res.names, sim.seed, r)
