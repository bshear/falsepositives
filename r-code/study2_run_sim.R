#---------------------------
## Run Study 2 Simulation ##
#---------------------------

#--------------
# To accompany:
# Shear, B. R., & Zumbo, B. D. (2013). False positives in multiple regression:
# Unanticipated consequences of measurement error in the predictor variables.
# Educational and Psychological Measurement, 73(5), 733–756. doi:10.1177/0013164413487738
#--------------

source("r-code/study2_sim_replication.R")

## Generate the scenarios:  
set.seed(7384)  # for reproduceability of the scenarios.
runs <- 2000    # number of scenarios that we will study.
cond <- data.frame(relX = round(runif(runs, 0.6, 1), digits = 7),
                   relU = round(runif(runs, 0.6, 1), digits = 7),
                   relY = round(runif(runs, 0.6, 1), digits = 7),
                   corxu = round(runif(runs, 0, 0.75), digits = 7),
                   rsq = round(runif(runs, 0, 0.75), digits = 7),
                   N = round(runif(runs, 10, 1000)))

## number of replications of each scenario.
replications = 1000

## create the results matrix
dv.names <- c("pval",
              "bx", "bu", "bx.se", "bu.se",
              "obs.rsq", "x.sr", "obs.sigma",
              "Bx", "Bu", "Bx.se", "Bu.se")
res.names <- c("Condition", "RelX", "RelU", "RelY", "Corxu", "Rsq", "Sample.N",
               "Type1_05",
               c(paste("mean_",dv.names[-1],sep="")),
               c(paste("sd_",dv.names[-1],sep=""))
)
results.mat <- matrix(-99, nrow = 0, ncol=length(res.names))
colnames(results.mat) <- res.names

## Make simulation reproduceable.
sim.seed <- 610
set.seed(sim.seed)

## in order to track length.
start.time <- Sys.time()
print("Beginning simulation for study 2")
print(start.time)

pb <- txtProgressBar(min = 0, max = nrow(cond), style = 3)

## RUN SIMULATION.
for (r in 1:length(cond[,1])){
  results.mat <- rbind(results.mat, c(r, Study2SimRep(relX=cond[r,"relX"],
                                                      relU=cond[r,"relU"],
                                                      relY=cond[r,"relY"],
                                                      corxu=cond[r,"corxu"],
                                                      r.sq=cond[r,"rsq"],
                                                      n=cond[r,"N"],
                                                      replics=replications,
                                                      var.names=dv.names
  )
  )
  )
  setTxtProgressBar(pb, r)
}

close(pb)

print(paste("Start time was ", start.time, ". End time is ", Sys.time(), ", and elapsed time is ", Sys.time()-start.time, ".", sep = ""))

write.csv(results.mat, file = "data/study2_simulation_results.csv", row.names = F)

## Optional clean-up:
## rm(cond, results.mat, dv.names, replications, res.names, runs, sim.seed, start.time, r)
