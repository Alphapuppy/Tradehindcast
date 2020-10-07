#!/usr/bin/env Rscript
#args = commandArgs(trailingOnly=TRUE)
args = c(1,0,2, "weightedrel") #scenario, year, margin data
#-----------------
#config
Scen = as.numeric(args[1])
Yr = as.numeric(args[2])
Mrg = as.numeric(args[3])
Err = as.character(args[4])
scenname = paste0("para.S",Scen,".Y",Yr,".",Err,".M",Mrg); print(scenname)
ces.demand = 3
logit.landsupply = -0.75

base.yr = 1995;
#Define initial values for parameters
if (Scen == 1) {
  parameters <- c(ces.demand, logit.landsupply, 3, 6); 
  scenario.path.SSE = paste0("R/Model.SSE.S1_2.R"); #Model.SSE.S1.R would be the same with Model.SSE.S2.R
  if (Yr == 0) {target.yr.all <- c(2000, 2005, 2010, 2015)} else
  {target.yr.all <- c(2000, 2005, 2010, 2015)[Yr]} 
  } else if (Scen == 2 ) {
    scenario.path.SSE = paste0("R/Model.SSE.S1_2.R");
    if (Yr == 0) {target.yr.all <- c(2000, 2005, 2010, 2015); parameters <- c(ces.demand, logit.landsupply, 3, 6, rep(1, 4))} else
      {target.yr.all <- c(2000, 2005, 2010, 2015)[Yr]; parameters <- c(ces.demand, logit.landsupply, 3, 6, 1)} 
    } else if (Scen == 3) {
      scenario.path.SSE = paste0("R/Model.SSE.S3.R");
      if (Yr == 0) {target.yr.all <- c(2000, 2005, 2010, 2015); parameters <- c(ces.demand, logit.landsupply, 3, 6, rep(3, 4))} else
      {target.yr.all <- c(2000, 2005, 2010, 2015)[Yr]; parameters <- c(ces.demand, logit.landsupply, 3, 6, 3)} 
      (1:length(c(base.yr, target.yr.all))) -> allyrID;
      }
margin.reg.data.name = c("margin.reg.pim_pp.mtax.shock", "margin.reg.pim_pexp.mtax.shock")[Mrg]

source(paste0("R/Model.para.optim.S",Err,".R"), local = F)

start_time <- Sys.time()
if (length(parameters) == 5) {
  optim(parameters, fn, method = "L-BFGS-B", 
        lower = c(0.1, -20, 0.1, 0.1, 0.2), 
        upper = c(30, -0.1, 30, 30, 20),
        control=list(pgtol=0.01, maxit = 1000)) -> sol} else 
if (length(parameters) == 8) {
  optim(parameters, fn, method = "L-BFGS-B",
        lower = c(0.1, -10, 0.1, 0.1, rep(0.2, 4)),
        upper = c(10, -0.1, 10, 10, rep(10, 4)),
        control=list(pgtol=0.01, maxit = 1000)) -> sol} else 
if (length(parameters) == 4) {
  optim(parameters, fn, method = "L-BFGS-B", 
        lower = c(0.1, -20, 0.1, 0.1), 
        upper = c(30, -0.1, 30, 30),
        control=list(pgtol=0.01, maxit = 1000)) -> sol}
end_time <- Sys.time()
print(end_time - start_time)
print(scenname)

list(sol, time = end_time - start_time, scenname = scenname) -> sol.out
save(sol.out, file = paste0("output/results/", scenname, ".Rdata"))



