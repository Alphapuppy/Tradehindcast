#!/usr/bin/env Rscript
#args = commandArgs(trailingOnly=TRUE)
args = c(2,0,2, "good", 2015) #scenario, year, margin data
#-----------------
#config
Scen = as.numeric(args[1])
Yr = as.numeric(args[2])
Mrg = as.numeric(args[3])
Err = as.character(args[4])
base.yr = as.numeric(args[5]);
scenname = paste0("para.S",Scen,".Y",Yr,".",Err,".M",Mrg, ".B", base.yr); print(scenname)
ces.demand = 1
logit.landsupply = -1

study.yr <- c(1995, 2000, 2005, 2010, 2015)

#Define initial values for parameters
if (Scen == 1) {
  parameters <- c(ces.demand, logit.landsupply, 1.2, 1.2)[c(-1,-2)]; 
  scenario.path.SSE = paste0("R/Model.SSE.S1_2.R"); #Model.SSE.S1.R would be the same with Model.SSE.S2.R
  if (Yr == 0) {target.yr.all <- setdiff(study.yr, base.yr)} else
  {target.yr.all <- setdiff(study.yr, base.yr)[Yr]} 
} else if (Scen == 2 ) {
  scenario.path.SSE = paste0("R/Model.SSE.S1_2.R");
  if (Yr == 0) {target.yr.all <- setdiff(study.yr, base.yr); parameters <- c(ces.demand, logit.landsupply, 1.2, 1.2, rep(1, 4))[c(-1,-2)]} else
  {target.yr.all <- setdiff(study.yr, base.yr)[Yr]; parameters <- c(ces.demand, logit.landsupply, 3, 6, 1)[c(-1,-2)]} 
} else if (Scen == 3) {
  scenario.path.SSE = paste0("R/Model.SSE.S3.R");
  if (Yr == 0) {target.yr.all <- setdiff(study.yr, base.yr); parameters <- c(ces.demand, logit.landsupply, 0.82, 1, c(0.9, 1, 1.08, 1.13))[c(-1,-2)]} else
  {target.yr.all <- setdiff(study.yr, base.yr)[Yr]; parameters <- c(ces.demand, logit.landsupply, 3, 6, 3)[c(-1,-2)]} 
  (1:length(c(base.yr, target.yr.all))) -> allyrID;
}
margin.reg.data.name = c("margin.reg.pim_pp.mtax.shock", "margin.reg.pim_pexp.mtax.shock")[Mrg]
#parameters <- c(1,1, .8)
source(paste0("R/Model.para.optim.S",Err,".R"), local = F)

start_time <- Sys.time()
if (length(parameters) == 3) {
  optim(parameters, fn, method = "L-BFGS-B", 
        lower = c(0.1, 0.1, 0.6), 
        upper = c(10, 10, 5),
        control=list(pgtol=0.0001, maxit = 1000)) -> sol} else 
          if (length(parameters) == 6) {
            optim(parameters, fn, method = "L-BFGS-B",
                  lower = c(0.1, 0.1, rep(0.4, 4)),
                  upper = c(3, 8, rep(4, 4)), hessian = T,
                  control=list(pgtol=0.0001, maxit = 1000)) -> sol} else 
                    if (length(parameters) == 2) {
                      optim(parameters, fn, method = "L-BFGS-B", 
                            lower = c(0.1, 0.1), 
                            upper = c(5, 10), hessian = T,
                            control=list(pgtol=0.0001, maxit = 1000)) -> sol}
end_time <- Sys.time()
print(end_time - start_time)
print(scenname)

list(sol, time = end_time - start_time, scenname = scenname) -> sol.out
save(sol.out, file = paste0("output/results/", scenname, "SSE.Rdata"))



