#-----------------
library(dplyr)
library(tidyverse)
library(nleqslv)
library(tidyr)

datadir <- ("./data/")
outdir <- ("./output/")
source("R/Model.func.R")      #basic funcs
source("R/Model.basedata.R")  #read processed base data
#-----------
#Data
#-----------
#read in base data of base.yr
source("R/Model.basedata.baseyr.R") #Filter base data given base.yr, for initial para. calibration.
#-----------
#Test initial values
#-----------
(1:length(target.yr.all)) -> targetyrID
ces.exponent.demand <- ces.demand
logit.exponent.land <- logit.landsupply
logit.exponent.regl <- lapply(sectorID, function(crop){parameters[1]})
logit.exponent.intl <- lapply(sectorID, function(crop){parameters[2]})

#theta.regl is the converging parameter in S2
if (Scen == 1 | Scen == 2) {
  if (length(parameters) == 3) {theta.regl <- lapply(targetyrID, function(yr){parameters[3]}) }else
    if (length(parameters) == 6) {theta.regl <- lapply(targetyrID, function(yr){parameters[3:6][yr]}) } else
      if (length(parameters) == 2) {theta.regl <- rep(1, length(target.yr.all)) %>% as.list() }
} else 
  if (Scen == 3) {
    if (length(parameters) == 3) {logit.exponent.regl.yr <- lapply(allyrID, function(yr){
      lapply(sectorID, function(crop){parameters[c(1,3)][yr]})})}else
        if (length(parameters) == 6) {logit.exponent.regl.yr <- lapply(allyrID, function(yr){
          lapply(sectorID, function(crop){parameters[c(1,3,4,5,6)][yr]})})}
  }

iter = 0
start_time <- Sys.time()
source(scenario.path.SSE, local = T)
end_time <- Sys.time(); end_time - start_time
print("Done testing initial value. Running optimization now.")
#----------------------------
#Optimization to min SSE
#----------------------------
iter = 0
fn <- function(parameters){
  #-----------
  #Parameter & calibration
  #-----------
  #ces.exponent.demand <- ces.demand
  #logit.exponent.land <- logit.landsupply
  logit.exponent.regl <- lapply(sectorID, function(crop){parameters[1]})
  logit.exponent.intl <- lapply(sectorID, function(crop){parameters[2]})
  if (Scen == 1 | Scen == 2) {
    if (length(parameters) == 3) {theta.regl <- lapply(targetyrID, function(yr){parameters[3]}) }else
      if (length(parameters) == 6) {theta.regl <- lapply(targetyrID, function(yr){parameters[3:6][yr]}) } else
        if (length(parameters) == 2) {theta.regl <- rep(1, length(target.yr.all)) %>% as.list() }
  } else 
    if (Scen == 3) {
      if (length(parameters) == 3) {logit.exponent.regl.yr <- lapply(allyrID, function(yr){
        lapply(sectorID, function(crop){parameters[c(1,3)][yr]})})}else
          if (length(parameters) == 6) {logit.exponent.regl.yr <- lapply(allyrID, function(yr){
            lapply(sectorID, function(crop){parameters[c(1,3,4,5,6)][yr]})})}
    }
  
  #-----------
  #Modeling
  #-----------
  source(scenario.path.SSE, local = T)
  
  updated.db.equil %>% gather(equil,"value", c(consumption, price)) %>% 
    filter(!(reg.imp == reg.exp & variable == "export")) %>% 
    spread(scenario, value) %>% 
    transmute(reg.imp, reg.exp, crop, variable, target.yr, equil, logdiff = (log(est / obs))^2) %>% 
    spread(equil, logdiff) %>% 
    filter(is.na(consumption) == F & is.infinite(consumption) == F
           #, is.finite(price)
    ) %>% 
    mutate(logdist = (consumption + price)^0.5) %>%
    left_join(updated.db.equil %>% filter(scenario == "obs") %>% 
                within(rm(scenario, price)) %>% rename(weight = consumption), 
              by = c("reg.imp", "reg.exp", "crop", "variable", "target.yr")) %>% 
    group_by(target.yr) %>% 
    summarise(wmean.logdist.logw = weighted.mean(logdist, log(weight + 1)),
              weight = sum(weight), .groups = "drop") %>%  ungroup() %>% 
    summarise(Err = weighted.mean(wmean.logdist.logw, weight)) %>% 
    pull(Err) -> Error
  
  print(paste0("Parameters: ", paste(parameters, collapse = ","), " SSE ", round(Error,3)))
  
  return(Error)
}
