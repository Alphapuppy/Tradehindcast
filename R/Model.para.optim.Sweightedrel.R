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
ces.exponent.demand <- parameters[1]
logit.exponent.land <- parameters[2]
logit.exponent.regl <- lapply(sectorID, function(crop){parameters[3]})
logit.exponent.intl <- lapply(sectorID, function(crop){parameters[4]})

#theta.regl is the converging parameter in S2
if (Scen == 1 | Scen == 2) {
  if (length(parameters) == 5) {theta.regl <- lapply(targetyrID, function(yr){parameters[5]}) }else
    if (length(parameters) == 8) {theta.regl <- lapply(targetyrID, function(yr){parameters[5:8][yr]}) } else
      if (length(parameters) == 4) {theta.regl <- rep(1, length(target.yr.all)) %>% as.list() }
} else 
  if (Scen == 3) {
    if (length(parameters) == 5) {logit.exponent.regl.yr <- lapply(allyrID, function(yr){
      lapply(sectorID, function(crop){parameters[c(3,5)][yr]})})}else
        if (length(parameters) == 8) {logit.exponent.regl.yr <- lapply(allyrID, function(yr){
          lapply(sectorID, function(crop){parameters[c(3,5,6,7,8)][yr]})})}
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
  ces.exponent.demand <- parameters[1]
  logit.exponent.land <- parameters[2]
  logit.exponent.regl <- lapply(sectorID, function(crop){parameters[3]})
  logit.exponent.intl <- lapply(sectorID, function(crop){parameters[4]})
  if (Scen == 1 | Scen == 2) {
    if (length(parameters) == 5) {theta.regl <- lapply(targetyrID, function(yr){parameters[5]}) }else
      if (length(parameters) == 8) {theta.regl <- lapply(targetyrID, function(yr){parameters[5:8][yr]}) } else
        if (length(parameters) == 4) {theta.regl <- rep(1, length(target.yr.all)) %>% as.list() }
  } else 
    if (Scen == 3) {
      if (length(parameters) == 5) {logit.exponent.regl.yr <- lapply(allyrID, function(yr){
        lapply(sectorID, function(crop){parameters[c(3,5)][yr]})})}else
          if (length(parameters) == 8) {logit.exponent.regl.yr <- lapply(allyrID, function(yr){
            lapply(sectorID, function(crop){parameters[c(3,5,6,7,8)][yr]})})}
    }
  
  #-----------
  #Modeling
  #-----------
  source(scenario.path.SSE, local = T)
  
  updated.db.equil %>% gather(equil,"value", c(consumption, price)) %>% 
    filter(equil == "consumption") %>% 
    spread(scenario, value) %>% 
    mutate(PE = abs(est / obs -1)*100,
           PE = abs(est - obs))  %>% 
    left_join(updated.db.equil %>% filter(scenario == "obs") %>% within(rm(scenario, price)), 
              by = c("reg.imp", "reg.exp", "crop", "variable", "target.yr")) %>% 
    filter_if(is.numeric, is.finite) %>% 
    group_by(target.yr, crop) %>% 
    # summarise(
    #   Err = weighted.mean(PE, w = consumption),
    #   Err1 = weighted.mean(PE, w = log(consumption + 1)), 
    #   #Err1 = mean(PE), 
    #   consumption = sum(consumption), .groups = 'drop') %>% ungroup() %>% 
    summarise(#Err = weighted.mean(Err, consumption),
              #Err1 = weighted.mean(Err1, consumption),
              Err1 = mean(PE)) %>% spread(crop, Err1)
    pull(Err1) -> Error
  
  print(paste0("Parameters: ", paste(parameters, collapse = ","), " SSE ", round(Error,3)))
  
  return(Error)
}
