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
ces.demand = 1
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


parameters <- sol$par



parameters <- c(1.4638757, -1.5131941,  1.3011367,  1.0088540,  0.8112664,  0.9264591,  1.0181305,  1.1276432)
0.8915112

parameters <- c(1.4638757, -1.5131941,  0.8,  1.0088540,  0.8112664,  0.9264591,  1.0181305,  1.1276432)
0.8956173
parameters <-  c(0.842899338046494,-2.63866035674275,0.764285956134445,0.933300081814049,1.10288249294876,1.1479787256978,1.12016302782384,1.25016015004554)
load(paste0("output/results/", scenname, ".Rdata"))
parameters <- sol.out[[1]]$par

sol.out -> sol.S3
sol.out -> sol.S2



updated.db.equil %>% gather(equil,"value", c(consumption, price)) %>% 
  filter(!(reg.imp == reg.exp & variable == "export")) %>% 
  spread(scenario, value) %>% 
  mutate(PE = abs(est / obs -1)*100 )  %>% 
  left_join(updated.db.equil %>% filter(scenario == "obs") %>% within(rm(scenario, price)), 
            by = c("reg.imp", "reg.exp", "crop", "variable", "target.yr"))  %>% 
  filter_if(is.numeric, is.finite) %>% 
  group_by(target.yr, equil) %>% 
  summarise(
    Err = weighted.mean(PE, w = log(1+consumption)),
    consumption = sum(consumption), .groups = 'drop') %>% ungroup() %>%
  group_by(equil) %>% 
  summarise(Err = weighted.mean(Err, consumption)) 
#-> Ep30.2p0
print(Err1)
parameters[3]= 1

parameters <- sol$par

theta.regl <- lapply(targetyrID, function(yr){c(1.1, 1.2, 1.3, 1.4)[yr]})

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
  summarise(mean.logdist = mean(logdist), 
            wmean.logdist = weighted.mean(logdist, weight),
            wmean.logdist.logw = weighted.mean(logdist, log(weight + 1)),
            weight = sum(weight)) -> goods2
0.8994294

Ap30.1p1         
Ap30.1p5


updated.db.equil %>% gather(equil,"value", c(consumption, price)) %>% 
  filter(!(reg.imp == reg.exp & variable == "export")) %>% 
  spread(scenario, value) -> B

ggplot(B) +
  geom_point(aes(x = log(obs), y = log(est))) +
  facet_grid(rows = vars(equil), cols = vars(target.yr), scales = "free") +
  geom_abline(intercept = 0, slope = 1) #+  coord_equal()

ggplot(B %>% mutate(Year = as.character(target.yr))) +
  geom_abline(intercept = 0, slope = 1) + 
  geom_point(aes(x = log(obs), y = log(est), color = Year), alpha = 0.8) +
  facet_wrap(~equil, scales = "free") +
  #scale_x_continuous(expand = c(0, 0)) +
  #scale_y_continuous(expand = c(0, 0)) +
  theme_bw() + theme0 + theme_leg +
  theme(panel.spacing.x=unit(1, "lines"))


B %>% mutate(logest = log(est), logobs = log(obs)) %>% 
  filter(is.finite(logest), is.finite(logobs)) %>% 
  group_by(target.yr, equil) %>% 
  summarise(cor(logest, logobs)^2 )


B %>% mutate(logest = log(est), logobs = log(obs)) %>% 
  filter(is.finite(logest), is.finite(logobs)) %>% 
  left_join(updated.db.equil %>% filter(scenario == "obs") %>% 
              within(rm(scenario, price)) %>% rename(weight = consumption), 
            by = c("reg.imp", "reg.exp", "crop", "variable", "target.yr")) -> C

B %>% mutate(logest = log(est/ref), logobs = log(obs/ref)) %>% 
  filter(is.finite(logest), is.finite(logobs)) %>% 
  left_join(updated.db.equil %>% filter(scenario == "obs") %>% 
              within(rm(scenario, price)) %>% rename(weight = consumption), 
            by = c("reg.imp", "reg.exp", "crop", "variable", "target.yr")) -> C
  
summary(lm(logest ~ logobs , data = C , weights = log(weight + 1)))
summary(lm(logest ~ logobs , data = C  ))
summary(lm(logest ~ logobs , data = C, weights = weight  ))
summary(lm(logest ~ logobs , data = C %>% filter(equil == "price"), weights = log(weight + 1)))

summary(lm(logest ~ logobs , data = C %>% filter(equil == "consumption"), weights = weight))
summary(lm(logest ~ logobs , data = C %>% filter(equil == "price"), weights = weight))


ggplot(C %>% mutate(Year = as.character(target.yr))) +
  geom_abline(intercept = 0, slope = 1) + 
  geom_point(aes(x = logobs, y = logest, color = Year), alpha = 0.8) +
  facet_wrap(~equil, scales = "free") +
  theme_bw() + theme0 + theme_leg +
  theme(panel.spacing.x=unit(1, "lines"))

ggplot(C) +
  geom_point(aes(x = logobs, y = logest)) +
  facet_grid(rows = vars(equil), cols = vars(target.yr), scales = "free") +
  geom_abline(intercept = 0, slope = 1) #+  coord_equal()



#%>% filter(equil == "consumption")

updated.db.equil %>% gather(equil,"value", c(consumption, price)) %>% 
  filter(!(reg.imp == reg.exp & variable == "export")) %>% 
  spread(equil, value) %>% 
  mutate(value = consumption * price) %>% 
  within(rm(consumption, price)) %>% 
  spread(scenario, value) %>% 
  mutate(logdiff = abs(log(est / obs))) %>% 
  filter(is.na(logdiff) == F) ->A
  

updated.db.equil %>% gather(equil,"value", c(consumption, price)) %>% 
  filter(!(reg.imp == reg.exp & variable == "export")) %>% 
  spread(scenario, value) %>% 
  mutate(PE = abs(est / obs -1)*100,
         PE1 = abs(est - obs)
  )  %>% 
  left_join(updated.db.equil %>% filter(scenario == "obs") %>% within(rm(scenario, price)), 
            by = c("reg.imp", "reg.exp", "crop", "variable", "target.yr"))  %>% 
  filter_if(is.numeric, is.finite) %>% 
  group_by(target.yr, crop, equil, variable) %>% 
  summarise(
    Err = weighted.mean(PE, w = consumption),
    consumption = sum(consumption), .groups = 'drop') %>% ungroup() %>% 
  within(rm(consumption)) %>% 
  spread(target.yr, Err) -> A


updated.db.equil %>% gather(equil,"value", c(consumption, price)) %>% 
  filter(!(reg.imp == reg.exp & variable == "export")) %>% 
  spread(scenario, value) %>% 
  mutate(PE = (est / obs -1)*100,
         PE1 = abs(est - obs)
  )  %>% 
  left_join(updated.db.equil %>% filter(scenario == "obs") %>% within(rm(scenario, price)), 
            by = c("reg.imp", "reg.exp", "crop", "variable", "target.yr"))  %>% 
  filter_if(is.numeric, is.finite) %>% 
  group_by(target.yr, crop, equil, variable) %>% 
  summarise(
    Err = weighted.mean(PE, w = consumption),
    consumption = sum(consumption), .groups = 'drop') %>% ungroup() %>% 
  within(rm(consumption)) %>% 
  spread(target.yr, Err) -> A1


updated.db.equil %>% gather(equil,"value", c(consumption, price)) %>% 
  filter(!(reg.imp == reg.exp & variable == "export")) %>% 
  spread(scenario, value) %>% 
  mutate(PE = abs(est / obs -1)*100,
         PE1 = abs(est - obs)
  )  %>% 
  left_join(updated.db.equil %>% filter(scenario == "obs") %>% within(rm(scenario, price)), 
            by = c("reg.imp", "reg.exp", "crop", "variable", "target.yr"))  %>% 
  filter_if(is.numeric, is.finite) %>% 
  filter(target.yr == 2015, crop == "Corn") -> A1





