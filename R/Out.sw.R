

parameters <- c()

args = c(2,0,2, "weightedrel") #scenario, year, margin data
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

source(paste0("R/Model.para.optim.S",Err,".R"), local = F)
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


(1:length(unique(basedata.regmkt$reg))) -> regID
(1:length(unique(basedata.regmkt$crop))) -> sectorID

crop_agg <- c("Corn", "Others", "Rapeseed", "Rice", "Soybeans", "Wheat") # unique(basedata.regmkt$crop)
region <- c("Africa", "Asia", "Europe","N. America", "Oceania","S. America") # unique(basedata.regmkt$reg)
study.year <- c(1995, 2000, 2005, 2010, 2015)

parameters <- c(ces.demand, logit.landsupply, 0.2, 3, rep(1, 4))

#base.yr = 2005;

#Figure relative share-weight vs relative prices 
lapply(c(study.year), function(base.yr){

  source("R/Model.basedata.baseyr.R", local = T) #local is important
  demand.regl.sw <- lapply(regID, function(reg){
    lapply(sectorID, function(crop){
      logit.sw.cali(c(consume.dom[[reg]][[crop]], consume.imp[[reg]][[crop]]),
                    c(pp[[reg]][[crop]], pimp[[reg]][[crop]]), logit.exponent.regl[[crop]]) 
    })
  }) 
  
  demand.regl.p <- lapply(regID, function(reg){
    lapply(sectorID, function(crop){
      c(pp[[reg]][[crop]], pimp[[reg]][[crop]])
    })
  }) 

  data.frame(
    year = base.yr,
    reg = lapply(regID, function(reg){lapply(sectorID, function(crop){
      region[[reg]] })}) %>% unlist(),
    crop = lapply(regID, function(reg){lapply(sectorID, function(crop){
      crop_agg[[crop]] })}) %>% unlist(),
    sw =lapply(regID, function(reg){lapply(sectorID, function(crop){
      demand.regl.sw[[reg]][[crop]][[2]] / demand.regl.sw[[reg]][[crop]][[1]] })}) %>% unlist(),
    p = lapply(regID, function(reg){lapply(sectorID, function(crop){
      demand.regl.p[[reg]][[crop]][[2]] / demand.regl.p[[reg]][[crop]][[1]] })}) %>% unlist(),
    cons = consume %>% unlist()
    ) -> data0
  
  
  
  return(data0)
  
  }) %>% bind_rows() -> data1

data1 %>% filter(is.finite(p)) %>% 
  group_by(year) %>% 
  summarise(sd.sw = sd(log(sw)), mean.sw = (mean(sw)), w.mean.sw = (weighted.mean(sw,w = cons)),
            sd.p = sd(log(p)), mean.p = (mean(p)), w.mean.p = (weighted.mean(p,w = cons)))


ggplot(data1 %>% filter(year %in% study.year, sw < 6) #%>%  mutate(year = as.character(year))
       )+
  geom_line(aes(x = year, y = sw)) + 
  geom_hline(yintercept = 1)+
  theme_bw() +   
  facet_grid(rows = vars(crop), cols = vars(reg), scales = "free")

ggplot(data1 %>% filter(year %in% study.year) %>%  mutate(year = as.character(year)))+
  geom_point(aes(x = sw, y = p, color = year))+ 
  geom_path(aes(x = sw, y = p, group = interaction(reg, crop))) +
  geom_hline(yintercept = 1)+
  geom_vline(xintercept = 1)+
  geom_abline(intercept = 0, slope = 1, linetype="dashed")+
  theme_bw() +
  facet_grid(rows = vars(crop), cols = vars(reg), scales = "free")

ggplot(data1 %>% mutate(year = as.character(year)))+
  geom_point(aes(x = sw, y = year))+  
  theme_bw()  

ggplot(data1 %>% mutate(year = as.character(year)) )+
  geom_line(aes(x = year, y = log(sw), group = crop, color = crop ))+ 
  geom_hline(yintercept = 0)+
  facet_wrap(~reg)+
  theme_bw()
  

basedata.trade.allyear %>%  group_by(crop, variable, year) %>% 
  summarise(value = sum(value), .groups = "drop") %>% 
  spread(variable, value) %>% 
  mutate(consume.tot = export + consume.dom,
    tradeshare = export /(export + consume.dom)) -> A

basedata.pricelink.allyear %>% filter(variable == "margin.mtax") -> A

ggplot() +
  geom_line(aes(x = year, y = value)) +
  facet_grid(rows = vars(reg.exp), cols = vars(reg.imp) )



#-----------------------------
parameters <- c(5.75904364269981,-0.333582827694084,0.1,1.34976783842864,1.11402130949745,1.1800458676658,1.18729946702743,1.29898359506157)
parameters <- c(5.75904364269981,-0.333582827694084,0.5,1.34976783842864,1.11402130949745,1.1800458676658,1.18729946702743,1.29898359506157)
parameters <- c(5.75904364269981,-0.333582827694084,3,1.34976783842864,1.11402130949745,1.1800458676658,1.18729946702743,1.29898359506157)
parameters <- c(3,-0.75,0.334659746936897,2.36371856004244)
parameters <- c(3,-0.75,1,2.36371856004244)
parameters <- c(3,-0.75,0.2,2.36371856004244)
load(paste0("output/results/", "para.S1.Y0.weightedrel.M2", ".Rdata"))
parameters <- sol.out[[1]]$par
parameters[3] <- 3
updated.db.equil00 <- updated.db.equil
updated.db.equilb02 <- updated.db.equil

updated.db.equil.agg <- updated.db.equil00 %>% mutate(db = "a01") %>% 
  bind_rows(updated.db.equil02 %>% mutate(db = "a02")
) %>% bind_rows(  updated.db.equil05 %>% mutate(db = "a05")
) %>% bind_rows(  updated.db.equil10 %>% mutate(db = "a10")
) %>% bind_rows(  updated.db.equil20 %>% mutate(db = "a20")
) %>% bind_rows(  updated.db.equil30 %>% mutate(db = "a30")
) %>% bind_rows(  updated.db.equilb02 %>% mutate(db = "b02")
) %>% bind_rows(  updated.db.equilb03 %>% mutate(db = "b03")
) %>% bind_rows(  updated.db.equilb10 %>% mutate(db = "b10")
) 




updated.db.equil.agg %>% gather(equil,"value", c(consumption, price)) %>% filter(db %in% c("b02", "b03", "b10")) %>% 
  spread(scenario, value) %>% 
  mutate(PE = abs(est / obs -1)*100 )  %>% 
  #mutate(PE = (est / obs -1)^2*100 )  %>% 
  #mutate(PE = (est - obs)^2)  %>% 
  #mutate(PE = abs(est - obs))  %>% 
  #mutate(PE = abs(log(est) - log(obs)))  %>% 
  left_join(updated.db.equil.agg %>% filter(scenario == "obs") %>% within(rm(scenario, price)), 
            by = c("reg.imp", "reg.exp", "crop", "variable", "target.yr", "db"))  %>% 
  filter_if(is.numeric, is.finite) %>% 
  #group_by(target.yr, equil) %>% 
  group_by(db, target.yr,equil) %>% #mutate(consumption = 1) %>% 
  summarise(Err = weighted.mean(PE, w = consumption), consumption = sum(consumption), .groups = 'drop') %>% ungroup() %>% 
  within(rm(consumption)) %>% #filter(equil == "price") %>% 
  spread(db, Err) -> A
  #summarise(Err = weighted.mean(Err, consumption)) %>% 
  pull(Err) 

  updated.db.equil.agg %>% gather(equil,"value", c(consumption, price)) %>% 
    filter(db %in% c("b02", "b03", "b10"), scenario == "est") %>% 
    spread(db, value) %>% left_join(
      updated.db.equil.agg %>% 
        gather(equil,"value", c(consumption, price)) %>% 
        filter(db == "b02", scenario == "obs") %>% spread(db, value) %>% spread(scenario, b02)) %>% 
    left_join(updated.db.equil.agg %>% filter(scenario == "obs", db == "b02") %>% within(rm(scenario, price, db))) -> C
  
  C %>% mutate(b02 = b02 - obs, b03 = b03 - obs, b10 = b10 - obs, 
               consumpercent = consumption / sum(consumption) * 100) -> C1
  
  
  
  
  

#fn(c(3,-0.75,0.1,2.13313384494904,1.12301883524901,1.14178721688077,1.325892184802,1.34515665875467))

#NULL model calculation
updated.db.equil %>% gather(equil,"value", c(consumption, price)) %>% 
  spread(scenario, value) %>% 
  mutate(PE = abs(ref / obs -1)*100 )  %>% 
  left_join(updated.db.equil %>% filter(scenario == "obs") %>% within(rm(scenario, price)), 
            by = c("reg.imp", "reg.exp", "crop", "variable", "target.yr")) %>% 
  filter_if(is.numeric, is.finite) %>% 
  group_by(target.yr, equil) %>% 
  summarise(Err = weighted.mean(PE, w = consumption), consumption = sum(consumption), .groups = 'drop') %>% ungroup() %>% 
  summarise(Err = weighted.mean(Err, consumption)) 

updated.db.equil %>% gather(equil,"value", c(consumption, price)) %>% 
  spread(scenario, value) %>% 
  mutate(PE = abs(ref / obs -1)*100 )  %>% 
  left_join(updated.db.equil %>% filter(scenario == "obs") %>% within(rm(scenario, price)), 
            by = c("reg.imp", "reg.exp", "crop", "variable", "target.yr")) %>% 
  filter_if(is.numeric, is.finite) %>% 
  group_by(target.yr, equil) %>%  
  summarise(Err = weighted.mean(PE, w = consumption), consumption = sum(consumption), .groups = 'drop') %>% ungroup() %>% 
  summarise(Err = weighted.mean(Err, consumption)) 








