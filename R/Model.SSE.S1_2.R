#In S2, only share-weights converge (distributions shift) and logit exponents are fixed.
#-----------
#Parameter & calibration
#-----------
land.sw <- lapply(regID, function(reg){logit.sw.cali(area[[reg]], rental[[reg]], logit.exponent.land)}) 
demand.sw <- lapply(regID, function(reg){logit.sw.cali(consum.nobiof[[reg]], pc[[reg]], ces.exponent.demand)}) 

demand.regl.sw <- lapply(regID, function(reg){
  lapply(sectorID, function(crop){
    logit.sw.cali(c(consume.dom[[reg]][[crop]], consume.imp[[reg]][[crop]]),
                  c(pp[[reg]][[crop]], pimp[[reg]][[crop]]), logit.exponent.regl[[crop]]) 
  })
}) 

demand.intl.sw <- lapply(regID, function(reg.imp){
  lapply(sectorID, function(crop){
    logit.sw.cali(
      lapply(regID, function(reg.exp){consume.imp.reg[[reg.exp]][[reg.imp]][[crop]]}) %>% unlist(),
      lapply(regID, function(reg.exp){pimp.reg[[reg.exp]][[reg.imp]][[crop]]}) %>% unlist(),
      logit.exponent.intl[[crop]]) 
  })
}) 

lapply(regID, function(reg.imp){
  lapply(sectorID, function(crop){
    demand.intl.sw[[reg.imp]][[crop]] = demand.intl.sw[[reg.imp]][[crop]]
  }) }) -> target.demand.intl.sw



#-----------
#Modeling
#-----------

lapply(target.yr.all, function(target.yr){
  #find targeted variables with historic shocks as drivers to the model changes
  source("R/Model.target.R", local = T) 
  fn.margin.reg.shock = target.margin.reg.shock
  fn.cropland.supply = target.cropland.supply
  fn.expense = target.expense
  fn.yield = target.yield
  fn.biofuelfeedstock.mandate = target.biofuelfeedstock.mandate
  
  #Armington parameters
  fn.demand.intl.sw = target.demand.intl.sw
  fn.logit.exponent.regl = logit.exponent.regl
  fn.logit.exponent.intl = logit.exponent.intl
  
  #Note that to revise later as theta is not including 1995
  fn.demand.regl.sw = lapply(regID, function(reg.imp){
      lapply(sectorID, function(crop){
        demand.regl.sw[[reg.imp]][[crop]] = demand.regl.sw[[reg.imp]][[crop]]^(1/theta.regl[[match(target.yr, target.yr.all)]])
      }) }) 
  
  source("R/Model.script.R", local = T)
  xstart <- rep(200,144)
  nleqslv(xstart, dslnex, control=list(btol=.01, maxit = 1000)) -> model.sol #, allowSingular=TRUE
  assign("iter", iter + 1, envir = .GlobalEnv); 
  print(paste0(model.sol[4], "; iter = ",iter))
  #-----------------------------
  
  #output results to generate updated.db.trade & updated.db.price
  source("R/Model.script.result.R", local = T)
  
  updated.db.trade %>% 
    left_join(updated.db.price, by = c("reg.imp", "reg.exp", "crop", "variable", "scenario")) %>% 
    mutate(target.yr = target.yr,
      reg.exp = if_else(as.factor(variable) == "consume.dom", 
                             "Domestic", 
                             as.character(reg.exp))) ->
    updated.db.equil.yr
  
  return(updated.db.equil.yr)

} )%>% bind_rows()  -> updated.db.equil



