#Model script

dslnex_result <- function(x = model.sol$x, output = T) {
  y <- numeric(144)
  fn.pc <- list(x[1:6], x[7:12], x[13:18], x[19:24], x[25:30], x[31:36])
  fn.pimp <- list(x[37:42],	x[43:48],	x[49:54],	x[55:60],	x[61:66],	x[67:72])
  fn.pp <- list(x[73:78],	x[79:84],	x[85:90],	x[91:96],	x[97:102],	x[103:108])
  fn.r <- list(x[109:114],	x[115:120],	x[121:126],	x[127:132],	x[133:138],	x[139:144])
  
  #import price across sources; added margins 
  #define pimp.reg
  fn.pimp.reg = lapply(regID, function(reg.exp){
    lapply(regID, function(reg.imp){
      fn.pp[[reg.exp]] * fn.margin.reg.shock[[reg.exp]][[reg.imp]]
    })
  }) 
  
  fn.expense.nobiof = lapply(regID, function(reg){fn.expense[[reg]] - sum(fn.biofuelfeedstock.mandate[[reg]] %>% unlist() * fn.pp[[reg]]) })
  
  #CES regional crop demand: f(pc)
  fn.consume.nobiof <- lapply(regID, function(reg){ces.share(fn.pc[[reg]], ces.exponent.demand, demand.sw[[reg]]) * fn.expense.nobiof[[reg]] / fn.pc[[reg]] })
  
  fn.consume <- lapply(regID, function(reg){fn.consume.nobiof[[reg]] +  fn.biofuelfeedstock.mandate[[reg]]})
  
  #demand.regl: f(pp, pimp, demand.regl.sw, consume)
  fn.consume.dom <- lapply(regID, function(reg.imp){
    lapply(sectorID, function(crop){
      (logit.share(c(fn.pp[[reg.imp]][[crop]], fn.pimp[[reg.imp]][[crop]]), 
                   fn.logit.exponent.regl[[crop]], 
                   fn.demand.regl.sw[[reg.imp]][[crop]]) * fn.consume[[reg.imp]][[crop]])[1] }) %>% unlist()
  })
  
  fn.consume.imp <- lapply(regID, function(reg.imp){
    lapply(sectorID, function(crop){
      (logit.share(c(fn.pp[[reg.imp]][[crop]], fn.pimp[[reg.imp]][[crop]]), 
                   fn.logit.exponent.regl[[crop]], 
                   fn.demand.regl.sw[[reg.imp]][[crop]]) * fn.consume[[reg.imp]][[crop]])[2] }) %>% unlist()
  })
  #zero-profit condition
  #aggregated consumer prices: f(pimp, pp, consume.imp, consume.dom, consume)
  y[1:36] <- lapply(regID, function(reg){
    fn.consume.imp[[reg]]/fn.consume[[reg]] * fn.pimp[[reg]] + fn.consume.dom[[reg]]/fn.consume[[reg]] * fn.pp [[reg]]}) %>%
    unlist() -
    fn.pc %>% unlist()
  
  ################################
  #demand.intl: f(pimp.reg, demand.intl.sw, consume.imp)
  fn.consume.imp.reg0 <- lapply(regID, function(reg.imp){
    lapply(sectorID, function(crop){
      logit.share(lapply(regID, function(reg.exp){fn.pimp.reg[[reg.exp]][[reg.imp]][[crop]]}) %>% unlist(), 
                  fn.logit.exponent.intl[[crop]], 
                  fn.demand.intl.sw[[reg.imp]][[crop]]) * fn.consume.imp[[reg.imp]][[crop]] })
  })
  
  fn.consume.imp.reg <- lapply(regID, function(reg.exp){
    lapply(regID, function(reg.imp){
      lapply(sectorID, function(crop){
        fn.consume.imp.reg0[[reg.imp]][[crop]][[reg.exp]]
      }) %>% unlist()
    })
  })
  
  #zero-profit condition
  #import prices: f(consume, consume.imp, consume.imp.reg, pimp.reg)
  y[37:72] <- lapply(regID, function(reg.imp){
    lapply(sectorID, function(crop){
      lapply(regID, function(reg.exp){
        fn.consume.imp.reg[[reg.exp]][[reg.imp]][[crop]]/fn.consume.imp[[reg.imp]][[crop]] * fn.pimp.reg[[reg.exp]][[reg.imp]][[crop]] } ) %>%
        unlist() %>% sum()
    }) %>% unlist()
  }) %>% unlist() - 
    fn.pimp %>% unlist()
  
  ################################
  #import price across sources; added margins 
  #see fn.pimp.reg in the beginning 
  
  #implied export; mkt clearing
  fn.prod.exp.reg = fn.consume.imp.reg  
  
  #export f(fn.prod.exp.reg) 
  fn.export <- lapply(regID, function(reg.exp){
    lapply(sectorID, function(crop){
      lapply(regID, function(reg.imp){fn.prod.exp.reg[[reg.exp]][[reg.imp]][crop]  } )%>% unlist() %>% sum()
    }) %>% unlist()
  }) %>% unlist()
  
  #area supply: f(r, land.sw, cropland.supply)
  fn.area <- lapply(regID, function(reg){logit.share(fn.r[[reg]], logit.exponent.land, land.sw[[reg]]) * fn.cropland.supply[[reg]] })
  
  #production: f(area, yield)
  fn.prod <- lapply(regID, function(reg){fn.area[[reg]] * fn.yield[[reg]] }) %>% unlist()
  
  #market clearing
  y[73:108] <- (fn.consume %>% unlist() - fn.consume.imp %>% unlist()) - (fn.prod - fn.export)   
  
  #zero-profit condition
  y[109:144] = (fn.pp %>% unlist() * nlc.share %>% unlist()) * fn.yield %>% unlist() - fn.r %>% unlist()           
  
  y
  
  if (output == T) {
    assign("fn.pimp.reg", fn.pimp.reg, envir = .GlobalEnv)
    assign("fn.pp", fn.pp, envir = .GlobalEnv)
    assign("fn.consume.dom", fn.consume.dom, envir = .GlobalEnv)
    assign("fn.consume.imp", fn.consume.imp, envir = .GlobalEnv)
    assign("fn.consume.imp.reg", fn.consume.imp.reg, envir = .GlobalEnv)
    assign("fn.area", fn.area, envir = .GlobalEnv)
    assign("fn.prod", fn.prod, envir = .GlobalEnv)
    assign("y", y, envir = .GlobalEnv)
    }
  
}

dslnex_result(x = model.sol$x, output = T)

# fn.biofuelfeedstock.mandate added here
basedata.trade %>% filter(variable == "consume.dom") %>%
  arrange(reg.exp, crop) %>% 
  bind_cols(data.frame(fn.value = fn.consume.dom %>% unlist() %>% unlist(), 
                       target.value = target.consume.dom %>% unlist(),
                       row.names = NULL)) %>% 
  bind_rows(basedata.trade %>% filter(variable == "export") %>% 
              arrange(reg.exp, reg.imp, crop) %>% 
              bind_cols(data.frame(fn.value = fn.consume.imp.reg %>% unlist(),
                                   target.value = target.consume.imp.reg %>% unlist(),
                                   row.names = NULL)) 
  ) %>% rename(ref = value, est = fn.value, obs = target.value) %>% 
  gather("scenario", "consumption", c(ref, est, obs)) -> 
  updated.db.trade

basedata.regmkt %>% filter(variable == "pp") %>%
  arrange(reg, crop) %>% 
  bind_cols(data.frame(fn.value = fn.pp %>% unlist(), 
                       target.value = target.pp %>% unlist(),
                       row.names = NULL)) %>% 
  rename(reg.imp = reg) %>% 
  mutate(reg.exp = reg.imp, variable = "consume.dom") %>% 
  bind_rows(
    basedata.trade %>% 
      filter(variable == "export") %>%
      within(rm(value)) %>% 
      bind_cols(data.frame(value = pimp.reg %>% unlist(),
                           fn.value = fn.pimp.reg %>% unlist(),
                           target.value = target.pimp.reg %>% unlist(), row.names = NULL))
  ) %>% rename(ref = value, est = fn.value, obs = target.value) %>% 
  gather("scenario", "price", c(ref, est, obs)) ->
  updated.db.price




