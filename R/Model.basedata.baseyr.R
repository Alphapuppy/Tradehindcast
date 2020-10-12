#base year data for calibration and/or results comparison
basedata.regmkt <- basedata.regmkt.allyear %>% 
  filter(year %in% c(base.yr)) %>% 
  within(rm(year))

basedata.trade <- basedata.trade.allyear %>% 
  filter(year %in% c(base.yr)) %>% 
  within(rm(year))

basedata.pricelink <- basedata.pricelink.allyear %>% 
  filter(year %in% c(base.yr)) %>%
  within(rm(year))

#Note that margin.reg.data.name need to be defined for data sensitivity analysis
#margin.reg.data.name = c("margin.reg.pim_pp.mtax.shock", "margin.reg.pim_pexp.mtax.shock")[1]

#-----------
#Data
#-----------
(1:length(unique(basedata.regmkt$reg))) -> regID
names(regID) <- unique(basedata.regmkt$reg)
(1:length(unique(basedata.regmkt$crop))) -> sectorID
names(sectorID) <- unique(basedata.regmkt$crop)
as.character(unique(basedata.regmkt$variable)) -> vars

lapply(vars, function(var){
  assign(var, 
         pull.list(basedata.regmkt %>% filter(variable == var)),
         envir = globalenv()
  )
} )

#nlc.share was updated now and differentiated by region and crop
cropland.supply = lapply(regID, function(reg){sum(area[[reg]]) } ) 
prod <- lapply(regID, function(reg){area[[reg]] * yield[[reg]] } )

rental <- lapply(regID, function(reg){(pp[[reg]] * nlc.share[[reg]]) * yield[[reg]] } )

consume.dom <- 
  pull.list(basedata.trade %>% filter(variable == "consume.dom"), group = "reg.imp", val.rm = c("variable", "crop"))
consume.imp <-
  pull.list(basedata.trade %>% filter(variable == "export"), group = "reg.imp", val.rm = c("variable", "crop"))

consume <- lapply(regID, function(reg){consume.dom[[reg]] +  consume.imp[[reg]]})

biofuelfeedstock.mandate <- lapply(regID, function(reg){consume[[reg]] * bioshare[[reg]] / 100  } )

consum.nobiof <- lapply(regID, function(reg){consume[[reg]] * (1- bioshare[[reg]] / 100)  } )

#international import demand
lapply(unique(basedata.trade$reg.imp), function(reg){
  pull.list(basedata.trade %>% filter(variable == "export", reg.exp == reg), group = "reg.imp", val.rm = c("variable", "crop"))
}) %>% as.list -> consume.imp.reg  

prod.exp.reg = consume.imp.reg  #market clearing

export <- lapply(regID, function(reg.exp){
  lapply(sectorID, function(crop){
    lapply(regID, function(reg.imp){prod.exp.reg[[reg.exp]][[reg.imp]][crop]  } )%>% unlist() %>% sum()
  }) %>% unlist()
}) %>% unlist()

#balance check
abs((consume %>% unlist()  - consume.imp %>% unlist()) - (prod %>% unlist() - export)) > 0.01  

#read margin shock as price wedge between pp and pim
lapply(unique(basedata.pricelink$reg.exp), function(region.exp){
  pull.list(basedata.pricelink %>% filter(variable == margin.reg.data.name, reg.exp == region.exp),
            group = "reg.imp", val.rm = c("variable", "crop"))
}) -> margin.reg.shock

#margins (and tariffs) are added here
pimp.reg <- lapply(regID, function(reg.exp){
  lapply(regID, function(reg.imp){
    pp[[reg.exp]] * margin.reg.shock[[reg.exp]][[reg.imp]]
  })
})

#import prices
pimp <- lapply(regID, function(reg.imp){
  lapply(sectorID, function(crop){
    lapply(regID, function(reg.exp){consume.imp.reg[[reg.exp]][[reg.imp]][[crop]]/consume.imp[[reg.imp]][[crop]] * pimp.reg[[reg.exp]][[reg.imp]][[crop]] } ) %>%
      unlist() %>% sum()
  }) %>% unlist()
})

#aggregated consumer prices
pc <- lapply(regID, function(reg){consume.imp[[reg]]/consume[[reg]] * pimp[[reg]] + consume.dom[[reg]]/consume[[reg]] * pp [[reg]]})

expense <- lapply(regID, function(reg){sum(pc[[reg]] * consume[[reg]] ) } )
