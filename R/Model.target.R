target.basedata.regmkt <- basedata.regmkt.allyear %>% 
  filter(year %in% c(target.yr)) %>% 
  within(rm(year))

target.basedata.trade <- basedata.trade.allyear %>% 
  filter(year %in% c(target.yr)) %>% 
  within(rm(year))

target.basedata.pricelink <- basedata.pricelink.allyear %>% 
  filter(year %in% c(target.yr)) %>%
  within(rm(year))





#-----------
#Data
#-----------
(1:length(unique(basedata.regmkt$reg))) -> regID
names(regID) <- unique(basedata.regmkt$reg)
(1:length(unique(basedata.regmkt$crop))) -> sectorID
names(sectorID) <- unique(basedata.regmkt$crop)
as.character(unique(target.basedata.regmkt$variable)) -> vars

lapply(vars, function(var){
  assign(paste0("target.", var), 
         pull.list(target.basedata.regmkt %>% filter(variable == var)),
         envir = parent.env(environment())  #globalenv()
  )
} )


target.cropland.supply = lapply(regID, function(reg){sum(target.area[[reg]]) } ) 
target.prod <- lapply(regID, function(reg){target.area[[reg]] * target.yield[[reg]] } )

target.rental <- lapply(regID, function(reg){(target.pp[[reg]] * nlc.share[[reg]]) * target.yield[[reg]] } )

target.consume.dom <- 
  pull.list(target.basedata.trade %>% filter(variable == "consume.dom"), group = "reg.imp", val.rm = c("variable", "crop"))
target.consume.imp <-
  pull.list(target.basedata.trade %>% filter(variable == "export"), group = "reg.imp", val.rm = c("variable", "crop"))

target.consume <- lapply(regID, function(reg){target.consume.dom[[reg]] +  target.consume.imp[[reg]]})

target.biofuelfeedstock.mandate <- lapply(regID, function(reg){target.consume[[reg]] * target.bioshare[[reg]]/100  } )

target.consum.nobiof <- lapply(regID, function(reg){target.consume[[reg]] * (1- target.bioshare[[reg]]/100)  } )

#international import demand
lapply(unique(target.basedata.trade$reg.imp), function(reg){
  pull.list(target.basedata.trade %>% filter(variable == "export", reg.exp == reg), group = "reg.imp", val.rm = c("variable", "crop"))
}) %>% as.list -> target.consume.imp.reg  

target.prod.exp.reg = target.consume.imp.reg  #market clearing

target.export <- lapply(regID, function(reg.exp){
  lapply(sectorID, function(crop){
    lapply(regID, function(reg.imp){target.prod.exp.reg[[reg.exp]][[reg.imp]][crop]  } )%>% unlist() %>% sum()
  }) %>% unlist()
}) %>% unlist()

#balance check
abs((target.consume %>% unlist()  - target.consume.imp %>% unlist()) - (target.prod %>% unlist() - target.export)) > 0.1  

#read margin shock as price wedge between pp and pim
lapply(unique(target.basedata.pricelink$reg.exp), function(region.exp){
  pull.list(target.basedata.pricelink %>% filter(variable == margin.reg.data.name, reg.exp == region.exp),
            group = "reg.imp", val.rm = c("variable", "crop"))
}) -> target.margin.reg.shock

#margins (and tariffs) are added here
target.pimp.reg <- lapply(regID, function(reg.exp){
  lapply(regID, function(reg.imp){
    pp[[reg.exp]] * target.margin.reg.shock[[reg.exp]][[reg.imp]]
  })
})

#import prices
target.pimp <- lapply(regID, function(reg.imp){
  lapply(sectorID, function(crop){
    lapply(regID, function(reg.exp){target.consume.imp.reg[[reg.exp]][[reg.imp]][[crop]]/target.consume.imp[[reg.imp]][[crop]] * target.pimp.reg[[reg.exp]][[reg.imp]][[crop]] } ) %>%
      unlist() %>% sum()
  }) %>% unlist()
})

#aggregated consumer prices
target.pc <- lapply(regID, function(reg){target.consume.imp[[reg]]/target.consume[[reg]] * pimp[[reg]] + target.consume.dom[[reg]]/target.consume[[reg]] * target.pp[[reg]]})

target.expense <- lapply(regID, function(reg){sum(target.pc[[reg]] * target.consume[[reg]] ) } )
