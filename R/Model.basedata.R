readRDS(paste0(datadir, "RDS/TradeGCAMmini.rds")) -> TradeGCAM
readRDS(paste0(datadir, "RDS/prodGCAMmini.rds")) -> prodGCAM

source("R/Model.gtap.proc.R") #source basedata.nlc & basedata.pricelink.margin.mtax
source("R/Model.OECD_FAO.proc.R") #source basedata.cropbioshare

#crop_agg <- c("Corn", "Wheat", "Rice", "Soybeans", "Rapeseed", "Others")
study.year <- c(1995, 2000, 2005, 2010, 2015)

#----------
#Production equilibrium
basedata.regmkt.allyear <- 
  prodGCAM %>% 
  transmute(reg = region, crop = Item, year, prod, revenue, area = H.area/1000) %>% 
  group_by(reg, crop) %>% 
  mutate_at(vars(prod, revenue, area), MA.5) %>% 
  mutate(yield = prod/1000/area, pp = revenue / prod) %>% 
  ungroup() %>% 
  gather("variable", "value", -c(reg, crop, year)) %>% 
  filter(year %in% study.year) %>% 
  bind_rows(basedata.nlc) %>% 
  bind_rows(basedata.cropbioshare) %>% 
  mutate(value = if_else(
    crop == "Soybeans" & reg == "Oceania" & 
      variable == "pp" & year == 2015, 500, value)) #Oceania soya price was missing $500 was used based on export prices

rm(basedata.nlc, basedata.cropbioshare)
#unique(basedata.regmkt.allyear$variable)

#----------
#Trade flows and domestic supply; imp.Q from FAO data were used
#Note that in TradeGCAM, reg.imp was reporting country for imp. data while reg.exp was reporting country for exp. data
#Using 5-year average data
TradeGCAM.MA5 <- TradeGCAM %>% dplyr::select(reg.imp, reg.exp, crop=Item, year, imp.Q, imp.V) %>% 
  left_join(TradeGCAM %>%  #change reporting countires to join
              transmute(reg.imp0 = reg.exp, reg.exp = reg.imp, 
                        crop = Item, year,exp.Q, exp.V),
            by = c("reg.imp" = "reg.imp0","reg.exp", "crop", "year")) %>% 
  group_by(reg.imp, reg.exp, crop) %>% 
  mutate_at(vars(imp.Q,imp.V,exp.Q,exp.V), MA.5) %>% 
  ungroup() %>% 
  filter(year %in% study.year) 

#Requiring balancing data to make sure consume.dom is non-negative
#Only region of adjustment was Europe soybean 2005 data
#Only imp.Q is used in this study!

basedata.regmkt.allyear %>% filter(variable == "prod") %>% 
  transmute(reg.exp = reg, crop, year, prod = value) %>% 
  left_join(TradeGCAM.MA5 %>%  
              mutate(imp.Q = if_else(
                crop == "Soybeans" & reg.imp == "Europe" & 
                  reg.exp == "Europe" & year == 2005, 
                exp.Q, imp.Q)) %>% 
              group_by(reg.exp, crop , year) %>% 
              summarise_at(vars(imp.Q), sum) %>% ungroup(), 
            by = c("reg.exp", "crop", "year")) %>% 
  replace(is.na(.), 0) %>% 
  mutate(consume.dom = prod - imp.Q) %>% 
  transmute(reg.imp = reg.exp, reg.exp, crop, year, 
            variable = "consume.dom", value = consume.dom / 1000) %>% 
  bind_rows(
    TradeGCAM.MA5 %>%  
      mutate(imp.Q = if_else(
        crop == "Soybeans" & reg.imp == "Europe" & 
          reg.exp == "Europe" & year == 2005, 
        exp.Q, imp.Q)) %>% 
      mutate(variable = "export") %>% 
      transmute(reg.exp, reg.imp, crop, year, variable, value = imp.Q / 1000) 
  ) -> 
  basedata.trade.allyear0

#move intraregional trade to domestic consumption
basedata.trade.allyear0 %>% 
  filter(reg.imp == reg.exp) %>% 
  mutate(variable = "consume.dom") %>% 
  group_by(reg.imp, reg.exp, crop, year, variable) %>% 
  summarise(value = sum(value), .groups = "drop") %>% 
  bind_rows(basedata.trade.allyear0 %>% 
              filter(variable == "export") %>% 
              mutate(value = if_else(reg.imp == reg.exp, 0, value))
  ) -> basedata.trade.allyear

rm(basedata.trade.allyear0)
#----------
#Price links

TradeGCAM.MA5 %>%  
  transmute(reg.exp, reg.imp, crop, year, imp.Q, exp.Q,
            pimp.reg = if_else(is.na(imp.V/imp.Q), 0, imp.V/imp.Q*1000),
            pexp.reg = if_else(is.na(exp.V/exp.Q), 0, exp.V/exp.Q*1000)) %>% 
  left_join(basedata.regmkt.allyear %>% filter(variable == "pp") %>% 
              spread(variable, value), by = c("reg.exp" = "reg", "crop", "year") ) %>% 
  mutate(pimp.reg = if_else(pimp.reg == 0 & pexp.reg == 0, pp * 1.12, pimp.reg),
         pexp.reg = if_else(pexp.reg <= pimp.reg, pexp.reg, pp),
         pexp.reg = if_else(pexp.reg < pimp.reg, pexp.reg, pimp.reg * 1.05),
         pexp.reg = if_else(3 * pexp.reg > pimp.reg, pexp.reg, pimp.reg * 3)) %>% 
  #filter(is.na(pimp.reg)) %>%  Note that the two margins are set to a lower bound of 1.05
  mutate(margin.reg.pim_pexp = pimp.reg / pexp.reg ) %>% 
  mutate(margin.reg.pim_pexp = if_else(is.finite(margin.reg.pim_pexp) &
                                         margin.reg.pim_pexp > 1, margin.reg.pim_pexp, 1.05)) %>%
  mutate(pexp.reg = pimp.reg/margin.reg.pim_pexp) %>% 
  mutate(margin.reg.pim_pp = pimp.reg / pp) %>% 
  mutate(margin.reg.pim_pp = if_else(is.finite(margin.reg.pim_pp) & 
                                       margin.reg.pim_pp > 1, margin.reg.pim_pp, 1.05)) %>%
  left_join(basedata.pricelink.margin.mtax %>% spread(variable, value),
            by = c("reg.exp", "reg.imp", "crop", "year")) %>% 
  mutate(margin.reg.pim_pexp.mtax.shock = margin.reg.pim_pexp * margin.mtax,
         margin.reg.pim_pp.mtax.shock = margin.reg.pim_pp * margin.mtax) %>% 
  gather("variable","value", -c(reg.exp, reg.imp, crop, year)) -> 
  basedata.pricelink.allyear

#unique(basedata.pricelink.allyear$variable)
rm(prodGCAM, TradeGCAM, TradeGCAM.MA5, basedata.pricelink.margin.mtax)
#----------




