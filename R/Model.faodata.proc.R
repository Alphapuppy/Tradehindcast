#rm(list=ls())
library(ggplot2)
library(dplyr)
library(tidyr)

source("R/Model.func.R")
outdir <- ("./output/")
datadir <- ("./data/")

FAO_ag_items_TRADE <- read.csv(paste0(datadir, "FAO_ag_items_TRADE.csv"), comment.char = '#')
AGLU_ctry <- read.csv(paste0(datadir, "FAO_GCAM_reg_mapping.csv"), comment.char = '#')
iso_GCAM_regID <- read.csv(paste0(datadir, "iso_GCAM_regID.csv"), comment.char = '#') %>% 
  dplyr::select(iso, region,REG_continent)

files <- gsub("_E_All_Data|_NOFLAG|_DetailedTradeMatrix|Primary|.csv", "", 
              list.files(path = datadir, pattern = "NOFLAG.csv$"))

files <- "Trade"

gather_years_na_zero <- function(.data){
  .data %>%
    gather(year, value, names(.)[grepl("[0-9]{4}", names(.))]) %>%
    mutate(year = as.integer(gsub("X|Y|F", "", year)),
           value = replace_na(value, 0)) %>%
    return()
}

lapply(files, function(file){
  assign(paste0(file), read.csv(
    paste0(datadir, list.files(path = datadir, pattern = paste0("^",file,".*NOFLAG.csv$")))
  ) %>% gather_years_na_zero(), 
  envir = .GlobalEnv) })

Trade %>% left_join(FAO_ag_items_TRADE %>% select(item.code, GCAM_commodity),
                    by = c(Item.Code = "item.code")) %>% 
  filter(!is.na(GCAM_commodity)) -> 
  Trade1
unique(Trade1$Element.Code)

Trade1 %>% 
  filter(Element.Code %in% c(5610, 5622, 5910, 5922)) %>% 
  within(rm(Unit, Element.Code)) %>%
  spread(Element, value) %>% 
  rename(imp.V = `Import Value`, imp.Q = `Import Quantity`, exp.V = `Export Value`, exp.Q = `Export Quantity`) %>% 
  replace(is.na(.), 0) %>%
  mutate(imp.V = if_else(imp.V * imp.Q == 0, 0, imp.V),
         imp.Q = if_else(imp.V * imp.Q == 0, 0, imp.Q),
         exp.V = if_else(exp.V * exp.Q == 0, 0, exp.V),
         exp.Q = if_else(exp.V * exp.Q == 0, 0, exp.Q)) ->
  Trade2

Trade2 %>% #filter(Item %in% top.traded.item) %>% 
  rename(reg.imp = Reporter.Countries, reg.exp = Partner.Countries) %>% 
  gather(variable, value, -c(1:8)) %>% 
  within(rm(Reporter.Country.Code, Partner.Country.Code))-> Trade3

saveRDS(Trade3,paste0(datadir, "Trade3.rds"))

readRDS(paste0(datadir, "RDS/Trade3GCAM.rds")) -> Trade3GCAM

Trade3 %>% 
  filter(reg.imp %in% unique(AGLU_ctry$FAO_country),
         reg.exp %in% unique(AGLU_ctry$FAO_country)) %>% 
  left_join(select(AGLU_ctry %>% 
                     group_by(FAO_country) %>% 
                     summarise_at(vars(-group_cols()), first), 
                   FAO_country, region.imp = region),
            by = c(reg.imp = "FAO_country")) %>%
  left_join(select(AGLU_ctry %>% 
                     group_by(FAO_country) %>% 
                     summarise_at(vars(-group_cols()), first),
                   FAO_country, region.exp = region),
            by = c(reg.exp = "FAO_country")) %>% 
  group_by(reg.imp = region.imp, reg.exp = region.exp, 
           Item = GCAM_commodity, year, variable) %>% 
  summarise(value = sum(value)) %>% 
  ungroup() %>% 
  spread(variable, value) %>%
  mutate(imp.P = if_else(is.na(imp.V/imp.Q), 0, imp.V/imp.Q)) -> 
  Trade3GCAM

saveRDS(Trade3GCAM,paste0(datadir, "RDS/Trade3GCAM.rds"))

unique(Trade3GCAM$Item)
GCAM_crop <-c("Corn", "Wheat", "Rice", "OilCrop", "FiberCrop", "MiscCrop", "OtherGrain", "PalmFruit", "RootTuber","SugarCrop", "FodderHerb",
              "Soybeans", "Rapeseed") # no "FodderGrass"
crop_agg <- c("Corn", "Wheat", "Rice", "Soybeans", "Rapeseed", "Others")

Trade3GCAM %>% filter(Item %in% GCAM_crop, year > 1990) %>% 
  mutate(Item = as.character(Item),
    Item = if_else(Item %in% crop_agg, Item, "Others")) %>% 
  left_join(iso_GCAM_regID %>% select(region, region.imp = REG_continent) %>% distinct(),
            by = c(reg.imp = "region")) %>% 
  left_join(iso_GCAM_regID %>% select(region, region.exp = REG_continent) %>% distinct(),
            by = c(reg.exp = "region")) %>% 
  mutate(Item = if_else(Item %in% crop_agg, Item, "Others")) %>% 
  group_by(reg.imp = region.imp, reg.exp = region.exp, 
           Item, year) %>% 
  summarise_at(vars(imp.Q,imp.V,exp.Q,exp.V), sum) %>% 
  mutate(imp.P = if_else(is.na(imp.V/imp.Q), 0, imp.V/imp.Q),
         exp.P = if_else(is.na(exp.V/exp.Q), 0, exp.V/exp.Q)) %>% 
  ungroup() -> TradeGCAMmini
saveRDS(TradeGCAMmini,paste0(datadir, "RDS/TradeGCAMmini.rds"))  


ggplot(TradeGCAMmini %>% filter(imp.P <10, year > 1994, reg.exp != "Oceania")) +
  geom_line(aes(x = year, y = imp.P, color = reg.exp)) + 
  facet_grid(cols = vars(reg.imp), rows = vars(Item), scales = "free") +
  theme_bw()

ggplot(TradeGCAMmini) +
  geom_bar(aes(x = year, y = imp.Q, fill = reg.exp), position="fill", stat="identity", color = "black", size = 0.3) + 
  facet_grid(cols = vars(reg.imp), rows = vars(Item), scales = "free") +
  theme_bw() -> import.share
Write_png(import.share, "importshare", w = 9000, h = 4500, r = 400 )

rm(Trade3)
################################

files <- c("Prices", "Production_Crops", "Production_Livestock")

lapply(files, function(file){
  assign(paste0(file), read.csv(
    paste0(datadir, list.files(path = datadir, pattern = paste0("^",file,".*NOFLAG.csv$")))
  ) %>% gather_years_na_zero(), 
  envir = .GlobalEnv) })


Production <- Production_Crops %>% bind_rows(Production_Livestock) %>% 
  filter(Element %in% c("Production", "Area harvested"), 
         Element.Code %in% c(5510, 5312), 
         year > 1986) 



COMM0 <- sort( intersect(unique(Prices$Item), unique(Production$Item)))  

Production %>% filter(Item %in% COMM0, Element== "Production") %>% 
  dplyr::select(Area, Item, year, prod = value) %>% 
  left_join(select(AGLU_ctry %>% 
                     group_by(FAO_country) %>% summarise_at(vars(-group_cols()), first), FAO_country, iso),
            by = c(Area = "FAO_country")) %>% filter(!is.na(iso)) %>% 
  left_join(Prices %>% filter(Item %in% COMM0,
                              Element.Code == 5532,
                              Area %in% unique(AGLU_ctry$FAO_country)) %>% 
              left_join(select(AGLU_ctry %>% 
                                 group_by(FAO_country) %>% summarise_at(vars(-group_cols()), first), FAO_country, iso, region),
                        by = c(Area = "FAO_country")) %>% 
              dplyr::select(Area, Item, year, iso, price = value, region,  Item.Code)) %>% 
  left_join(FAO_ag_items_TRADE %>% select(item.code, GCAM_commodity),
            by = c(Item.Code = "item.code")) %>% 
  filter(!is.na(GCAM_commodity)) %>% 
  replace(is.na(.), 0) %>%
  mutate(revenue =  price * prod) %>% 
  group_by(region, Item = GCAM_commodity, year) %>% 
  summarise_at(vars(prod, revenue), sum) %>% 
  ungroup()  -> prod.value

Production %>% filter(Item %in% COMM0, Element== "Area harvested") %>% 
  dplyr::select(Area, Item.Code, Item, year, H.area = value) %>% 
  left_join(select(AGLU_ctry %>% 
                     group_by(FAO_country) %>% summarise_at(vars(-group_cols()), first), FAO_country, iso, region),
            by = c(Area = "FAO_country")) %>% filter(!is.na(iso)) %>% 
  left_join(FAO_ag_items_TRADE %>% select(item.code, GCAM_commodity),
            by = c(Item.Code = "item.code")) %>% 
  filter(!is.na(GCAM_commodity)) %>% 
  replace(is.na(.), 0) %>%
  group_by(region, Item = GCAM_commodity, year) %>% 
  summarise_at(vars(H.area), sum) %>% 
  ungroup()  -> area.harvested
  
unique(area.harvested$Item)

prod.value %>% filter(Item %in% GCAM_crop) %>% 
  left_join(area.harvested) %>% 
  replace(is.na(.), 0) %>%
  mutate(Item = as.character(Item),
         Item = if_else(Item %in% crop_agg, Item, "Others")) %>% 
  left_join(iso_GCAM_regID %>% select(region, REG_continent) %>% distinct()) %>% 
  mutate(Item = if_else(Item %in% crop_agg, Item, "Others")) %>% 
  group_by(region = REG_continent, Item, year) %>% 
  summarise_at(vars(prod, revenue, H.area), sum) %>% 
  mutate(pp = if_else(is.na(revenue/prod), 0, revenue/prod)) %>% 
  ungroup() -> prodGCAMmini

saveRDS(prodGCAMmini,paste0(datadir, "RDS/prodGCAMmini.rds"))  

