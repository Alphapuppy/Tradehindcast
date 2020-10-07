#This file processes gtap data

gtapdatadir <- paste0(datadir, "gtap/")
files <- list.files(path = gtapdatadir, pattern = ".csv$", ignore.case = T)
gtap_bind <- function(gtapheader = "BI02"){
  lapply(files,function(file){
    read.csv(paste0(gtapdatadir, file),header = F, 
             fill = T, col.names = c(1:6), na.strings = NA) %>% 
      tibble::rowid_to_column("ID")  %>% 
      filter(grepl(pattern = "!Header:", X1)) %>% 
      transmute(ID, header = gsub("!Header:","", X1)) %>% 
      right_join(
        read.csv(paste0(gtapdatadir, file),header = F, 
                 fill = T, col.names = c(1:6), na.strings = NA) %>% 
          tibble::rowid_to_column("ID"), by = "ID" 
      ) %>% arrange(ID)%>% fill(header) %>% 
      filter(grepl(pattern = gtapheader, header),
             grepl(pattern = "!Header:", X1) == F) %>% 
      mutate_all(funs(na_if(., ""))) %>%
      #janitor::remove_empty("cols") %>%
      dplyr::select_if(function(col) !all(is.na(col))) %>%  #select_if to replace janitor
      within(rm(ID)) %>% 
      mutate(year = gsub("Gtp6x6_Y|.csv", "", file, ignore.case = T))
  } ) %>% bind_rows() 
}

#-----------------------------
#nls share 
gtap_bind("SF01") %>%  
  mutate_if(is.factor, as.character)%>%
  setNames(c("header", as.character(.[1, ])[2:(ncol(.)-1)], "year")) %>% 
  filter(Value != "Value") -> gtap.SF01

#unique(gtap.SF01$year)
study.year <- c(1995, 2000, 2005, 2010, 2015)
gtap.year  <- c(1995, 2001, 2004, 2011, 2011)

#unique(gtap.SF01$PROD_COMM)
crop_agg   <- c("Corn", "Wheat", "Rice", "Soybeans", "Rapeseed", "Others")
gtap.crop  <- c("gro", "wht", "pdr", "osd", "osd", "others")

region <- c("Africa",    "Asia",      "Europe",
            "N. America", "Oceania",   "S. America") 
gtap.reg <- setdiff(unique(gtap.SF01$REG), "ROW")


gtap.SF01 %>% 
  filter(PROD_COMM %in% gtap.crop, 
         REG != "ROW",
         year %in% gtap.year) %>% 
  group_by_at(vars(names(.)[2:4], year)) %>% 
  summarise(value = sum(as.numeric(Value)),.groups = 'drop') %>% 
  ungroup() %>% 
  group_by_at(vars(REG, PROD_COMM, year)) %>% 
  mutate(value = value / sum(value)) %>% 
  ungroup() %>% 
  filter(DEMD_COMM == "Land") %>% 
  transmute(reg = REG, gtap.crop = PROD_COMM, gtap.year = as.numeric(year), 
            variable = "nlc.share", value = 1 - value) -> 
  gtap.nlc.share

expand.grid(region = region, 
            crop_agg = crop_agg, study.year = study.year) %>% 
  left_join(data.frame(study.year, gtap.year), by = "study.year") %>% 
  left_join(data.frame(crop_agg, gtap.crop), by = "crop_agg") %>% 
  left_join(data.frame(region, gtap.reg), by = "region") %>% 
  left_join(gtap.nlc.share, by = c("gtap.year", "gtap.crop")) %>% 
  transmute(reg = region, crop = crop_agg, year = study.year, 
            variable, value) ->
  basedata.nlc

#-----------------------------
#tariffs 

gtap_bind("BI02") %>% 
  setNames(c("header", "TRAD_COMM", "reg.exp", "reg.imp", "IMPVALUE", "Value","year")) %>% 
  filter(Value != "Value") -> gtap.BI02

gtap.BI02 %>% 
  filter(TRAD_COMM %in% gtap.crop, 
         reg.exp != "ROW",
         reg.imp != "ROW",
         year %in% gtap.year) %>% 
  transmute(gtap.crop = TRAD_COMM,
            gtap.reg.exp = reg.exp,
            gtap.reg.imp = reg.imp,
            gtap.year = as.numeric(year),
            IMPVALUE, value = as.numeric(Value)) %>% 
  spread(IMPVALUE, value) %>% 
  mutate(value = if_else(is.finite((impcost + mtax) / impcost),
                               (impcost + mtax) / impcost, 1) ) ->
  gtap.margin.mtax


expand.grid(reg.exp = region, reg.imp = region,
            crop_agg = crop_agg, study.year = study.year) %>% 
  left_join(data.frame(study.year, gtap.year), by = "study.year") %>% 
  left_join(data.frame(crop_agg, gtap.crop), by = "crop_agg") %>% 
  left_join(data.frame(reg.exp = region, gtap.reg.exp = gtap.reg), by = "reg.exp") %>% 
  left_join(data.frame(reg.imp = region, gtap.reg.imp = gtap.reg), by = "reg.imp") %>%
  left_join(gtap.margin.mtax, by = c("gtap.year", "gtap.crop", "gtap.reg.exp", "gtap.reg.imp")) %>% 
  transmute(reg.exp, reg.imp, crop = crop_agg, year = study.year, 
            variable = "margin.mtax", 
            value = if_else(is.finite(value), value, 1)) ->
  basedata.pricelink.margin.mtax

rm(list = ls(pattern = "gtap*"))
#-----------------------------
  
  






