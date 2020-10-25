#load rds result and prcess
list.files(paste0(outdir, "results"))
load(paste0(outdir, "results/","para.S1.Y0.good.M2.Rdata"))

sol.out[[1]]$par -> para.S1.Y0.good
parameters <- para.S1.Y0.good

optimHess(para.S1.Y0.good, fn) -> para.S1.Y0.good.Hess
save(para.S1.Y0.good.Hess, file = paste0("output/results/para.S1.Y0.good.Hess.Rdata"))
sqrt(diag(2*sol.out[[1]]$value /(6^3 * 4 - 2) * solve(para.S1.Y0.good.Hess))) *1.96
sqrt(diag(solve(para.S1.Y0.good.Hess)))




load(paste0(outdir, "results/","para.S2.Y0.good.M2.Rdata"))
sol.out[[1]]$par -> para.S2.Y0.good
optimHess(para.S2.Y0.good, fn) -> para.S2.Y0.good.Hess
save(para.S2.Y0.good.Hess, file = paste0("output/results/para.S2.Y0.good.Hess.Rdata"))
sqrt(diag(2*sol.out[[1]]$value /(6^3 * 4 - 2) * solve(para.S2.Y0.good.Hess))) *1.96

sqrt(diag(sol.out[[1]]$value /(6^3 * 4 - 2) * solve(para.S2.Y0.good.Hess))) *1.96

sqrt(2* sol.out[[1]]$value /(6^3 * 4 - 2) * diag( solve(para.S2.Y0.good.Hess))) *1.96

sqrt(2* sol.out[[1]]$value /(6^3 * 4 - 5)) *
sqrt(diag( solve(para.S2.Y0.good.Hess))) *1.96

sqrt(2*sol.out[[1]]$value ) 
/(6^3 * 4 - 5)


load(paste0(outdir, "results/","para.S3.Y0.good.M2SSE.Rdata"))
sol.out[[1]]$par -> para.S3.Y0.good
optimHess(para.S3.Y4.good, fn) -> para.S3.Y4.good.Hess

parameters <- para.S3.Y0.good

parameters <- para.S2.Y0.good





load(paste0(outdir, "results/","para.S2.Y4.good.M2.Rdata"))
sol.out[[1]]$par -> para.S2.Y4.good
optimHess(para.S2.Y4.good, fn) -> para.S2.Y4.good.Hess

sqrt(2* sol.out[[1]]$value /(6^3  - 3) * diag( solve(para.S2.Y4.good.Hess))) *1.96




load(paste0(outdir, "results/","para.S3.Y0.good2.M2.Rdata"))
sol.out[[1]]$par -> para.S3.Y0.good2

parameters <- para.S2.Y0.good

list.files(paste0(outdir, "results"),pattern = "^para.S3.*?good.M2.Rdata$")


###############################
list.files(paste0(outdir, "results"),pattern = "^para.*?good.M2SSE.Rdata$")

load(paste0(outdir, "results/","para.S2.Y0.good.M2SSE.Rdata"))
sol.out[[1]]$par -> parameters
dof = (6^3 * 4  - length(sol.out[[1]]$par))
sqrt(diag(2*sol.out[[1]]$value / dof * solve(sol.out[[1]]$hessian)))*1.96

(sol.out[[1]]$par - c(0, 0, 1,1,1,1))/
sqrt(diag(2*sol.out[[1]]$value /dof * solve(sol.out[[1]]$hessian))) 

2*pt(
  (sol.out[[1]]$par - c(0, 0, 1,1,1,1))/
    sqrt(diag(2*sol.out[[1]]$value /dof * solve(sol.out[[1]]$hessian))), 
  dof, lower=FALSE) * 100


load(paste0(outdir, "results/","para.S2.Y4.good.M2SSE.Rdata"))
sol.out[[1]]$par
dof = (6^3   - length(sol.out[[1]]$par))
sqrt(diag(2*sol.out[[1]]$value / dof * solve(sol.out[[1]]$hessian)))*1.96

2*pt(
  (sol.out[[1]]$par - c(0, 0, 1))/
    sqrt(diag(2*sol.out[[1]]$value /dof * solve(sol.out[[1]]$hessian))), 
  dof, lower=FALSE) * 100



load(paste0(outdir, "results/","para.S1.Y0.good.M2SSE.Rdata"))
sol.out[[1]]$par 
dof = (6^3 * 4  - length(sol.out[[1]]$par))
sqrt(diag(2*sol.out[[1]]$value / dof * solve(sol.out[[1]]$hessian)))*1.96

2*pt(
  (sol.out[[1]]$par - c(0, 0))/
    sqrt(diag(2*sol.out[[1]]$value /dof * solve(sol.out[[1]]$hessian))), 
  dof, lower=FALSE) * 100

load(paste0(outdir, "results/","para.S1.Y4.good.M2SSE.Rdata"))
sol.out[[1]]$par
dof = (6^3 * 1  - length(sol.out[[1]]$par))
sqrt(diag(2*sol.out[[1]]$value / dof * solve(sol.out[[1]]$hessian)))*1.96

2*pt(
  (sol.out[[1]]$par - c(0, 0))/
    sqrt(diag(2*sol.out[[1]]$value /dof * solve(sol.out[[1]]$hessian))), 
  dof, lower=FALSE) * 100


load(paste0(outdir, "results/","para.S3.Y0.good.M2SSE.Rdata"))
sol.out[[1]]$par -> parameters
dof = (6^3 * 4  - length(sol.out[[1]]$par))
sqrt(diag(2*sol.out[[1]]$value / dof * solve(sol.out[[1]]$hessian)))*1.96

2*pt(
  (sol.out[[1]]$par - c(0, 0, 1.641313, 1.641313, 1.641313, 1.641313))/
    sqrt(diag(2*sol.out[[1]]$value /dof * solve(sol.out[[1]]$hessian))), 
  dof, lower=FALSE) * 100

load(paste0(outdir, "results/","para.S3.Y1.good.M2SSE.Rdata"))
sol.out[[1]]$par
dof = (6^3 * 4  - length(sol.out[[1]]$par))
sqrt(diag(2*sol.out[[1]]$value / dof * solve(sol.out[[1]]$hessian)))*1.96

2*pt(
  (sol.out[[1]]$par - c(0, 0, 1.641313, 1.641313, 1.641313, 1.641313))/
    sqrt(diag(2*sol.out[[1]]$value /dof * solve(sol.out[[1]]$hessian))), 
  dof, lower=FALSE) * 100

load(paste0(outdir, "results/","para.S1.Y0.good.M2SSE.Rdata"))
sol.out[[1]]$par -> parameters
updated.db.equil -> updated.db.equil.S1.y0
load(paste0(outdir, "results/","para.S2.Y0.good.M2SSE.Rdata"))
sol.out[[1]]$par -> parameters
updated.db.equil -> updated.db.equil.S2.y0

load(paste0(outdir, "results/","para.S3.Y0.good.M2SSE.Rdata"))
sol.out[[1]]$par -> parameters
updated.db.equil -> updated.db.equil.S3.y0

updated.db.equil -> updated.db.equil.S1.3030
updated.db.equil -> updated.db.equil.S1.36
updated.db.equil -> updated.db.equil.S1.y0
updated.db.equil -> updated.db.equil.S2.y0
updated.db.equil -> updated.db.equil.S3.y0
##################################


updated.db.equil.S2.y0 %>% 
  filter(scenario == "est") %>% mutate(scenario = "S2") %>% 
  bind_rows(
    updated.db.equil.S1.y0 %>% 
      filter(scenario == "est") %>% mutate(scenario = "S1")
  ) %>% bind_rows(
    updated.db.equil.S1.36  %>% 
      filter(scenario == "est") %>% mutate(scenario = "S0")
  ) %>% bind_rows(
    updated.db.equil.S3.y0 %>% 
      filter(scenario == "est") %>% mutate(scenario = "S3")
  ) %>% bind_rows(
    updated.db.equil.S1.36 %>% 
      filter(scenario %in% c("obs", "ref"))
  ) -> data1

saveRDS(data1, "./data/RDS/outbase.rds")  


data1 %>% 
  gather(equil,"value", c(consumption, price)) %>% 
  filter(!(reg.imp == reg.exp & variable == "export")) %>% 
  mutate(reg.exp = if_else(reg.exp == "Domestic", reg.imp, reg.exp)) %>% 
  rename(year = target.yr) %>% 
  spread(scenario, value) -> 
  Tong

saveRDS(Tong, "./data/RDS/Tong.rds")  

data1 %>% 
  mutate(expense = price * consumption) %>% 
  gather(equil,"value", c(consumption, price, expense)) %>% 
  filter(!(reg.imp == reg.exp & variable == "export")) %>% 
  mutate(reg.exp = if_else(reg.exp == "Domestic", reg.imp, reg.exp),
         scenario = if_else(scenario == "obs", "Obs.", scenario),
         scenario = if_else(scenario == "ref", "1995", scenario)) -> 
  data2


reg_agg <- c("Africa", "Asia", "Europe", "N. America", "S. America", "Oceania")  
crop_agg <- c("Corn", "Wheat", "Rice", "Soybeans", "Rapeseed", "Others")
data2$reg.exp <-  factor(data2$reg.exp, levels = reg_agg)  
data2$reg.imp <-  factor(data2$reg.imp, levels = reg_agg)  
data2$crop <-  factor(data2$crop, levels = crop_agg)  
data2$scenario <-  factor(data2$scenario , levels = c("1995", "Obs.", "S0",
                                                      "S1", "S2", "S3"))  


##########################################
#per year results

for (yr in seq(2000, 2015, 5)) {
  
  ggplot(data2 %>% filter(!scenario %in% c("1995", "S3"), equil == "consumption",
                          target.yr == yr) %>% 
           mutate(value = value / 1000)) + 
    geom_bar(aes(x = scenario, y = value, fill = reg.exp), 
             color = "black", alpha = 0.85, stat="identity",position= "stack") +
    ggsci::scale_fill_npg(name = "Source") +
    labs(x = "Scenario", y = "Volume (Million tons)") +
    facet_grid(rows = vars(crop),
               cols = vars(reg.imp), scales = "free") +
    theme_bw() + theme0 + theme_leg -> scen.compare1
  
  Write_png(scen.compare1, paste0("good/result.stack.y", yr), h = 6000, w = 11000)
}


for (yr in seq(2000, 2015, 5)) {
  
  ggplot(data2 %>% filter(!scenario %in% c("1995", "S3"), 
                          equil == "consumption", target.yr == yr) %>% 
           mutate(value = value / 1000)) + 
    geom_bar(aes(x = scenario, y = value, fill = reg.exp), 
             color = "black", alpha = 0.85, stat="identity",position= "fill") +
    ggsci::scale_fill_npg(name = "Source") +
    labs(x = "Scenario", y = "Volume share") +
    facet_grid(rows = vars(crop),
               cols = vars(reg.imp), scales = "free") +
    theme_bw() + theme0 + theme_leg -> scen.compare1
  
  Write_png(scen.compare1, paste0("good/result.fill.y", yr), h = 6000, w = 11000)
}
#################################################


#per year results

for (yr in seq(2000, 2015, 5)) {
  
  ggplot(data2 %>% filter(equil == "consumption",
                          target.yr == yr) %>% 
           mutate(value = value / 1000)) + 
    geom_bar(aes(x = scenario, y = value, fill = reg.exp), 
             color = "black", alpha = 0.85, stat="identity",position= "stack") +
    ggsci::scale_fill_npg(name = "Source") +
    labs(x = "Scenario", y = "Volume (Million tons)") +
    facet_grid(rows = vars(crop),
               cols = vars(reg.imp), scales = "free") +
    theme_bw() + theme0 + theme_leg -> scen.compare1
  
  Write_png(scen.compare1, paste0("good/SI.result.stack.y", yr), h = 7000, w = 13000)
}


for (yr in seq(2000, 2015, 5)) {
  
  ggplot(data2 %>% filter(equil == "consumption", target.yr == yr) %>% 
           mutate(value = value / 1000)) + 
    geom_bar(aes(x = scenario, y = value, fill = reg.exp), 
             color = "black", alpha = 0.85, stat="identity",position= "fill") +
    ggsci::scale_fill_npg(name = "Source") +
    labs(x = "Scenario", y = "Volume share") +
    facet_grid(rows = vars(crop),
               cols = vars(reg.imp), scales = "free") +
    theme_bw() + theme0 + theme_leg -> scen.compare1
  
  Write_png(scen.compare1, paste0("good/SI.result.fill.y", yr), h = 7000, w = 13000)
}
  

##########################################
#per scenario results
for (ss in c("S0", "S1", "S2", "S3")) {

 ggplot(data2 %>% filter(scenario == ss, equil == "consumption") %>% 
         mutate(value = value / 1000)) + 
  geom_bar(aes(x = target.yr, y = value, fill = reg.exp), 
           color = "black", alpha = 0.85, stat="identity",position= "stack") +
  ggsci::scale_fill_npg(name = "Source") +
  labs(x = "Scenario", y = "Volume (Million tons)") +
  facet_grid(rows = vars(crop),
             cols = vars(reg.imp), scales = "free") +
  theme_bw() + theme0 + theme_leg -> scen.stack
  
  Write_png(scen.stack, paste0("good/result.stack.", ss), h = 6000, w = 11000)
}


for (ss in c("S0", "S1", "S2", "S3")) {
  
  ggplot(data2 %>% filter(scenario == ss, equil == "consumption") %>% 
           mutate(value = value / 1000)) + 
    geom_bar(aes(x = target.yr, y = value, fill = reg.exp), 
             color = "black", alpha = 0.85, stat="identity",position= "fill") +
    ggsci::scale_fill_npg(name = "Source") +
    labs(x = "Scenario", y = "Volume") +
    facet_grid(rows = vars(crop),
               cols = vars(reg.imp), scales = "free") +
    scale_y_continuous(breaks = c(0.25, 0.5, 0.75)) +
    theme_bw() + theme0 + theme_leg -> scen.fill
  
  Write_png(scen.fill, paste0("good/result.fill.", ss), h = 6000, w = 11000)
}


####################################################################

#------------------------------------------
fontfamily = "Arial"
windowsFonts(Arial=windowsFont("TT Arial"))
theme0 <- theme(
  #panel.grid.minor = element_line(size = 0.1, linetype = 2,colour = "grey30"),
  #panel.grid.major = element_line(size = 0.1, linetype = 2,colour = "grey30"),
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),
  panel.border = element_rect(colour = "black", size=1),
  text = element_text(family= fontfamily, size = 15),
  axis.text.y = element_text(angle = 0, color = "black", size = 15, margin = margin(r = 10)),
  axis.text.x = element_text(angle = 0, color = "black", size = 15, margin = margin(t = 10), vjust= 0.5),
  axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 10, b = 0, l = 0)),
  axis.title.x = element_text(size = 15, margin = margin(t = 10, r = 0, b = 0, l = 0)),
  #axis.ticks = element_line(linetype = 1,size = 0.5),
  #axis.ticks.length = unit(-0.1, "cm"),
  axis.text.y.right =  element_blank(),  axis.title.y.right = element_blank(),
  axis.text.x.top =  element_blank(),  axis.title.x.top = element_blank(),
  strip.background = element_rect(fill="grey95"),
  strip.text = element_text(size = 16),
  plot.title = element_text(hjust = 0.5,margin=margin(0,0,15,0)),
  plot.margin = margin(t = 10, r = 20, b = 10, l = 10) #panel.spacing = unit(1, "lines"),
)

theme_leg <- theme(legend.position="right", legend.justification = "center",
                   #legend.position=c(.1,0.7),
                   #legend.title = element_blank(),
                   legend.key.size = unit(1.5, "cm"),
                   legend.key.height=unit(1.5,"line"),
                   legend.spacing.x = unit(1, 'cm'), #legend.spacing.y = unit(5, 'cm'),
                   legend.text = element_text(margin = margin(l = -25,t=0, b=0), size = 15),
                   legend.box.margin=margin(-10, 10,-8,10),
                   legend.background = element_blank()) 








updated.db.equil %>% gather(equil,"value", c(consumption, price)) %>% 
  filter(!(reg.imp == reg.exp & variable == "export")) %>% 
  spread(scenario, value) -> B

updated.db.equil %>% 
  mutate(expense = price * consumption) %>% 
  gather(equil,"value", c(consumption, price, expense)) %>% 
  filter(!(reg.imp == reg.exp & variable == "export")) -> data1 

data1 %>% filter(equil == "expense") %>% 
  group_by(reg.imp, variable, scenario, target.yr) %>% 
  summarise(value = sum(value)) %>% 
  spread(scenario, value) %>% 
  #mutate(est = est / obs -1, obs = obs / obs -1) %>% 
  #mutate(est = est / ref -1, obs = obs / ref -1) %>% 
  #mutate(est = est - ref, obs = obs - ref) %>% 
  #mutate(est = log(est / ref), obs = log(obs / ref)) %>% 
  gather(scenario, value, "est", "obs", "ref") -> A



ggplot(A %>% filter( target.yr == 2015)) +
    geom_bar(aes(x = scenario, y = value, fill = scenario), 
             stat="identity", color="black", position=position_dodge()) + 
    facet_grid(vars(variable), vars(reg.imp), scales = "free")

ggplot(A %>% filter(scenario != "ref", target.yr == 2015)) +
  geom_bar(aes(x = crop, y = value, group = scenario, fill = scenario), 
           stat="identity", color="black", position=position_dodge()) + 
  facet_grid(vars(variable), vars(reg.imp), scales = "free")



ggplot(data1 %>% filter(crop == "soybeans"))  



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
  summarise(wmean.logdist.logw = weighted.mean(logdist, log(weight + 1)),
            weight = sum(weight), .groups = "drop") %>%  ungroup() %>% 
  summarise(Err = weighted.mean(wmean.logdist.logw, weight)) %>% 
  pull(Err) -> Error


updated.db.equil %>% gather(equil,"value", c(consumption, price)) %>% 
  filter(!(reg.imp == reg.exp & variable == "export")) %>% 
  spread(scenario, value) %>% 
  transmute(reg.imp, reg.exp, crop, variable, target.yr, equil, logdiff = (log(est / obs))^2) %>% 
  spread(equil, logdiff) %>% 
  filter(is.na(consumption) == F & is.infinite(consumption) == F
         #, is.finite(price)
  ) %>% 
  mutate(logdist = (consumption + price)) %>%
  left_join(updated.db.equil %>% filter(scenario == "obs") %>% 
              within(rm(scenario, price)) %>% rename(weight = consumption), 
            by = c("reg.imp", "reg.exp", "crop", "variable", "target.yr")) %>% 
  group_by(target.yr) %>% 
  summarise(wmean.logdist.logw = weighted.mean(logdist, weight^0.5),
            weight = sum(weight), .groups = "drop") %>%  ungroup() %>% 
  summarise(Err = weighted.mean(wmean.logdist.logw, weight)) %>% 
  pull(Err) -> Error

###########################


updated.db.equil %>% gather(equil,"value", c(consumption, price)) %>% 
  filter(!(reg.imp == reg.exp & variable == "export")) %>% 
  spread(scenario, value) -> B

ggplot(B) +
  geom_point(aes(x = log(obs), y = log(est))) +
  facet_grid(rows = vars(equil), cols = vars(target.yr), scales = "free") +
  geom_abline(intercept = 0, slope = 1) #+  coord_equal()

ggplot(B %>% mutate(Year = as.character(target.yr))) +
  geom_abline(intercept = 0, slope = 1) + 
  geom_point(aes(x = (obs), y = (est), color = Year), alpha = 0.8) +
  facet_wrap(~equil, scales = "free") +
  #scale_x_continuous(expand = c(0, 0)) +
  #scale_y_continuous(expand = c(0, 0)) +
  theme_bw() + theme0 + theme_leg +
  theme(panel.spacing.x=unit(1, "lines"))


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
summary(lm(logest ~ logobs , data = C, weights = weight ))
summary(lm(logest ~ logobs , data = C %>% filter(equil == "price"), weights = log(weight + 1)))
summary(lm(logest ~ logobs , data = C %>% filter(equil == "consumption"), weights = log(weight + 1)))

summary(lm(logest ~ logobs , data = C %>% filter(equil == "consumption"), weights = weight))
summary(lm(logest ~ logobs , data = C %>% filter(equil == "price"), weights = weight))

summary(lm(logobs ~ logest , data = C , weights = log(weight + 1)))
summary(lm(logobs ~ logest , data = C %>% filter(equil == "price"), weights = log(weight + 1)))
summary(lm(logobs ~ logest , data = C %>% filter(equil == "consumption"), weights = log(weight + 1)))



summary(lm(logest ~ logobs , data = C %>% filter(equil == "price"), weights = weight^0.5))
summary(lm(logest ~ logobs , data = C %>% filter(equil == "consumption"), weights = weight^0.5))


ggplot(C) +
  geom_point(aes(x = logobs, y = logest)) +
  facet_grid(rows = vars(equil), cols = vars(target.yr), scales = "free") +
  geom_abline(intercept = 0, slope = 1)



B %>% mutate(logest = log(est/ref), logobs = log(obs/ref)) %>% 
  filter(is.finite(logest), is.finite(logobs)) %>% 
  left_join(updated.db.equil %>% filter(scenario == "obs") %>% 
              within(rm(scenario, price)) %>% rename(weight = consumption), 
            by = c("reg.imp", "reg.exp", "crop", "variable", "target.yr")) %>% 
  mutate(logest = logest * weight, logobs = logobs * weight)-> C














updated.db.equil %>% gather(equil,"value", c(consumption, price)) %>% 
  filter(!(reg.imp == reg.exp & variable == "export"),
         equil != "price") %>% 
  group_by(variable, scenario, target.yr) %>% 
  summarize(value = sum(value)) %>% 
  spread(scenario, value) %>% 
  mutate(diff = est/obs)-> A




updated.db.equil -> df1
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
  summarise(wmean.logdist.logw = weighted.mean(logdist, weight ^0.5),
            weight = sum(weight), .groups = "drop") %>%  ungroup() %>% 
  summarise(Err = weighted.mean(wmean.logdist.logw, weight)) %>% 
  pull(Err) -> Error





