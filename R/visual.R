#Check weights
#----------
basedata.trade.fig <-  basedata.trade.allyear %>%  
  mutate(reg.exp = if_else(as.factor(variable) == "consume.dom", "Domestic", as.character(reg.exp)))

basedata.trade.fig$crop <-  factor(basedata.trade.fig$crop, levels = crop_agg)
basedata.trade.fig$reg.exp <- factor(basedata.trade.fig$reg.exp, levels = c("Domestic", as.character(unique(basedata.trade.allyear$reg.exp))))
ggplot(basedata.trade.fig) +
  geom_bar(aes(x = year, y = value, fill = reg.exp), position="stack", stat="identity", color = "black", size = 0.3) + 
  facet_grid(cols = vars(reg.imp), rows = vars(crop), scales = "fixed") +
  theme_bw() -> import.share.stack

ggplot(basedata.trade.fig) +
  geom_bar(aes(x = year, y = value, fill = reg.exp), position="fill", stat="identity", color = "black", size = 0.3) + 
  facet_grid(cols = vars(reg.imp), rows = vars(crop), scales = "fixed") +
  theme_bw() -> import.share.fill

#-----------------------

basedata.trade.fig %>% 
  mutate(variable = if_else(reg.imp == reg.exp, "intraregional", variable),
         variable = if_else(variable == "export", "interregional", variable)) %>% 
  group_by(year, crop, variable) %>% 
  summarise(value = sum(value)) %>% ungroup() -> basedata.trade.fig1 

basedata.trade.fig1 %>% #group_by(year, crop) %>% 
  filter(!crop %in% c("Others", "Rapeseed")) %>% 
  group_by(variable) %>% summarise(value = sum(value)) %>% ungroup() %>% 
  mutate(value = value / sum(value) *100) %>% 
  spread(variable, value) -> A

basedata.trade.fig1 %>% 
  group_by(variable, crop) %>% summarise(value = sum(value)) %>% ungroup() %>% 
  group_by(crop) %>% 
  mutate(value = value / sum(value) *100) %>% 
  spread(variable, value)

ggplot(basedata.trade.fig1) +
  geom_bar(aes(x = year, y = value, fill = variable), position="stack", stat="identity", color = "black", size = 0.3) + 
  facet_wrap(~crop, scales = "fixed", nrow = 1) +
  theme_bw() -> import.share.stack1

ggplot(basedata.trade.fig1) +
  geom_bar(aes(x = year, y = value, fill = variable), position="fill", stat="identity", color = "black", size = 0.3) + 
  facet_wrap(~crop, scales = "fixed", nrow = 1) +
  theme_bw() -> import.share.fill1


ggplot(B) +
  geom_bar(aes(x = year, y = value, fill = crop), position="stack", stat="identity") + 
  facet_grid(rows = vars(reg.exp)) + 
  theme_bw() + theme0 + theme_leg


basedata.trade.allyear %>% group_by(year) %>% 
  #mutate(value = value ^.25) %>% 
  #mutate(value = log(value+1) ) %>% 
  mutate(value = value / sum(value) *100) %>% 
  mutate(reg.exp = if_else(variable == "consume.dom", "Domestic", reg.exp))->A 
  #A %>% spread(year, value)

ggplot(A) +
  geom_bar(aes(x = year, y = value, fill = crop), position="stack", stat="identity") + 
  facet_grid(rows = vars(reg.imp), cols = vars(reg.exp)) + 
  theme_bw() + theme0 + theme_leg


updated.db.equil %>% filter(!crop %in% c("Others")) %>% 
   group_by(scenario, target.yr, variable) %>% 
  summarise(value = sum(consumption)/1000) ->B

ggplot(updated.db.equil %>% filter(scenario == "obs", crop != "Others")) +
  geom_point(aes(x = consumption, y = price, shape = scenario, color = reg.exp), alpha = 0.9, size = 2) + 
  labs(y ="Price" , x = "Quantity") +
  theme_bw() + theme0 + theme_leg 

ggplot(updated.db.equil %>% 
         filter(target.yr == 2000, scenario == "obs", crop != "Others")) +
  geom_point(aes(x = consumption, y = price), color = "grey50", alpha = 0.9, size = 1) + 
  geom_point(data=updated.db.equil %>% 
               filter(target.yr == 2015, scenario == "obs", crop != "Others"),
             aes(x = consumption, y = price), color = "grey50", shape = 2, alpha = 0.9, size = 1) + 
  geom_point(data=updated.db.equil %>% 
               filter(target.yr == 2000, scenario == "obs", crop == "Soybeans", 
                      reg.imp == "Asia"),
    aes(x = consumption, y = price), color = "red", alpha = 0.9, size = 2) + 
  geom_point(data=updated.db.equil %>% 
               filter(target.yr == 2015, scenario == "obs", crop == "Soybeans", 
                      reg.imp == "Asia"),
             aes(x = consumption, y = price), color = "red", shape = 2, alpha = 0.9, size = 2) +
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log') +
  labs(y ="log(price)" , x = "log(quantity)") +
  theme_bw() + theme0 + theme_leg + theme(legend.position = "none") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1))-> mkteq

png(paste0(outdir,"mkteq3.png"), width = 4000, height = 3000, res = 600)
print(mkteq)
dev.off()



ggplot(updated.db.equil %>% filter(scenario == "obs", crop != "Others")) +
  geom_point(aes(x = consumption, y = price, shape = scenario, color = reg.exp), alpha = 0.9, size = 2) + 
  facet_grid(cols = vars(reg.imp), rows = vars(crop), scales = "free") +
  #scale_x_continuous(trans = 'log') +
  #scale_y_continuous(trans = 'log') +
  theme_bw()


ggplot(updated.db.equil %>% filter(scenario == "obs", crop != "Others")) +
  geom_point(aes(x = consumption, y = price, shape = scenario, color = reg.exp), alpha = 0.6) + 
  facet_grid(cols = vars(reg.imp), rows = vars(crop), scales = "free") +
  #scale_x_continuous(trans = 'log') +
  #scale_y_continuous(trans = 'log') +
  theme_bw()



#------------------------
updated.basedata.trade.fig <-  updated.db.trade %>% 
  mutate(reg.exp = if_else(as.factor(variable) == "consume.dom", "Domestic", as.character(reg.exp)))

updated.basedata.trade.fig$crop <-  factor(updated.basedata.trade.fig$crop, levels = crop_agg)
updated.basedata.trade.fig$reg.exp <- factor(updated.basedata.trade.fig$reg.exp, levels = c("Domestic", as.character(unique(updated.db.trade$reg.exp))))

ggplot(updated.basedata.trade.fig) +
  geom_bar(aes(x = scenario, y = consumption, fill = reg.exp), position= "fill",width=0.75, stat="identity", color = "black", size = 0.3) + 
  facet_grid(cols = vars(reg.imp), rows = vars(crop), scales = "free") +
  theme_bw() -> import.share

ggplot(updated.basedata.trade.fig %>% filter(crop !="Others")) +
  geom_bar(aes(x = scenario, y = consumption, fill = crop), position= "stack",width=0.75, stat="identity", color = "black", size = 0.3) + 
  facet_grid(cols = vars(reg.imp), rows = vars(reg.exp), scales = "free") +
  theme_bw() 
#------------------------

updated.db.price %>% 
  spread(scenario, price) %>% 
  summarise(cor(est, obs))

updated.db.trade  %>% 
  spread(scenario, consumption) %>% 
  summarise(cor(est, obs))

updated.db.equil%>% gather(equil,"value", c(consumption, price)) %>% 
  spread(scenario, value) %>% 
  summarise(cor(est, obs))

updated.db.equil%>% gather(equil,"value", c(consumption, price)) %>% 
  spread(scenario, value) %>% mutate(est.c = est - ref, obs.c = obs - ref) -> diff
diff %>% 
  group_by(target.yr, equil) %>% 
  summarise(cor(est.c, obs.c)) 

diff %>% 
  group_by(equil) %>% 
  summarise(cor(est, obs)) 


ggplot(diff %>% mutate(target.yr = as.character(target.yr))) +
  geom_point(aes(x = est, y = obs, color = target.yr), alpha = 0.9)+ 
  facet_wrap(~equil, scales = "free") +
  labs(y ="Observation" , x = "Estimation") +
  #scale_x_continuous(trans = 'log') +
  #scale_y_continuous(trans = 'log') +
  theme_bw() + geom_abline(intercept = 0, slope = 1)+
  theme0 + theme_leg -> fit1

png(paste0(outdir,"fit1.png"), width = 7000, height = 3000, res = 600)
print(fit1)
dev.off()
#------------------------------------------
fontfamily = "Arial"
windowsFonts(Arial=windowsFont("TT Arial"))
theme0 <- theme(
  #panel.grid.minor = element_line(size = 0.1, linetype = 2,colour = "grey75"),panel.grid.major = element_line(size = 0.1, linetype = 2,colour = "grey75"),
  #panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
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
