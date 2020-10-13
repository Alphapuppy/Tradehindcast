#In thir script, we will explore:

#1.1. Weibull distributions for preference calibration
#1.2 Armington responses: price ratio vs. imported consumption share

#2.1 Armington preference distribution (t or theta) shocks: price ratio vs. t ratio
#2.2 Pass-through elasticity
 
#-----------------
library(dplyr)
library(tidyverse)
library(nleqslv)
library(tidyr)
library("ggpubr")

datadir <- ("./data/")
outdir <- ("./output/")
source("R/Model.func.R")      #basic funcs
source("R/Model.basedata.R")  #read processed base data

study.year <- c(1995, 2000, 2005, 2010, 2015)
reg_agg <- c("Africa", "Asia", "Europe", "N. America", "S. America", "Oceania")  
crop_agg <- c("Corn", "Wheat", "Rice", "Soybeans", "Rapeseed", "Others")

unique(basedata.trade.allyear$reg.imp)
#-----------------
#domestic consumption P Q
basedata.trade.allyear %>% 
  filter(variable == "consume.dom")  %>% 
  rename(imp.Q = value) %>% left_join(
      basedata.regmkt.allyear %>% filter(variable == "pp") %>%
        rename(reg.imp = reg) %>% spread(variable, value),
      by = c("reg.imp", "crop", "year")
    ) %>% 
  rename(imp.P = pp) -> DB.consume.dom
#imported consumption P Q  
basedata.trade.allyear %>% 
  filter(variable == "export")  %>% 
  mutate(variable = "trade") %>% 
  rename(imp.Q = value) %>% 
  left_join(
    basedata.pricelink.allyear %>% 
      filter(variable %in% c("margin.reg.pim_pexp.mtax.shock",  "margin.mtax", "pp")) %>% 
      spread(variable, value) %>% 
      mutate(pim.reg = margin.reg.pim_pexp.mtax.shock * pp),
    by = c("reg.imp", "reg.exp", "crop", "year")
  ) -> DB.consume.trade 

DB.consume.dom %>% within(rm(reg.exp)) %>% bind_rows(
  DB.consume.trade %>% 
    group_by(reg.imp, crop, year, variable) %>% 
    summarise(imp.P = weighted.mean(pim.reg, imp.Q), 
              imp.Q = sum(imp.Q), .groups = "drop")
  ) -> DB.consume  

  
DB.consume %>% filter(reg.imp == reg_agg[2],
               crop == crop_agg[4], 
               year == study.year[1]) -> DB2 

DB.consume %>% filter(reg.imp == "Africa",
               crop == "Wheat", 
               year == 1995) -> DB2 


data.q <- DB2 %>% pull(imp.Q)       #Initial prices of domestic and imported
data.p <- DB2 %>% pull(imp.P)       #Initial prices of domestic and imported

#data.q <- c(16.921,	10.000)         #Initial prices of domestic and imported
#data.p <- c(93.137,	59.100)         #Initial consumption of domestic and imported

#Single region/nesting logit-based Armington testing
#initial data for calibration for domestic and imported

elas.demand <- -0.5                 #Aggregated demand elasticity 
elas.supply= c(0.5, 0.5)            #Supply elasticity by source

#parameter calibrations
#logit.sw.cali is the defined calibration function
para.demand <- sum(data.q)/weighted.mean(data.p,data.q)^elas.demand
para.supply <- data.q / data.p^elas.supply


theta <- 3                          #Armington parameter in logit
para.share.weight <- logit.sw.cali(data.q, data.p, theta)

#Verify initial calibration
#logit is the defined logit sharing function
logit.share(data.p, theta, para.share.weight) == data.q / sum(data.q)

#Weibull dist. calibration 
#----------------------------
#para.share.weight moved out
Weibull.cdf <-  function(z){(1- exp(-(gamma(1+1/theta)^(-1) )^(-theta) * (para.share.weight * z) ^theta )) } 
Weibull.cdf(c(.5,.5))
Weibull.cdf.inv <- function(F, theta0 = theta, para.share.weight0 = para.share.weight){
  (log(1 - F) * ( -(gamma(1+1/theta0)^(-1) )^(-theta) ) ^(-1) ) ^ (1/theta)* para.share.weight0^(-1)}  
data.frame(Weibull.cdf.inv(c(0.5,0.5)))

niter = 1000
lapply(seq(niter), function(x){
  data.frame(ID =c(x, x) , 
             source = c("domestic", "imported"), 
             value = Weibull.cdf.inv(c(runif(1),runif(1)), theta0 = theta, para.share.weight0 = (para.share.weight) ) * data.p )} ) %>%  # * data.p
  bind_rows() -> Weibull.min

Weibull.min %>% group_by(source) %>% summarise(value = mean(value))

Weibull.min %>% spread(source, value) %>% filter(domestic - imported <= 0) %>% arrange(domestic) %>% 
  bind_rows(Weibull.min %>% spread(source, value) %>% filter(domestic - imported > 0) %>% arrange(imported)) %>% 
  mutate(ID = row_number()) %>% gather(source, value, -ID) -> Weibull.min1

Weibull.min1$source <-  factor(Weibull.min1$source, levels = c("domestic", "imported"), labels = c("Domestic", "Imported"))  

ggplot(Weibull.min1) + 
  geom_point(aes(x = ID, y = value, color = source, shape = source), size = 1.1) + 
  geom_vline(xintercept = data.q[1]/sum(data.q) * niter, linetype = 5, size = 1) +
  labs(x = "Consumption shares by source (1000 trails)",  #in consumption 
       y = "Preference-adjusted price") +  #between imported and domestic products
  scale_x_continuous(expand = c(0, 0), limits = c(0, niter)) +
  scale_y_continuous(expand = c(0, 0)) +
  ggsci::scale_color_npg(name = "Source" ) +
  scale_shape_manual(name = "Source", values = c(1, 2)) +
  theme_bw() + theme0 + theme_leg +
  theme(axis.text.x = element_blank()) +
  theme(legend.position = c(0.62, 0.13))  -> Weibull.dist

Write_png(Weibull.dist, "Weibull.dist_Asia_soy", h = 3000, w = 4500)
#----------------------------
#trade responses
theta0 <- c(0.5, 3, 30)
x = seq(0.001, 2, 0.01)

lapply(theta0, function(theta){
  data.frame(
    theta = as.character(theta),
    p.ratio = x,
    q.share = sapply(x, function(x){logit.share(c(1, x), theta, logit.sw.cali(data.q, data.p, theta))[2] *100} )
  )  }) %>% bind_rows() -> df0

df0$theta <-  factor(df0$theta, levels = theta0, labels = paste0(expression(theta), " = ", theta0))    

ggplot() +
  geom_hline(yintercept = data.p[2]/ data.p[1], linetype = 2, size = 1) +
  geom_line(data = df0, aes(x = q.share, y = p.ratio, group = theta, color = theta), size = 1.5) +
  geom_point(data = data.frame(
    p.ratio = data.p[2]/ data.p[1],
    q.share = logit.share(c(1, data.p[2]/ data.p[1]), theta, logit.sw.cali(data.q, data.p, theta))[2] *100 ),
    aes(x = q.share, y = p.ratio, fill = "Calibration point"), color = "black", size = 2.5, shape = 21, stroke = 1.5) +
  labs(x = "Share of imported consumption (%)",  #in consumption 
       y = "Price ratio (imported / domestic)") +  #between imported and domestic products
  scale_x_continuous(expand = c(0, 0), limits = c(0, 100)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2)) +
  ggsci::scale_color_npg(name = "Armington parameter" ) +
  ggsci::scale_fill_npg(name = NULL) +
  theme_bw() + theme0 + theme_leg +
  #guides(fill = guide_legend(order = 1),col = guide_legend(order = 2)) +
  theme(legend.position = c(0.7, 0.72)) -> Armington.response

Write_png(Armington.response, "Armington.response_Asia_soy", h = 3000, w = 4500)
#----------------------------

Write_png(ggarrange(Weibull.dist + labs(title = "a. Preference calibration (theta = 3)"), 
                    Armington.response + labs(title = "b. Trade responses"),
                    nrow = 1, align = "h"), 
          "Fig1", h = 3000, w = 8000)
#----------------------------

theta0 <- c(1, 3, 30)
sigma0 <- seq(1, 10, .5)

lapply(theta0, function(theta){
  para.share.weight <- logit.sw.cali(data.q, data.p, theta)
  lapply(sigma0, function(sigma){
    
    dslnex <- function(x, theta1 = theta, para.share.weight1 = para.share.weight ^ (1/sigma)) {
      y <- numeric(5)
      p <- x[1:2]
      q <- x[3:4]
      demand.regional <- x[5]
      y[1:2] <- logit.share(p, theta1, para.share.weight1) * demand.regional - q
      y[3:4] <- para.supply*p^(elas.supply) - q
      #market clearing
      y[5]   <- para.demand*sum(logit.share(p, theta1, para.share.weight1)*p)^elas.demand - demand.regional
      y
    }
    xstart <- c(1,1, 2, 2, 26)
    nleqslv(xstart, dslnex, control=list(btol=.001)) -> sol
    data.frame(
      scale = "fixed",
      theta = theta,
      sigma = sigma,
      p.ratio = sol$x[2] / sol$x[1],
      t.ratio = (para.share.weight ^ (1/sigma))[2] / (para.share.weight ^ (1/sigma))[1]  )
  }) %>% bind_rows() %>% 
    bind_rows(
      lapply(seq(theta, 100, .5), function(sigma){
        
        dslnex <- function(x, theta1 = sigma, para.share.weight1 = para.share.weight ^ (theta/sigma)) {
          y <- numeric(5)
          p <- x[1:2]
          q <- x[3:4]
          demand.regional <- x[5]
          y[1:2] <- logit.share(p, theta1, para.share.weight1) * demand.regional - q
          y[3:4] <- para.supply*p^(elas.supply) - q
          #market clearing
          y[5]   <- para.demand*sum(logit.share(p, theta1, para.share.weight1)*p)^elas.demand - demand.regional
          y
        }
        xstart <- c(100,100, 16, 20, 26)
        nleqslv(xstart, dslnex, control=list(btol=.001)) -> sol
        data.frame(
          scale = "sigma",
          theta = theta,
          sigma = sigma,
          p.ratio = sol$x[2] / sol$x[1],
          t.ratio = (para.share.weight ^ (theta/sigma))[2] / (para.share.weight ^ (theta/sigma))[1]  )
      }) %>% bind_rows()
    )
}) %>% bind_rows() -> df1

df1$theta <-  factor(df1$theta, levels = theta0, labels = paste0(expression(theta), " = ", theta0))  
df1$scale <-  factor(df1$scale, levels = c("fixed", "sigma"), labels = c("shift", "shift & scale"))  

ggplot(df1) +
  geom_abline(intercept = 0, slope = 1, linetype = 1, size = 1.05) +
  geom_vline(xintercept = 1, linetype = 1, size = 1.05) +
  geom_hline(yintercept = 1, linetype = 1, size = 1.05) +
  geom_hline(yintercept = data.p[2]/ data.p[1], linetype = 5, size = 1) +
  geom_line(aes(x = t.ratio, y = p.ratio, color = as.character(theta), linetype = scale ), size = 1.2) +
  geom_point(data = df1 %>% filter(sigma == 1, scale == "shift"),
    aes(x = t.ratio, y = p.ratio,  fill = as.character(theta)), color = "black", size = 2.5, shape = 21, stroke = 1.5) +
  labs(x = "Preference parameter ratio (imported / domestic)",  #in consumption 
       y = "Price ratio (imported / domestic)") +  #between imported and domestic products
  scale_x_continuous(expand = c(0, 0), limits = c(0, 2)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2)) +
  ggsci::scale_color_npg(name = "Armington parameter" ) +
  ggsci::scale_fill_npg(name = "Calibration points") +
  scale_linetype_manual(name = "Integration path", values = c(5, 1)) + 
  theme_bw() + theme0 + theme_leg +
  guides(fill = guide_legend(order = 1), col = guide_legend(order = 2)) +
  theme(legend.position = "right") -> Armington.t.shock ;Armington.t.shock

Write_png(Armington.t.shock, "Armington.t.shock", h = 4000, w = 4500)



theta = 5
sigma = 10
dslnex <- function(x, theta1 = theta, para.share.weight1 = para.share.weight ^ (1/sigma)) {
  y <- numeric(5)
  p <- x[1:2]
  q <- x[3:4]
  demand.regional <- x[5]
  
  y[1:2] <- logit.share(p, theta1, para.share.weight1) * demand.regional - q
  y[3:4] <- para.supply*p^(elas.supply) - q
  #market clearing
  y[5]   <- para.demand*sum(logit.share(p, theta1, para.share.weight1)*p)^elas.demand - demand.regional
  y
}

xstart <- c(1, 1, 2, 2, 26)
nleqslv(xstart, dslnex, control=list(btol=.001)) -> sol

Hm <- function(theta, data.p, data.q){
  (elas.demand * elas.supply[2] * (((data.p * data.q)/sum(data.p * data.q)) - (data.q / sum(data.q)))[2] + 
     elas.demand * ((data.p * data.q)/sum(data.p * data.q))[2] +
     theta * (data.q / sum(data.q))[2]) *
    (elas.supply[1] - elas.demand + 
       elas.demand * elas.supply[1] * (((data.p * data.q)/sum(data.p * data.q)) - (data.q / sum(data.q)))[2] +
       elas.demand * ((data.p * data.q)/sum(data.p * data.q))[2] +
       theta * (data.q / sum(data.q))[2])^(-1)
}

#Hm(theta, data.p, data.q)

data.frame(
  scale = "fixed",
  theta = theta,
  sigma = sigma,
  p.ratio = sol$x[2] / sol$x[1],
  t.ratio = (para.share.weight ^ (1/sigma))[2] / (para.share.weight ^ (1/sigma))[1],
  Hm = Hm(theta, data.p = c(sol$x[1], sol$x[2]), data.q = c(sol$x[3], sol$x[4])))



#------------------------------------------
fontfamily = "Arial"
windowsFonts(Arial=windowsFont("TT Arial"))
theme0 <- theme(
  #panel.grid.minor = element_line(size = 0.1, linetype = 2,colour = "grey30"),
  panel.grid.major = element_line(size = 0.1, linetype = 2,colour = "grey30"),
  #panel.grid.major = element_blank(), 
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
