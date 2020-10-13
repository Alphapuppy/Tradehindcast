library(dplyr)
library(tidyverse)
library(nleqslv)
library(tidyr)

datadir <- ("./data/")
outdir <- ("./output/")
source("R/Model.func.R")      #basic funcs
source("R/Model.basedata.R")  #read processed base data

study.year <- c(1995, 2000, 2005, 2010, 2015)
reg_agg <- c("Africa", "Asia", "Europe", "N. America", "S. America", "Oceania")  
crop_agg <- c("Corn", "Wheat", "Rice", "Soybeans", "Rapeseed", "Others")

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

for (r in c(1:6)) {
  for (c in c(1:6)) {
    
DB.consume %>% filter(reg.imp == reg_agg[r],
                      crop == crop_agg[c], 
                      year == study.year[1]) -> DB2 

data.q <- DB2 %>% pull(imp.Q)
data.p <- DB2 %>% pull(imp.P)

#Single region/nesting logit-based Armington testing
#initial data for calibration for domestic and imported

theta <- 3                          #Armington parameter in logit
elas.demand <- -0.5                 #Aggregated demand elasticity 
elas.supply= c(0.5, 0.5)            #Supply elasticity by source

#parameter calibrations
#logit.sw.cali is the defined calibration function
para.demand <- sum(data.q)/weighted.mean(data.p,data.q)^elas.demand
para.supply <- data.q / data.p^elas.supply
para.share.weight <- logit.sw.cali(data.q, data.p, theta)

#Verify initial calibration
#logit is the defined logit sharing function
logit.share(data.p, theta, para.share.weight) == data.q / sum(data.q)


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
  theme(legend.position = "right") -> Armington.t.shock

Write_png(Armington.t.shock, paste0("theory/Armington.shock/",reg_agg[r],crop_agg[c]), h = 4000, w = 6000)


  }
  
}




for (r in c(1:6)) {
  for (c in c(1:6)) {
    
    DB.consume %>% filter(reg.imp == reg_agg[r],
                          crop == crop_agg[c], 
                          year == study.year[1]) -> DB2 
    
    data.q <- DB2 %>% pull(imp.Q)
    data.p <- DB2 %>% pull(imp.P)
    
    theta0 <- c(0.5, 3, 30)
    x = seq(0.01, 2, 0.01)
    
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
      theme(legend.position = "right") -> Armington.response
    
    Write_png(Armington.response, paste0("theory/Armington.response/",reg_agg[r],crop_agg[c]), h = 4000, w = 6000)

  }
  
}


