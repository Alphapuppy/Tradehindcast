Write_png <- function(.plot, name, w = 9000, h = 4500, r = 600){
  png(paste0(outdir,name,".png"), width = w, height = h, res = r)
  print(.plot)
  dev.off()
}

MA.5 <- function(x){
  (x+lag(x,n = 1)+ lag(x,n = 2)+ lead(x,n = 1)+lead(x,n = 2))/5
}



logit.sw.cali <- function(q, p, logit.exponent, sw_relative = "sw_share"){
  if( length(q[!is.na(q)]) < 2 ) {
    stop( "argument 'q' must include at least 2 non NA rows" )
  }
  sw.cali <- tibble(q, p) %>% 
    mutate(ID = row_number()) %>% 
    arrange(desc(q)) %>% 
    mutate(sw = ifelse(is.finite(p * q^(1/logit.exponent)), p * q^(1/logit.exponent), 0),
           sw_default = sw / first(sw),
           sw_share = sw / sum(sw)) %>% 
    arrange(ID)
  return(sw.cali %>% pull(sw_relative))
  #return(sw.cali)
}

#volume share from logit
logit.share <- function(p, logit.exponent, share.weight){
  y <- numeric(length(p))
  y <- p^(-logit.exponent)*share.weight^(logit.exponent)/sum(p^(-logit.exponent)*share.weight^(logit.exponent))
  return(y)
}


#value share from CES
ces.share <- function(p, ces.exponent, share.weight){
  y <- p^(1-ces.exponent)*share.weight^(ces.exponent)/sum(p^(1-ces.exponent)*share.weight^(ces.exponent))
  return(y)
}


Agg <- function(.data, ...){
  .data %>%
    group_by(...) %>%
    summarise(value = sum(value), .groups = 'drop') %>%
    ungroup() %>%
    return()
}

#Convert 
pull.list<- function(.df, group = "reg", val.rm = c("variable", "crop")){
  .df %>% group_by_at(vars(one_of(c(group, val.rm)))) %>% 
    summarize(value = sum(value), .groups = 'drop') %>%  #, .groups = 'drop'
    ungroup() %>% 
    spread(group, value) %>% 
    select_at(vars(-one_of(val.rm))) %>% 
    as.list() }


