setwd("C:/Users/Aldo/MEGA/Projects/SIDE/TipiPastoraliPiemonte/gjam/")
library(dplyr)

# read data
ipla <- read.csv("data/Maira_IPLA.csv")

# Formatting --------------------------------------------------------

# remove TOTPROF (useless as far as I am concerned)
ipla <- subset(ipla, X.3 != "TOTPROF")

# cumulated depths
profili   <- unique(ipla$PROFILO)
ipla_list <- list() 
for(i in 1:length(profili)){
  
  ipla_list[[i]] <- subset(ipla,PROFILO == profili[i])
  spess_vec      <- cumsum(ipla_list[[i]]$SPESS)
  ipla_list[[i]] <- mutate(ipla_list[[i]], 
                           SPESS_CUM = spess_vec)
  
}
# stack it all in one file
ipla_cum <- Reduce(function(...) rbind(...), ipla_list)

# N with depth
plot(ipla_cum$SPESS_CUM, ipla_cum$N_TOT, pch = 16)


# Calculate weighted average with depth -------------------------------

# function to comute soil averages
avg_soil <- function(x,depth,vars){
  
  if(x$SPESS[1] < depth){
    
    r               <- min( which(x$SPESS_CUM >= depth) )
    
    if(r > 1){
      
      subtract_spess  <- depth - x$SPESS_CUM[r-1]
      weight_vec      <- x$SPESS[1:r]
      if(subtract_spess > 0) weight_vec[r] <- subtract_spess            
      avg             <- weighted.mean(x[1:r, vars], weight_vec)
      
    } else {
      
      avg <- x[1, vars] 
      
    }
    
  } else {
    
    avg <- x[1, vars]
    
  }
  
  return(avg)
  
}


# calculate average values
avg_n = list()
for(i in 1:length(profili)){
  
  x           <- subset(ipla_cum,PROFILO == profili[i])
  avg_n[[i]]  <- data.frame(PROFILO         = profili[i],
                            avg_n_top20cm   = avg_soil(x,20,"N_TOT"),
                            max_depth       = max(x$SPESS_CUM) )
  
}
# stack it all in one file
avg_n_top20cm = Reduce(function(...) rbind(...),avg_n)

# write it out
write.csv(avg_n_top20cm,"data/average_N_top20cm.csv",row.names=F)
