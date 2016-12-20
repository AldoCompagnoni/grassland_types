#setwd("C:/Users/ac79/MEGA/Projects/SIDE/TipiPastoraliPiemonte/gjam/")
setwd("C:/Users/Aldo/MEGA/Projects/SIDE/TipiPastoraliPiemonte/gjam/")
#install.packages("gjam") 
library(gjam)
library(dplyr)

# Read data 
spp_calat=read.csv("data/spp_calate.csv")
spp_vasta=read.csv("data/spp_area_vasta.csv")
station=read.csv("data/Dati_stazioni.csv")

# Format
spp_calat[is.na(spp_calat)]=0 # calate
spp_vasta[is.na(spp_vasta)]=0 # spp area vasta

# remove non-numeric data point, and convert column to numeric
spp_calat       <- spp_calat[-789,]
spp_calat[,47]  <- as.numeric( as.character(spp_calat[,47]) )
spp_calat[,47][is.na(spp_calat[,47])] <- 0

# most abundant species -----------------------------------
spp_n       <- 20
calate_mat  <- spp_calat[1:777,-c(1:2)]

# calculate totals 
spp_calat$totale        <- NA 
spp_calat$totale[1:777] <- apply(calate_mat, 1, sum)

# the top n. species
spp_tot     <- select(spp_calat,totale,SPECIE)
spp_tot     <- spp_tot[order(spp_tot$totale, spp_tot$SPECIE,
                             decreasing = T),]
spp_top     <- as.character(spp_tot$SPECIE[1:spp_n])

# Raw data --------------------------------------------
# ydf: response
r     <- which(spp_calat$SPECIE %in% spp_top)
y_df  <- spp_calat[r,-2]
t_ydf <- as.data.frame( t(y_df[,-1]) )
t_ydf <- mutate(t_ydf, Codice = rownames(t_ydf) )
ydf   <- setNames(t_ydf, c(as.character(spp_calat$SPECIE[r]),
                           "Codice"))
ydf   <- ydf[,c((spp_n+1),1:spp_n)]

# xdf: predictors
xdf   <- station[,c("Codice","Quota","Altezza.erba",
                    "Assolazione","Pendenza..")] #"Esposizione..N"
xdf   <- setNames(xdf, c("Codice","quota","erba","sole","pend") )

# all data
alld    <- merge(ydf, xdf)

# final list
fo           <- list()
fo$y         <- alld[,2:(spp_n+1)]
fo$x         <- select(alld,quota,erba,sole,pend)
fo$x         <- mutate(fo$x, intercept = 1)
fo$x         <- select(fo$x,intercept,quota,erba,sole,pend)
fo$formula   <- as.formula(~quota+ erba + sole + pend) 
fo$typeNames <- rep("CC",ncol(fo$y) )
fo$ef        <- list(columns = c(1:spp_n),
                     values  = rep(50 * spp_n,nrow(fo$y)))

# gjams fit and results ----------------------------------------------
ml  <- list(ng = 10000, burnin = 1000, typeNames = fo$typeNames, effort = fo$ef)
out <- gjamGibbs(fo$formula, fo$x, fo$y, modelList = ml)
summary(out)

# plot
#SAVEPLOTS = T, 
pl  <- list(GRIDPLOTS = T, SMALLPLOTS = F, outfolder = 'plots')
gjamPlot(output = out, plotPars = pl)




# Example of simulated effort
S   <- 5                             
n   <- 50
ef  <- list( columns = 1:S, values = round(runif(n,.5,5),1) )
f   <- gjamSimData(n, S, typeNames = 'DA', effort = ef)
