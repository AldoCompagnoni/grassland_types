setwd("C:/Users/ac79/MEGA/Projects/SIDE/TipiPastoraliPiemonte/gjam/")
#setwd("C:/Users/Aldo/MEGA/Projects/SIDE/TipiPastoraliPiemonte/gjam/")

install.packages("gjam") 
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

# most abundant 5 species -----------------------------------
calate_mat <- spp_calat[1:777,-c(1:2)]

# 
spp_calat$totale        <- NA 
spp_calat$totale[1:777] <- apply(calate_mat, 1, sum)

# top 5 species
spp_tot   <- select(spp_calat,totale,SPECIE)
spp_tot   <- spp_tot[order(spp_tot$totale, spp_tot$SPECIE,
                         decreasing = T),]
# the list!
spp_top_5 <- as.character(spp_tot$SPECIE[1:5])

# Raw data --------------------------------------------
# ydf: response
r     <- which(spp_calat$SPECIE %in% spp_top_5)
y_df  <- spp_calat[r,-2]
t_ydf <- as.data.frame( t(y_df[,-1]) )
t_ydf <- mutate(t_ydf, Codice = rownames(t_ydf) )
ydf   <- setNames(t_ydf, c(as.character(spp_calat$SPECIE[r]),
                           "Codice"))
ydf   <- ydf[,c(6,1:5)]

# xdf: predictors
xdf   <- station[,c("Codice","Quota","Altezza.erba",
                    "Assolazione","Pendenza..")] #"Esposizione..N"
xdf   <- setNames(xdf, c("Codice","qout","erba","sole","pend") )

# all data
alld    <- merge(ydf, xdf)

# final list
fo           <- list()
fo$y         <- alld[,2:6]
fo$x         <- select(alld,qout,erba,sole,pend)
fo$x         <- mutate(fo$x, intercept = 1)
fo$x         <- select(fo$x,intercept,qout,erba,sole,pend)
fo$formula   <- as.formula(~qout+ erba + sole + pend) 
fo$typeNames <- rep("CA",ncol(fo$y) )
                  

# gjams data ----------------------------------------------

ml  <- list(ng = 40000, burnin = 4000, typeNames = fo$typeNames)
out <- gjamGibbs(fo$formula, fo$x, fo$y, modelList = ml)
summary(out)

# plot
pl  <- list(GRIDPLOTS = T, SMALLPLOTS = F)
gjamPlot(output = out, plotPars = pl)





# ???
par(bty = 'n', mfrow = c(1,3), family='')
plot(f$trueValues$beta, out$parameterTables$betaMu, cex = .2)
plot(f$trueValues$corSpec, out$parameterTables$corMu, cex = .2)
plot(f$y,out$modelSummary$yMu, cex = .2)






#Dactylis onluy
dact_ab=subset(spp_calat, SPECIE == "Dactylis glomerata")
dact_pa=subset(spp_vasta, SPECIE == "Dactylis glomerata")

dact_ab=subset(spp_calat, SPECIE == "Festuca gr. rubra")
dact_pa=subset(spp_vasta, SPECIE == "Festuca gr. rubra")




sumz = rbind(dact_pa,dact_ab)[,-c(1:2)]
for(i in 1:ncol(sumz)) { sumz[,i] = as.numeric(sumz[,i])}
sumz = as.matrix(sumz)

dact   <- data.frame(Codice = colnames(sumz),
                     abund = colSums(sumz))

#Antennaria only
unique(spp_calat$SPECIE)


#Merge
prova= merge(dact,station[,c("Codice","Quota","Altezza.erba")])

#
plot(prova$Altezza.erba,prova$abund,pch=16)
plot(prova$Quota,prova$abund,pch=16)

summary(lm(prova$abund ~ prova$Altezza.erba * prova$Quota))
