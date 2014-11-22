#  Pre-process 2014 field campaign data sets
#
#  Copyright (C) 2014 Vanessa Wilzek, Alice Ziegler, Thomas Nauss
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#  Please send any comments, suggestions, criticism, or (for our sake) bug
#  reports to admin@environmentalinformatics-marburg.de
#
#  Details
#  The script merges several csv files and adds geo-location information.
#  To run the script, adjust the following variables (if no one else did it
#  for you):
#  inpath - full path to the top level input folder
#  top_level_outpath - full path to the output folder
#  proj_out - proj4 string of the target projection (see web pages above)
#  
#  NOTE: DO NOT USE A TOP LEVEL OUTPUT PATH WHICH IS INSIDE THE TOP LEVEL INPUT
#        PATH
#### User setttings ############################################################
rm(list = ls(all = T))

tlpath <- 
  "/home/alice/Desktop/kap_verde_exkursion/AGNauss_182/field_campaign_2014/"
tlpath <- "active/bis-fogo/data/field-campaign_2014/"

dsn <- switch(Sys.info()[["sysname"]], 
              "Linux" = "/media/permanent/",
              "Windows" = "D:/")
setwd(paste0(dsn, tlpath))
inpath <- paste0(getwd(), "/procd/")
outpath <- paste0(getwd(), "/procd/")

library(rgdal)
library(sp)
library(raster)
library(lattice)
library(corrplot)

# Use this for the Cape Verdian national projection as defined by
# SR-ORG:7391 at www.spatialreference.org and EPSG:4825 at
# www.epsg.io
proj_out <- paste0("+proj=lcc +lat_1=15 +lat_2=16.66666666666667 ",
                   "+lat_0=15.83333333333333 +lon_0=-24 +x_0=161587.83 ",
                   "+y_0=128511.202 +datum=WGS84 +units=m +no_defs")


#### DO NOT CHANGE ANYTHING BELOW THIS LINE EXCEPT YOU KNOW WHAT YOU DO ########
#### Merge vegetation and animal files #########################################
veg <- read.table(paste0(outpath, "plots_veg_anm_geo_2014.csv"), 
                  sep = ",", header = TRUE)
str(veg)
hist(veg$Formicidae_Hymenoptera)
hist(sqrt(sqrt(veg$Formicidae_Hymenoptera)))


agr_temp <- ifelse(veg[grepl("_5_AGR", colnames(veg))] > 0, 1, 0)


scol <- which(colnames(veg) == "Formicidae_Hymenoptera")+1
ecol <- which(colnames(veg) == "ele")-1

nat <- data.frame(ID = veg$ID,
                  ELEV = veg$ele,
                  GLC = veg$GLC,
                  BLC = veg$BLC,
                  COVRG = veg$GLC + veg$BLC + veg$TLC,
                  Formicidae_Hymenoptera = veg$Formicidae_Hymenoptera,
                  Tenebrionidae_Coleoptera = veg$Tenebrionidae_Coleoptera,
                  Coleoptera = rowSums(veg[grepl("Coleoptera", colnames(veg))]),
                  Saltatoria = rowSums(veg[grepl("Saltatoria", colnames(veg))]),
                  NAT = rowSums(veg[grepl("_5_NAT", colnames(veg))]),
                  AGR = rowSums(ifelse(veg[grepl("_5_AGR", colnames(veg))] > 0, 1, 0)),
                  TOT = rowSums(veg[grepl("_5_NAT", colnames(veg))]) + 
                    rowSums(ifelse(veg[grepl("_5_AGR", colnames(veg))] > 0, 1, 0)),
                  AGR_5_SN = rowSums(veg[grepl("_5_AGR", colnames(veg))]),
                  ANIMALS = rowSums(veg[,scol:ecol]),
                  ANI = rowSums(ifelse(veg[,scol:ecol] > 0, 1, 0)))


corrplot(cor(nat[,3:ncol(nat)], use = "complete.obs"), type = "lower")





# ANIMALS vs. COVERAGE
# ANIMALS vs. RICHNESS NAT -> negative
# ANUMALS vs. RICHNESS AGR -> positive
# summary(lm(anim~nat$COVRG+nat$AGR+nat$NAT, na.action = "na.omit"))


anim <- sqrt(nat$ANIMALS)
anim <- nat$ANIMALS
anim <- nat$ANI
hist(anim)
hist(sqrt(anim))
# plants <- asin(sqrt(nat$COVRG/100))
plants <- nat$BLC
plants <- nat$GLC
plants <- asin(sqrt(nat$COVRG/max(nat$COVRG)))
plants <- nat$COVRG
plants <- nat$AGR
plants <- nat$NAT
plants <- nat$TOT


anim <- sqrt(nat$ANIMALS)
anim <- sqrt(nat$ANIMALS)
plants <- nat$COVRG
plot(plants, anim)
l <- loess(anim ~ plants, na.action = "na.omit")
summary(l)
n <- seq(min(plants),max(plants),0.01)
p <- predict(l, n)
plot(plants, anim)
lines(n, p, col = "red", lwd = 2)
summary(lm(anim ~ plants, na.action = "na.omit"))



anim <- nat$ANIMALS
anim <- sqrt(nat$ANIMALS)
plants <- nat$AGR
plot(plants, anim)
l <- loess(anim ~ plants, na.action = "na.omit")
summary(l)
n <- seq(min(plants),max(plants),0.01)
p <- predict(l, n)
plot(plants, anim)
lines(n, p, col = "red", lwd = 2)
summary(lm(anim ~ plants, na.action = "na.omit"))


anim <- nat$ANIMALS
anim <- sqrt(nat$ANIMALS)
plants <- nat$NAT
plot(plants, anim)
l <- loess(anim ~ plants, na.action = "na.omit")
summary(l)
n <- seq(min(plants),max(plants),0.01)
p <- predict(l, n)
plot(plants, anim)
lines(n, p, col = "red", lwd = 2)
summary(lm(anim ~ plants, na.action = "na.omit"))
