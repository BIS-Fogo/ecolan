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
inpath <- paste0(getwd(), "/raw/")
outpath <- paste0(getwd(), "/procd/")

library(rgdal)
library(sp)
library(raster)

# Use this for the Cape Verdian national projection as defined by
# SR-ORG:7391 at www.spatialreference.org and EPSG:4825 at
# www.epsg.io
proj_out <- paste0("+proj=lcc +lat_1=15 +lat_2=16.66666666666667 ",
                   "+lat_0=15.83333333333333 +lon_0=-24 +x_0=161587.83 ",
                   "+y_0=128511.202 +datum=WGS84 +units=m +no_defs")


#### DO NOT CHANGE ANYTHING BELOW THIS LINE EXCEPT YOU KNOW WHAT YOU DO ########
#### Merge vegetation and animal files #########################################
veg_tec <- read.table(paste0(inpath, "plots_vegetation_2014_tec.csv"), 
                  header = FALSE, sep = ";", skip = 2, stringsAsFactors = FALSE)
veg_agr <- read.table(paste0(inpath, "plots_vegetation_2014_agr.csv"), 
                      header = FALSE, sep = ";", skip = 2, stringsAsFactors = FALSE)
veg_nat <- read.table(paste0(inpath, "plots_vegetation_2014_nat.csv"), 
                      header = FALSE, sep = ";", skip = 2, stringsAsFactors = FALSE)
anm <- read.table(paste0(inpath, "plots_animals_2014.csv"), 
                  header = TRUE, sep = ";", stringsAsFactors = FALSE)

# Set column names of vegetation data frames
colnames(veg_tec) <- sapply(veg_tec[2,], toupper)
colnames(veg_agr) <- sapply(seq(ncol(veg_agr[2,])), function(x){
  paste0(toupper(veg_agr[2,x]), "_", 
         substr(veg_agr[3,x], 1, nchar(as.character(veg_agr[3,x]))-1), "_AGR")
})
colnames(veg_nat) <- sapply(seq(ncol(veg_nat[2,])), function(x){
  paste0(toupper(veg_nat[2,x]), "_", 
         substr(veg_nat[3,x], 1, nchar(as.character(veg_nat[3,x]))-1), "_NAT")
})

# Clean vegetation datasets
## getting rid of line two
veg_tec <- veg_tec[-(1:4),!(grepl("LAT|LON|ALT", colnames(veg_tec)))]
veg_agr <- veg_agr[-(1:4),]
veg_nat <- veg_nat[-(1:4),]



##AZ: replace all numeric values in the veg_nat table with "x"
for(i in seq(2, ncol(veg_nat))){
  veg_nat[,i][(grepl("[[:digit:]]", veg_nat[,i]) & veg_nat[,i] != 0)] <- "x"
}

## merge vegetation data sets
veg <- merge(veg_tec, veg_agr, by.x = "ID", by.y = "ID__AGR")
veg <- merge(veg, veg_nat, by.x = "ID", by.y = "ID__NAT")

##AZ: replace all values "x" with 1  #works!
##idea from: http://stackoverflow.com/questions/5824173/
##replace-a-value-in-a-data-frame-based-on-a-conditional-if-statement-in-r
# veg[2:163,22:125][veg[2:163,22:125]=="x"] <- 1

#AZ: replace all values "5m" with 5
#origin of this problem ist the autofill of excel. "5m" should only be in the 
#headlines and not in the values
veg[veg == "5m"] <- 5

# write.table(veg, paste0(outpath, "plots_vegetation_2014_merged.csv"),
#             sep = ",", row.names = FALSE)

# Clean animal data set and merge with vegetation data set
anm$ID <- NULL
anm <- anm[,!(grepl("Total.*", colnames(anm)))]

veg_anm <- merge(veg, anm, by = "IDA", all.x = TRUE)

# write.table(veg_anm, paste0(outpath, "plots_veg_anm_2014.csv"), 
#             sep = ",", row.names = FALSE)

#read GPS Data from shapefile: coordinates, elevation and Plot ID
GPS <- readOGR(paste0(inpath, "Vegplotsall.shp"), layer = "Vegplotsall")
GPS <- data.frame(GPS[,c(1:2,5)])

colnames(GPS)[4] <- "Lon"
colnames(GPS)[5] <-"Lat"

veg_anm_geo <- merge(veg_anm, GPS, by.x = "ID", by.y = "name")

convert <- c("ASP", "SLP", "GLH", "BLC", "BLH", "TLC", "TLH")
for(conv in convert){
  veg_anm_geo[,grepl(conv, colnames(veg_anm_geo))] <-
    as.numeric(veg_anm_geo[,grepl(conv, colnames(veg_anm_geo))])
}

veg_anm_geo$GLC[veg_anm_geo$GLC == "<5"] <- 3
veg_anm_geo$GLC[grepl("<", veg_anm_geo$GLC)] <- 0.1
veg_anm_geo$GLC[veg_anm_geo$GLC == "0.01"] <- 0.1
veg_anm_geo$GLC <- as.numeric(veg_anm_geo$GLC)

for(col in colnames(veg_anm_geo[,grepl("5_AGR", colnames(veg_anm_geo))])){
  veg_anm_geo[,col] <- as.numeric(veg_anm_geo[,col])
}

veg_anm_geo[veg_anm_geo == "x" & !is.na(veg_anm_geo)] <- 1
for(col in colnames(veg_anm_geo[,grepl("10_AGR|_NAT", colnames(veg_anm_geo))])){
  veg_anm_geo[,col] <- as.numeric(veg_anm_geo[,col])
}

for(col in colnames(veg_anm_geo[,grepl("_AGR|_NAT|GLC|GLH|BLC|BLH|TLC|TLH", 
                                       colnames(veg_anm_geo))])){
  veg_anm_geo[,col][is.na(veg_anm_geo[,col])] <- 0
}

veg_anm_geo$IDA[veg_anm_geo$IDA == ""] <- NA

scol <- which(colnames(veg_anm_geo) == "RemarksAnimal")+1
ecol <- which(colnames(veg_anm_geo) == "ele")-1

veg_anm_geo[!is.na(veg_anm_geo$IDA),
            scol:ecol][is.na(veg_anm_geo[!is.na(veg_anm_geo$IDA),
                                         scol:ecol])] <- 0



summary(veg_anm_geo)

write.table(veg_anm_geo, paste0(outpath, "plots_veg_anm_geo_2014.csv"), 
            sep = ",", row.names = FALSE)

coordinates(veg_anm_geo) <- ~Lon + Lat
projection(veg_anm_geo) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
str(veg_anm_geo)

veg_anm_geo_lambert <- spTransform(veg_anm_geo, CRS(proj_out))
writeOGR(veg_anm_geo_lambert, paste0(outpath, "plots_veg_anm_geo_2014_7391.shp"), 
         "plots_veg_anm_geo_2014_7391", driver<-"ESRI Shapefile", 
         overwrite_layer = TRUE)

