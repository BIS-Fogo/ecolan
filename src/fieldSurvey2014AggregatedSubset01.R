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

tlpath <- "active/bis-fogo/data/field-campaign_2014/"

dsn <- switch(Sys.info()[["sysname"]], 
              "Linux" = "/media/permanent/",
              "Windows" = "D:/")
setwd(paste0(dsn, tlpath))
inpath <- paste0(getwd(), "/procd/")
outpath <- inpath

#### DO NOT CHANGE ANYTHING BELOW THIS LINE EXCEPT YOU KNOW WHAT YOU DO ########
#### Merge vegetation and animal files #########################################
df <- read.table(paste0(inpath, "plots_veg_anm_geo_2014.csv"), 
                 sep = ",", header = TRUE)
str(df)


scol <- which(colnames(df) == "Formicidae_Hymenoptera")+1
ecol <- which(colnames(df) == "ele")-1

df_out <- data.frame(ID = df$ID,
                     ELEV = df$ele,
                     COVRG = df$GLC + df$BLC + df$TLC,
                  NAT = rowSums(df[grepl("_5_NAT", colnames(df))]),
                  AGR = rowSums(ifelse(df[grepl("_5_AGR", colnames(df))] > 0, 1, 0)),
                  ANIMALS = rowSums(df[,scol:ecol]))

write.table(df_out, paste0(outpath, "fieldSurvey2014_Subset01.csv"), sep = ",", 
            row.names = FALSE)
