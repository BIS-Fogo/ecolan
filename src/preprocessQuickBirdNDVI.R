#  Pre-process NDVI data from Quick Bird
#
#  Copyright (C) 2014 Hanna Meyer, Thomas Nauss
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
#
rm(list = ls(all = T))

#### Define Working directory ##################################################
working_directory <- "D:/active/bis-fogo/"
in_path <- paste0(working_directory,"data/remote-sensing/quickbird/")
out_path <- paste0(working_directory,"data/procd")
setwd(working_directory)


#### Load required packages ####################################################
library(rgdal)
library(raster)

# Use this for the Cape Verdian national projection as defined by
# SR-ORG:7391 at www.spatialreference.org and EPSG:4825 at
# www.epsg.io
proj_out <- paste0("+proj=lcc +lat_1=15 +lat_2=16.66666666666667 ",
                   "+lat_0=15.83333333333333 +lon_0=-24 +x_0=161587.83 ",
                   "+y_0=128511.202 +datum=WGS84 +units=m +no_defs")


#### Read Quickbird data #######################################################
channels <- list.files(in_path, glob2rx("*.tif"), full.names = TRUE)
qb <- lapply(channels, function(x){
  stack(x)[[2]]
})

qb <- mosaic(qb[[1]], qb[[2]], qb[[3]], qb[[4]], qb[[5]], qb[[6]], fun = "max")


