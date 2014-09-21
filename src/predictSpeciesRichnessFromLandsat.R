#  Quick and dirty prediction of plant species richness using Landsat 8
#
#  Copyright (C) 2014 Hanna Meyer, Thomas Nauss, Roland Brandl
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
#  The script reads Landsat 8 and species richness data and applies machine
#  learning models to predict the species richness based on the Landsat 
#  spectral information.

#### User setttings ############################################################
tlpath <- "/home/hanna/Documents/Projects/KapVerde/"
datapath <- paste0(tlpath, "data/")
figurepath <- paste0(tlpath, "figures/")
useAllPredictors=TRUE #use all predictors or only those with highest importance?
method <- "rf"

#### General setttings #########################################################
setwd(tlpath)
packages <- c("raster", "rgdal", "caret", "corrplot", "latticeExtra","hydroGOF")
lapply(packages, library, character.only = TRUE)

#### Load predictors: Landsat data ############# ###############################
landsat <- stack(paste0(datapath, "landsat_fogo.tif"))
ndvi=raster(paste0(datapath,"NDVI.tif"))
ndvi5x5=raster(paste0(datapath,"fogoSd5x5.tif"))
ndvi3x3=stack(paste0(datapath,"fogoSd3x3.tif"))
landsat=stack(landsat,ndvi,ndvi5x5,ndvi3x3)

#### Load response: species richness ###########################################
load(paste0(datapath, "richness_2007.sav"))
proj <- paste0("+proj=utm +zone=26 +datum=WGS84 +units=m +no_defs ",
               "+ellps=WGS84 +towgs84=0,0,0")
coordinates(data.richness) <- ~E + N
projection(data.richness) <- proj

#### Compile Train data set ####################################################
dlr <- data.frame(extract(landsat, data.richness), 
                  data.richness)
comit <- which(colnames(dlr)==c("N")|colnames(dlr)=="E")
dlr <- dlr[,-comit]
romit <- which(dlr$landsat_fogo.10 < 20000) #remove outliers
dlr <- dlr[-romit,]

#### Predict species richness from Landsat data set ############################
# Reduce bands based on high auto-correlation 
if (!useAllPredictors){
  dlr.cor <- as.matrix(cor(dlr[,-which( colnames(dlr)==c("rich"))]))
  corrplot(dlr.cor)
  dlr.cor.rm <- findCorrelation(dlr.cor, verbose = TRUE)
  dlr.clean <- dlr[,-(dlr.cor.rm+1)]
  # dlr.clean$Rich <- as.factor(dlr.clean$Rich)
  # dlr.clean <- rbind(dlr.clean, dlr.clean)
}

predPerformance <- data.frame(PRED = factor(), VALD = factor())
importance <- data.frame(band = factor(), importance = numeric())
if (useAllPredictors){
  dlr.clean=dlr 
}
validation=data.frame(matrix(ncol=4,nrow=3))
colnames(validation)=c("rsquared","rmse","me","mae")

model.train=list()
for(x in 1:10){
  trainIndex <- createDataPartition(dlr.clean[,ncol(dlr.clean)], 
                                    p = 0.7, list = FALSE)
  df.train <- dlr.clean[trainIndex,]
  df.test <- dlr.clean[-trainIndex,]
  
  model.train[[x]] <- train(df.train[,-ncol(df.train)], df.train[,ncol(df.train)], 
                       method = method, tuneLength = 5)
  plot(model.train[[x]])
  #### Predict on test samples #################################################
  model.test <- predict(model.train[[x]], df.test[,-ncol(df.train)])
  tmp <- data.frame(PRED = model.test,
                    VALD = df.test[,ncol(df.train)])
  predPerformance <- data.frame(rbind(predPerformance, tmp))
 
  rsquared=summary(lm(predPerformance$VALD ~ predPerformance$PRED))$r.squared
  rmse=rmse(predPerformance$PRED,predPerformance$VALD)
  me=rmse(predPerformance$PRED,predPerformance$VALD)
  mae=rmse(predPerformance$PRED,predPerformance$VALD)
  validation[x,]=data.frame(rsquared,rmse,me,mae)
  #### Determine and Plot Varibale Importance ################################## 
  if(method == "rf"){
    pdf("figures/predict_SR_Landsat.pdf")
    varImpPlot(model.train[[x]]$finalModel)
    tmp <- importance(model.train[[x]]$finalModel)
    tmp <- data.frame(band = rownames(tmp), 
                      importance = tmp[,1])
    importance <- data.frame(rbind(importance, tmp))
    dev.off()
  } 
}

if (method=="rf"){
  pdf("figures/predict_SR_Landsat_bwPlot.pdf")
  sort <- aggregate(importance$importance, by = list(importance$band), FUN = mean)
  sort <- sort[with(sort, order(x)), ][1]
  importance$band <- factor(importance$band, levels=as.character(sort[,1]))
  bwplot(band~importance, data = importance)
  dev.off()
}
print(validation) #results of each run

#### Predict on Landsat data ################################################### 
preddata=data.frame(coordinates(landsat),
                    sapply(names(dlr.clean[,-ncol(df.train)]), function (x)
                      as.vector(eval(parse(text=paste0("landsat$",x))))))

preddata=preddata[!is.na(rowSums(preddata)),]
names(preddata)=c("x","y",names(dlr.clean[,-ncol(df.train)]))

###mean prediction for each model run
prediction=list()
prediction_raster=list()
fogobase=landsat[[1]]
values(fogobase)=NA
for (x in 1:length(model.train)){
  prediction[[x]]=predict(model.train[[x]],
                          preddata[,names(dlr.clean[,-ncol(df.train)])])
#  prediction[[x]]=data.frame(preddata$x,preddata$y,prediction)
#  names(prediction[[x]])=c("x","y","prediction")
  prediction_raster[[x]]=rasterize(cbind(preddata$x,
                                         preddata$y),
                                   fogobase,field=prediction[[x]])
}
prediction_raster=stack(prediction_raster)
prediction_raster_mean=calc(prediction_raster, mean)
clrs.spec <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
png(paste0(figurepath,"prediction_raster_mean.png"))
spplot(prediction_raster_mean,col.regions =clrs.spec(100),
       scales = list(draw = TRUE),zlab="Species")
dev.off()
