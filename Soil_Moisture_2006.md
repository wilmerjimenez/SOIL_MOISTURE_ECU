## SOIL MOISTURE ECUADOR ESA 2006

setwd("C:/Users/WILMER JIMENEZ/Desktop/MAPEO DIGITAL DE SUELOS/R/Humedad_suelos_satelital_Ecuador/sm/2006")
library(raster)
library(rasterVis)
library(ncdf4)
sm  <- stack(list.files(pattern='nc'), varname='sm')
cou <- readRDS ('C:/Users/WILMER JIMENEZ/Desktop/MAPEO DIGITAL DE SUELOS/R/Humedad_suelos_satelital_Ecuador/Eculim2014/Eculim2014.rds')
plot(cou, 
     main='Ecuador')
#cou <- raster::getData("GADM", country='ECUADOR', level=1)
sm  <- crop(sm, cou)
idx <- seq(as.Date('2006-01-01'), as.Date('2006-12-31'), 'day')
sm <- setZ(sm, idx)
names(sm) <- idx
sm <- stack(sm)
plot(sm)
smX <- stack(calc(sm, fun=mean, na.rm=TRUE),

             calc(sm, fun=min, na.rm=TRUE),

             calc(sm, fun=max, na.rm=TRUE))
names(smX) <- c('SMmean', 'SMmin', 'SMmax')
#plot(smX,col=colorRampPalette(c("gray", "brown", "blue"))(255))
densityplot(smX)
saveRDS(as(sm, "SpatialPixelsDataFrame"), file = "sm-esa-cci-2006.rds")
  bwplot(sm, scales=list(x=list(at=c(1, 180, 365))))
smsp <- as(sm, 'SpatialPointsDataFrame')
plot(smsp, main='training sm-data-ESA-CCI_2006')
plot(cou, add=TRUE)
saveRDS(smsp, file='SM-ECU-2006.rds')
####### 2da parte
library(raster)
library(caret)
library(doMC)
library(doParallel)
sm <- readRDS('SM-ECU-2006.rds')
topo <- stack ('C:/Users/WILMER JIMENEZ/Desktop/MAPEO DIGITAL DE SUELOS/Google_Drive/Ecuador/ECUtopo/ECUtopo.tif')
names(topo) <- readRDS ('C:/Users/WILMER JIMENEZ/Desktop/MAPEO DIGITAL DE SUELOS/Google_Drive/Ecuador/ECUtopo/namesTOPO.rds')
plot(topo)
smrm <- cbind(sm@data, extract(topo, sm))
smrm$averageSM <-rowMeans(smrm[1:366], na.rm = TRUE)
smX <- na.omit(smrm[367:380])
##VARIABLE IMPORTANCE 
cl <- makeCluster(detectCores(), type='PSOCK')
registerDoParallel(cl)
control2 <- rfeControl(functions=rfFuncs, method="repeatedcv", number=5, repeats=5)
(RFE_RF_model <- rfe(smX[-06], smX[,06], sizes=c(1:6), rfeControl=control2) )
plot(RFE_RF_model, type=c("g", "o"))
predictors(RFE_RF_model)[1:15]
stopCluster(cl = cl)
##SOIL MOISTURE PREDICTION
beginCluster(8)
library(randomForest)
(predRFE <- clusterR(topo, predict, args=list(model=RFE_RF_model)))
plot(predRFE, col=colorRampPalette(c("gray", "brown", "blue"))(255),
     main='Humedad del Suelo-Random Forest-Ecuador-2006')
endCluster()
writeRaster(predRFE, file="PredRF_ms2006_lim2014.tif")
## end
