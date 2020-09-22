#############################
## GDM RUN
#############################

## Library

library(gdm)
library(raster)

## Read inputs (species_site table and enviromental table)
sppdata <- read.csv("./data/occ_insel_10km/semraref7var/sppdata7var.csv")

list_envs <- list.files("./data/envs/ASC10Km/", full.names= T, pattern = "asc")
insele10Km_envs <-stack(list_envs)


### GDM Transformer (fit gdm using rasters)

## Formatimg spp table and rater data

sitePairRast <- formatsitepair(sppdata,
                               bioFormat=2,
                               XColumn="Long",                                                         YColumn="Lat",
                               sppColumn="species",
                               siteColumn="site",
                               predData=insele10Km_envs,
                               sppFilter=0)
##remove NA values
sitePairRast <- na.omit(sitePairRast)

## fit raster GDM
gdmRastMod <- gdm(sitePairRast, geo=TRUE)
summary(gdmRastMod)

##raster input, raster output
transRasts <- gdm.transform(gdmRastMod, insele10Km_envs)

plot(transRasts)

# map biological patterns
rastDat <- sampleRandom(transRasts, 10000)  #radom points
pcaSamp <- prcomp(rastDat)
plot(pcaSamp)

# This funtion predicts biological distances between sites or times using a model object returned from gdm. Predictions between site pairs require a data frame containing the values of predictors for pairs of locations, formatted as follows: distance, weights, s1.X, s1.Y, s2.X, s2.Y, s1.Pred1, s1.Pred2, ..., s1.PredN, s2.Pred1, s2.Pred2, ..., s2.PredN.. Predictions of biological change through time require two raster stacks or bricks for environmental conditions at two time periods, each with a layer for each environmental predictor in the fitted model.

pcaRast <- predict(transRasts, pcaSamp, index=1:3)

plot(pcaRast)


# scale rasters
pcaRast[[1]] <- (pcaRast[[1]]-pcaRast[[1]]@data@min) /
  (pcaRast[[1]]@data@max-pcaRast[[1]]@data@min)*255
plot(pcaRast)


pcaRast[[2]] <- (pcaRast[[2]]-pcaRast[[2]]@data@min) /
  (pcaRast[[2]]@data@max-pcaRast[[2]]@data@min)*255
plot(pcaRast)

pcaRast[[3]] <- (pcaRast[[3]]-pcaRast[[3]]@data@min) /
  (pcaRast[[3]]@data@max-pcaRast[[3]]@data@min)*255
plotRGB(pcaRast, r=1, g=2, b=3)

writeRaster(pcaRast, "./results/Dissimi_Predicted_insel10km.tif")
#################################################################
















gdmTab.rast<-na.omit(gdmTab.rast)
gdm.1 <- gdm(gdmTab.rast, geo=T)

## get idea of number of panels
length(gdm.1$predictors)

png(filename="./results/New_11_setembro/GDM/Maps/GDM.png")
plot(gdm.1, plot.layout=c(3,3))
dev.off()

summary(gdm.1)

# porcentagem de explicação do GDM. Figure X. Fitted functions of observed turnover in composition of species for a Generalized Dissimilarity Model using 4 environmental variables.
#The maximum height reached by each function provides an indication of the total amount of compositional turnover associated with that variable, holding all other variables constant.
#The slope of each function provides an indication of the rate of compositional turnover and how this rate varies along the gradient.

png(filename="./results/New_11_setembro/GDM/Maps/plotUncertainty.png")
plotUncertainty(gdmTab.rast,  sampleSites=0.7, bsIters=5,
                geo=T, spline=NULL,
                knots=NULL, splineCol="blue", errCol="grey80",
                plot.linewidth=2.0, plot.layout=c(3,2))
dev.off()



envRast<-caatinga_envs
rastTrans <- gdm.transform(gdm.1, envRast)

png(filename="./results/New_11_setembro/GDM/Maps/env_variables.png")
plot(rastTrans, plot.layout=c(3,3))
dev.off()


rastDat <- na.omit(getValues(rastTrans))
#rastDat <- sampleRandom(rastTrans, 50000) # can use if rasters are large
pcaSamp <- prcomp(rastDat)
# note the use of the 'index' argument
pcaRast <- predict(rastTrans, pcaSamp, index=1:3)
# scale rasters
pcaRast[[1]] <- (pcaRast[[1]]-pcaRast[[1]]@data@min) /
  (pcaRast[[1]]@data@max-pcaRast[[1]]@data@min)*255
pcaRast[[2]] <- (pcaRast[[2]]-pcaRast[[2]]@data@min) /
  (pcaRast[[2]]@data@max-pcaRast[[2]]@data@min)*255
pcaRast[[3]] <- (pcaRast[[3]]-pcaRast[[3]]@data@min) /
  (pcaRast[[3]]@data@max-pcaRast[[3]]@data@min)*255

###mesmo gráfico com outro padrão de cor
#é tudo o mesmo resultado, só muda aparência (combinação de cores). Escolher qual fica melhor de interpretar
png(filename="./results/New_11_setembro/GDM/Maps/plotRGB.png")
par(mfrow=c(3,2))
plotRGB(pcaRast, r=2, g=3, b=1,stretch='hist')
plotRGB(pcaRast, r=2, g=1, b=3,stretch='hist')
plotRGB(pcaRast, r=3, g=2, b=1,stretch='hist')
plotRGB(pcaRast, r=3, g=1, b=2,stretch='hist')
plotRGB(pcaRast, r=1, g=2, b=3,stretch='hist')
plotRGB(pcaRast, r=1, g=3, b=2,stretch='hist')
dev.off()

png(filename="./results/New_11_setembro/GDM/Maps/plotRGB123.png")
plotRGB(pcaRast, r=1, g=2, b=3, stretch='hist', axes=T, main="RGB123", colNA="gray", bgalpha=100)
dev.off()

png(filename="./results/New_11_setembro/GDM/Maps/plotRGB231.png")
plotRGB(pcaRast, r=2, g=3, b=1,stretch='hist', axes=T, main="RGB231", colNA="gray", bgalpha=100)
dev.off()

##Figure X: Predicted spatial variation in species composition.
#Grid cells mapped in similar colors are predicted to have similar species composition, while cells mapped in very different colors are predicted to be highly dissimilar in composition.
# Colors represent gradients in species composition derived from transformed environmental predictors.
# Locations with similar colors are expected to contain similar communities.

