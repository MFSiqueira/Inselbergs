#############################
## GDM RUN
#############################

library(gdm)


##table data, species and environmental
load(system.file("./data/gdm.RData", package="gdm"))
sppData <- gdmExpData[, c(1,2,13,14)]
envTab <- gdmExpData[, c(2:ncol(gdmExpData))]


write.csv(envTab, "./data/envTab_vinheta_GDM", dec = ".")

write.csv(sppData, "./data/sppData_vinheta_GDM")

#########table type 2
##site xy spp list, table-table
exFormat2a_2 <- formatsitepair(sppData, 2, XColumn="Long", YColumn="Lat", sppColumn="species",
                             siteColumn="site", predData=envTab)

vignet <- gdm(exFormat2a_2, geo = FALSE, splines = NULL, knots = NULL)

plot(vignet)
##site xy spp list, table-raster
##commented out to reduce example run time
#exFormat2b <- formatsitepair(sppData, 2, XColumn="Long", YColumn="Lat", sppColumn="species",
#	siteColumn="site", predData=envRast)


############### our data


## Read
sppdata <- read.csv("/home/jbrj/Documentos/ntbox/rafaela/occ_insel_10km/semraref7var/sppdata7var.csv")

envtab <- read.csv("/home/jbrj/Documentos/ntbox/rafaela/occ_insel_10km/semraref7var/envdatainsel10km7var.csv")

## site xy spp list, table-table
exFormat2a <- formatsitepair(sppdata, 2, XColumn="Long", YColumn="Lat", sppColumn="species", siteColumn="site", predData=envtab)

gdm_run <- gdm(exFormat2a, geo=TRUE, splines=NULL, knots=NULL)
par(mar = rep(2, 4))

plot(gdm_run)
str(gdm_run)
dev.off()
#comentario de teste

write.csv(exFormat2a,"/home/jbrj/Documentos/ntbox/rafaela/exFormat2a.csv", sep = ",", dec = ".")

spTable <- read.csv("/home/jbrj/Documentos/ntbox/rafaela/exFormat2a.csv")

var_imp <- gdm.varImp(spTable[,-1], geo = T,
           nPerm = 50, parallel = FALSE, cores = 4, sampleSites = 1, sampleSitePairs = 1,
           outFile = NULL)

barplot(sort(var_imp[[2]][,1], decreasing = T),
        space = 1.5, axisnames = T, font.sub = 2, cex.names = 0.8)
