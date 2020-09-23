#####Este script corta variáveis raster (.asc) por shapes (.shp)


### Library pcgks

library(raster)
library(rgdal)
library(maptools)

### Read and load Atlantic Forest Shape and altitude raster

af_shape <- readOGR("./data/shapes/mata_atlantica11428.shp") # carregando o shape pela função OGR do pacote rgdal
plot(af_shape) #plotando

lista_asc <- list.files("./data/Asc/", full.names = T, pattern = ".asc") #criando uma lista de arquivos raster pela função list.files


ascs <- stack(lista_asc) #juntando todos os rasters num único objeto
plot(ascs)

### Mask Crop for atlantic forest 
af_masked <- mask(x = ascs, mask = af_shape) #aplicando a máscara (shape) pela função mask do pacote raster 
plot(af_masked) # plote 

af_extention <- crop(x = af_masked, y = extent(af_shape)) #agora corte por essa máscara
plot(af_extention)

a <- paste0(names(ascs), ".asc")

writeRaster(af_extention, filename=a, bylayer=TRUE) ### Isto vai salvar no local em que estiver o projeto. Veja este local no canto superior  direito 

