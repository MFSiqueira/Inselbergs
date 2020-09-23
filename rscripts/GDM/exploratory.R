#########################################
## Exploratory analysis for Bryophytes
## 22 de setembro de 2020
#########################################

## Library
library(raster)

new <- read.csv("/home/jbrj/Documentos/ntbox/rafaela/occ_insel_10km.csv", sep = ",", dec = ".")

## Read and load envs data (all raster)

predictors_list <- list.files("/home/jbrj/Documentos/curso_modelagem_2020/Datasets/ASC/", full.names = T, pattern = ".asc")

predictors <- stack(predictors_list)

plot(predictors)

### Plotar pontos no mapa para verficar

### Extract envs values from points using ratser pckg

envs_values_in_points <- raster::extract(predictors,
                                         new[,c("Long","Lat")],method="bilinear")

## NA remove
envs_values_in_points_2 <- na.omit(envs_values_in_points)

## Write the table of env values in lat long
write.csv(envs_values_in_points,"/home/jbrj/Documentos/ntbox/rafaela/occ_extract_insel10km.csv", sep = ",", dec = ".")

write.csv(envs_values_in_points_2,"/home/jbrj/Documentos/ntbox/rafaela/occ_extract_insel10km_noNA.csv", sep = ",", dec = ".")
