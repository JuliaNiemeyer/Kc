
library (raster)
library(rgeos)
library(terra)
library(rgdal)
library(tidyverse)
library(tictoc)


##########
#script elaborado por Julia Niemeyer
#23/03/2022
## Para Santos Lab
##########


#############PRECISA DE AÇÃO

#botar onde achar cada mapa base para análise

#definir o nome do bioma
bioma_n <- "Amazonia"

#ler tif de lulc do bioma
lulc <- raster("./Maps/mapbiomas-brazil-collection-60-amazonia-2020.tif")


#path pro Kc
path_Kc <- './Maps/MODIS_LAI_KC_MONTH2021.tif'

#path pros outputs 
path_outputs_maps <- "./outputs/output_maps/"

path_outputs_tables <- "./outputs/tabelas/"


#########fim da ação

dir.create("./outputs")
dir.create(path_outputs_tables)
dir.create(path_outputs_maps)

# South America Albers Equal Area Conic
crs.albers <-
  CRS("+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60
                  +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs")

####Resample lulc - 240m (baseado no Cerrado)
#WGS84
WGS <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

lulc[lulc == 0] <- NA

tic(msg ='mudar resolução de lulc para 240m')
##Mudar a resolução para 240m (~Cerrado)
lulc_res <- aggregate(lulc, fact = 8, fun = modal, na.rm = TRUE, progress= "text")
writeRaster(lulc_res, paste0(path_outputs_maps, bioma_n, "240m"), format = "GTiff")
toc()
gc()


###Versão 1 da análise: Kc em format raster
##mudar pra cada banda de Kc separadamente
tic(msg = 'mudar resolução de Kc para 240m')
##resample Kc
for (a in 1:12) {
  cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'ressampling Kc do mes', a, '\n')
  Kc_month <- raster(path_Kc, band = a)
  Kc_res <- resample(Kc_month, lulc_res, method="ngb")
  writeRaster(Kc_res, paste0(path_outputs_maps, "Kc_240m_mês_", a), format = "GTiff")
  gc()
}
toc()
