

library(raster)
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

#definir o nome do bioma
bioma_n <- "Amazonia"

#nome do arquivo de regiões climáticas
layer.reg_clima <- "ClusterKc_AM_v2"

#path pros mapas de 240m (Kc e lulc) e para salvar outros mapas 
path_outputs_maps <- "./outputs/output_maps_v2/"

#path para exportar as tabelas de Kc
path_outputs_tables <- "./outputs/tabelas/"


#########fim da ação

dir.create("./outputs")
dir.create(path_outputs_tables)
dir.create(path_outputs_maps)


##############################


##CALCULAR ZONAL STATS por região climática e por mês

#ler os mapas que mudaram de resolução pelo outro script
reg_clima_p <- readOGR(dsn = "./Maps", layer = layer.reg_clima)
lulc_res <- raster(paste0("./Maps/", bioma_n, "240m.tif"))

#para cada ID (região cimática), extrair lulc do bioma e reclassificar
for (i in 1:length(reg_clima_p)){
  started_time = Sys.time()
  cat( format( started_time, "%a %b %d %X %Y"), '-', 'começando análise para região climática', i, 'em polígono', '\n')
  
  reg_clima_pp <- reg_clima_p[which(reg_clima_p$value == i),]
  tic(msg = "fazer mask de lulc por região climatica")
  lulc_clima <- mask(lulc_res, reg_clima_pp)
  toc()
  gc()
  
  tic(msg = "mudar o código de cada classe de acordo com a região climática")
  ##mudar o código de cada classe de acordo com a região climática e exportar mapa
  lulc_clima[lulc_clima,] <- paste0(i, lulc_clima[lulc_clima,])
  lulc_clima[lulc_clima == 0] <- NA
  
  ##salvar novo raster de LULC reclassificado
  writeRaster(lulc_clima, paste0(path_outputs_maps, bioma_n, "240m_reclas_clima", i), format = "GTiff", overwrite = T)
  gc()
  toc()
  
}

#Juntar os rasters reclassificados de LULC
files <- list.files(path_outputs_maps, pattern = paste0(bioma_n, "240m_reclas_clima"))
Biome <- raster::stack(paste0(path_outputs_maps, files))

mosaic <- merge(Biome)

#passar pra albers para rodar no invest
crs.albers <-
  CRS("+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60
                  +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs")

crs(mosaic) <- crs.albers

writeRaster(mosaic, paste0(path_outputs_maps, bioma_n, "240m_reclas.tif"))

