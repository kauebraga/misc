library(gtfstools)
library(sf)
library(dplyr)
library(ggplot2)
library(patchwork)


gtfs_rio <- read_gtfs('/home/kaue/Downloads/GTFS Rio feed_20190516.zip') 
gtfs_spo <- read_gtfs('/home/kaue/Downloads/gtfs_spo_bernardo_2020-11.zip')
gtfs_for <- read_gtfs('/home/kaue/Downloads/gtfs_for_etufor_2019-10.zip')
gtfs_rec <- read_gtfs('/home/kaue/Downloads/gtfs_rec_transpor_2019-09.zip')
gtfs_cur <- read_gtfs('/home/kaue/Downloads/gtfs_cur_urbs_2019-10.zip')

# transform to sf
shapes_rio <- gtfstools::convert_shapes_to_sf(gtfs_rio) %>% mutate(cidade = "Rio de Janeiro")
shapes_spo <- gtfstools::convert_shapes_to_sf(gtfs_spo) %>% mutate(cidade = "São Paulo")
shapes_for <- gtfstools::convert_shapes_to_sf(gtfs_for) %>% mutate(cidade = "Fortaleza")
shapes_rec <- gtfstools::convert_shapes_to_sf(gtfs_rec) %>% mutate(cidade = "Recife")
shapes_cur <- gtfstools::convert_shapes_to_sf(gtfs_cur) %>% mutate(cidade = "Curitiba")

shapes <- rbind(shapes_rio,
                shapes_spo,
                shapes_for,
                shapes_rec,
                shapes_cur)

map_rio <-  
ggplot()+
  geom_sf(data = shapes_rio, alpha = 0.2, size = 0.2) +
  theme_void()

map_for <- 
ggplot()+
  geom_sf(data = shapes_for, alpha = 0.2, size = 0.2) +
  theme_void() 

map_cur <- 
ggplot()+
  geom_sf(data = shapes_cur, alpha = 0.2, size = 0.2) +
  theme_void()

map_rec <- 
ggplot()+
  geom_sf(data = shapes_rec, alpha = 0.2, size = 0.2) +
  theme_void()

map_spo <- 
ggplot()+
  geom_sf(data = shapes_spo, alpha = 0.2, size = 0.2) +
  theme_void()
  

(map_rio) / (map_cur + map_rec) /  (map_for + map_spo)  + plot_layout(widths = 1, heights = 1)







# mesma escala --------------------------

sfc_as_cols <- function(x, names = c("lon","lat")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))                                                                                                                                                                                                                                                     
  ret <- sf::st_coordinates(x)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  ui <- dplyr::bind_cols(x,ret)
  st_set_geometry(ui, NULL)
}
# calcular centroides
centroide_rio <- st_centroid(geobr::read_municipality(3304557)) %>% st_transform(4326) %>% sfc_as_cols()
centroide_cur <- st_centroid(geobr::read_municipality(4106902)) %>% st_transform(4326) %>% sfc_as_cols()
centroide_rec <- st_centroid(geobr::read_municipality(2611606)) %>% st_transform(4326) %>% sfc_as_cols()
centroide_for <- st_centroid(geobr::read_municipality(2304400)) %>% st_transform(4326) %>% sfc_as_cols()
centroide_spo <- st_centroid(geobr::read_municipality(3550308)) %>% st_transform(4326) %>% sfc_as_cols()


# calcular bouningbox spo
bbox_spo <- st_bbox(shapes_spo)
bbox_rio <- st_bbox(shapes_rio)

# calcular offset entre bbox e centroide de spo
offset_xmin <- bbox_rio[[1]] - centroide_rio$lon 
offset_xmax <- bbox_rio[[3]] - centroide_rio$lon 
offset_ymin <- bbox_rio[[2]] - centroide_rio$lat
offset_ymax <- bbox_rio[[4]] - centroide_rio$lat 



map_rio_scale <-  
  ggplot()+
  geom_sf(data = shapes_rio, alpha = 0.2, size = 0.2) +
  coord_sf(xlim = c(centroide_rio$lon + offset_xmin, centroide_rio$lon + offset_xmax),
           ylim = c(centroide_rio$lat + offset_ymin, centroide_rio$lat + offset_ymax))+
  theme_void()

map_for_scale <- 
  ggplot()+
  geom_sf(data = shapes_for, alpha = 0.2, size = 0.2) +
  coord_sf(xlim = c(centroide_for$lon + offset_xmin, centroide_for$lon + offset_xmax),
           ylim = c(centroide_for$lat + offset_ymin, centroide_for$lat + offset_ymax))+
  theme_void()

map_cur_scale <- 
  ggplot()+
  geom_sf(data = shapes_cur, alpha = 0.2, size = 0.2) +
  coord_sf(xlim = c(centroide_cur$lon + offset_xmin, centroide_cur$lon + offset_xmax),
           ylim = c(centroide_cur$lat + offset_ymin, centroide_cur$lat + offset_ymax))+
  theme_void()

map_rec_scale <- 
  ggplot()+
  geom_sf(data = shapes_rec, alpha = 0.2, size = 0.2) +
  coord_sf(xlim = c(centroide_rec$lon + offset_xmin, centroide_rec$lon + offset_xmax),
           ylim = c(centroide_rec$lat + offset_ymin, centroide_rec$lat + offset_ymax))+
  theme_void()

map_spo_scale <- 
  ggplot()+
  geom_sf(data = shapes_spo, alpha = 0.2, size = 0.2) +
  coord_sf(xlim = c(centroide_spo$lon + offset_xmin, centroide_spo$lon + offset_xmax),
           ylim = c(centroide_spo$lat + offset_ymin, centroide_spo$lat + offset_ymax))+
  theme_void()



map_rio_scale + map_cur_scale + map_rec_scale + map_for_scale + map_spo_scale
