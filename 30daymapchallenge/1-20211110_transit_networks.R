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
