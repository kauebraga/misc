library(sf)
library(osmextract)
library(stringr)
library(dplyr)
library(ggplot2)
library(ggnewscale)
library(ggtext)


# fazer download dos dados do OpenStreetMap - isso pode demorar
osm_points_for <- oe_get("Fortaleza", layer = "points", stringsAsFactors = FALSE, quiet = FALSE)
osm_points_spo <- oe_get("Sao Paulo, Brazil", layer = "points", stringsAsFactors = FALSE, quiet = FALSE)

# garantir que os pontos estejam somente dentro das cidades
# boundingbox to fortaleza
muni_for <- geobr::read_municipality(2304400) %>%
  st_transform(4326)

# boundingbox to sao paulo
muni_spo <- geobr::read_municipality(3550308) %>%
  st_transform(4326)


osm_points_for <- st_join(osm_points_for, muni_for) %>%
  filter(!is.na(code_muni))

osm_points_spo <- st_join(osm_points_spo, muni_spo) %>%
  filter(!is.na(code_muni))



# maptiles
# maptile_for <- readr::read_rds("data/maptiles/maptile_crop_mapbox_for_2019.rds")
# maptile_spo <- readr::read_rds("data/maptiles/maptile_crop_mapbox_spo_2019.rds")



# extrair os locais com bicicletas compartilhadas -----------------
# a tag bicycle_rental identifica os pontos com bicicletas compartilhadas
osm_bikesharing_for <- osm_points_for %>%
  filter(str_detect(string = other_tags, pattern = "bicycle_rental")) %>%
  mutate(other_tags1 = gsub(pattern = "\"", replacement = "", x = other_tags)) %>%
  # extract capacity
  mutate(capacity = gsub(pattern = "(.*),capacity=>(\\d{1,})(,.*)", replacement = "\\2", x = other_tags1)) %>%
  mutate(capacity = ifelse(grepl(pattern = "^\\d{1,}$", x = capacity), capacity, NA)) %>%
  filter(!is.na(capacity)) %>%
  mutate(capacity = as.numeric(capacity))

osm_bikesharing_spo <- osm_points_spo %>%
  filter(str_detect(string = other_tags, pattern = "bicycle_rental")) %>%
  mutate(other_tags1 = gsub(pattern = "\"", replacement = "", x = other_tags)) %>%
  # extract capacity
  mutate(capacity = gsub(pattern = "(.*),capacity=>(\\d{1,})(,.*)", replacement = "\\2", x = other_tags1)) %>%
  mutate(capacity = ifelse(grepl(pattern = "^\\d{1,}$", x = capacity), capacity, NA)) %>%
  filter(!is.na(capacity)) %>%
  mutate(capacity = as.numeric(capacity))



# gsub(pattern = "(.*),capacity=>(\\d{2})(,.*)", replacement = "\\2", x = "amenity=>bicycle_rental,capacity=>30,network=>Bike Salvador")


max_legend <- max(osm_bikesharing_for$capacity, osm_bikesharing_spo$capacity) 
min_legend <- min(osm_bikesharing_for$capacity, osm_bikesharing_spo$capacity) 

map_for <- ggplot()+
  geom_raster(data = maptile_for, aes(x, y, fill = hex), alpha = 1) +
  coord_equal() +
  scale_fill_identity()+
  # nova escala
  new_scale_fill() +
  geom_sf(data = st_transform(muni_for, 3857), fill = NA)+
  geom_sf(data = st_transform(osm_bikesharing_for, 3857), aes(size = as.numeric(capacity)), alpha = 0.3, show.legend = FALSE)+
  scale_size_continuous(limits = c(min_legend, max_legend))+
  theme_void()


map_spo <- ggplot()+
  geom_raster(data = maptile_spo, aes(x, y, fill = hex), alpha = 1) +
  coord_equal() +
  scale_fill_identity()+
  # nova escala
  new_scale_fill() +
  geom_sf(data = st_transform(muni_spo, 3857), fill = NA)+
  geom_sf(data = st_transform(osm_bikesharing_spo, 3857), aes(size = as.numeric(capacity)), alpha = 0.3)+
  scale_size_continuous(limits = c(min_legend, max_legend))+
  labs(caption = "Fonte: OpenStreetMap<br>**Atencao:** Dados podem estar incompletos/desatualizados")+
  theme_void()+
  theme(plot.caption = element_markdown(lineheight = 1.2))


# gerar plot
library(patchwork)
(map_for / map_spo) + plot_layout(guides = "collect") & theme(legend.position = 'bottom') & labs(size = "Bicicletas disponiveis")
