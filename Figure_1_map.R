library(tidyverse)
library(sf)
library(viridis)
library(ggrepel)

# load in cambodia shapefile
country.shp <- st_read('Spatial_data/Natural_earth/ne_50m_admin_0_countries.shp')
plot(country.shp)
str(country.shp)

ggplot(country.shp, aes(group=SOVEREIGNT, fill=SOVEREIGNT))+
  geom_sf(show.legend = F)


# select SEA countries
country.shp$SUBREGION
sea.shp <- country.shp %>% filter(SUBREGION=="South-Eastern Asia")

# extract centre coords for labels
sea.shp <- sea.shp %>% mutate(lon=map_dbl(geometry, ~st_centroid(.x)[[1]]),
                              lat=map_dbl(geometry, ~st_centroid(.x)[[2]]))

# add column to identify Cambodia
sea.shp <- sea.shp %>% mutate(Index = ifelse(SOVEREIGNT=="Cambodia","1","0"))

# set colours
cols <- c("grey", "firebrick2")

Fig1_methods <- ggplot(sea.shp, aes(group=SOVEREIGNT, fill=Index))+
                geom_sf(show.legend = F)+
                scale_fill_manual(values = cols)+
                theme(panel.background = element_blank())

ggsave("Write up/Figures/Fig1_methods.png",Fig1_methods,
       dpi=300, width = 20, height = 20, unit="cm")
