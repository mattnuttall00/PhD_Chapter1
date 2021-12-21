library(tidyverse)
library(sf)
library(viridis)
library(ggrepel)
library(raster)
library(rgdal)
library(patchwork)
library(gridGraphics)
library(rasterVis)

### Cambodia within SEA ####

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


### Cambodia - elevation ####

# load in raster
dem <- raster("Spatial_data/elevation/dem_cont_fin60.img")
hillshade <- raster("Spatial_data/elevation/elevatioin_shade.img")

plot(dem)
image(dem)

# load in country shapefile
cambodia <- readOGR("Spatial_data/Country Boundary.shp")
proj4string(cambodia) <- CRS("+proj=utm +zone=48 +a=6377276.3452 +rf=300.801699438502 +units=m +no_defs")

plot(r, add=T)
plot(cambodia, add=T)

# clip raster by country border
dem_clip <- mask(dem, cambodia)
plot(dem_clip)



# change colours
cols <- terrain.colors(10)
cols <- viridis(10)
cols <- magma(10)
image(dem_clip, col=cols)
plot(dem_clip, col=cols, axes=FALSE, box=FALSE)

levelplot(dem_clip, margin = FALSE)

### Cambodia - land cover ####

# load in country shapefile
cambodia <- readOGR("Spatial_data/Country Boundary.shp")

lc <- raster("Spatial_data/ESACCI_clip_KH/CCI_clip.tif")
plot(lc)
colortable(lc) <- ctab
ctab <- c("white", ,"chartreuse4", , , , , ,"chartreuse4" , ,"chartreuse4", , , , , ,"chartreuse4", , ,"chartreuse4",)

x <- levelplot(lc)

forest <- c(50,60,61,62,70,80,90,100)
farm <- c(10,20,30,40,121,120)
grass <- c(130,180,160,11,170,110,12,150)
water <- 210
urban <- c(190,202,200,201)
nums <- data.frame(values = unique(values(lc)))
nums$cols <- ifelse(is.na(nums$values),"white",
                    ifelse(nums$values %in% forest, "chartreuse4", 
                           ifelse(nums$values %in% farm, "darkgoldenrod1",
                                  ifelse(nums$values %in% grass, "khaki", 
                                         ifelse(nums$values %in% water, "dodgerblue3", 
                                                ifelse(nums$values %in% urban, "gray70", NA))))))

ctab <- as.vector(nums$cols)



r <- raster(ncol=10, nrow=10)
values(r) <- sample(0:255, ncell(r), replace=TRUE)
ctab <- sample(rainbow(256))
colortable(r) <- ctab
plot(r)
valuehead(colortable(r)) 



### final plot - general intro ####


### plot together
Fig1_methods / ~plot(dem_clip, col=cols, axes=FALSE, box=FALSE)

Fig1_methods + ~levelplot(lc)


plot(dem_clip, col=cols, axes=FALSE, box=FALSE)
legend("right", legend = values,text.width=0.05)


## ggplot - DON'T USE GGPLOT

# convert raster to spatialpixeldatraframe
dem_spdf <- as(dem_clip, "SpatialPixelsDataFrame")
dem_df <- as.data.frame(dem_spdf)
colnames(dem_df) <- c("value", "x", "y")

ggplot(dem_df, aes(x=x, y=y, fill=value))+
  geom_tile()+
  scale_fill_viridis()

