# Load required libraries
library(tidyverse)
library(sf)
library(nhdplusTools)
library(elevatr)
library(hillshader)
library(raster)
library(terra)
library(tidyterra)
library(ggspatial)
library(ggpattern)
library(ggnewscale)
library(ggrepel)
library(scales)
library(cowplot)


saveplot = F
oroville_df <- data.frame(Y = 39.534344, X = -121.486103)
oroville_sf <- st_as_sf(oroville_df, coords = c("X", "Y"), crs = 4326)

# Get US state map data
us_states <- map_data("state")
cali <- us_states[us_states$region == "california",]
polygon <- st_polygon(list(as.matrix(us_states[us_states$region == "california",c("long", "lat")])))
cali_sf <- st_sf(id = cali$region[1], geometry = st_sfc(polygon), crs = 4326)

target_huc8 <- nhdplusTools::get_huc(AOI = oroville_sf, type = "huc08")

#Load NHD HUCs
huc6 <- nhdplusTools::get_huc(AOI = oroville_sf, type = "huc06")
huc6 <- st_transform(huc6, st_crs(cali_sf))

huc8 <- nhdplusTools::get_huc(id = c("18020121", "18020122", "18020123"), type = "huc08")
huc8 <- st_transform(huc8, st_crs(cali_sf))

feather_dissolve <- huc8 %>%
  summarise(geometry = st_union(geometry)) %>%  # Combine geometries
  st_as_sf()  # Ensure the result is an sf object
feather_bbox <- st_bbox(feather_dissolve)
feather_bbox_sf<- st_as_sfc(feather_bbox)

#Load rivers
nhd_flow <- nhdplusTools::get_nhdplus(AOI = feather_dissolve, realization = "flowline")
nhd_flow <- st_transform(nhd_flow, st_crs(cali_sf))
nhd_flow <- merge(nhd_flow, data.frame(streamorde = 1:6, width = rev(c(1,.8, .6, .4, .2,.1))))

#load lakes
nhd_wb <- nhdplusTools::get_waterbodies(AOI = feather_dissolve)
nhd_wb <- st_transform(nhd_wb, st_crs(cali_sf))

nhd_wb_centroids <- st_centroid(nhd_wb) #%>% st_coordinates()
nhd_wb_centroids[nhd_wb_centroids$areasqkm == 62.408,"gnis_name"] <- "Lake Oroville"

# Download elevation data using the 'elevatr' package
elevation_data <- get_elev_raster(locations = feather_dissolve, z = 10, prj = st_crs(4326)$proj4string)
elevation_data <- mask(elevation_data, feather_dissolve)

r <- rast(elevation_data)
## Create hillshade effect
slope <- terrain(r, "slope", unit = "radians")
aspect <- terrain(r, "aspect", unit = "radians")
hill <- shade(slope, aspect, 30, 270)

# normalize names
names(hill) <- "shades"

# Hillshading, but we need a palette
pal_greys <- hcl.colors(1000, "Grays")

# Use a vector of colors
index <- hill %>%
  mutate(index_col = rescale(shades, to = c(1, length(pal_greys)))) %>%
  mutate(index_col = round(index_col)) %>%
  pull(index_col)
vector_cols <- pal_greys[index]

##Save map
if(saveplot == T){jpeg("output/Draped_hypso_dem_feather_inset_nhdfull.jpg",
                      height = 6, width = 6, units = "in", res = 1000, family = "serif")}

map <- ggplot() +
  geom_spatraster(data = hill, fill = vector_cols, maxcell = Inf, alpha = 1) +
  geom_spatraster(data = r, maxcell = Inf, show.legend = F) +
  scale_fill_hypso_tint_c(limits = c(0, 2730),
                           palette = "wiki-2.0_hypso",
                          # palette = "colombia",
                          # palette = "usgs-gswa2",
                           alpha = 0.6,
                           labels = label_comma(),
                           breaks = c(seq(0, 1000, 200),
                             seq(1100, 2500, 100),
                             2600)) +
  geom_sf(data = nhd_wb[nhd_wb$ftype %in% "LakePond",], color = NA, fill = "darkslategrey") +
  geom_sf(data = nhd_flow, color = "darkslategrey", aes(linewidth = width), show.legend = F) +
  scale_linewidth(range = c(0.05, .3)) +
  geom_sf(data = feather_dissolve, fill = NA, color = "grey40", linetype = 2) +
  geom_sf(fill = NA, color = "black", linetype = 2) +
  theme_bw() +
  geom_text_repel(data = nhd_wb_centroids[nhd_wb_centroids$areasqkm >14, ], 
                   aes(geometry = geometry, label = gnis_name),stat = "sf_coordinates",
                   bg.color = "white", color = "darkslategrey",
                   bg.r = 0.25, nudge_x = .1, nudge_y = .03, size = 3) +
  coord_sf(xlim = c(feather_bbox["xmin"]-.02, feather_bbox["xmax"]+.02),
    ylim = c(feather_bbox["ymin"]-.02, feather_bbox["ymax"]+.02),
    expand = FALSE) +
  labs(title = "Draped hypsometric DEM", x = "Longitude", y = "Latitude") +
  annotation_scale(location = "bl", width_hint = 0.2, line_width = 1,
                   pad_x = unit(.35, "in")) + 
  annotation_north_arrow(location = "bl", which_north = "false", 
                         style = north_arrow_fancy_orienteering(),
                         height = unit(0.3,"in"), width = unit(0.3,"in"),
                         pad_x = unit(.02, "in"), pad_y = unit(.02, "in"))

inset <- ggplot() +
  geom_sf(data = cali_sf, fill = "gray90") +
  geom_sf(data = feather_dissolve, fill = "grey20") +
  geom_sf(data = feather_bbox_sf, fill = NA, color = "black") +
  theme_void()

(combined_map <- ggdraw() +
  draw_plot(map) +
  draw_plot(inset, x = .73, y = .65, width = 0.25, height = 0.25))

if(saveplot == T){dev.off()}
