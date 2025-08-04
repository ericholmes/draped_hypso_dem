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
library(scales)

saveplot = F
oroville_df <- data.frame(Y = 39.534344, X = -121.486103)

# Get US state map data
us_states <- map_data("state")
cali <- us_states[us_states$region == "california",]
polygon <- st_polygon(list(as.matrix(us_states[us_states$region == "california",c("long", "lat")])))
cali_sf <- st_sf(id = cali$region[1], geometry = st_sfc(polygon), crs = 4326)

oroville_sf <- st_as_sf(oroville_df, coords = c("X", "Y"), crs = 4326)

#Load NHD HUC8s
huc6 <- nhdplusTools::get_huc(AOI = oroville_sf, type = "huc06")

huc6 <- st_transform(huc6, st_crs(cali_sf))

# huc8_all$huc4 <- substr(huc8_all$huc8, 1, 4)
# huc8_all$huc6 <- substr(huc8_all$huc8, 1, 6)

# huc8_dissolve <- huc8_all %>%
#   group_by(huc6) %>%  # Group by the categorical variable
#   summarise(geometry = st_union(geometry)) %>%  # Combine geometries
#   st_as_sf()  # Ensure the result is an sf object

# get_upstream_ids(target_comid, max_search_distance = 1e6)

# huc8_single <- st_union(huc8_all)

# Filter HUC-8 polygons by intersection with cali boundary
# huc8_nv <- st_intersection(huc8_all, cali_sf)

#Load rivers
nhd_flow <- nhdplusTools::get_nhdplus(AOI = huc6, realization = "flowline", streamorder = 4)
nhd_flow <- st_transform(nhd_flow, st_crs(cali_sf))

#load lakes
nhd_wb <- nhdplusTools::get_waterbodies(AOI = huc6)
nhd_wb <- st_transform(nhd_wb, st_crs(cali_sf))

# Download elevation data using the 'elevatr' package
elevation_data <- get_elev_raster(locations = huc6, z = 8, prj = st_crs(4326)$proj4string)
elevation_data <- mask(elevation_data, huc8_dissolve)

# Convert raster to a data frame for ggplot
# elevation_df <- as.data.frame(as(elevation_data, "SpatialPixelsDataFrame"))
# elev_sf <- st_as_sf(elevation_df, coords = c("x", "y"))
# colnames(elevation_df) <- c("elevation", "x", "y")
# 
# hs <- hillshader(elevation_data)

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
if(saveplot == T){png("output/Draped_hypso_dem.png",
                      height = 5, width = 5, units = "in", res = 1000, family = "serif")}

ggplot() +
  geom_spatraster(data = hill, fill = vector_cols, maxcell = Inf, alpha = 1) +
  geom_spatraster(data = r, maxcell = Inf, show.legend = F) +
  scale_fill_hypso_tint_c( limits = c(-200, 4500),
                           palette = "wiki-2.0_hypso",
                           alpha = 0.5,
                           labels = label_comma(),
                           breaks = c(
                             seq(-200, 1000, 200),
                             seq(1100, 2500, 100),
                             2600)) +
  geom_sf(data = nhd_wb[nhd_wb$ftype %in% "LakePond",], color = NA, fill = "darkslategrey") +
  geom_sf(data = nhd_flow, color = "darkslategrey", linewidth = .2) +
  geom_sf(data = huc6, fill = NA, color = "grey40") +
  geom_sf(fill = NA, color = "black", linetype = 2) +
  theme_bw() +
  labs(title = "Draped hypsometric DEM", x = "Longitude", y = "Latitude") +
  annotation_scale(location = "bl", width_hint = 0.2, line_width = 1,
                   pad_x = unit(.35, "in")) + 
  annotation_north_arrow(location = "bl", which_north = "false", 
                         style = north_arrow_fancy_orienteering(),
                         height = unit(0.3,"in"), width = unit(0.3,"in"),
                         pad_x = unit(.02, "in"),pad_y = unit(.02, "in"))

if(saveplot == T){dev.off()}