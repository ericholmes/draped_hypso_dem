# ------------------------------------------------------------
# Setup
# ------------------------------------------------------------
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

# Create image output directory
if (!dir.exists("images")) dir.create("images")

save_step <- function(plot, filename, width = 6, height = 6, dpi = 300) {
  ggsave(file.path("images", filename), plot, width = width, height = height, dpi = dpi)
}

# ------------------------------------------------------------
# 1. AOI
# ------------------------------------------------------------
oroville_df <- data.frame(Y = 39.534344, X = -121.486103)
oroville_sf <- st_as_sf(oroville_df, coords = c("X", "Y"), crs = 4326)

step1 <- ggplot() +
  geom_sf(data = oroville_sf, size = 3, color = "red") +
  theme_minimal() +
  labs(title = "AOI: Oroville, CA")

save_step(step1, "step1_aoi.png")

# ------------------------------------------------------------
# 2. California boundary
# ------------------------------------------------------------
us_states <- map_data("state")
cali <- us_states[us_states$region == "california",]

polygon <- st_polygon(list(as.matrix(cali[,c("long", "lat")])))
cali_sf <- st_sf(id = "california", geometry = st_sfc(polygon), crs = 4326)

step2 <- ggplot() +
  geom_sf(data = cali_sf, fill = "gray90") +
  geom_sf(data = oroville_sf, color = "red", size = 3) +
  theme_void() +
  labs(title = "California Boundary")

save_step(step2, "step2_cali_boundary.png")

# ------------------------------------------------------------
# 3. HUC boundaries
# ------------------------------------------------------------
huc8 <- nhdplusTools::get_huc(
  id = c("18020121", "18020122", "18020123"),
  type = "huc08"
) %>% st_transform(4326)

feather_dissolve <- huc8 %>%
  summarise(geometry = st_union(geometry)) %>%
  st_as_sf()

feather_bbox <- st_bbox(feather_dissolve)
feather_bbox_sf <- st_as_sfc(feather_bbox)

step3 <- ggplot() +
  geom_sf(data = huc8, fill = "lightblue", color = "blue") +
  geom_sf(data = feather_dissolve, fill = NA, color = "black", linewidth = 1) +
  theme_minimal() +
  labs(title = "HUC8 Boundaries")

save_step(step3, "step3_huc8.png")

# ------------------------------------------------------------
# 4. NHD layers
# ------------------------------------------------------------
nhd_flow <- nhdplusTools::get_nhdplus(
  AOI = feather_dissolve,
  realization = "flowline"
) %>% st_transform(4326)

nhd_flow <- merge(
  nhd_flow,
  data.frame(streamorde = 1:6, width = rev(c(1, .8, .6, .4, .2, .1)))
)

nhd_wb <- nhdplusTools::get_waterbodies(AOI = feather_dissolve) %>%
  st_transform(4326)

step4 <- ggplot() +
  geom_sf(data = nhd_wb, fill = "steelblue3", color = NA) +
  geom_sf(data = nhd_flow, aes(linewidth = width), color = "navy") +
  scale_linewidth(range = c(0.1, 1)) +
  theme_minimal() +
  labs(title = "NHD Flowlines & Waterbodies")

save_step(step4, "step4_nhd_layers.png")

# ------------------------------------------------------------
# 5. DEM download + clip
# ------------------------------------------------------------
elevation_data <- get_elev_raster(
  locations = feather_dissolve,
  z = 10,
  prj = st_crs(4326)$proj4string
)

elevation_data <- mask(elevation_data, feather_dissolve)
r <- rast(elevation_data)

step5 <- ggplot() +
  geom_spatraster(data = r) +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(title = "Clipped DEM")

save_step(step5, "step5_dem.png")

# ------------------------------------------------------------
# 6. Hillshade (your exact method)
# ------------------------------------------------------------
slope  <- terrain(r, "slope", unit = "radians")
aspect <- terrain(r, "aspect", unit = "radians")
hill   <- shade(slope, aspect, 30, 270)
names(hill) <- "shades"

step6 <- ggplot() +
  geom_spatraster(data = hill) +
  scale_fill_gradient(low = "gray10", high = "gray95") +
  theme_minimal() +
  labs(title = "Hillshade")

save_step(step6, "step6_hillshade.png")

# ------------------------------------------------------------
# 7. EXACT DEM DRAPING (your code)
# ------------------------------------------------------------
pal_greys <- hcl.colors(1000, "Grays")

index <- hill %>%
  mutate(index_col = rescale(shades, to = c(1, length(pal_greys)))) %>%
  mutate(index_col = round(index_col)) %>%
  pull(index_col)

vector_cols <- pal_greys[index]

map_hypso <- ggplot() +
  geom_spatraster(data = hill, fill = vector_cols, maxcell = Inf, alpha = 1) +
  geom_spatraster(data = r, maxcell = Inf, show.legend = FALSE) +
  scale_fill_hypso_tint_c(
    limits = c(0, 2730),
    palette = "wiki-2.0_hypso",
    alpha = 0.6,
    labels = label_comma(),
    breaks = c(seq(0, 1000, 200),
               seq(1100, 2500, 100),
               2600)
  ) +
  theme_minimal() +
  labs(title = "Hypsometric DEM Draped on Hillshade")

save_step(map_hypso, "step7_hypso_draped.png")

# ------------------------------------------------------------
# 8. Add NHD overlays (your code)
# ------------------------------------------------------------
map_hypso_nhd <- map_hypso +
  geom_sf(data = nhd_wb[nhd_wb$ftype %in% "LakePond",], fill = "darkslategrey", color = NA) +
  geom_sf(data = nhd_flow, aes(linewidth = width), color = "darkslategrey", show.legend = F) +
  scale_linewidth(range = c(0.05, .3)) +
  labs(title = "Hypsometric DEM + NHD Overlays")

save_step(map_hypso_nhd, "step8_hypso_nhd.png")

# ------------------------------------------------------------
# 9. Inset + final map (your code)
# ------------------------------------------------------------
inset <- ggplot() +
  geom_sf(data = cali_sf, fill = "gray90") +
  geom_sf(data = feather_dissolve, fill = "grey20") +
  geom_sf(data = feather_bbox_sf, fill = NA, color = "black") +
  theme_void()

final_map <- ggdraw() +
  draw_plot(map_hypso_nhd) +
  draw_plot(inset, x = .73, y = .65, width = 0.25, height = 0.25)

save_step(final_map, "step9_final_map.png")

message("All intermediate images saved to 'images/'")
