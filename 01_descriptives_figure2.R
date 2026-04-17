# THIS CODE
  # Uses the official spatial layers from the `geobr` to recreate Figure 2. 

# =========================================================
# Part 0: Preparation
# =========================================================

# Load libraries and data
rm(list=ls())

library(haven)
library(dplyr)
library(ggplot2)
library(sf)
library(geobr)
library(patchwork)
library(ggpattern)
library(grid)

#municipios <- read_municipality(code_muni = "all", year = 2010)
#biomes <- read_biomes(year = 2019)
#conservation_units <- read_conservation_units(date = 201909)

#write_sf(municipios,"data/municipios.gpkg")
#write_sf(biomes,"data/amazon_biome.gpkg")
#write_sf(conservation_units, "data/conservation_units.gpkg")

analysis_df <- read_dta("data/analysisdataset.dta")
municipios <- read_sf("data/municipios.gpkg")
biomes <- read_sf("data/amazon_biome.gpkg")
conservation_units <- read_sf("data/conservation_units.gpkg")


# Filter and adapt datasets, create variables for plotting
municipios <- municipios %>% mutate(muni_ID = as.numeric(code_muni))

map_data <- analysis_df %>% filter(year == 2010) %>% select(muni_ID, priority, GN)

merged_municipios <- municipios %>%
  left_join(map_data, by = "muni_ID") %>%
  mutate(priority_status = case_when(
      priority == 1 ~ "rainforest, on list",        
      priority == 0 ~ "rainforest, not on list",         
      is.na(priority) ~ "not rainforest"))

merged_municipios <- merged_municipios %>%
  mutate(gn_status = case_when(
    GN == 1 ~ "priority, treated",
    GN == 2 ~ "spillover",
    GN == 0 ~ "control",
    is.na(GN) ~ "not rainforest"))

amazon_biome <- biomes %>% filter(code_biome == 1)

rm(municipios, map_data, analysis_df)

sf_use_s2(FALSE)  
conservation_amazon <- st_intersection(conservation_units, amazon_biome)
sf_use_s2(TRUE)

# palettes and labels
status_palette <- c("rainforest, on list" = "#FF8B00", "rainforest, not on list" = "#006027",  "not rainforest" = "#DCDEDD")
gn_palette <- c("control" = "#006027","priority, treated" = "#FF8B00", "spillover" = "#0073C2", "not rainforest" = "#DCDEDD")

amazon_label <- "Amazon biome"

# =========================================================
# Plot 1: Bio-status of municipalities
# =========================================================

# map without protected boundaries
map_status <- ggplot() + geom_sf(data = merged_municipios, aes(fill = priority_status), colour = NA) +
  geom_sf(data = amazon_biome, aes(colour = amazon_label), fill = NA,linewidth = 0.4, show.legend = TRUE) +
  scale_fill_manual(values = status_palette, name = NULL) +
  scale_colour_manual(values = c("Amazon biome" = "black"),name = NULL) +
  guides(fill = guide_legend(order = 1),colour = guide_legend(order = 2,override.aes = list(fill = NA, linewidth = 0.8))) +
  labs(title = "(a) Priority List") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))

# map with protected boundaries
map_protected <- ggplot() +
  geom_sf(data = merged_municipios, aes(fill = priority_status), colour = NA) +
  ggpattern::geom_sf_pattern(data = conservation_amazon, fill = NA, colour = "black", pattern = "stripe", pattern_colour = "black",
                              pattern_fill = "black", pattern_alpha = 1, pattern_size = 0.05, pattern_spacing = 0.02, pattern_density = 0.35,
                               pattern_angle = 45, show.legend = FALSE) +
  geom_sf(data = amazon_biome, aes(colour = amazon_label), fill = NA, linewidth = 0.4, show.legend = TRUE) +
  scale_fill_manual(values = status_palette, name = NULL) +
  scale_colour_manual(values = c("Amazon biome" = "black"), name = NULL) +
  guides(fill = guide_legend(order = 1), colour = guide_legend(order = 2,override.aes = list(fill = NA, linewidth = 0.8))) +
  labs(title = "(b) Priority List and Protected area") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))

# saving
final_map <-  wrap_plots(map_status, map_protected, ncol = 2, guides = "collect") 
final_map

if (!dir.exists("output")) {dir.create("output")}

ggsave(filename = "output/final_map.png", plot = final_map, width = 12, height = 7, units = "in", dpi = 300)
ggsave(filename = "output/final_map.pdf", plot = final_map, width = 12, height = 7)


# =========================================================
# Plot 2: Protection-status of municipalities (GN Variable)
# =========================================================

# map without protected boundaries
map_gn_status <- ggplot() + geom_sf(data = merged_municipios, aes(fill = gn_status), colour = NA) +
  geom_sf(data = amazon_biome, aes(colour = amazon_label), fill = NA, linewidth = 0.4, show.legend = TRUE) +
  scale_fill_manual(values = gn_palette,name = NULL) +
  scale_colour_manual(values = c("Amazon biome" = "black"), name = NULL) +
  guides(fill = guide_legend(order = 1), colour = guide_legend(order = 2, override.aes = list(fill = NA, linewidth = 0.8))) +
  labs(title = "(c) Treatment and Spillover") + theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))

# map with protected boundaries
map_gn_protected <- ggplot() +
  geom_sf(data = merged_municipios, aes(fill = gn_status), colour = NA) +
  ggpattern::geom_sf_pattern(data = conservation_amazon, fill = NA, colour = "black", pattern = "stripe", pattern_colour = "black",
                              pattern_fill = "black", pattern_alpha = 1, pattern_size = 0.05, pattern_spacing = 0.02, pattern_density = 0.35,
                              pattern_angle = 45, show.legend = FALSE) +
  geom_sf(data = amazon_biome, aes(colour = amazon_label), fill = NA, linewidth = 0.4,show.legend = TRUE) +
  scale_fill_manual(values = gn_palette, name = NULL) +
  scale_colour_manual(values = c("Amazon biome" = "black"), name = NULL) +
  guides(fill = guide_legend(order = 1),colour = guide_legend(order = 2, override.aes = list(fill = NA, linewidth = 0.8))) +
  labs(title = "(d) Treatment, Spillover and Protected Area") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))

# save
final_map_gn <- wrap_plots(map_gn_status, map_gn_protected, ncol = 2, guides = "collect") 
final_map_gn

ggsave(filename = "output/final_map_gn.png", plot = final_map_gn, width = 12, height = 7, units = "in", dpi = 300)
ggsave(filename = "output/final_map_gn.pdf", plot = final_map_gn, width = 12, height = 7)
