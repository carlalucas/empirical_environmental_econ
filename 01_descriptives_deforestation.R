# =========================================================
# Part 0: Preparation
# =========================================================
rm(list=ls())

library(haven)
library(dplyr)
library(ggplot2)
library(sf)
library(geobr)
library(patchwork)
library(ggpattern)
library(grid)
library(scales)

analysis_df <- read_dta("data/analysisdataset.dta")
municipios <- read_sf("data/municipios.gpkg")
biomes <- read_sf("data/amazon_biome.gpkg")
conservation_units <- read_sf("data/conservation_units.gpkg")

municipios <- municipios %>% mutate(muni_ID = as.numeric(code_muni))

# create variables
df_2008 <- analysis_df %>% 
  filter(year == 2008) %>% 
  select(muni_ID, share_08 = share_deforested, flow_08 = deforested_flow, priority)

df_2010 <- analysis_df %>% 
  filter(year == 2010) %>% 
  select(muni_ID, share_10 = share_deforested, flow_10 = deforested_flow)

change_data <- df_2008 %>%
  left_join(df_2010, by = "muni_ID") %>%
  mutate(
    change_share = share_10 - share_08,
    change_flow = flow_10 - flow_08)

merged_change_map <- municipios %>%
  left_join(change_data, by = "muni_ID") %>%
  filter(!is.na(change_share)) 

priority_borders <- merged_change_map %>% 
  filter(priority == 1) %>%
  sf::st_union() 

# =========================================================
# Part 1: summary stats
# =========================================================

summary_table <- analysis_df %>%
  filter(year %in% c(2008, 2010)) %>%
  group_by(priority, year) %>%
  summarise(
    n_municipalities = n(),
    mean_flow = mean(deforested_flow, na.rm = TRUE),
    sd_flow   = sd(deforested_flow, na.rm = TRUE),
    mean_share = mean(share_deforested, na.rm = TRUE),
    sd_share   = sd(share_deforested, na.rm = TRUE),
    .groups = "drop" )

# =========================================================
# Part 2: Flow map
# =========================================================

map_flow <- ggplot() +
  geom_sf(data = merged_change_map, aes(fill = change_flow), colour = "grey50", linewidth = 0.2) +
  geom_sf(data = priority_borders, fill = NA, colour = "black", linewidth = 0.5) +
  
  scale_fill_gradient2(
    low = "#276419", 
    mid = "white", 
    high = "#8E0152", 
    midpoint = 0,
    limits = c(-150, 150), 
    oob = scales::squish,
    name = expression("ΔFlow (km"^2*")"),
    na.value = "grey90") +
  
  labs(title = "(a) Deforestation Flow (km²)") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    legend.position = "bottom",
    legend.key.width = unit(1.5, "cm"))

# =========================================================
# Part 3: Share map
# =========================================================
map_share <- ggplot() +
  geom_sf(data = merged_change_map, aes(fill = change_share), colour = "grey50", linewidth = 0.2) +
  geom_sf(data = priority_borders, fill = NA, colour = "black", linewidth = 0.7) +
  
  scale_fill_gradient2(
    low = "#276419", 
    mid = "white", 
    high = "#8E0152", 
    midpoint = 0,
    limits = c(-0.025, 0.025), 
    oob = scales::squish,
    name = "ΔShare",
    na.value = "grey90") +
  
  labs(title = "(b) Deforestation Share") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    legend.position = "bottom",
    legend.key.width = unit(1.5, "cm"))

# =========================================================
# Part 4: Combine and plot
# =========================================================

combined_change_maps <- map_flow + map_share

combined_change_maps

ggsave(filename = "output/combined_deforestation_change.png", plot = combined_change_maps, width = 14, height = 7, units = "in", dpi = 300)
ggsave(filename = "output/combined_deforestation_change.pdf", plot = combined_change_maps, width = 14, height = 7)