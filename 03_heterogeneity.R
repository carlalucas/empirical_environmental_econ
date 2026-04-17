# ============================================================
# Part 0: Preparation
# ============================================================

library(haven)
library(dplyr)
library(ggplot2)
library(fixest)
library(modelsummary)
library(patchwork)
library(car)

df <- read_dta("data/analysisdataset.dta")

# same period as the author for better comparison
df <- df %>% filter(year >= 2006 & year <= 2010) 

# treatment group indicator (= 1 if on priority list from 2008)
treated_2008 <- df %>%
  filter(year == 2008, priority == 1) %>%
  pull(muni_ID)

df <- df %>%
  mutate(treated_group = as.integer(muni_ID %in% treated_2008))

# post treatment indicator
df <- df %>%
  mutate(
    post = as.integer(year >= 2009),
    treat_x_post = treated_group * post,
    rel_year = year - 2008   # relatively to reform year
  )

# ============================================================
# Part 1: Surroundedness
# ============================================================

median_wg <- df %>%
  filter(treated_group == 1, year == 2007) %>%
  summarise(med = median(WG_nbr, na.rm = TRUE)) %>%
  pull(med)

cat("Median WG_nbr among treated municipalities (2007):", median_wg, "\n")

# time-invariant surroundedness indicator from 2007 values
surround_indicator <- df %>%
  filter(year == 2007) %>%
  select(muni_ID, WG_nbr) %>%
  mutate(high_surround = as.integer(WG_nbr >= median_wg))

df <- df %>%
  left_join(surround_indicator, by = "muni_ID") %>%
  mutate(
    treat_post_highsurr = treat_x_post * high_surround,
    treat_post_lowsurr  = treat_x_post * (1 - high_surround)
  )

# regression
het_surround <- feols(
  logodds ~ treat_post_lowsurr + treat_post_highsurr + l_rain + l_rain2 + l_temperature + pa_share + prices_cattle + prices_agro + l_PIB | muni_ID + year,
  data    = df,
  cluster = ~muni_ID)

summary(het_surround)

# are the two coefficients statistically different?
cat("\nWald test - high vs low surroundedness (H0: equal effects)\n")
linearHypothesis(het_surround,"treat_post_highsurr - treat_post_lowsurr = 0")

# ============================================================
# Part 2: Distance to port
# ============================================================

median_dist <- df %>%
  filter(year == 2007) %>%
  summarise(med = median(dist_port, na.rm = TRUE)) %>%
  pull(med)

cat("\nMedian distance to nearest port:", round(median_dist), "km\n")

df <- df %>%
  mutate(dist_port_dm = dist_port - mean(dist_port, na.rm = TRUE))

port_indicator <- df %>%
  filter(year == 2007) %>%
  select(muni_ID, close_port = dist_port) %>%
  mutate(close_port = as.integer(close_port <= median_dist))

df <- df %>%
  left_join(port_indicator, by = "muni_ID") %>%
  mutate(
    treat_post_far   = treat_x_post * (1 - close_port),
    treat_post_close = treat_x_post * close_port
  )

# regression on binary split
het_port <- feols(
  logodds ~ treat_post_far + treat_post_close + l_rain + l_rain2 + l_temperature + pa_share + prices_cattle + prices_agro + l_PIB| muni_ID + year,
  data    = df, cluster = ~muni_ID )

summary(het_port)

# are the two coefficients statistically different?
cat("\nWald test - far vs close to port (H0: equal effects)\n")
linearHypothesis(het_port, "treat_post_far - treat_post_close = 0")

# Regression on continuous interaction (robustness)
het_port_cont <- feols(
  logodds ~ treat_x_post + treat_x_post:dist_port_dm + l_rain + l_rain2 + l_temperature + pa_share + prices_cattle + prices_agro + l_PIB| muni_ID + year,
  data    = df,cluster = ~muni_ID)

summary(het_port_cont)

# ============================================================
# Part 3: Economic Incentives (Commodity Prices)
# ============================================================

# demean
df <- df %>%
  mutate(prices_cattle_dm = prices_cattle - mean(prices_cattle, na.rm = TRUE),
    prices_agro_dm   = prices_agro - mean(prices_agro, na.rm = TRUE))

# prices cattle
het_price_cattle <- feols(
  logodds ~ treat_x_post + treat_x_post:prices_cattle_dm + l_rain + l_rain2 + l_temperature + pa_share + prices_agro + l_PIB | muni_ID + year,
  data    = df, cluster = ~muni_ID)

# prices agro
het_price_agro <- feols(logodds ~ treat_x_post + treat_x_post:prices_agro_dm + l_rain + l_rain2 + l_temperature + pa_share + prices_cattle + l_PIB| muni_ID + year,
  data    = df, cluster = ~muni_ID)

summary(het_price_cattle)
summary(het_price_agro)

# ============================================================
# Table
# ============================================================

models_ext <- list(
  "(1) Surroundedness"= het_surround,
  "(2) Port (Binary)"= het_port,
  "(3) Port (Cont.)" = het_port_cont,
  "(4) Beef Price"= het_price_cattle,
  "(5) Crop Price" = het_price_agro
)

coef_labels_ext <- c("treat_post_lowsurr" = "Priority×Post (low surroundedness)",
  "treat_post_highsurr" = "Priority×Post (high surroundedness)",
  "treat_post_far" = "Priority×Post (far from port)",
  "treat_post_close"= "Priority×Post (close to port)",
  "treat_x_post"= "Priority×Post (at average distance/price)",
  "treat_x_post:dist_port_dm" = "Priority×Post × Distance to port",
  "treat_x_post:prices_cattle_dm" = "Priority×Post × Beef Price",
  "treat_x_post:prices_agro_dm"= "Priority×Post × Crop Price")

# table complète
modelsummary(
  models_ext,
  stars    = c("*" = .10, "**" = .05, "***" = .01),
  coef_map = coef_labels_ext,
  gof_map  = c("nobs", "adj.r.squared"),
  title    = "Table 2: Heterogeneous effects",
  output   = "output/het_results.html")

# ============================================================
# Figures
# ============================================================

# Surroundedness
coef_surr <- data.frame(
  group = factor(c("Low surroundedness\n(few treated neighbours)", "High surroundedness\n(many treated neighbours)"),
  levels = c("High surroundedness\n(many treated neighbours)", "Low surroundedness\n(few treated neighbours)")),
  estimate = c(coef(het_surround)["treat_post_lowsurr"], coef(het_surround)["treat_post_highsurr"]),
  se = c(sqrt(vcov(het_surround)["treat_post_lowsurr","treat_post_lowsurr"]),
  sqrt(vcov(het_surround)["treat_post_highsurr","treat_post_highsurr"]))
) %>%
  mutate(lo = estimate - 1.96*se, hi = estimate + 1.96*se)

p_surround <- ggplot(coef_surr, aes(x = group, y = estimate, ymin = lo, ymax = hi)) + geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbar(width = 0.10, linewidth = 1.0, colour = "#d62728") + geom_point(size = 5, colour = "#d62728") + coord_flip() +
  labs(title    = "Heterogeneity by surroundedness", subtitle = "Split at median number of treated neighbours (2007)",
    x = NULL, y = "DiD coefficient on log-odds (95% CI)") + theme_bw(base_size = 13)

# Port distance
coef_port <- data.frame(
  group = factor(c("Far from port\n(> 743 km)", "Close to port\n(≤ 743 km)"), levels = c("Close to port\n(≤ 743 km)", "Far from port\n(> 743 km)")),
  estimate = c(coef(het_port)["treat_post_far"], coef(het_port)["treat_post_close"]),
  se= c(sqrt(vcov(het_port)["treat_post_far","treat_post_far"]), sqrt(vcov(het_port)["treat_post_close","treat_post_close"]))) %>%
  mutate(lo = estimate - 1.96*se, hi = estimate + 1.96*se)

p_port <- ggplot(coef_port, aes(x = group, y = estimate, ymin = lo, ymax = hi)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") + geom_errorbar(width = 0.10, linewidth = 1.0, colour = "#1f77b4") +
  geom_point(size = 5, colour = "#1f77b4") + coord_flip() +
  labs(title    = "Heterogeneity by distance to nearest port", subtitle = "Split at median distance (~743 km)", x = NULL, y = "DiD coefficient on log-odds (95% CI)") +
  theme_bw(base_size = 13)

# Combined figure
p_combined <- p_surround / p_port +
  plot_annotation(title   = "Heterogeneous treatment effects of the priority list", caption = "TWFE DiD with municipality + year FE and paper covariates (2006–2010).\n95% CI based on SE clustered at municipality level.")

print(p_surround)
print(p_port)
print(p_combined)

ggsave("output/het_surround.png", p_surround, width = 8, height = 4, dpi = 300)
ggsave("output/het_port.png",     p_port,     width = 8, height = 4, dpi = 300)
ggsave("output/het_combined.png", p_combined, width = 9, height = 8, dpi = 300)
