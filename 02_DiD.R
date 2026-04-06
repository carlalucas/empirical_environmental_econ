# ============================================================
# Part 0: Preparation
# ============================================================

library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(fixest)
library(modelsummary)
library(patchwork)
library(scales)

df <- read_dta("data/analysisdataset.dta")

# same period as the author for better comparison
df <- df %>% filter(year >= 2006 & year <= 2010) 

# treatment group indicator (= 1 if on priority list from 2008)
treated_2008 <- df %>%
  filter(year == 2008, priority == 1) %>%
  pull(muni_ID)

df <- df %>%
  mutate(treated_group = as.integer(muni_ID %in% treated_2008))

# check - should be 36
cat("Treated municipalities (2008 list only):", 
    n_distinct(df$muni_ID[df$treated_group == 1]), "\n")

# post treatment indicator
df <- df %>%
  mutate(
    post         = as.integer(year >= 2009),
    treat_x_post = treated_group * post,
    rel_year     = year - 2008   # relatively to reform year
  )

# check - treatment and post cells
cat("Treatment × Post cell counts:\n")
df %>% count(treated_group, post) %>% print()
# check - should be 36
cat("\nTreated municipalities:", sum(df$treated_group == 1 & df$year == 2008), "\n")

# ============================================================
# Part 1: parallel-trends evidence
# ============================================================

# 1.1. Raw trends plot
trends_raw <- df %>%
  group_by(year, treated_group) %>%
  summarise(
    mean_logodds = mean(logodds, na.rm = TRUE),
    se           = sd(logodds, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  mutate(group = ifelse(treated_group == 1, "Priority (treated)", "Control"))

p_trends <- ggplot(trends_raw, aes(x = year, y = mean_logodds,
                                   colour = group, fill = group)) +
  geom_ribbon(aes(ymin = mean_logodds - 1.96*se,
                  ymax = mean_logodds + 1.96*se), alpha = 0.15, colour = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5) +
  geom_vline(xintercept = 2008.5, linetype = "dashed", colour = "grey40") +
  annotate("text", x = 2008.6, y = Inf, label = "Treatment",
           hjust = 0, vjust = 1.4, size = 3, colour = "grey40") +
  scale_x_continuous(breaks = 2006:2010) +
  scale_colour_manual(values = c("Priority (treated)" = "#d62728",
                                 "Control"            = "#1f77b4")) +
  scale_fill_manual(values   = c("Priority (treated)" = "#d62728",
                                 "Control"            = "#1f77b4")) +
  labs(
    title    = "Average deforestation log-odds: treated vs control municipalities",
    subtitle = "Sample period 2006–2010, shaded = 95% CI.",
    x = "Year", y = "Log-odds of deforested share",
    colour = NULL, fill = NULL
  ) +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom")

print(p_trends)
ggsave("output/trends_raw.png", p_trends, width = 8, height = 5, dpi = 300)

# 1.2. Event-study 
# logodds_it = α_i + λ_t + Σ_k β_k·(treated_group_i × 1[t-2008=k]) + ε

event_mod <- feols(
  logodds ~ i(rel_year, treated_group, ref = 0) | muni_ID + year,
  data    = df,
  cluster = ~muni_ID
)

summary(event_mod)

# event-study plot
png("output/event_study.png", width = 800, height = 500, res = 120)
iplot(event_mod,
      xlab  = "Years relative to treatment (2008 = 0)",
      main  = "Event-study: effect of priority list on log-odds of deforestation",
      col   = "#d62728", pt.pch = 19)
dev.off()

iplot(event_mod,
      xlab  = "Years relative to treatment (2008 = 0)",
      main  = "Event-study: effect of priority list on log-odds of deforestation",
      col   = "#d62728", pt.pch = 19)

# 1.3. pre-trend test
# Restrict to pre-treatment years only and test joint significance of pre-trend interactions (reference = year 2007, rel_year = -1).

pre_df <- df %>% filter(year < 2009)

pre_trend_test <- feols(
  logodds ~ i(rel_year, treated_group, ref = -1) | muni_ID + year,
  data    = pre_df,
  cluster = ~muni_ID
)

summary(pre_trend_test)
# Joint F-test: null = no differential pre-trend between treated & control
cat("\nWald test for pre-trend (should be insignificant):\n")
wald(pre_trend_test, keep = "rel_year")

# ============================================================
# Part 2: TWFE DiD estimations
# ============================================================

# Model 1: Baseline (same period & outcome as paper, no covariates)
did_1 <- feols(
  logodds ~ treat_x_post | muni_ID + year,
  data    = df,
  cluster = ~muni_ID
)

# Model 2: with paper's covariates (comparable to CiC)
did_2 <- feols(
  logodds ~ treat_x_post
  + l_rain + l_rain2 # lagged rainfall + squared
  + l_temperature # lagged temperature
  + pa_share # share of protected areas
  + prices_cattle # lagged beef price
  + prices_agro # lagged crop price index
  + l_PIB # lagged municipality GDP
  + soybeans_rain_high # FAO-GAEZ soy productivity
  + maize_rain_high # FAO-GAEZ maize productivity
  + dist_port # distance to nearest port
  + crop_area2001 # predetermined cropland (2001)
  + n_cattle2001 # predetermined cattle (2001)
  | muni_ID + year, # FE absorb state dummies
  data    = df,
  cluster = ~muni_ID
)

# Model 3: excluding spillover municipalities
# GN > 0 is for untreated municipalities neighbouring a treated one, dropping them purifies the control group
did_3 <- feols(
  logodds ~ treat_x_post
  + l_rain + l_rain2 + l_temperature + pa_share
  + prices_cattle + prices_agro + l_PIB
  + soybeans_rain_high + maize_rain_high
  + dist_port + crop_area2001 + n_cattle2001
  | muni_ID + year,
  data    = df %>% filter(GN == 0 | treated_group == 1),
  cluster = ~muni_ID
)

# Model 4: robustness — log(deforested_flow + 1)
df <- df %>% mutate(log_flow = log(deforested_flow + 1))

did_4 <- feols(
  log_flow ~ treat_x_post
  + l_rain + l_rain2 + l_temperature + pa_share
  + prices_cattle + prices_agro + l_PIB
  + soybeans_rain_high + maize_rain_high
  + dist_port + crop_area2001 + n_cattle2001
  | muni_ID + year,
  data    = df,
  cluster = ~muni_ID
)

# Results

models_did <- list(
  "(1) Baseline\n(log-odds)"         = did_1,
  "(2) With covariates\n(log-odds)"  = did_2,
  "(3) Excl. spillover\n(log-odds)"  = did_3,
  "(4) Robustness\nlog(flow+1)"      = did_4
)

rows_extra <- data.frame(
  term = c("Municipality FE", "Year FE", "Paper's covariates", "Excl. spillover", "Outcome"),
  `(1) Baseline\n(log-odds)`        = c("Yes","Yes","No", "No", "log-odds"),
  `(2) With covariates\n(log-odds)` = c("Yes","Yes","Yes","No", "log-odds"),
  `(3) Excl. spillover\n(log-odds)` = c("Yes","Yes","Yes","Yes","log-odds"),
  `(4) Robustness\nlog(flow+1)`     = c("Yes","Yes","Yes","No", "log(flow+1)"),
  check.names = FALSE
)
attr(rows_extra, "position") <- c(5, 6, 7, 8, 9)

modelsummary(
  models_did,
  stars       = c("*" = .10, "**" = .05, "***" = .01),
  coef_rename = c("treat_x_post" = "Priority × Post 2008"),
  coef_omit   = "l_rain|l_temp|pa_share|prices|l_PIB|soybeans|maize|dist|crop|cattle",
  gof_map     = c("nobs", "r.squared", "adj.r.squared"),
  add_rows    = rows_extra,
  title       = "Table 1: DiD estimates - effect of priority list on deforestation (2006–2010)",
  notes       = "SE clustered at municipality level. Sample: 2006–2010 as in Assuncao et al. (2023). Post = 1 for 2009–2010. Model (2) uses the same covariates as the authors' partialling-out regression.",
  output      = "output/did_results.html"
)

modelsummary(
  models_did,
  stars       = c("*" = .10, "**" = .05, "***" = .01),
  coef_rename = c("treat_x_post" = "Priority × Post 2008"),
  coef_omit   = "l_rain|l_temp|pa_share|prices|l_PIB|soybeans|maize|dist|crop|cattle",
  gof_map     = c("nobs", "r.squared", "adj.r.squared"),
  add_rows    = rows_extra,
  title       = "Table 1: DiD estimates - effect of priority list on deforestation (2006–2010)"
)