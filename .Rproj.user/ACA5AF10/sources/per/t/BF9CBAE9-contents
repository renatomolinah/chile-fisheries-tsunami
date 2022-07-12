################################################################################
# Replication script for:
#"The Lack of Property Rights Can Make Natural Disasters Worse:
# The Case of Small-Scale Fisheries in Chile" at Ecological Economics
# Author: Renato Molina
# Contact: renato.molina@rsmas.miami.edu
################################################################################

###############################################
#### Required packages and functions ##########
###############################################

rm(list=ls(all=TRUE))
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, here, fixest, zoo, broom, readr, cowplot
)

##########################
##### Load Databases #####
##########################

WDB <- readRDS("WDB_benthic.rds")

###############################
##### Capital Regressions #####
###############################

# Metric for wave height

WDB <- WDB %>% mutate(wave = ((wave3)^2),
                      y_boats = (boats),
                      y_cap = (cap),
                      y_power = (power),
                      common = (1-share.turf)
                      )

# Regression for boat flow

reg.boat.1 = feols(y_boats ~ post*wave + funds, WDB)

reg.boat.2 = feols(y_boats ~ post*wave*common + funds | roa, WDB)

reg.boat.3 = feols(y_boats ~ post*wave*common | roa + year^month^comuna, WDB)

# Regression for capacity flow

reg.cap.1 = feols(y_cap ~ post*wave + funds, WDB)

reg.cap.2 = feols(y_cap ~ post*wave*common + funds | roa, WDB)

reg.cap.3 = feols(y_cap ~ post*wave*common | roa + year^month^comuna, WDB) 

# Regression for engine power flow

reg.power.1 = feols(y_power ~ post*wave + funds, WDB)

reg.power.2 = feols(y_power ~ post*wave*common + funds | roa , WDB)

reg.power.3 = feols(y_power ~ post*wave*common | roa + year^month^comuna, WDB)

# Create table

etable(reg.boat.1, reg.boat.2, reg.boat.3,
       reg.cap.1, reg.cap.2, reg.cap.3,
       reg.power.1, reg.power.2, reg.power.3,
       drop = c("Intercept","funds"),
       signifCode = c("***"=0.001, "**"=0.01, "*"=0.05),
       cluster = "comuna")

etable(reg.boat.1, reg.boat.2, reg.boat.3,
       reg.cap.1, reg.cap.2, reg.cap.3,
       reg.power.1, reg.power.2, reg.power.3,
       drop = c("Intercept","funds"),
       signifCode = c("***"=0.001, "**"=0.01, "*"=0.05),
       cluster = "comuna",
       digits = 3,
       tex = TRUE)

#######################
##### Event Study #####
#######################

EWDB <- WDB %>%
  arrange(date) %>%
  group_by(roa) %>%
  mutate(
    roll_boats = rollmean(y_boats, 3, NA),
    roll_cap = rollmean(y_cap, 3, NA),
    roll_power = rollmean(y_power, 3, NA)
  ) %>%
  ungroup() %>%
  mutate(period = mopost,
         treat = common*wave
  )

rt = -12 # Reference month relative to Feb 2010

# Event study for engine power flow

boat_did = feols(roll_boats ~ i(period, treat, rt) | roa + year^month^comuna,
                 cluster = "comuna",
                 EWDB)

# Event study for engine power flow

cap_did = feols(roll_cap ~ i(period, treat, rt)  | roa + year^month^comuna,
                cluster = "comuna",
                EWDB)

# Event study for engine power flow

power_did = feols(roll_power ~ i(period, treat, rt)   | roa + year^month^comuna, 
                  cluster = "comuna",
                  EWDB)

# Visualization

coef_boat <- tidy(boat_did, conf.int = TRUE) %>%
  mutate(term = gsub("period::", "", term)) %>%
  add_row(
    term = "-12", estimate = 0,
    conf.high = 0, conf.low = 0
  ) %>%
  mutate(month =  parse_number(term)
  ) %>%
  arrange(month)

coef_cap <- tidy(cap_did, conf.int = TRUE) %>%
  mutate(term = gsub("period::", "", term)) %>%
  add_row(
    term = "-12", estimate = 0,
    conf.high = 0, conf.low = 0
  ) %>%
  mutate(month =  parse_number(term)
  ) %>%
  arrange(month)

coef_power <- tidy(power_did, conf.int = TRUE) %>%
  mutate(term = gsub("period::", "", term)) %>%
  add_row(
    term = "-12", estimate = 0,
    conf.high = 0, conf.low = 0
  ) %>%
  mutate(month =  parse_number(term)
  ) %>%
  arrange(month)

ev_1 <- coef_boat %>% ggplot(aes(month, estimate)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "black", linetype = "longdash") +
  geom_vline(xintercept = 13, color = "blue", linetype = "longdash") +
  geom_vline(xintercept = 26, color = "blue", linetype = "longdash") +
  geom_vline(xintercept = 39, color = "blue", linetype = "longdash") +
  geom_ribbon(aes(x = month, ymin = conf.low, ymax = conf.high), alpha = 0.9, fill = "grey") +
  geom_line(aes(group = 1)) +
  geom_point(size = 1) +
  labs(
    x = "Month", y = "Estimate and 95% CI",
    title = "a) Boats"
  ) +
  xlab("Month") +
  ylab("Estimate and 95% CI") +
  theme_minimal() +
  theme(
    legend.position = "none",
    title = element_text(size = 14),
    axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10),
    panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(),
    axis.line = element_line(colour = "black"), axis.ticks = element_line()
  )

ev_2 <- coef_cap %>% ggplot(aes(month, estimate)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "black", linetype = "longdash") +
  geom_vline(xintercept = 13, color = "blue", linetype = "longdash") +
  geom_vline(xintercept = 26, color = "blue", linetype = "longdash") +
  geom_vline(xintercept = 39, color = "blue", linetype = "longdash") +
  geom_ribbon(aes(x = month, ymin = conf.low, ymax = conf.high), alpha = 0.9, fill = "grey") +
  geom_line(aes(group = 1)) +
  geom_point(size = 1) +
  labs(
    x = "Month", y = "Estimate and 95% CI",
    title = "b) Capacity"
  ) +
  xlab("Month") +
  ylab("Estimate and 95% CI") +
  theme_minimal() +
  theme(
    legend.position = "none",
    title = element_text(size = 14),
    axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10),
    panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(),
    axis.line = element_line(colour = "black"), axis.ticks = element_line()
  )

ev_3 <- coef_power %>% ggplot(aes(month, estimate)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "black", linetype = "longdash") +
  geom_vline(xintercept = 13, color = "blue", linetype = "longdash") +
  geom_vline(xintercept = 26, color = "blue", linetype = "longdash") +
  geom_vline(xintercept = 39, color = "blue", linetype = "longdash") +
  geom_ribbon(aes(x = month, ymin = conf.low, ymax = conf.high), alpha = 0.9, fill = "grey") +
  geom_line(aes(group = 1)) +
  geom_point(size = 1) +
  labs(
    x = "Month", y = "Estimate and 95% CI",
    title = "c) Engine power"
  ) +
  xlab("Month") +
  ylab("Estimate and 95% CI") +
  theme_minimal() +
  theme(
    legend.position = "none",
    title = element_text(size = 14),
    axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10),
    panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(),
    axis.line = element_line(colour = "black"), axis.ticks = element_line()
  )

plot_grid(ev_1, ev_2, ev_3,
          nrow = 1)

# Save plot

ggsave("event_study_capital.pdf", width = 10, height = 3)