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

##############################
##### Output Regressions #####
##############################

# Metric for wave height

WDB <- WDB %>% mutate(wave = ((wave3)^2),
                      y_harvest = (total.harvest),
                      y_revenue = (total.revenue),
                      common = (1-share.turf)
                      )

# Regression for harvest flow

reg.harv.1 = feols(y_harvest ~ post*wave + funds, WDB)

reg.harv.2 = feols(y_harvest ~ post*wave*common + funds | roa, WDB)

reg.harv.3 = feols(y_harvest ~ post*wave*common | roa + year^month^comuna, WDB)

# Regression for revenue flow

reg.rev.1 = feols(y_revenue ~ post*wave + funds, WDB)

reg.rev.2 = feols(y_revenue ~ post*wave*common + funds | roa, WDB)

reg.rev.3 = feols(y_revenue ~ post*wave*common | roa + year^month^comuna, WDB) 

# Create table

etable(reg.harv.1, reg.harv.2, reg.harv.3,
       reg.rev.1, reg.rev.2, reg.rev.3,
       drop = c("Intercept","funds"),
       signifCode = c("***"=0.001, "**"=0.01, "*"=0.05),
       cluster = "comuna")

etable(reg.harv.1, reg.harv.2, reg.harv.3,
       reg.rev.1, reg.rev.2, reg.rev.3,
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
    roll_harvest = rollmean(y_harvest, 3, NA),
    roll_revenue = rollmean(y_revenue, 3, NA)
  ) %>%
  ungroup() %>%
  mutate(period = mopost,
         treat = common*wave
  )

rt = -12 # Reference month relative to Feb 2010

# Event study for harvest flow

harv_did = feols(roll_harvest ~ i(period, treat, rt) | roa + year^month^comuna,
                 cluster = "comuna",
                 EWDB)

# Event study for revenue flow

rev_did = feols(roll_revenue ~ i(period, treat, rt)  | roa + year^month^comuna,
                cluster = "comuna",
                EWDB)

# Visualization

coef_harv <- tidy(harv_did, conf.int = TRUE) %>%
  mutate(term = gsub("period::", "", term)) %>%
  add_row(
    term = "-12", estimate = 0,
    conf.high = 0, conf.low = 0
  ) %>%
  mutate(month =  parse_number(term)
  ) %>%
  arrange(month)


coef_rev <- tidy(rev_did, conf.int = TRUE) %>%
  mutate(term = gsub("period::", "", term)) %>%
  add_row(
    term = "-12", estimate = 0,
    conf.high = 0, conf.low = 0
  ) %>%
  mutate(month =  parse_number(term)
  ) %>%
  arrange(month)


ev_1 <- coef_harv %>% ggplot(aes(month, estimate)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "black", linetype = "longdash") +
  geom_ribbon(aes(x = month, ymin = conf.low, ymax = conf.high), alpha = 0.5, fill = "grey") +
  geom_line(aes(group = 1)) +
  geom_point(size = 2) +
  labs(
    x = "Month", y = "Estimate and 95% CI",
    title = "Harvest"
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

ev_2 <- coef_rev %>% ggplot(aes(month, estimate)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "black", linetype = "longdash") +
  geom_ribbon(aes(x = month, ymin = conf.low, ymax = conf.high), alpha = 0.5, fill = "grey") +
  geom_line(aes(group = 1)) +
  geom_point(size = 2) +
  labs(
    x = "Month", y = "Estimate and 95% CI",
    title = "Revenue"
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

plot_grid(ev_1, ev_2,
          labels = c('A)', 'B)'), 
          label_size = 12,
          nrow = 1)

# Save plot

ggsave("event_study_output.pdf", width = 10, height = 3)