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

SWDB <- readRDS("WDB_benthic.rds") %>%
  group_by(year, turf) %>%
  summarise(boats = mean(boats),
            cap = mean(cap),
            power = mean(power),
            total.harvest = mean(total.harvest),
            total.revenue = mean(total.revenue))

# Create groupings

SWDB$Regime <- 0

SWDB$Regime[SWDB$turf==0]<-"H-COMMON"
SWDB$Regime[SWDB$turf==1]<-"H-TURF"
SWDB$Regime<- as.factor(SWDB$Regime)

####################################
##### Average investment plots #####
####################################


cf_1 <- SWDB %>%
  ggplot(aes(x = year, y = boats, group = Regime, linetype = Regime)) +
  geom_line() +
  theme_minimal() +
  geom_vline(xintercept = 2010, color = "black", linetype = "longdash") +
  geom_point(size = 2) +
  scale_x_discrete(
    limit = c(2003:2013)
  ) +
  labs(
    x = "Year", y = "Monthly flow of boats (#)",
    title = "a) Boats"
  ) +
  theme_minimal() +
  theme(
    title = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10), axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10),
    panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(),
    axis.line = element_line(colour = "black"), axis.ticks = element_line(),
    legend.direction="horizontal"
  ) +
  guides(linetype=guide_legend(title="Regime:"))

legend <- cowplot::get_legend(cf_1)

cf_1 <- cf_1 +
  theme(legend.position = "none")

cf_2 <- SWDB %>%
  ggplot(aes(x = year, y = cap, group = Regime, linetype = Regime)) +
  geom_line() +
  theme_minimal() +
  geom_vline(xintercept = 2010, color = "black", linetype = "longdash") +
  geom_point(size = 2) +
  scale_x_discrete(
    limit = c(2003:2013)
  ) +
  labs(
    x = "Year", y = "Monthly flow of loading capacity (tons)",
    title = "b) Capacity"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    title = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10), axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10),
    panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(),
    axis.line = element_line(colour = "black"), axis.ticks = element_line()
  )

cf_3 <- SWDB %>%
  ggplot(aes(x = year, y = power, group = Regime, linetype = Regime)) +
  geom_line() +
  theme_minimal() +
  geom_vline(xintercept = 2010, color = "black", linetype = "longdash") +
  geom_point(size = 2) +
  scale_x_discrete(
    limit = c(2003:2013)
  ) +
  labs(
    x = "Year", y = "Monthly flow of engine power (hp)",
    title = "c) Engine power"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    title = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10), axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10),
    panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(),
    axis.line = element_line(colour = "black"), axis.ticks = element_line()
  )

################################
##### Average output plots #####
################################

of_1 <- SWDB %>%
  ggplot(aes(x = year, y = total.harvest, group = Regime, linetype = Regime)) +
  geom_line() +
  theme_minimal() +
  geom_vline(xintercept = 2010, color = "black", linetype = "longdash") +
  geom_point(size = 2) +
  scale_x_discrete(
    limit = c(2003:2013)
  ) +
  labs(
    x = "Year", y = "Monthly flow of harvest (tons)",
    title = "d) Harvest"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    title = element_text(size = 14),
    #text = element_text(family = "Lato"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10), axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10),
    panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(),
    axis.line = element_line(colour = "black"), axis.ticks = element_line()
  )


of_2 <- SWDB %>%
  ggplot(aes(x = year, y = total.revenue, group = Regime, linetype = Regime)) +
  geom_line() +
  theme_minimal() +
  geom_vline(xintercept = 2010, color = "black", linetype = "longdash") +
  geom_point(size = 2) +
  scale_x_discrete(
    limit = c(2003:2013)
  ) +
  labs(
    x = "Year", y = "Monthly flow of revenue (TUSD)",
    title = "e) Revenue"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    title = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10), axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10),
    panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(),
    axis.line = element_line(colour = "black"), axis.ticks = element_line()
  )

######################################
##### Distribution of commonness #####
######################################

WDB <- readRDS("WDB_benthic.rds") %>%
  group_by(roa) %>%
  summarise(turfiness = mean(share.turf))

tf <-WDB %>%
  ggplot(aes(x = turfiness)) +
  geom_histogram()+
  theme_minimal() +
  labs(
    x = "Share from common regime", title = "f) Commonness",
    y = "Organization count (#)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    title = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10), axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10),
    panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(),
    axis.line = element_line(colour = "black"), axis.ticks = element_line()
  )

#####################################
##### Create grid with plots ########
#####################################

grid <- plot_grid(cf_1, cf_2, cf_3, of_1, of_2,  tf, ncol = 3)

plot_grid(grid, legend, ncol = 1, rel_heights = c(3, .2))

# Save plot

ggsave(file="data_plot.pdf", height  = 7, width = 10)