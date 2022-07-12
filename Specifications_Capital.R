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

# Regressions for the flow of boats

reg.boat.1 = feols(y_boats ~ post*wave*common , WDB)

reg.boat.2 = feols(y_boats ~ post*wave*common | roa , WDB)

reg.boat.3 = feols(y_boats ~ post*wave*common | roa + month, WDB)

reg.boat.4 = feols(y_boats ~ post*wave*common | roa + year, WDB)

reg.boat.5 = feols(y_boats ~ post*wave*common | roa + municipality, WDB)

reg.boat.6 = feols(y_boats ~ post*wave*common | roa + month^year, WDB)

reg.boat.7 = feols(y_boats ~ post*wave*common | roa + month^municipality, WDB)

reg.boat.8 = feols(y_boats ~ post*wave*common | roa + year^municipality, WDB)

reg.boat.9 = feols(y_boats ~ post*wave*common | roa + year^month^municipality, WDB)

# Create tables for boat specifications

etable(reg.boat.1, reg.boat.2, reg.boat.3, reg.boat.4, 
       reg.boat.5, reg.boat.6, reg.boat.7, reg.boat.8, reg.boat.9, 
       drop = c("Intercept","funds"),
       signifCode = c("***"=0.001, "**"=0.01, "*"=0.05),
       cluster = "municipality")

etable(reg.boat.1, reg.boat.2, reg.boat.3, reg.boat.4, 
       reg.boat.5, reg.boat.6, reg.boat.7, reg.boat.8, reg.boat.9, 
       drop = c("Intercept","funds"),
       signifCode = c("***"=0.001, "**"=0.01, "*"=0.05),
       cluster = "municipality",
       digits = 3,
       tex = TRUE)

# Regressions for the flow of loading capacity

reg.cap.1 = feols(y_cap ~ post*wave*common , WDB)

reg.cap.2 = feols(y_cap ~ post*wave*common | roa , WDB)

reg.cap.3 = feols(y_cap ~ post*wave*common | roa + month, WDB)

reg.cap.4 = feols(y_cap ~ post*wave*common | roa + year, WDB)

reg.cap.5 = feols(y_cap ~ post*wave*common | roa + municipality, WDB)

reg.cap.6 = feols(y_cap ~ post*wave*common | roa + month^year, WDB)

reg.cap.7 = feols(y_cap ~ post*wave*common | roa + month^municipality, WDB)

reg.cap.8 = feols(y_cap ~ post*wave*common | roa + year^municipality, WDB)

reg.cap.9 = feols(y_cap ~ post*wave*common | roa + year^month^municipality, WDB)

# Create tables for capacity specifications

etable(reg.cap.1, reg.cap.2, reg.cap.3, reg.cap.4, 
       reg.cap.5, reg.cap.6, reg.cap.7, reg.cap.8, reg.cap.9, 
       drop = c("Intercept","funds"),
       signifCode = c("***"=0.001, "**"=0.01, "*"=0.05),
       cluster = "municipality")

etable(reg.cap.1, reg.cap.2, reg.cap.3, reg.cap.4, 
       reg.cap.5, reg.cap.6, reg.cap.7, reg.cap.8, reg.cap.9, 
       drop = c("Intercept","funds"),
       signifCode = c("***"=0.001, "**"=0.01, "*"=0.05),
       cluster = "municipality",
       digits = 3,
       tex = TRUE)

# Regressions for the flow of engine power

reg.power.1 = feols(y_power ~ post*wave*common , WDB)

reg.power.2 = feols(y_power ~ post*wave*common | roa , WDB)

reg.power.3 = feols(y_power ~ post*wave*common | roa + month, WDB)

reg.power.4 = feols(y_power ~ post*wave*common | roa + year, WDB)

reg.power.5 = feols(y_power ~ post*wave*common | roa + municipality, WDB)

reg.power.6 = feols(y_power ~ post*wave*common | roa + month^year, WDB)

reg.power.7 = feols(y_power ~ post*wave*common | roa + month^municipality, WDB)

reg.power.8 = feols(y_power ~ post*wave*common | roa + year^municipality, WDB)

reg.power.9 = feols(y_power ~ post*wave*common | roa + year^month^municipality, WDB)

# Create tables for engine power specifications

etable(reg.power.1, reg.power.2, reg.power.3, reg.power.4, 
       reg.power.5, reg.power.6, reg.power.7, reg.power.8, reg.power.9, 
       drop = c("Intercept","funds"),
       signifCode = c("***"=0.001, "**"=0.01, "*"=0.05),
       cluster = "municipality")

etable(reg.power.1, reg.power.2, reg.power.3, reg.power.4, 
       reg.power.5, reg.power.6, reg.power.7, reg.power.8, reg.power.9, 
       drop = c("Intercept","funds"),
       signifCode = c("***"=0.001, "**"=0.01, "*"=0.05),
       cluster = "municipality",
       digits = 3,
       tex = TRUE)
