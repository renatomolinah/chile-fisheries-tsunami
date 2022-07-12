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

WDB <- readRDS("WDB_stock.rds") %>%
  na.omit(municipality)

###############################
##### Stock Regressions #####
###############################

# Metric for wave height

WDB <- WDB %>% 
  mutate(wave = (wave3)^2,
         post = ifelse(year >= 2010, 1, 0))

# Regression for biomass

reg.biomass.1 = feols((biomass) ~ post*wave , WDB)

reg.biomass.2 = feols((biomass) ~ post*wave | id_turf + year^municipality, WDB)

reg.biomass.3 = feols((biomass) ~ post*wave | id_turf + year^municipality, 
                      WDB %>% filter(benthic==1))

reg.biomass.4 = feols((biomass) ~ post*wave | id_turf + year^municipality, 
                      WDB %>% filter(benthic == 1, wave3 >= 2 ))

reg.biomass.5 = feols((biomass) ~ post*wave | id_turf + year^municipality, 
                      WDB %>% filter(benthic == 1, wave3 >= 4 ))

reg.biomass.6 = feols((biomass) ~ post*wave | id_turf + year^municipality, 
                      WDB %>% filter(benthic == 1, wave3 >= 6 ))

etable(reg.biomass.1, reg.biomass.2, reg.biomass.3,
       reg.biomass.4, reg.biomass.5, reg.biomass.6,
       drop = c("Intercept","funds"),
       signif.code = c("***"=0.001, "**"=0.01, "*"=0.05),
       cluster = "municipality")

etable(reg.biomass.1, reg.biomass.2, reg.biomass.3,
       reg.biomass.4, reg.biomass.5, reg.biomass.6,
       drop = c("Intercept","funds"),
       signif.code = c("***"=0.001, "**"=0.01, "*"=0.05),
       cluster = "municipality",
       digits = 3,
       tex = TRUE)


# Regression for fishing mortality

reg.mort.1 = feols((fishmortw) ~ post*wave , WDB)

reg.mort.2 = feols((fishmortw) ~ post*wave | id_turf + year^municipality, WDB)

reg.mort.3 = feols((fishmortw) ~ post*wave | id_turf + year^municipality, 
                      WDB %>% filter(benthic==1))

reg.mort.4 = feols((fishmortw) ~ post*wave | id_turf + year^municipality, 
                      WDB %>% filter(benthic == 1, wave3 >= 2 ))

reg.mort.5 = feols((fishmortw) ~ post*wave | id_turf + year^municipality, 
                      WDB %>% filter(benthic == 1, wave3 >= 4 ))

reg.mort.6 = feols((fishmortw) ~ post*wave | id_turf + year^municipality, 
                      WDB %>% filter(benthic == 1, wave3 >= 6 ))

# Create table

etable(reg.mort.1, reg.mort.2, reg.mort.3,
       reg.mort.4, reg.mort.5, reg.mort.6,
       drop = c("Intercept","funds"),
       signif.code = c("***"=0.001, "**"=0.01, "*"=0.05),
       cluster = "municipality")

etable(reg.mort.1, reg.mort.2, reg.mort.3,
       reg.mort.4, reg.mort.5, reg.mort.6,
       drop = c("Intercept","funds"),
       signif.code = c("***"=0.001, "**"=0.01, "*"=0.05),
       cluster = "municipality",
       digits = 3,
       tex = TRUE)



