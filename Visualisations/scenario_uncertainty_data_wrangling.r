
# set working directory with model run and uncertainty run datasets exported from ema_workbench
setwd()

library(tidyverse)
library(patchwork)
library(scales)
library(readr)

# ensure local file paths are correct 
LSP1_run <- read_csv("Scenario model runs/LSP_1.csv")
LSP1_unc <- read_csv("scenario uncertainty/LSP1_data.csv")
LSP2_run <- read_csv("Scenario model runs/LSP_2.csv")
LSP2_unc <- read_csv("scenario uncertainty/LSP2_data.csv")
LSP3_run <- read_csv("Scenario model runs/LSP_3.csv")
LSP3_unc <- read_csv("scenario uncertainty/LSP3_data.csv")
LSP4_run <- read_csv("Scenario model runs/LSP_4.csv")
LSP4_unc <- read_csv("scenario uncertainty/LSP4_data.csv")
LSP5_run <- read_csv("Scenario model runs/LSP_5.csv")
LSP5_unc <- read_csv("scenario uncertainty/LSP5_data.csv")
LSP6_run <- read_csv("Scenario model runs/LSP_6.csv")
LSP6_unc <- read_csv("scenario uncertainty/LSP6_data.csv")

# MUST CALL THESE THREE LINES OF CODE BEFORE STARTINGá
Time <- (seq(from = 2000, to = 2050, by = 0.25))
unc_summ <- data.frame(Time)

# data wrangling then write to a csv for easier handling

# rename variables for ggplot to remove spaces - total population
LSP1_run$LSP1runTPop <- LSP1_run$`total population`
LSP1_unc$LSP1uncTPop <- LSP1_unc$`total population`
LSP2_run$LS21runTPop <- LSP2_run$`total population`
LSP2_unc$LSP2uncTPop <- LSP2_unc$`total population`
LSP3_run$LSP3runTPop <- LSP3_run$`total population`
LSP3_unc$LSP3uncTPop <- LSP3_unc$`total population`
LSP4_run$LSP4runTPop <- LSP4_run$`total population`
LSP4_unc$LSP4uncTPop <- LSP4_unc$`total population`
LSP5_run$LSP5runTPop <- LSP5_run$`total population`
LSP5_unc$LSP5uncTPop <- LSP5_unc$`total population`
LSP6_run$LSP6runTPop <- LSP6_run$`total population`
LSP6_unc$LSP6uncTPop <- LSP6_unc$`total population`

# this code summarises min and max values for the model simulations
unc_summ$LSP1minTPop <- tapply(LSP1_unc$LSP1uncTPop, LSP1_unc$TIME, min)
unc_summ$LSP1maxTPop <- tapply(LSP1_unc$LSP1uncTPop, LSP1_unc$TIME, max)
unc_summ$LSP2minTPop <- tapply(LSP2_unc$LSP2uncTPop, LSP2_unc$TIME, min)
unc_summ$LSP2maxTPop <- tapply(LSP2_unc$LSP2uncTPop, LSP2_unc$TIME, max)
unc_summ$LSP3minTPop <- tapply(LSP3_unc$LSP3uncTPop, LSP3_unc$TIME, min)
unc_summ$LSP3maxTPop <- tapply(LSP3_unc$LSP3uncTPop, LSP3_unc$TIME, max)
unc_summ$LSP4minTPop <- tapply(LSP4_unc$LSP4uncTPop, LSP4_unc$TIME, min)
unc_summ$LSP4maxTPop <- tapply(LSP4_unc$LSP4uncTPop, LSP4_unc$TIME, max)
unc_summ$LSP5minTPop <- tapply(LSP5_unc$LSP5uncTPop, LSP5_unc$TIME, min)
unc_summ$LSP5maxTPop <- tapply(LSP5_unc$LSP5uncTPop, LSP5_unc$TIME, max)
unc_summ$LSP6minTPop <- tapply(LSP6_unc$LSP6uncTPop, LSP6_unc$TIME, min)
unc_summ$LSP6maxTPop <- tapply(LSP6_unc$LSP6uncTPop, LSP6_unc$TIME, max)

# rename variables for ggplot to remove spaces - Total Other Productivity
LSP1_run$LSP1runTOP <- LSP1_run$`Total Other Productivity`
LSP1_unc$LSP1uncTOP <- LSP1_unc$`Total Other Productivity`
LSP2_run$LS21runTOP <- LSP2_run$`Total Other Productivity`
LSP2_unc$LSP2uncTOP <- LSP2_unc$`Total Other Productivity`
LSP3_run$LSP3runTOP <- LSP3_run$`Total Other Productivity`
LSP3_unc$LSP3uncTOP <- LSP3_unc$`Total Other Productivity`
LSP4_run$LSP4runTOP <- LSP4_run$`Total Other Productivity`
LSP4_unc$LSP4uncTOP <- LSP4_unc$`Total Other Productivity`
LSP5_run$LSP5runTOP <- LSP5_run$`Total Other Productivity`
LSP5_unc$LSP5uncTOP <- LSP5_unc$`Total Other Productivity`
LSP6_run$LSP6runTOP <- LSP6_run$`Total Other Productivity`
LSP6_unc$LSP6uncTOP <- LSP6_unc$`Total Other Productivity`

# this code summarises min and max values for the model simulations
unc_summ$LSP1minTOP <- tapply(LSP1_unc$LSP1uncTOP, LSP1_unc$TIME, min)
unc_summ$LSP1maxTOP <- tapply(LSP1_unc$LSP1uncTOP, LSP1_unc$TIME, max)
unc_summ$LSP2minTOP <- tapply(LSP2_unc$LSP2uncTOP, LSP2_unc$TIME, min)
unc_summ$LSP2maxTOP <- tapply(LSP2_unc$LSP2uncTOP, LSP2_unc$TIME, max)
unc_summ$LSP3minTOP <- tapply(LSP3_unc$LSP3uncTOP, LSP3_unc$TIME, min)
unc_summ$LSP3maxTOP <- tapply(LSP3_unc$LSP3uncTOP, LSP3_unc$TIME, max)
unc_summ$LSP4minTOP <- tapply(LSP4_unc$LSP4uncTOP, LSP4_unc$TIME, min)
unc_summ$LSP4maxTOP <- tapply(LSP4_unc$LSP4uncTOP, LSP4_unc$TIME, max)
unc_summ$LSP5minTOP <- tapply(LSP5_unc$LSP5uncTOP, LSP5_unc$TIME, min)
unc_summ$LSP5maxTOP <- tapply(LSP5_unc$LSP5uncTOP, LSP5_unc$TIME, max)
unc_summ$LSP6minTOP <- tapply(LSP6_unc$LSP6uncTOP, LSP6_unc$TIME, min)
unc_summ$LSP6maxTOP <- tapply(LSP6_unc$LSP6uncTOP, LSP6_unc$TIME, max)

# rename variables for ggplot to remove spaces - Housing Land
LSP1_run$LSP1runHL <- LSP1_run$`Housing Land`
LSP1_unc$LSP1uncHL <- LSP1_unc$`Housing Land`
LSP2_run$LS21runHL <- LSP2_run$`Housing Land`
LSP2_unc$LSP2uncHL <- LSP2_unc$`Housing Land`
LSP3_run$LSP3runHL <- LSP3_run$`Housing Land`
LSP3_unc$LSP3uncHL <- LSP3_unc$`Housing Land`
LSP4_run$LSP4runHL <- LSP4_run$`Housing Land`
LSP4_unc$LSP4uncHL <- LSP4_unc$`Housing Land`
LSP5_run$LSP5runHL <- LSP5_run$`Housing Land`
LSP5_unc$LSP5uncHL <- LSP5_unc$`Housing Land`
LSP6_run$LSP6runHL <- LSP6_run$`Housing Land`
LSP6_unc$LSP6uncHL <- LSP6_unc$`Housing Land`

# this code summarises min and max values for the model simulations
unc_summ$LSP1minHL <- tapply(LSP1_unc$LSP1uncHL, LSP1_unc$TIME, min)
unc_summ$LSP1maxHL <- tapply(LSP1_unc$LSP1uncHL, LSP1_unc$TIME, max)
unc_summ$LSP2minHL <- tapply(LSP2_unc$LSP2uncHL, LSP2_unc$TIME, min)
unc_summ$LSP2maxHL <- tapply(LSP2_unc$LSP2uncHL, LSP2_unc$TIME, max)
unc_summ$LSP3minHL <- tapply(LSP3_unc$LSP3uncHL, LSP3_unc$TIME, min)
unc_summ$LSP3maxHL <- tapply(LSP3_unc$LSP3uncHL, LSP3_unc$TIME, max)
unc_summ$LSP4minHL <- tapply(LSP4_unc$LSP4uncHL, LSP4_unc$TIME, min)
unc_summ$LSP4maxHL <- tapply(LSP4_unc$LSP4uncHL, LSP4_unc$TIME, max)
unc_summ$LSP5minHL <- tapply(LSP5_unc$LSP5uncHL, LSP5_unc$TIME, min)
unc_summ$LSP5maxHL <- tapply(LSP5_unc$LSP5uncHL, LSP5_unc$TIME, max)
unc_summ$LSP6minHL <- tapply(LSP6_unc$LSP6uncHL, LSP6_unc$TIME, min)
unc_summ$LSP6maxHL <- tapply(LSP6_unc$LSP6uncHL, LSP6_unc$TIME, max)

# rename variables for ggplot to remove spaces - housing demand
LSP1_run$LSP1runHD <- LSP1_run$`housing demand`
LSP1_unc$LSP1uncHD <- LSP1_unc$`housing demand`
LSP2_run$LS21runHD <- LSP2_run$`housing demand`
LSP2_unc$LSP2uncHD <- LSP2_unc$`housing demand`
LSP3_run$LSP3runHD <- LSP3_run$`housing demand`
LSP3_unc$LSP3uncHD <- LSP3_unc$`housing demand`
LSP4_run$LSP4runHD <- LSP4_run$`housing demand`
LSP4_unc$LSP4uncHD <- LSP4_unc$`housing demand`
LSP5_run$LSP5runHD <- LSP5_run$`housing demand`
LSP5_unc$LSP5uncHD <- LSP5_unc$`housing demand`
LSP6_run$LSP6runHD <- LSP6_run$`housing demand`
LSP6_unc$LSP6uncHD <- LSP6_unc$`housing demand`

# this code summarises min and max values for the model simulations
unc_summ$LSP1minHD <- tapply(LSP1_unc$LSP1uncHD, LSP1_unc$TIME, min)
unc_summ$LSP1maxHD <- tapply(LSP1_unc$LSP1uncHD, LSP1_unc$TIME, max)
unc_summ$LSP2minHD <- tapply(LSP2_unc$LSP2uncHD, LSP2_unc$TIME, min)
unc_summ$LSP2maxHD <- tapply(LSP2_unc$LSP2uncHD, LSP2_unc$TIME, max)
unc_summ$LSP3minHD <- tapply(LSP3_unc$LSP3uncHD, LSP3_unc$TIME, min)
unc_summ$LSP3maxHD <- tapply(LSP3_unc$LSP3uncHD, LSP3_unc$TIME, max)
unc_summ$LSP4minHD <- tapply(LSP4_unc$LSP4uncHD, LSP4_unc$TIME, min)
unc_summ$LSP4maxHD <- tapply(LSP4_unc$LSP4uncHD, LSP4_unc$TIME, max)
unc_summ$LSP5minHD <- tapply(LSP5_unc$LSP5uncHD, LSP5_unc$TIME, min)
unc_summ$LSP5maxHD <- tapply(LSP5_unc$LSP5uncHD, LSP5_unc$TIME, max)
unc_summ$LSP6minHD <- tapply(LSP6_unc$LSP6uncHD, LSP6_unc$TIME, min)
unc_summ$LSP6maxHD <- tapply(LSP6_unc$LSP6uncHD, LSP6_unc$TIME, max)

# rename variables for ggplot to remove spaces - Species Richness
LSP1_run$LSP1runSR <- LSP1_run$`Species Richness`
LSP1_unc$LSP1uncSR <- LSP1_unc$`Species Richness`
LSP2_run$LS21runSR <- LSP2_run$`Species Richness`
LSP2_unc$LSP2uncSR <- LSP2_unc$`Species Richness`
LSP3_run$LSP3runSR <- LSP3_run$`Species Richness`
LSP3_unc$LSP3uncSR <- LSP3_unc$`Species Richness`
LSP4_run$LSP4runSR <- LSP4_run$`Species Richness`
LSP4_unc$LSP4uncSR <- LSP4_unc$`Species Richness`
LSP5_run$LSP5runSR <- LSP5_run$`Species Richness`
LSP5_unc$LSP5uncSR <- LSP5_unc$`Species Richness`
LSP6_run$LSP6runSR <- LSP6_run$`Species Richness`
LSP6_unc$LSP6uncSR <- LSP6_unc$`Species Richness`

# this code summarises min and max values for the model simulations
unc_summ$LSP1minSR <- tapply(LSP1_unc$LSP1uncSR, LSP1_unc$TIME, min)
unc_summ$LSP1maxSR <- tapply(LSP1_unc$LSP1uncSR, LSP1_unc$TIME, max)
unc_summ$LSP2minSR <- tapply(LSP2_unc$LSP2uncSR, LSP2_unc$TIME, min)
unc_summ$LSP2maxSR <- tapply(LSP2_unc$LSP2uncSR, LSP2_unc$TIME, max)
unc_summ$LSP3minSR <- tapply(LSP3_unc$LSP3uncSR, LSP3_unc$TIME, min)
unc_summ$LSP3maxSR <- tapply(LSP3_unc$LSP3uncSR, LSP3_unc$TIME, max)
unc_summ$LSP4minSR <- tapply(LSP4_unc$LSP4uncSR, LSP4_unc$TIME, min)
unc_summ$LSP4maxSR <- tapply(LSP4_unc$LSP4uncSR, LSP4_unc$TIME, max)
unc_summ$LSP5minSR <- tapply(LSP5_unc$LSP5uncSR, LSP5_unc$TIME, min)
unc_summ$LSP5maxSR <- tapply(LSP5_unc$LSP5uncSR, LSP5_unc$TIME, max)
unc_summ$LSP6minSR <- tapply(LSP6_unc$LSP6uncSR, LSP6_unc$TIME, min)
unc_summ$LSP6maxSR <- tapply(LSP6_unc$LSP6uncSR, LSP6_unc$TIME, max)

# rename variables for ggplot to remove spaces - inequality indicator
LSP1_run$LSP1runII <- LSP1_run$`inequality indicator`
LSP1_unc$LSP1uncII <- LSP1_unc$`inequality indicator`
LSP2_run$LS21runII <- LSP2_run$`inequality indicator`
LSP2_unc$LSP2uncII <- LSP2_unc$`inequality indicator`
LSP3_run$LSP3runII <- LSP3_run$`inequality indicator`
LSP3_unc$LSP3uncII <- LSP3_unc$`inequality indicator`
LSP4_run$LSP4runII <- LSP4_run$`inequality indicator`
LSP4_unc$LSP4uncII <- LSP4_unc$`inequality indicator`
LSP5_run$LSP5runII <- LSP5_run$`inequality indicator`
LSP5_unc$LSP5uncII <- LSP5_unc$`inequality indicator`
LSP6_run$LSP6runII <- LSP6_run$`inequality indicator`
LSP6_unc$LSP6uncII <- LSP6_unc$`inequality indicator`

# this code summarises min and max values for the model simulations
unc_summ$LSP1minII <- tapply(LSP1_unc$LSP1uncII, LSP1_unc$TIME, min)
unc_summ$LSP1maxII <- tapply(LSP1_unc$LSP1uncII, LSP1_unc$TIME, max)
unc_summ$LSP2minII <- tapply(LSP2_unc$LSP2uncII, LSP2_unc$TIME, min)
unc_summ$LSP2maxII <- tapply(LSP2_unc$LSP2uncII, LSP2_unc$TIME, max)
unc_summ$LSP3minII <- tapply(LSP3_unc$LSP3uncII, LSP3_unc$TIME, min)
unc_summ$LSP3maxII <- tapply(LSP3_unc$LSP3uncII, LSP3_unc$TIME, max)
unc_summ$LSP4minII <- tapply(LSP4_unc$LSP4uncII, LSP4_unc$TIME, min)
unc_summ$LSP4maxII <- tapply(LSP4_unc$LSP4uncII, LSP4_unc$TIME, max)
unc_summ$LSP5minII <- tapply(LSP5_unc$LSP5uncII, LSP5_unc$TIME, min)
unc_summ$LSP5maxII <- tapply(LSP5_unc$LSP5uncII, LSP5_unc$TIME, max)
unc_summ$LSP6minII <- tapply(LSP6_unc$LSP6uncII, LSP6_unc$TIME, min)
unc_summ$LSP6maxII <- tapply(LSP6_unc$LSP6uncII, LSP6_unc$TIME, max)

# rename variables for ggplot to remove spaces - Safer Healthier People
LSP1_run$LSP1runSHP <- LSP1_run$`Safer Healthier People`
LSP1_unc$LSP1uncSHP <- LSP1_unc$`Safer Healthier People`
LSP2_run$LS21runSHP <- LSP2_run$`Safer Healthier People`
LSP2_unc$LSP2uncSHP <- LSP2_unc$`Safer Healthier People`
LSP3_run$LSP3runSHP <- LSP3_run$`Safer Healthier People`
LSP3_unc$LSP3uncSHP <- LSP3_unc$`Safer Healthier People`
LSP4_run$LSP4runSHP <- LSP4_run$`Safer Healthier People`
LSP4_unc$LSP4uncSHP <- LSP4_unc$`Safer Healthier People`
LSP5_run$LSP5runSHP <- LSP5_run$`Safer Healthier People`
LSP5_unc$LSP5uncSHP <- LSP5_unc$`Safer Healthier People`
LSP6_run$LSP6runSHP <- LSP6_run$`Safer Healthier People`
LSP6_unc$LSP6uncSHP <- LSP6_unc$`Safer Healthier People`

# this code summarises min and max values for the model simulations
unc_summ$LSP1minSHP <- tapply(LSP1_unc$LSP1uncSHP, LSP1_unc$TIME, min)
unc_summ$LSP1maxSHP <- tapply(LSP1_unc$LSP1uncSHP, LSP1_unc$TIME, max)
unc_summ$LSP2minSHP <- tapply(LSP2_unc$LSP2uncSHP, LSP2_unc$TIME, min)
unc_summ$LSP2maxSHP <- tapply(LSP2_unc$LSP2uncSHP, LSP2_unc$TIME, max)
unc_summ$LSP3minSHP <- tapply(LSP3_unc$LSP3uncSHP, LSP3_unc$TIME, min)
unc_summ$LSP3maxSHP <- tapply(LSP3_unc$LSP3uncSHP, LSP3_unc$TIME, max)
unc_summ$LSP4minSHP <- tapply(LSP4_unc$LSP4uncSHP, LSP4_unc$TIME, min)
unc_summ$LSP4maxSHP <- tapply(LSP4_unc$LSP4uncSHP, LSP4_unc$TIME, max)
unc_summ$LSP5minSHP <- tapply(LSP5_unc$LSP5uncSHP, LSP5_unc$TIME, min)
unc_summ$LSP5maxSHP <- tapply(LSP5_unc$LSP5uncSHP, LSP5_unc$TIME, max)
unc_summ$LSP6minSHP <- tapply(LSP6_unc$LSP6uncSHP, LSP6_unc$TIME, min)
unc_summ$LSP6maxSHP <- tapply(LSP6_unc$LSP6uncSHP, LSP6_unc$TIME, max)

# rename variables for ggplot to remove spaces - Internet Service Demand
LSP1_run$LSP1runISD <- LSP1_run$`Internet Service Demand`
LSP1_unc$LSP1uncISD <- LSP1_unc$`Internet Service Demand`
LSP2_run$LS21runISD <- LSP2_run$`Internet Service Demand`
LSP2_unc$LSP2uncISD <- LSP2_unc$`Internet Service Demand`
LSP3_run$LSP3runISD <- LSP3_run$`Internet Service Demand`
LSP3_unc$LSP3uncISD <- LSP3_unc$`Internet Service Demand`
LSP4_run$LSP4runISD <- LSP4_run$`Internet Service Demand`
LSP4_unc$LSP4uncISD <- LSP4_unc$`Internet Service Demand`
LSP5_run$LSP5runISD <- LSP5_run$`Internet Service Demand`
LSP5_unc$LSP5uncISD <- LSP5_unc$`Internet Service Demand`
LSP6_run$LSP6runISD <- LSP6_run$`Internet Service Demand`
LSP6_unc$LSP6uncISD <- LSP6_unc$`Internet Service Demand`

# this code summarises min and max values for the model simulations
unc_summ$LSP1minISD <- tapply(LSP1_unc$LSP1uncISD, LSP1_unc$TIME, min)
unc_summ$LSP1maxISD <- tapply(LSP1_unc$LSP1uncISD, LSP1_unc$TIME, max)
unc_summ$LSP2minISD <- tapply(LSP2_unc$LSP2uncISD, LSP2_unc$TIME, min)
unc_summ$LSP2maxISD <- tapply(LSP2_unc$LSP2uncISD, LSP2_unc$TIME, max)
unc_summ$LSP3minISD <- tapply(LSP3_unc$LSP3uncISD, LSP3_unc$TIME, min)
unc_summ$LSP3maxISD <- tapply(LSP3_unc$LSP3uncISD, LSP3_unc$TIME, max)
unc_summ$LSP4minISD <- tapply(LSP4_unc$LSP4uncISD, LSP4_unc$TIME, min)
unc_summ$LSP4maxISD <- tapply(LSP4_unc$LSP4uncISD, LSP4_unc$TIME, max)
unc_summ$LSP5minISD <- tapply(LSP5_unc$LSP5uncISD, LSP5_unc$TIME, min)
unc_summ$LSP5maxISD <- tapply(LSP5_unc$LSP5uncISD, LSP5_unc$TIME, max)
unc_summ$LSP6minISD <- tapply(LSP6_unc$LSP6uncISD, LSP6_unc$TIME, min)
unc_summ$LSP6maxISD <- tapply(LSP6_unc$LSP6uncISD, LSP6_unc$TIME, max)

# rename variables for ggplot to remove spaces - whole population bus frequency discrepancy
LSP1_run$LSP1runBF <- LSP1_run$`whole population bus frequency discrepancy`
LSP1_unc$LSP1uncBF <- LSP1_unc$`whole population bus frequency discrepancy`
LSP2_run$LS21runBF <- LSP2_run$`whole population bus frequency discrepancy`
LSP2_unc$LSP2uncBF <- LSP2_unc$`whole population bus frequency discrepancy`
LSP3_run$LSP3runBF <- LSP3_run$`whole population bus frequency discrepancy`
LSP3_unc$LSP3uncBF <- LSP3_unc$`whole population bus frequency discrepancy`
LSP4_run$LSP4runBF <- LSP4_run$`whole population bus frequency discrepancy`
LSP4_unc$LSP4uncBF <- LSP4_unc$`whole population bus frequency discrepancy`
LSP5_run$LSP5runBF <- LSP5_run$`whole population bus frequency discrepancy`
LSP5_unc$LSP5uncBF <- LSP5_unc$`whole population bus frequency discrepancy`
LSP6_run$LSP6runBF <- LSP6_run$`whole population bus frequency discrepancy`
LSP6_unc$LSP6uncBF <- LSP6_unc$`whole population bus frequency discrepancy`

# this code summarises min and max values for the model simulations
unc_summ$LSP1minBF <- tapply(LSP1_unc$LSP1uncBF, LSP1_unc$TIME, min)
unc_summ$LSP1maxBF <- tapply(LSP1_unc$LSP1uncBF, LSP1_unc$TIME, max)
unc_summ$LSP2minBF <- tapply(LSP2_unc$LSP2uncBF, LSP2_unc$TIME, min)
unc_summ$LSP2maxBF <- tapply(LSP2_unc$LSP2uncBF, LSP2_unc$TIME, max)
unc_summ$LSP3minBF <- tapply(LSP3_unc$LSP3uncBF, LSP3_unc$TIME, min)
unc_summ$LSP3maxBF <- tapply(LSP3_unc$LSP3uncBF, LSP3_unc$TIME, max)
unc_summ$LSP4minBF <- tapply(LSP4_unc$LSP4uncBF, LSP4_unc$TIME, min)
unc_summ$LSP4maxBF <- tapply(LSP4_unc$LSP4uncBF, LSP4_unc$TIME, max)
unc_summ$LSP5minBF <- tapply(LSP5_unc$LSP5uncBF, LSP5_unc$TIME, min)
unc_summ$LSP5maxBF <- tapply(LSP5_unc$LSP5uncBF, LSP5_unc$TIME, max)
unc_summ$LSP6minBF <- tapply(LSP6_unc$LSP6uncBF, LSP6_unc$TIME, min)
unc_summ$LSP6maxBF <- tapply(LSP6_unc$LSP6uncBF, LSP6_unc$TIME, max)

# write dataframe to csv
write.csv(unc_summ, "scenario uncertainty/unc_summ_scenarios.csv")