#set working directory
setwd()

library(tidyverse)
library(patchwork)
library(scales)
library(readr)

#colours
LSP1col <- "#77AADD"
LSP2col <- "#EE8866"
LSP3col <- "#99DDFF"
LSP4col <- "#FFAABB"
LSP5col <- "#44BB99"
LSP6col <- "#AAAA00"

# read in dataframes (set correct local file path)
data <- read_csv("scenario uncertainty/unc_summ_scenarios.csv") #created in data_wrangling r script
LSP1 <- read_csv("LSP1_rundata.csv")
LSP2 <- read_csv("LSP2_rundata.csv")
LSP3 <- read_csv("LSP3_rundata.csv")
LSP4 <- read_csv("LSP4_rundata.csv")
LSP5 <- read_csv("LSP5_rundata.csv")
LSP6 <- read_csv("LSP6_rundata.csv")

# total population variable

# rename variables for ggplot to remove spaces 

LSP1$totpop <- LSP1$`total population`
LSP2$totpop <- LSP2$`total population`
LSP3$totpop <- LSP3$`total population`
LSP4$totpop <- LSP4$`total population`
LSP5$totpop <- LSP5$`total population`
LSP6$totpop <- LSP6$`total population`

TP <- ggplot() +
   geom_ribbon(data = data, aes(x = Time, ymin = LSP1minTPop, ymax = LSP1maxTPop), fill = LSP1col, alpha = 0.2) +
   geom_ribbon(data = data, aes(x = Time, ymin = LSP2minTPop, ymax = LSP2maxTPop), fill = LSP2col, alpha = 0.2) +
   geom_ribbon(data = data, aes(x = Time, ymin = LSP4minTPop, ymax = LSP4maxTPop), fill = LSP3col, alpha = 0.2) +
   geom_ribbon(data = data, aes(x = Time, ymin = LSP3minTPop, ymax = LSP3maxTPop), fill = LSP4col, alpha = 0.2) +
   geom_ribbon(data = data, aes(x = Time, ymin = LSP6minTPop, ymax = LSP6maxTPop), fill = LSP5col, alpha = 0.2) +
   geom_ribbon(data = data, aes(x = Time, ymin = LSP5minTPop, ymax = LSP5maxTPop), fill = LSP6col, alpha = 0.2) +
   geom_line(data = LSP1, aes(x = Time, y = totpop, color = "LSP1"), size = 0.75) +
   geom_line(data = LSP2, aes(x = Time, y = totpop, color = "LSP2"), size = 0.75) +
   geom_line(data = LSP3, aes(x = Time, y = totpop, color = "LSP3"), size = 0.75) +
   geom_line(data = LSP4, aes(x = Time, y = totpop, color = "LSP4"), size = 0.75) +
   geom_line(data = LSP5, aes(x = Time, y = totpop, color = "LSP5"), size = 0.75) +
   geom_line(data = LSP6, aes(x = Time, y = totpop, color = "LSP6"), size = 0.75) +
   annotate("segment", x = 2051, xend = 2051, y = 364.43488, yend = 4805.4070, colour = LSP1col, size = 1) +
   annotate("segment", x = 2052, xend = 2052, y = 370.29797, yend = 4835.6055, colour = LSP2col, size = 1) +
   annotate("segment", x = 2053, xend = 2053, y = 217.03859, yend = 1099.6418, colour = LSP3col, size = 1) +
   annotate("segment", x = 2054, xend = 2054, y = 259.95224, yend = 3082.8772, colour = LSP4col, size = 1) +
   annotate("segment", x = 2055, xend = 2055, y = 193.65126, yend = 404.2101, colour = LSP5col, size = 1) +
   annotate("segment", x = 2056, xend = 2056, y = 237.04845, yend = 711.2303, colour = LSP6col, size = 1) +
   scale_color_manual(name = "Scenario", values = c("LSP1" = LSP1col, "LSP2" = LSP2col, "LSP3" = LSP3col,
                                                    "LSP4" = LSP4col, "LSP5" = LSP5col, "LSP6" = LSP6col)) +
   theme_bw() + ylim(0, 5000) +
   theme(plot.title = element_text(size=14), axis.title.x=element_blank(), axis.text.x=element_blank()) +
   ylab("People") + xlab("Year") + ggtitle("Total Population")
TP

# Total Other Productivity variable

LSP1$TOP <- LSP1$`Total Other Productivity`
LSP2$TOP <- LSP2$`Total Other Productivity`
LSP3$TOP <- LSP3$`Total Other Productivity`
LSP4$TOP <- LSP4$`Total Other Productivity`
LSP5$TOP <- LSP5$`Total Other Productivity`
LSP6$TOP <- LSP6$`Total Other Productivity`

TOP <- ggplot() +
   geom_ribbon(data = data, aes(x = Time, ymin = LSP1minTOP, ymax = LSP1maxTOP), fill = LSP1col, alpha = 0.2) +
   geom_ribbon(data = data, aes(x = Time, ymin = LSP2minTOP, ymax = LSP2maxTOP), fill = LSP2col, alpha = 0.2) +
   geom_ribbon(data = data, aes(x = Time, ymin = LSP4minTOP, ymax = LSP4maxTOP), fill = LSP3col, alpha = 0.2) +
   geom_ribbon(data = data, aes(x = Time, ymin = LSP3minTOP, ymax = LSP3maxTOP), fill = LSP4col, alpha = 0.2) +
   geom_ribbon(data = data, aes(x = Time, ymin = LSP6minTOP, ymax = LSP6maxTOP), fill = LSP5col, alpha = 0.2) +
   geom_ribbon(data = data, aes(x = Time, ymin = LSP5minTOP, ymax = LSP5maxTOP), fill = LSP6col, alpha = 0.2) +
   geom_line(data = LSP1, aes(x = Time, y = TOP, color = "LSP1"), size = 0.75) +
   geom_line(data = LSP2, aes(x = Time, y = TOP, color = "LSP2"), size = 0.75) +
   geom_line(data = LSP3, aes(x = Time, y = TOP, color = "LSP3"), size = 0.75) +
   geom_line(data = LSP4, aes(x = Time, y = TOP, color = "LSP4"), size = 0.75) +
   geom_line(data = LSP5, aes(x = Time, y = TOP, color = "LSP5"), size = 0.75) +
   geom_line(data = LSP6, aes(x = Time, y = TOP, color = "LSP6"), size = 0.75) +
   annotate("segment", x = 2051, xend = 2051, y = 5911532.5, yend = 245095040, colour = LSP1col, size = 1) +
   annotate("segment", x = 2052, xend = 2052, y = 6634802, yend = 246797940, colour = LSP2col, size = 1) +
   annotate("segment", x = 2053, xend = 2053, y = 2561013.8, yend = 43311280, colour = LSP3col, size = 1) +
   annotate("segment", x = 2054, xend = 2054, y = 5531422.5, yend = 183748750, colour = LSP4col, size = 1) +
   annotate("segment", x = 2055, xend = 2055, y = 1397276, yend = 5067848, colour = LSP5col, size = 1) +
   annotate("segment", x = 2056, xend = 2056, y = 1997106.2, yend = 19000594, colour = LSP6col, size = 1) +
   scale_color_manual(name = "Scenario", values = c("LSP1" = LSP1col, "LSP2" = LSP2col, "LSP3" = LSP3col,
                                                    "LSP4" = LSP4col, "LSP5" = LSP5col, "LSP6" = LSP6col)) +
   theme_bw() +
   scale_y_continuous("Dollars (millions)", limits = c(0, 250000000),
                      labels = comma_format(scale = 1/1000000)) +
   theme(plot.title = element_text(size=14), axis.title.x=element_blank(), axis.text.x=element_blank()) +
   ggtitle("Total Productivity\n('Other' Sector)")
TOP

# Housing Land variable

# rename variables for ggplot to remove spaces 

LSP1$HL <- LSP1$`Housing Land`
LSP2$HL <- LSP2$`Housing Land`
LSP3$HL <- LSP3$`Housing Land`
LSP4$HL <- LSP4$`Housing Land`
LSP5$HL <- LSP5$`Housing Land`
LSP6$HL <- LSP6$`Housing Land`

HL <- ggplot() +
   geom_ribbon(data = data, aes(x = Time, ymin = LSP1minHL, ymax = LSP1maxHL), fill = LSP1col, alpha = 0.2) +
   geom_ribbon(data = data, aes(x = Time, ymin = LSP2minHL, ymax = LSP2maxHL), fill = LSP2col, alpha = 0.2) +
   geom_ribbon(data = data, aes(x = Time, ymin = LSP4minHL, ymax = LSP4maxHL), fill = LSP3col, alpha = 0.2) +
   geom_ribbon(data = data, aes(x = Time, ymin = LSP3minHL, ymax = LSP3maxHL), fill = LSP4col, alpha = 0.2) +
   geom_ribbon(data = data, aes(x = Time, ymin = LSP6minHL, ymax = LSP6maxHL), fill = LSP5col, alpha = 0.2) +
   geom_ribbon(data = data, aes(x = Time, ymin = LSP5minHL, ymax = LSP5maxHL), fill = LSP6col, alpha = 0.2) +
   geom_line(data = LSP1, aes(x = Time, y = HL, color = "LSP1"), size = 0.75) +
   geom_line(data = LSP2, aes(x = Time, y = HL, color = "LSP2"), size = 0.75) +
   geom_line(data = LSP3, aes(x = Time, y = HL, color = "LSP3"), size = 0.75) +
   geom_line(data = LSP4, aes(x = Time, y = HL, color = "LSP4"), size = 0.75) +
   geom_line(data = LSP5, aes(x = Time, y = HL, color = "LSP5"), size = 0.75) +
   geom_line(data = LSP6, aes(x = Time, y = HL, color = "LSP6"), size = 0.75) +
   annotate("segment", x = 2051, xend = 2051, y = 507.9014, yend = 1408.6097, colour = LSP1col, size = 1) +
   annotate("segment", x = 2052, xend = 2052, y = 520.44214, yend = 1414.5306, colour = LSP2col, size = 1) +
   annotate("segment", x = 2053, xend = 2053, y = 402.18048, yend = 856.2593, colour = LSP3col, size = 1) +
   annotate("segment", x = 2054, xend = 2054, y = 488.82327, yend = 1227.7811, colour = LSP4col, size = 1) +
   annotate("segment", x = 2055, xend = 2055, y = 477.80878, yend = 915.3853, colour = LSP5col, size = 1) +
   annotate("segment", x = 2056, xend = 2056, y = 446.12772, yend = 886.3395, colour = LSP6col, size = 1) +
   scale_color_manual(name = "Scenario", values = c("LSP1" = LSP1col, "LSP2" = LSP2col, "LSP3" = LSP3col,
                                                    "LSP4" = LSP4col, "LSP5" = LSP5col, "LSP6" = LSP6col)) +
   theme_bw() + ylim(0, 1500) +
   theme(plot.title = element_text(size=14), axis.title.x=element_blank(), axis.text.x=element_blank()) +
   ylab("Hectares") + xlab("Year") + ggtitle("Housing Land")

HL

# housing demand variable

# rename variables for ggplot to remove spaces 

LSP1$HD <- LSP1$`housing demand`
LSP2$HD <- LSP2$`housing demand`
LSP3$HD <- LSP3$`housing demand`
LSP4$HD <- LSP4$`housing demand`
LSP5$HD <- LSP5$`housing demand`
LSP6$HD <- LSP6$`housing demand`

HD <- ggplot() +
   geom_ribbon(data = data, aes(x = Time, ymin = LSP1minHD, ymax = LSP1maxHD), fill = LSP1col, alpha = 0.2) +
   geom_ribbon(data = data, aes(x = Time, ymin = LSP2minHD, ymax = LSP2maxHD), fill = LSP2col, alpha = 0.2) +
   geom_ribbon(data = data, aes(x = Time, ymin = LSP4minHD, ymax = LSP4maxHD), fill = LSP3col, alpha = 0.2) +
   geom_ribbon(data = data, aes(x = Time, ymin = LSP3minHD, ymax = LSP3maxHD), fill = LSP4col, alpha = 0.2) +
   geom_ribbon(data = data, aes(x = Time, ymin = LSP6minHD, ymax = LSP6maxHD), fill = LSP5col, alpha = 0.2) +
   geom_ribbon(data = data, aes(x = Time, ymin = LSP5minHD, ymax = LSP5maxHD), fill = LSP6col, alpha = 0.2) +
   geom_line(data = LSP1, aes(x = Time, y = HD, color = "LSP1"), size = 0.75) +
   geom_line(data = LSP2, aes(x = Time, y = HD, color = "LSP2"), size = 0.75) +
   geom_line(data = LSP3, aes(x = Time, y = HD, color = "LSP3"), size = 0.75) +
   geom_line(data = LSP4, aes(x = Time, y = HD, color = "LSP4"), size = 0.75) +
   geom_line(data = LSP5, aes(x = Time, y = HD, color = "LSP5"), size = 0.75) +
   geom_line(data = LSP6, aes(x = Time, y = HD, color = "LSP6"), size = 0.75) +
   annotate("segment", x = 2051, xend = 2051, y = 234.77159, yend = 2511.6475, colour = LSP1col, size = 1) +
   annotate("segment", x = 2052, xend = 2052, y = 234.76305, yend = 2526.4353, colour = LSP2col, size = 1) +
   annotate("segment", x = 2053, xend = 2053, y = 154.77988, yend = 661.72925, colour = LSP3col, size = 1) +
   annotate("segment", x = 2054, xend = 2054, y = 185.90303, yend = 1654.2664, colour = LSP4col, size = 1) +
   annotate("segment", x = 2055, xend = 2055, y = 140.69211, yend = 312.55368, colour = LSP5col, size = 1) +
   annotate("segment", x = 2056, xend = 2056, y = 161.72403, yend = 468.87167, colour = LSP6col, size = 1) +
   scale_color_manual(name = "Scenario", values = c("LSP1" = LSP1col, "LSP2" = LSP2col, "LSP3" = LSP3col,
                                                    "LSP4" = LSP4col, "LSP5" = LSP5col, "LSP6" = LSP6col)) +
   theme_bw() + ylim(0, 2600) +
   theme(plot.title = element_text(size=14), axis.title.x=element_blank(), axis.text.x=element_blank()) +
   ylab("Houses") + xlab("Year") + ggtitle("Housing Demand")

HD

# Species Richness variable

# rename variables for ggplot to remove spaces 

LSP1$SR <- LSP1$`Species Richness`
LSP2$SR <- LSP2$`Species Richness`
LSP3$SR <- LSP3$`Species Richness`
LSP4$SR <- LSP4$`Species Richness`
LSP5$SR <- LSP5$`Species Richness`
LSP6$SR <- LSP6$`Species Richness`

SR <- ggplot() +
   geom_ribbon(data = data, aes(x = Time, ymin = LSP1minSR, ymax = LSP1maxSR), fill = LSP1col, alpha = 0.2) +
   geom_ribbon(data = data, aes(x = Time, ymin = LSP2minSR, ymax = LSP2maxSR), fill = LSP2col, alpha = 0.2) +
   geom_ribbon(data = data, aes(x = Time, ymin = LSP4minSR, ymax = LSP4maxSR), fill = LSP3col, alpha = 0.2) +
   geom_ribbon(data = data, aes(x = Time, ymin = LSP3minSR, ymax = LSP3maxSR), fill = LSP4col, alpha = 0.2) +
   geom_ribbon(data = data, aes(x = Time, ymin = LSP6minSR, ymax = LSP6maxSR), fill = LSP5col, alpha = 0.2) +
   geom_ribbon(data = data, aes(x = Time, ymin = LSP5minSR, ymax = LSP5maxSR), fill = LSP6col, alpha = 0.2) +
   geom_line(data = LSP1, aes(x = Time, y = SR, color = "LSP1"), size = 0.75) +
   geom_line(data = LSP2, aes(x = Time, y = SR, color = "LSP2"), size = 0.75) +
   geom_line(data = LSP3, aes(x = Time, y = SR, color = "LSP3"), size = 0.75) +
   geom_line(data = LSP4, aes(x = Time, y = SR, color = "LSP4"), size = 0.75) +
   geom_line(data = LSP5, aes(x = Time, y = SR, color = "LSP5"), size = 0.75) +
   geom_line(data = LSP6, aes(x = Time, y = SR, color = "LSP6"), size = 0.75) +
   annotate("segment", x = 2051, xend = 2051, y = 5340.569, yend = 29746.81, colour = LSP1col, size = 1) +
   annotate("segment", x = 2052, xend = 2052, y = 4699.471, yend = 2264.2942, colour = LSP2col, size = 1) +
   annotate("segment", x = 2053, xend = 2053, y = 5196.3574, yend = 28973.512, colour = LSP3col, size = 1) +
   annotate("segment", x = 2054, xend = 2054, y = 5309.8823, yend = 29571.479, colour = LSP4col, size = 1) +
   annotate("segment", x = 2055, xend = 2055, y = 2265.912, yend = 4704.014, colour = LSP5col, size = 1) +
   annotate("segment", x = 2056, xend = 2056, y = 3175.349, yend = 9130.855, colour = LSP6col, size = 1) +
   scale_color_manual(name = "Scenario", values = c("LSP1" = LSP1col, "LSP2" = LSP2col, "LSP3" = LSP3col,
                                                    "LSP4" = LSP4col, "LSP5" = LSP5col, "LSP6" = LSP6col)) +
   theme_bw() + ylim(0, 30000) +
   theme(plot.title = element_text(size=14), axis.title.x=element_blank(), axis.text.x=element_blank()) +
   ylab("Species") + xlab("Year") + ggtitle("Species Richness")

SR

# inequality indicator variable

# rename variables for ggplot to remove spaces 

LSP1$II <- LSP1$`inequality indicator`
LSP2$II <- LSP2$`inequality indicator`
LSP3$II <- LSP3$`inequality indicator`
LSP4$II <- LSP4$`inequality indicator`
LSP5$II <- LSP5$`inequality indicator`
LSP6$II <- LSP6$`inequality indicator`

II <- ggplot() +
   geom_ribbon(data = data, aes(x = Time, ymin = LSP1minII, ymax = LSP1maxII), fill = LSP1col, alpha = 0.2) +
   geom_ribbon(data = data, aes(x = Time, ymin = LSP2minII, ymax = LSP2maxII), fill = LSP2col, alpha = 0.2) +
   geom_ribbon(data = data, aes(x = Time, ymin = LSP4minII, ymax = LSP4maxII), fill = LSP3col, alpha = 0.2) +
   geom_ribbon(data = data, aes(x = Time, ymin = LSP3minII, ymax = LSP3maxII), fill = LSP4col, alpha = 0.2) +
   geom_ribbon(data = data, aes(x = Time, ymin = LSP6minII, ymax = LSP6maxII), fill = LSP5col, alpha = 0.2) +
   geom_ribbon(data = data, aes(x = Time, ymin = LSP5minII, ymax = LSP5maxII), fill = LSP6col, alpha = 0.2) +
   geom_line(data = LSP1, aes(x = Time, y = II, color = "LSP1"), size = 0.75) +
   geom_line(data = LSP2, aes(x = Time, y = II, color = "LSP2"), size = 0.75) +
   geom_line(data = LSP3, aes(x = Time, y = II, color = "LSP3"), size = 0.75) +
   geom_line(data = LSP4, aes(x = Time, y = II, color = "LSP4"), size = 0.75) +
   geom_line(data = LSP5, aes(x = Time, y = II, color = "LSP5"), size = 0.75) +
   geom_line(data = LSP6, aes(x = Time, y = II, color = "LSP6"), size = 0.75) +
   annotate("segment", x = 2051, xend = 2051, y = 0.13553636, yend = 0.6912276, colour = LSP1col, size = 1) +
   annotate("segment", x = 2052, xend = 2052, y = 0.14488468, yend = 0.6843423, colour = LSP2col, size = 1) +
   annotate("segment", x = 2053, xend = 2053, y = 0.10634294, yend = 0.31799844, colour = LSP3col, size = 1) +
   annotate("segment", x = 2054, xend = 2054, y = 0.20908205, yend = 0.66617924, colour = LSP4col, size = 1) +
   annotate("segment", x = 2055, xend = 2055, y = 0.23677205, yend = 0.3053826, colour = LSP5col, size = 1) +
   annotate("segment", x = 2056, xend = 2056, y = 0.21835794, yend = 0.31329638, colour = LSP6col, size = 1) +
   scale_color_manual(name = "Scenario", values = c("LSP1" = LSP1col, "LSP2" = LSP2col, "LSP3" = LSP3col,
                                                    "LSP4" = LSP4col, "LSP5" = LSP5col, "LSP6" = LSP6col)) +
   theme_bw() + ylim(0, 0.70) +
   theme(plot.title = element_text(size=14), axis.title.x=element_blank(), axis.text.x=element_blank()) +
   ylab("Dimensionless") + xlab("Year") + ggtitle("Inequality Indicator")

II

# Safer Healthier people variable

# rename variables for ggplot to remove spaces 

LSP1$SHP <- LSP1$`Safer Healthier People`
LSP2$SHP <- LSP2$`Safer Healthier People`
LSP3$SHP <- LSP3$`Safer Healthier People`
LSP4$SHP <- LSP4$`Safer Healthier People`
LSP5$SHP <- LSP5$`Safer Healthier People`
LSP6$SHP <- LSP6$`Safer Healthier People`

SHP <- ggplot() +
   geom_ribbon(data = data, aes(x = Time, ymin = LSP1minSHP, ymax = LSP1maxSHP), fill = LSP1col, alpha = 0.2) +
   geom_ribbon(data = data, aes(x = Time, ymin = LSP2minSHP, ymax = LSP2maxSHP), fill = LSP2col, alpha = 0.2) +
   geom_ribbon(data = data, aes(x = Time, ymin = LSP4minSHP, ymax = LSP4maxSHP), fill = LSP3col, alpha = 0.2) +
   geom_ribbon(data = data, aes(x = Time, ymin = LSP3minSHP, ymax = LSP3maxSHP), fill = LSP4col, alpha = 0.2) +
   geom_ribbon(data = data, aes(x = Time, ymin = LSP6minSHP, ymax = LSP6maxSHP), fill = LSP5col, alpha = 0.2) +
   geom_ribbon(data = data, aes(x = Time, ymin = LSP5minSHP, ymax = LSP5maxSHP), fill = LSP6col, alpha = 0.2) +
   geom_line(data = LSP1, aes(x = Time, y = SHP, color = "LSP1"), size = 0.75) +
   geom_line(data = LSP2, aes(x = Time, y = SHP, color = "LSP2"), size = 0.75) +
   geom_line(data = LSP3, aes(x = Time, y = SHP, color = "LSP3"), size = 0.75) +
   geom_line(data = LSP4, aes(x = Time, y = SHP, color = "LSP4"), size = 0.75) +
   geom_line(data = LSP5, aes(x = Time, y = SHP, color = "LSP5"), size = 0.75) +
   geom_line(data = LSP6, aes(x = Time, y = SHP, color = "LSP6"), size = 0.75) +
   annotate("segment", x = 2051, xend = 2051, y = 136.19905, yend = 177.618, colour = LSP1col, size = 1) +
   annotate("segment", x = 2052, xend = 2052, y = 135.7602, yend = 169.20581, colour = LSP2col, size = 1) +
   annotate("segment", x = 2053, xend = 2053, y = 137.89825, yend = 168.67317, colour = LSP3col, size = 1) +
   annotate("segment", x = 2054, xend = 2054, y = 44.387398, yend = 161.08334, colour = LSP4col, size = 1) +
   annotate("segment", x = 2055, xend = 2055, y = 64.4056, yend = 157.65549, colour = LSP5col, size = 1) +
   annotate("segment", x = 2056, xend = 2056, y = 109.48884, yend = 163.84985, colour = LSP6col, size = 1) +
   scale_color_manual(name = "Scenario", values = c("LSP1" = LSP1col, "LSP2" = LSP2col, "LSP3" = LSP3col,
                                                    "LSP4" = LSP4col, "LSP5" = LSP5col, "LSP6" = LSP6col)) +
   theme_bw() + ylim(0, 180) +
   #   theme(plot.title = element_text(size=14), axis.title.x=element_blank(), axis.text.x=element_blank()) +
   ylab("People") + xlab("Year") + ggtitle("Safer Healthier People")

SHP

# Internet service demand variable

# rename variables for ggplot to remove spaces 

LSP1$ISD <- LSP1$`Internet Service Demand`
LSP2$ISD <- LSP2$`Internet Service Demand`
LSP3$ISD <- LSP3$`Internet Service Demand`
LSP4$ISD <- LSP4$`Internet Service Demand`
LSP5$ISD <- LSP5$`Internet Service Demand`
LSP6$ISD <- LSP6$`Internet Service Demand`

ISD <- ggplot() +
   geom_ribbon(data = data, aes(x = Time, ymin = LSP1minISD, ymax = LSP1maxISD), fill = LSP1col, alpha = 0.2) +
   geom_ribbon(data = data, aes(x = Time, ymin = LSP2minISD, ymax = LSP2maxISD), fill = LSP2col, alpha = 0.2) +
   geom_ribbon(data = data, aes(x = Time, ymin = LSP4minISD, ymax = LSP4maxISD), fill = LSP3col, alpha = 0.2) +
   geom_ribbon(data = data, aes(x = Time, ymin = LSP3minISD, ymax = LSP3maxISD), fill = LSP4col, alpha = 0.2) +
   geom_ribbon(data = data, aes(x = Time, ymin = LSP6minISD, ymax = LSP6maxISD), fill = LSP5col, alpha = 0.2) +
   geom_ribbon(data = data, aes(x = Time, ymin = LSP5minISD, ymax = LSP5maxISD), fill = LSP6col, alpha = 0.2) +
   geom_line(data = LSP1, aes(x = Time, y = ISD, color = "LSP1"), size = 0.75) +
   geom_line(data = LSP2, aes(x = Time, y = ISD, color = "LSP2"), size = 0.75) +
   geom_line(data = LSP3, aes(x = Time, y = ISD, color = "LSP3"), size = 0.75) +
   geom_line(data = LSP4, aes(x = Time, y = ISD, color = "LSP4"), size = 0.75) +
   geom_line(data = LSP5, aes(x = Time, y = ISD, color = "LSP5"), size = 0.75) +
   geom_line(data = LSP6, aes(x = Time, y = ISD, color = "LSP6"), size = 0.75) +
   annotate("segment", x = 2051, xend = 2051, y = 152.48569, yend = 247.58337, colour = LSP1col, size = 1) +
   annotate("segment", x = 2052, xend = 2052, y = 152.87576, yend = 249.13278, colour = LSP2col, size = 1) +
   annotate("segment", x = 2053, xend = 2053, y = 145.02267, yend = 160.22177, colour = LSP3col, size = 1) +
   annotate("segment", x = 2054, xend = 2054, y = 151.83183, yend = 230.69128, colour = LSP4col, size = 1) +
   annotate("segment", x = 2055, xend = 2055, y = 143.55914, yend = 147.95287, colour = LSP5col, size = 1) +
   annotate("segment", x = 2056, xend = 2056, y = 144.99667, yend = 154.61266, colour = LSP6col, size = 1) +
   scale_color_manual(name = "Scenario", values = c("LSP1" = LSP1col, "LSP2" = LSP2col, "LSP3" = LSP3col,
                                                    "LSP4" = LSP4col, "LSP5" = LSP5col, "LSP6" = LSP6col)) +
   theme_bw() + ylim(0, 250) +
   #   theme(plot.title = element_text(size=14), axis.title.x=element_blank(), axis.text.x=element_blank()) +
   ylab("Connections") + xlab("Year") + ggtitle("Internet Service Demand")

ISD

# bus frequency variable

# rename variables for ggplot to remove spaces 

LSP1$BF <- LSP1$`whole population bus frequency discrepancy`
LSP2$BF <- LSP2$`whole population bus frequency discrepancy`
LSP3$BF <- LSP3$`whole population bus frequency discrepancy`
LSP4$BF <- LSP4$`whole population bus frequency discrepancy`
LSP5$BF <- LSP5$`whole population bus frequency discrepancy`
LSP6$BF <- LSP6$`whole population bus frequency discrepancy`

BF <- ggplot() +
   geom_ribbon(data = data, aes(x = Time, ymin = LSP1minBF, ymax = LSP1maxBF), fill = LSP1col, alpha = 0.2) +
   geom_ribbon(data = data, aes(x = Time, ymin = LSP2minBF, ymax = LSP2maxBF), fill = LSP2col, alpha = 0.2) +
   geom_ribbon(data = data, aes(x = Time, ymin = LSP4minBF, ymax = LSP4maxBF), fill = LSP3col, alpha = 0.2) +
   geom_ribbon(data = data, aes(x = Time, ymin = LSP3minBF, ymax = LSP3maxBF), fill = LSP4col, alpha = 0.2) +
   geom_ribbon(data = data, aes(x = Time, ymin = LSP6minBF, ymax = LSP6maxBF), fill = LSP5col, alpha = 0.2) +
   geom_ribbon(data = data, aes(x = Time, ymin = LSP5minBF, ymax = LSP5maxBF), fill = LSP6col, alpha = 0.2) +
   geom_line(data = LSP1, aes(x = Time, y = BF, color = "LSP1"), size = 0.75) +
   geom_line(data = LSP2, aes(x = Time, y = BF, color = "LSP2"), size = 0.75) +
   geom_line(data = LSP3, aes(x = Time, y = BF, color = "LSP3"), size = 0.75) +
   geom_line(data = LSP4, aes(x = Time, y = BF, color = "LSP4"), size = 0.75) +
   geom_line(data = LSP5, aes(x = Time, y = BF, color = "LSP5"), size = 0.75) +
   geom_line(data = LSP6, aes(x = Time, y = BF, color = "LSP6"), size = 0.75) +
   annotate("segment", x = 2051, xend = 2051, y = 1254.3231, yend = 37756.14, colour = LSP1col, size = 1) +
   annotate("segment", x = 2052, xend = 2052, y = 1276.3843, yend = 39287.035, colour = LSP2col, size = 1) +
   annotate("segment", x = 2053, xend = 2053, y = 262.48572, yend = 7725.4106, colour = LSP3col, size = 1) +
   annotate("segment", x = 2054, xend = 2054, y = 537.5612, yend = 7675.683, colour = LSP4col, size = 1) +
   annotate("segment", x = 2055, xend = 2055, y = 275.1484, yend = 724.3533, colour = LSP5col, size = 1) +
   annotate("segment", x = 2055, xend = 2055, y = 426.7739, yend = 1726.8026, colour = LSP6col, size = 1) +
   scale_color_manual(name = "Scenario", values = c("LSP1" = LSP1col, "LSP2" = LSP2col, "LSP3" = LSP3col,
                                                    "LSP4" = LSP4col, "LSP5" = LSP5col, "LSP6" = LSP6col)) +
   theme_bw() + ylim(-215, 40000) +
   #   theme(plot.title = element_text(size=14), axis.title.x=element_blank(), axis.text.x=element_blank()) +
   ylab("Trips per year") + xlab("Year") + ggtitle("Bus Frequency Shortfall")

BF

# plot all on one plot
combined <- (TP + HD + HL + TOP + SR + II + SHP + ISD + BF) + 
   plot_layout(guides = "collect") & theme(legend.position = "bottom")
combined 

# save image file
ggsave("scenario_unc_plots_300dpi.png", plot = combined, device = "png", dpi = 300) #save 300dpi file
ggsave("scenario_unc_plots_600dpi.png", plot = combined, device = "png", dpi = 600) #save 600dpi file
