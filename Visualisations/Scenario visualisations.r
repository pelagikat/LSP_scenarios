
#set working directory for run data

setwd()
library(readr)
LSP1 <- read_csv("LSP1_rundata.csv")
LSP2 <- read_csv("LSP2_rundata.csv")
LSP3 <- read_csv("LSP3_rundata.csv")
LSP4 <- read_csv("LSP4_rundata.csv")
LSP5 <- read_csv("LSP5_rundata.csv")
LSP6 <- read_csv("LSP6_rundata.csv")

library(tidyverse)
library(patchwork)
library(scales)

LSP1col <- "#77AADD"
LSP2col <- "#EE8866"
LSP3col <- "#99DDFF"
LSP4col <- "#FFAABB"
LSP5col <- "#44BB99"
LSP6col <- "#AAAA00"

# total population variable

# rename variables for ggplot to remove spaces 

LSP1$totpop <- LSP1$`total population`
LSP2$totpop <- LSP2$`total population`
LSP3$totpop <- LSP3$`total population`
LSP4$totpop <- LSP4$`total population`
LSP5$totpop <- LSP5$`total population`
LSP6$totpop <- LSP6$`total population`

plot_totpop <- ggplot() +
  geom_line(data = LSP1, aes(x = Time, y = totpop, color = "LSP1"), size = 0.75) +
  geom_line(data = LSP2, aes(x = Time, y = totpop, color = "LSP2"), size = 0.75) +
  geom_line(data = LSP3, aes(x = Time, y = totpop, color = "LSP3"), size = 0.75) +
  geom_line(data = LSP4, aes(x = Time, y = totpop, color = "LSP4"), size = 0.75) +
  geom_line(data = LSP5, aes(x = Time, y = totpop, color = "LSP5"), size = 0.75) +
  geom_line(data = LSP6, aes(x = Time, y = totpop, color = "LSP6"), size = 0.75) +
  scale_color_manual(name = "Scenario", values = c("LSP1" = LSP1col, "LSP2" = LSP2col, "LSP3" = LSP3col,
                                                   "LSP4" = LSP4col, "LSP5" = LSP5col, "LSP6" = LSP6col)) +
  theme_bw() + ylim(0, 1000) +
  theme(plot.title = element_text(size=14), axis.title.x=element_blank(), axis.text.x=element_blank()) +
  ylab("People") + xlab("Year") + ggtitle("Total Population")
plot_totpop

# axis.title.x=element_blank(), axis.text.x=element_blank()

# Total Other Productivity variable

# rename variables for ggplot to remove spaces 

LSP1$tototprod <- LSP1$`Total Other Productivity`
LSP2$tototprod <- LSP2$`Total Other Productivity`
LSP3$tototprod <- LSP3$`Total Other Productivity`
LSP4$tototprod <- LSP4$`Total Other Productivity`
LSP5$tototprod <- LSP5$`Total Other Productivity`
LSP6$tototprod <- LSP6$`Total Other Productivity`

plot_tototprod <- ggplot() +
  geom_line(data = LSP1, aes(x = Time, y = tototprod, color = "LSP1"), size = 0.75) +
  geom_line(data = LSP2, aes(x = Time, y = tototprod, color = "LSP2"), size = 0.75) +
  geom_line(data = LSP3, aes(x = Time, y = tototprod, color = "LSP3"), size = 0.75) +
  geom_line(data = LSP4, aes(x = Time, y = tototprod, color = "LSP4"), size = 0.75) +
  geom_line(data = LSP5, aes(x = Time, y = tototprod, color = "LSP5"), size = 0.75) +
  geom_line(data = LSP6, aes(x = Time, y = tototprod, color = "LSP6"), size = 0.75) +
  scale_color_manual(name = "Scenario", values = c("LSP1" = LSP1col, "LSP2" = LSP2col, "LSP3" = LSP3col,
                                                   "LSP4" = LSP4col, "LSP5" = LSP5col, "LSP6" = LSP6col)) +
  theme_bw() + 
  theme(plot.title = element_text(size=14), axis.title.x=element_blank(), axis.text.x=element_blank()) +
  xlab("Year") +  scale_y_continuous("Dollars (millions)", limits = c(0, 30000000),
                     labels = comma_format(scale = 1/1000000)) +
  ggtitle("Total Productivity\n('Other' Sector)")
plot_tototprod

# Housing Land variable

# rename variables for ggplot to remove spaces 

LSP1$HouLand <- LSP1$`Housing Land`
LSP2$HouLand <- LSP2$`Housing Land`
LSP3$HouLand <- LSP3$`Housing Land`
LSP4$HouLand <- LSP4$`Housing Land`
LSP5$HouLand <- LSP5$`Housing Land`
LSP6$HouLand <- LSP6$`Housing Land`

plot_houland <- ggplot() +
  geom_line(data = LSP1, aes(x = Time, y = HouLand, color = "LSP1"), size = 0.75) +
  geom_line(data = LSP2, aes(x = Time, y = HouLand, color = "LSP2"), size = 0.75) +
  geom_line(data = LSP3, aes(x = Time, y = HouLand, color = "LSP3"), size = 0.75) +
  geom_line(data = LSP4, aes(x = Time, y = HouLand, color = "LSP4"), size = 0.75) +
  geom_line(data = LSP5, aes(x = Time, y = HouLand, color = "LSP5"), size = 0.75) +
  geom_line(data = LSP6, aes(x = Time, y = HouLand, color = "LSP6"), size = 0.75) +
  scale_color_manual(name = "Scenario", values = c("LSP1" = LSP1col, "LSP2" = LSP2col, "LSP3" = LSP3col,
                                                   "LSP4" = LSP4col, "LSP5" = LSP5col, "LSP6" = LSP6col)) +
  theme_bw() + ylim(0, 800) +
  theme(plot.title = element_text(size=14), axis.title.x=element_blank(), axis.text.x=element_blank()) +
  ylab("Hectares") + xlab("Year") + ggtitle("Housing Land")
plot_houland

# housing demand variable

# rename variables for ggplot to remove spaces 

LSP1$houdem <- LSP1$`housing demand`
LSP2$houdem <- LSP2$`housing demand`
LSP3$houdem <- LSP3$`housing demand`
LSP4$houdem <- LSP4$`housing demand`
LSP5$houdem <- LSP5$`housing demand`
LSP6$houdem <- LSP6$`housing demand`

plot_houdem <- ggplot() +
  geom_line(data = LSP1, aes(x = Time, y = houdem, color = "LSP1"), size = 0.75) +
  geom_line(data = LSP2, aes(x = Time, y = houdem, color = "LSP2"), size = 0.75) +
  geom_line(data = LSP3, aes(x = Time, y = houdem, color = "LSP3"), size = 0.75) +
  geom_line(data = LSP4, aes(x = Time, y = houdem, color = "LSP4"), size = 0.75) +
  geom_line(data = LSP5, aes(x = Time, y = houdem, color = "LSP5"), size = 0.75) +
  geom_line(data = LSP6, aes(x = Time, y = houdem, color = "LSP6"), size = 0.75) +
  scale_color_manual(name = "Scenario", values = c("LSP1" = LSP1col, "LSP2" = LSP2col, "LSP3" = LSP3col,
                                                   "LSP4" = LSP4col, "LSP5" = LSP5col, "LSP6" = LSP6col)) +
  theme_bw() + ylim(0, 600) +
  theme(plot.title = element_text(size=14), axis.title.x=element_blank(), axis.text.x=element_blank()) +
  ylab("Houses") + xlab("Year") + ggtitle("Housing Demand")
plot_houdem

# Species Richness variable

# rename variables for ggplot to remove spaces 

LSP1$specrich <- LSP1$`Species Richness`
LSP2$specrich <- LSP2$`Species Richness`
LSP3$specrich <- LSP3$`Species Richness`
LSP4$specrich <- LSP4$`Species Richness`
LSP5$specrich <- LSP5$`Species Richness`
LSP6$specrich <- LSP6$`Species Richness`

plot_specrich <- ggplot() +
  geom_line(data = LSP1, aes(x = Time, y = specrich, color = "LSP1"), size = 0.75) +
  geom_line(data = LSP2, aes(x = Time, y = specrich, color = "LSP2"), size = 0.75) +
  geom_line(data = LSP3, aes(x = Time, y = specrich, color = "LSP3"), size = 0.75) +
  geom_line(data = LSP4, aes(x = Time, y = specrich, color = "LSP4"), size = 0.75) +
  geom_line(data = LSP5, aes(x = Time, y = specrich, color = "LSP5"), size = 0.75) +
  geom_line(data = LSP6, aes(x = Time, y = specrich, color = "LSP6"), size = 0.75) +
  scale_color_manual(name = "Scenario", values = c("LSP1" = LSP1col, "LSP2" = LSP2col, "LSP3" = LSP3col,
                                                   "LSP4" = LSP4col, "LSP5" = LSP5col, "LSP6" = LSP6col)) +
  theme_bw() + ylim(0, 14000) +
  theme(plot.title = element_text(size=14), axis.title.x=element_blank(), axis.text.x=element_blank()) +
  ylab("Species") + xlab("Year") + ggtitle("Species Richness")
plot_specrich

ggplot() +  geom_line(data = LSP5, aes(x = Time, y = specrich, color = "LSP5"), size = 0.75)

# inequality indicator variable

# rename variables for ggplot to remove spaces 

LSP1$ineqin <- LSP1$`inequality indicator`
LSP2$ineqin <- LSP2$`inequality indicator`
LSP3$ineqin <- LSP3$`inequality indicator`
LSP4$ineqin <- LSP4$`inequality indicator`
LSP5$ineqin <- LSP5$`inequality indicator`
LSP6$ineqin <- LSP6$`inequality indicator`

plot_ineqin <- ggplot() +
  geom_line(data = LSP1, aes(x = Time, y = ineqin, color = "LSP1"), size = 0.75) +
  geom_line(data = LSP2, aes(x = Time, y = ineqin, color = "LSP2"), size = 0.75) +
  geom_line(data = LSP3, aes(x = Time, y = ineqin, color = "LSP3"), size = 0.75) +
  geom_line(data = LSP4, aes(x = Time, y = ineqin, color = "LSP4"), size = 0.75) +
  geom_line(data = LSP5, aes(x = Time, y = ineqin, color = "LSP5"), size = 0.75) +
  geom_line(data = LSP6, aes(x = Time, y = ineqin, color = "LSP6"), size = 0.75) +
  scale_color_manual(name = "Scenario", values = c("LSP1" = LSP1col, "LSP2" = LSP2col, "LSP3" = LSP3col,
                                                   "LSP4" = LSP4col, "LSP5" = LSP5col, "LSP6" = LSP6col)) +
  theme_bw() + ylim(0, 0.32) +
  theme(plot.title = element_text(size=14), axis.title.x=element_blank(), axis.text.x=element_blank()) +
  ylab("Dimensionless") + xlab("Year") + ggtitle("Inequality Indicator")
plot_ineqin

# Safer Healthier people variable

# rename variables for ggplot to remove spaces 

LSP1$SHP <- LSP1$`Safer Healthier People`
LSP2$SHP <- LSP2$`Safer Healthier People`
LSP3$SHP <- LSP3$`Safer Healthier People`
LSP4$SHP <- LSP4$`Safer Healthier People`
LSP5$SHP <- LSP5$`Safer Healthier People`
LSP6$SHP <- LSP6$`Safer Healthier People`

plot_SHP <- ggplot() +
  geom_line(data = LSP1, aes(x = Time, y = SHP, color = "LSP1"), size = 0.75) +
  geom_line(data = LSP2, aes(x = Time, y = SHP, color = "LSP2"), size = 0.75) +
  geom_line(data = LSP3, aes(x = Time, y = SHP, color = "LSP3"), size = 0.75) +
  geom_line(data = LSP4, aes(x = Time, y = SHP, color = "LSP4"), size = 0.75) +
  geom_line(data = LSP5, aes(x = Time, y = SHP, color = "LSP5"), size = 0.75) +
  geom_line(data = LSP6, aes(x = Time, y = SHP, color = "LSP6"), size = 0.75) +
  scale_color_manual(name = "Scenario", values = c("LSP1" = LSP1col, "LSP2" = LSP2col, "LSP3" = LSP3col,
                                                   "LSP4" = LSP4col, "LSP5" = LSP5col, "LSP6" = LSP6col)) +
  theme_bw() + ylim(0, 180) +
  theme(plot.title = element_text(size=14)) +
  ylab("People") + xlab("Year") + ggtitle("Safer Healthier People")
plot_SHP

# Internet service demand variable

# rename variables for ggplot to remove spaces 

LSP1$ISD <- LSP1$`Internet Service Demand`
LSP2$ISD <- LSP2$`Internet Service Demand`
LSP3$ISD <- LSP3$`Internet Service Demand`
LSP4$ISD <- LSP4$`Internet Service Demand`
LSP5$ISD <- LSP5$`Internet Service Demand`
LSP6$ISD <- LSP6$`Internet Service Demand`

plot_ISD <- ggplot() +
  geom_line(data = LSP1, aes(x = Time, y = ISD, color = "LSP1"), size = 0.75) +
  geom_line(data = LSP2, aes(x = Time, y = ISD, color = "LSP2"), size = 0.75) +
  geom_line(data = LSP3, aes(x = Time, y = ISD, color = "LSP3"), size = 0.75) +
  geom_line(data = LSP4, aes(x = Time, y = ISD, color = "LSP4"), size = 0.75) +
  geom_line(data = LSP5, aes(x = Time, y = ISD, color = "LSP5"), size = 0.75) +
  geom_line(data = LSP6, aes(x = Time, y = ISD, color = "LSP6"), size = 0.75) +
  scale_color_manual(name = "Scenario", values = c("LSP1" = LSP1col, "LSP2" = LSP2col, "LSP3" = LSP3col,
                                                   "LSP4" = LSP4col, "LSP5" = LSP5col, "LSP6" = LSP6col)) +
  theme_bw() + ylim(0, 170) +
  theme(plot.title = element_text(size=14)) +
  ylab("Connections") + xlab("Year") + ggtitle("Internet Service Demand")
plot_ISD

# bus frequency for travel equity variable

# rename variables for ggplot to remove spaces 

LSP1$bfreq <- LSP1$`whole population bus frequency discrepancy`
LSP2$bfreq <- LSP2$`whole population bus frequency discrepancy`
LSP3$bfreq <- LSP3$`whole population bus frequency discrepancy`
LSP4$bfreq <- LSP4$`whole population bus frequency discrepancy`
LSP5$bfreq <- LSP5$`whole population bus frequency discrepancy`
LSP6$bfreq <- LSP6$`whole population bus frequency discrepancy`

plot_bfreq <- ggplot() +
  geom_line(data = LSP1, aes(x = Time, y = bfreq, color = "LSP1"), size = 0.75) +
  geom_line(data = LSP2, aes(x = Time, y = bfreq, color = "LSP2"), size = 0.75) +
  geom_line(data = LSP3, aes(x = Time, y = bfreq, color = "LSP3"), size = 0.75) +
  geom_line(data = LSP4, aes(x = Time, y = bfreq, color = "LSP4"), size = 0.75) +
  geom_line(data = LSP5, aes(x = Time, y = bfreq, color = "LSP5"), size = 0.75) +
  geom_line(data = LSP6, aes(x = Time, y = bfreq, color = "LSP6"), size = 0.75) +
  scale_color_manual(name = "Scenario", values = c("LSP1" = LSP1col, "LSP2" = LSP2col, "LSP3" = LSP3col,
                                                   "LSP4" = LSP4col, "LSP5" = LSP5col, "LSP6" = LSP6col)) +
  theme_bw() + ylim(0, 6000) +
  theme(plot.title = element_text(size=14)) +
  ylab("Trips per year") + xlab("Year") + ggtitle("Bus Frequency Shortfall")

plot_bfreq

# use patchwork to combine all plots
combined <- (plot_totpop + plot_houdem + plot_houland + plot_tototprod + plot_specrich + plot_ineqin + plot_SHP +
  plot_ISD + plot_bfreq) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
combined 

# save image files
ggsave("scenario_run_plots_300dpi.png", plot = combined, device = "png", dpi = 300) #save 300dpi file
ggsave("scenario_run_plots_600dpi.png", plot = combined, device = "png", dpi = 600) #save 600dpi file

# Economic productivity LSP3 plot
LSP3$TAP <- LSP3$`Total Agricultural Productivity`
LSP3$TTP <- LSP3$`Total Tourism Productivity`
ggplot() +
  geom_line(data = LSP3, aes(x = Time, y = TAP, color = "Agriculture"), size = 0.75) +
  geom_line(data = LSP3, aes(x = Time, y = tototprod, color = "Other"), size = 0.75) +
  geom_line(data = LSP3, aes(x = Time, y = TTP, color = "Tourism"), size = 0.75) +
  scale_color_manual(name = "Economic sector", values = c("Agriculture" = LSP3col, "Other" = LSP2col, 
                                                          "Tourism" = LSP5col)) +
  theme_bw() + 
  xlab("Year") +  scale_y_continuous("Dollars (millions)", limits = c(0, 15000000),
                                     labels = comma_format(scale = 1/1000000)) +
  ggtitle("Economic Productivity Under LSP3")

ggsave("Economic_productivity_LSP3_300dpi.png", device = "png", dpi = 300) #save 300dpi file
ggsave("Economic_productivity_LSP3_600dpi.png", device = "png", dpi = 600) #save 600dpi file

