
#set working directory for run data

setwd()
library(readr)
LSPperf <- read_csv("LSP_perf.csv")

library(tidyverse)
library(patchwork)
library(scales)

#colours
popcol <- "#88CCEE"  #light blue
epcol <- "#882255"  #dark red
dbcol <- "#999933"  #olive/tan
hdcol <- "#332288"  #dark blue
iicol <- "#AA4499"  #light magenta
isdcol <- "#DDCC77"  #dirty yellow
shpcol <- "#44AA99"  #light green
srcol <- "#117733"  #dark green 

# variables in plots
yoff <- 20
yneg <- -60
ypos <- 300
txs <- 2.5

LSP1 <- ggplot(LSPperf) +
  geom_col(aes(x = Indicator, y = LSP1_perf, fill = Indicator )) +
  scale_fill_manual(name = "Indicator", values = c("Economic Productivity" = epcol, "Health" = shpcol, 
                                              "Housing Shortfall" = hdcol, "Inequality" = iicol, 
                                              "Internet Performance" = isdcol, "Population" = popcol,  
                                              "Species Richness" = srcol, "Wastewater Disease Burden" = dbcol)) +
  geom_text(size = txs, nudge_y = yoff*sign(LSPperf$LSP1_perf),
             aes(x = Indicator, y = LSP1_perf, label = paste0(LSPperf$LSP1_perf, "%"))) +
  theme_bw() + 
  theme(plot.title = element_text(size=14), axis.title.x=element_blank(), axis.text.x=element_blank()
  ) +
  ylim(yneg,ypos) +
  ylab("Percentage change (%)") + ggtitle("LSP 1 change from BAU")

LSP1

LSP2 <- ggplot(LSPperf) +
  geom_col(aes(x = Indicator, y = LSP2_perf, fill = Indicator )) +
  scale_fill_manual(name = "Indicator", values = c("Economic Productivity" = epcol, "Health" = shpcol, 
                                              "Housing Shortfall" = hdcol, "Inequality" = iicol, 
                                              "Internet Performance" = isdcol, "Population" = popcol,  
                                              "Species Richness" = srcol, "Wastewater Disease Burden" = dbcol)) +
  geom_text(size = txs, nudge_y = yoff*sign(LSPperf$LSP2_perf),
            aes(x = Indicator, y = LSP2_perf, label = paste0(LSPperf$LSP2_perf, "%"))) + 
  theme_bw() + 
  theme(plot.title = element_text(size=14), axis.title.x=element_blank(), axis.text.x=element_blank(),
        axis.text.y=element_blank(), axis.title.y=element_blank()
#        legend.position = "none"
        ) +
  ylim(yneg,ypos) +
  ylab("Percentage change (%)") + ggtitle("LSP 2 change from BAU")

LSP2

LSP3 <- ggplot(LSPperf) +
  geom_col(aes(x = Indicator, y = LSP3_perf, fill = Indicator )) +
  scale_fill_manual(name = "Indicator", values = c("Economic Productivity" = epcol, "Health" = shpcol, 
                                              "Housing Shortfall" = hdcol, "Inequality" = iicol, 
                                              "Internet Performance" = isdcol, "Population" = popcol,  
                                              "Species Richness" = srcol, "Wastewater Disease Burden" = dbcol)) +
  geom_text(size = txs, nudge_y = yoff*sign(LSPperf$LSP3_perf),
             aes(x = Indicator, y = LSP3_perf, label = paste0(LSPperf$LSP3_perf, "%"))) + 
  theme_bw() + 
  theme(plot.title = element_text(size=14), axis.title.x=element_blank(), axis.text.x=element_blank(), 
        axis.title.y=element_blank(), axis.text.y=element_blank(),
        legend.position = "none"
 #       axis.text.x=element_text(size = rel(0.75), angle = 45, hjust = 1)
        ) +
  ylim(yneg,ypos) +
  ylab("Percentage change (%)") + ggtitle("LSP 3 change from BAU")

LSP3

LSP4 <- ggplot(LSPperf) +
  geom_col(aes(x = Indicator, y = LSP4_perf, fill = Indicator )) +
  scale_fill_manual(name = "Indicator", values = c("Economic Productivity" = epcol, "Health" = shpcol, 
                                              "Housing Shortfall" = hdcol, "Inequality" = iicol, 
                                              "Internet Performance" = isdcol, "Population" = popcol,  
                                              "Species Richness" = srcol, "Wastewater Disease Burden" = dbcol)) +
  geom_text(size = txs, nudge_y =yoff*sign(LSPperf$LSP4_perf),
             aes(x = Indicator, y = LSP4_perf, label = paste0(LSPperf$LSP4_perf, "%"))) + 
  theme_bw() + 
  theme(plot.title = element_text(size=14), axis.title.x=element_blank(), axis.text.x=element_blank(),
        legend.position = "none"
#        axis.text.y=element_blank(), axis.title.y=element_blank(), 
#        axis.text.x=element_text(size = rel(0.75), angle = 45, hjust = 1)
        ) +
  ylim(yneg,ypos) +
  ylab("Percentage change (%)") + ggtitle("LSP 4 change from BAU")

LSP4

LSP5 <- ggplot(LSPperf) +
  geom_col(aes(x = Indicator, y = LSP5_perf, fill = Indicator )) +
  scale_fill_manual(name = "Indicator", values = c("Economic Productivity" = epcol, "Health" = shpcol, 
                                              "Housing Shortfall" = hdcol, "Inequality" = iicol, 
                                              "Internet Performance" = isdcol, "Population" = popcol,  
                                              "Species Richness" = srcol, "Wastewater Disease Burden" = dbcol)) +
  geom_text(size = txs, nudge_y = yoff*sign(LSPperf$LSP5_perf),
             aes(x = Indicator, y = LSP5_perf, label = paste0(LSPperf$LSP5_perf, "%"))) +
  theme_bw() + 
  theme(plot.title = element_text(size=14), axis.title.x=element_blank(), axis.text.x=element_blank(), 
       axis.text.y=element_blank(), axis.title.y=element_blank(),
       legend.position = "none"
#        axis.text.x=element_text(size = rel(0.75), angle = 45, hjust = 1)
        ) +
  ylim(yneg,ypos) +
  ylab("Percentage change (%)") + ggtitle("LSP 5 change from BAU")

LSP5

layout <- "
AABBCC
#DDEE#
"
LSPs_combined <- (LSP1 + LSP2 + LSP3 + LSP4 + LSP5) + 
  plot_layout(design = layout, guides = "collect") & theme(legend.position = "bottom")

LSPs_combined 

# save image file
ggsave("ind_perf_300dpi_V4.png", plot = LSPs_combined, device = "png", dpi = 300) #save 300dpi file
ggsave("ind_perf_600dpi_V4.png", plot = LSPs_combined, device = "png", dpi = 600) #save 600dpi file
