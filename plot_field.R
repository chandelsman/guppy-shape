library(ggplot2)
library(reshape2)
source("~/Dropbox/R/summaryscripts.R")
# source("~/Dropbox/R/multiplot.R")

# set theme for plots
lh_theme <- theme(
  plot.title = element_text(face = "plain", size = 18),
  axis.title.x = element_text(face = "plain", hjust = 0.5, vjust = 0.3, 
                              size = 18, angle = 0),
  axis.text.x = element_text(face = "plain", hjust = 1, vjust = 1, 
                             size = 16, angle = 45,
                             colour = "black"),
  axis.title.y = element_text(face = "plain", vjust = 1, 
                              size = 18, angle = 90),
  axis.text.y = element_text(face = "plain", vjust = 0.5, 
                             size = 16, angle = 0,
                             colour = "black"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  panel.border = element_rect(colour = "black", fill = NA, 
                              size = 0.75, linetype = 1),
  legend.position = "none",
  legend.title = element_blank(),
  legend.key = element_rect(colour = "white", fill = "white")
)

### Field data
## restructure data and plot CS, PC1, PC2, PC3
# Centroid Size
csize   <- read.csv("~/Dropbox/Projects/morphology/data/field_CS.csv", 
                    header = T)
csize.long   <- melt(csize, id = "pop", na.rm=T)
csize   <- summarySE(csize.long, "value", c("pop", "variable"))
csize.1 <- subset(csize, pop %in% c("GH", "LL", "UL", "CL", "TL", "TB"))
csize.2 <- subset(csize, pop %in% c("GH", "IC", "IT", "CL", "TL", "TB"))

# PC1
PC1   <- read.csv("~/Dropbox/Projects/morphology/data/field_PC1.csv", 
                  header = T)
PC1.long   <- melt(PC1, id = "pop", na.rm=T)
PC1   <- summarySE(PC1.long, "value", c("pop", "variable"))
PC1.1 <- subset(PC1, pop %in% c("GH", "LL", "UL", "CL", "TL", "TB"))
PC1.2 <- subset(PC1, pop %in% c("GH", "IC", "IT", "CL", "TL", "TB"))

# PC2
PC2   <- read.csv("~/Dropbox/Projects/morphology/data/field_PC2.csv", 
                  header = T)
PC2.long   <- melt(PC2, id = "pop", na.rm=T)
PC2   <- summarySE(PC2.long, "value", c("pop", "variable"))
PC2.1 <- subset(PC2, pop %in% c("GH", "LL", "UL", "CL", "TL", "TB"))
PC2.2 <- subset(PC2, pop %in% c("GH", "IC", "IT", "CL", "TL", "TB"))

# Fill colors
cbPalette <- c("#E69F00", "#000000", "white", "#009E73", "#0072B2", "#999999")
cbPalette <- c("black", "#777777", "#777777", "white", "white", "white")

plot.CS1 <-
ggplot(csize.1, aes(x = variable , y = value, group = pop)) + 
  geom_errorbar(aes(ymin = value - ci, ymax = value + ci), width = .0) + 
  geom_point(aes(shape = pop, fill = pop), size = 5) + 
  scale_shape_manual(values = c(21, 22, 23, 21, 22, 23),
                     limits = c("GH", "LL", "UL", "CL", "TL", "TB")) + 
  scale_fill_manual(
    values = cbPalette, 
    limits = c("GH", "LL", "UL", "CL", "TL", "TB")) +
  xlab(NULL) + 
  ylab("Centroid size") + 
  scale_x_discrete(limits = c("Source","X1","X2","X3","X4","X5","X6","X7",
                              "X8","X9","X10","X11","X12", "LP_1", 
                              "LP_3", "LP_2"), 
                   #LP_2 refers to TB under the variable name in the source values, but the code for figures here changed TB to LP3
                   labels = c("Source", "Apr-08", "May-08", "Jun-08", "Jul-08",
                              "Aug-08", "Sep-08", "Oct-08", "Nov-08", "Dec-08",
                              "Jan-09", "Feb-09", "Mar-09", "LP1", "LP2",
                              "LP3")) + 
  scale_y_continuous(limits = c(min(csize.1$value - csize.1$ci), 
                                max(csize.1$value + csize.1$ci))) + 
  ggtitle("2008 Introductions") +
  lh_theme
  
plot.CS2 <-
ggplot(csize.2, aes(x = variable , y = value, group = pop)) + 
  geom_errorbar(aes(ymin = value - ci, ymax = value + ci), width = .0) + 
  geom_point(aes(shape = pop, fill = pop), size = 5) + 
  scale_shape_manual(values = c(21, 24, 25, 21, 22, 23),
                     limits = c("GH", "IC", "IT", "CL", "TL", "TB")) + 
  scale_fill_manual(
    values = cbPalette, 
    limits = c("GH", "IC", "IT", "CL", "TL", "TB")) +
  xlab(NULL) + 
  ylab("Centroid size") + 
  scale_x_discrete(limits = c("Source","X1","X2","X3","X4","X5","X6","X7",
                              "X8","X9","X10","X11","X12", "LP_1",
                              "LP_3", "LP_2"), 
                   #LP_2 refers to TB under the variable name in the source values, but the code for figures here changed TB to LP3
                   labels = c("Source", "Apr-09", "May-09", "Jun-09", "Jul-09",
                              "Aug-09", "Sep-09", "Oct-09", "Nov-09", "Dec-09",
                              "Jan-10", "Feb-10", "Mar-10", "LP1", "LP2",
                              "LP3")) + 
  scale_y_continuous(limits = c(min(csize.2$value - csize.2$ci), 
                                max(csize.2$value + csize.2$ci))) + 
  ggtitle("2009 Introductions") +
  lh_theme

plot.PC1.1 <-
ggplot(PC1.1, aes(x = variable , y = value, group = pop)) + 
  geom_errorbar(aes(ymin = value - ci, ymax = value + ci), width = .0) + 
  geom_point(aes(shape = pop, fill = pop), size = 5) + 
  scale_shape_manual(values = c(21, 22, 23, 21, 22, 23),
                     limits = c("GH", "LL", "UL", "CL", "TL", "TB")) + 
  scale_fill_manual(
    values = cbPalette, 
    limits = c("GH", "LL", "UL", "CL", "TL", "TB")) +
  xlab(NULL) + 
  ylab("PC1 (40.34%)") + 
  scale_x_discrete(limits = c("Source","X1","X2","X3","X4","X5","X6","X7",
                              "X8","X9","X10","X11","X12", "LP_1",
                              "LP_3", "LP_2"), 
                   #LP_2 refers to TB under the variable name in the source values, but the code for figures here changed TB to LP3
                   labels = c("Source", "Apr-08", "May-08", "Jun-08", "Jul-08",
                              "Aug-08", "Sep-08", "Oct-08", "Nov-08", "Dec-08",
                              "Jan-09", "Feb-09", "Mar-09", "LP1", "LP2", 
                              "LP3")) + 
  scale_y_continuous(limits = c(min(PC1.1$value - PC1.1$ci), 
                                max(PC1.2$value + PC1.2$ci))) + 
  ggtitle("2008 Introductions") +
  lh_theme

plot.PC1.2 <-
ggplot(PC1.2, aes(x = variable , y = value, group = pop)) + 
  geom_errorbar(aes(ymin = value - ci, ymax = value + ci), width = .0) + 
  geom_point(aes(shape = pop, fill = pop), size = 5) + 
  scale_shape_manual(values = c(21, 24, 25, 21, 22, 23),
                     limits = c("GH", "IC", "IT", "CL", "TL", "TB")) + 
  scale_fill_manual(
    values = cbPalette, 
    limits = c("GH", "IC", "IT", "CL", "TL", "TB")) +
  xlab(NULL) + 
  ylab("PC1 (40.34%)") + 
  scale_x_discrete(limits = c("Source","X1","X2","X3","X4","X5","X6","X7",
                              "X8","X9","X10","X11","X12", "LP_1", 
                              "LP_3", "LP_2"), 
                   #LP_2 refers to TB under the variable name in the source values, but the code for figures here changed TB to LP3
                   labels = c("Source", "Apr-09", "May-09", "Jun-09", "Jul-09",
                              "Aug-09", "Sep-09", "Oct-09", "Nov-09", "Dec-09",
                              "Jan-10", "Feb-10", "Mar-10", "LP1", "LP2", 
                              "LP3")) + 
  scale_y_continuous(limits = c(min(PC1.2$value - PC1.2$ci), 
                                max(PC1.2$value + PC1.2$ci))) + 
  ggtitle("2009 Introductions") +
  lh_theme

plot.PC2.1 <-
ggplot(PC2.1, aes(x = variable , y = value, group = pop)) + 
  geom_errorbar(aes(ymin = value - ci, ymax = value + ci), width = .0) + 
  geom_point(aes(shape = pop, fill = pop), size = 5) + 
  scale_shape_manual(values = c(21, 22, 23, 21, 22, 23),
                     limits = c("GH", "LL", "UL", "CL", "TL", "TB")) + 
  scale_fill_manual(
    values = cbPalette, 
    limits = c("GH", "LL", "UL", "CL", "TL", "TB")) +
  xlab(NULL) + 
  ylab("PC2 (19.23%)") + 
  scale_x_discrete(limits = c("Source","X1","X2","X3","X4","X5","X6","X7",
                              "X8","X9","X10","X11","X12", "LP_1", 
                              "LP_3", "LP_2"), 
                   #LP_2 refers to TB under the variable name in the source values, but the code for figures here changed TB to LP3
                   labels = c("Source", "Apr-08", "May-08", "Jun-08", "Jul-08",
                              "Aug-08", "Sep-08", "Oct-08", "Nov-08", "Dec-08",
                              "Jan-09", "Feb-09", "Mar-09", "LP1", "LP2", 
                              "LP3")) + 
  scale_y_continuous(limits = c(min(PC2.1$value - PC2.1$ci), 
                                max(PC2.2$value + PC2.2$ci))) + 
  ggtitle("2008 Introductions") +
  lh_theme

plot.PC2.2 <-
ggplot(PC2.2, aes(x = variable , y = value, group = pop)) + 
  geom_errorbar(aes(ymin = value - ci, ymax = value + ci), width = .0) + 
  geom_point(aes(shape = pop, fill = pop), size = 5) + 
  scale_shape_manual(values = c(21, 24, 25, 21, 22, 23),
                     limits = c("GH", "IC", "IT", "CL", "TL", "TB")) + 
  scale_fill_manual(
    values = cbPalette, 
    limits = c("GH", "IC", "IT", "CL", "TL", "TB")) +
  xlab(NULL) + 
  ylab("PC2 (19.23%)") + 
  scale_x_discrete(limits = c("Source","X1","X2","X3","X4","X5","X6","X7",
                              "X8","X9","X10","X11","X12", "LP_1",  
                              "LP_3", "LP_2"), 
#LP_2 refers to TB under the variable name in the source values, but the code for figures here changed TB to LP3
                   labels = c("Source", "Apr-09", "May-09", "Jun-09", "Jul-09",
                              "Aug-09", "Sep-09", "Oct-09", "Nov-09", "Dec-09",
                              "Jan-10", "Feb-10", "Mar-10", "LP1", "LP2", 
                              "LP3")) + 
  scale_y_continuous(limits = c(min(PC2.1$value - PC2.1$ci), 
                                max(PC2.2$value + PC2.2$ci))) + 
  ggtitle("2009 Introductions") +
  lh_theme

#########################
# save plots as pdf files
#########################

pdf("figs/plot.CS1.pdf", width=7, height=7, useDingbats=F)
print(plot.CS1)
dev.off()

pdf("figs/plot.CS2.pdf", width=7, height=7, useDingbats=F)
print(plot.CS2)
dev.off()

pdf("figs/plot.PC1.1.pdf", width=7, height=7, useDingbats=F)
print(plot.PC1.1)
dev.off()

pdf("figs/plot.PC1.2.pdf", width=7, height=7, useDingbats=F)
print(plot.PC1.2)
dev.off()

pdf("figs/plot.PC2.1.pdf", width=7, height=7, useDingbats=F)
print(plot.PC2.1)
dev.off()

pdf("figs/plot.PC2.2.pdf", width=7, height=7, useDingbats=F)
print(plot.PC2.2)
dev.off()