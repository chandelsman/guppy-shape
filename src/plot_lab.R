library(ggplot2)
### Lab data
## load data and calculate means and confidence intervals
lab.data <- read.csv("data/PCscores_lab.csv", header = T)
CS.lab <- summarySE(lab.data, "CS", c("pop", "trt"))
CS.lab.1 <- subset(CS.lab, pop %in% c("GH", "LL", "UL", "CL", "TL", "TB"))
CS.lab.2 <- subset(CS.lab, pop %in% c("GH", "IC", "IT", "CL", "TL", "TB"))
PC1.lab <- summarySE(lab.data, "PC1", c("pop", "trt"))
PC1.lab.1 <- subset(PC1.lab, pop %in% c("GH", "LL", "UL", "CL", "TL", "TB"))
PC1.lab.2 <- subset(PC1.lab, pop %in% c("GH", "IC", "IT", "CL", "TL", "TB"))
PC2.lab <- summarySE(lab.data, "PC2", c("pop", "trt"))
PC2.lab.1 <- subset(PC2.lab, pop %in% c("GH", "LL", "UL", "CL", "TL", "TB"))
PC2.lab.2 <- subset(PC2.lab, pop %in% c("GH", "IC", "IT", "CL", "TL", "TB"))

# Set theme for plots 
lab_theme <- theme(
  plot.title = element_text(face = "plain", size = 18),
  axis.title.x = element_text(face = "plain", hjust = 0.5, vjust = 0.3, 
                              size = 18, angle = 0),
  axis.text.x = element_text(face = "plain", hjust = 0.5, vjust = 1, 
                             size = 16, angle = 0,
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
  legend.key = element_rect(colour = "white", fill = NULL)
)

# Fill colors
cbPalette1 <- c("#E69F00", "#000000", "white", "#009E73", "#0072B2", "#999999")
cbPalette2 <- c("#E69F00", "#000000", "white", "#56B4E9", "#F0E442", "#009E73",
                "#0072B2", "#999999")
cbPalette3 <- c("black", "#777777", "#777777", "white", "white", "white")

# specify distance to offset points
pd <- position_dodge(width = 0.15)


 plot.CS1.lab <- 
ggplot(CS.lab.1, aes(x = trt , y = CS, group = pop)) + 
  geom_line(aes(type = 1, group = pop), colour = "gray", position = pd) +
  geom_errorbar(aes(ymax = CS + ci, ymin= CS - ci), 
                position = pd, width = 0.0) +
  geom_point(aes(shape = pop, fill = pop), size = 5, position = pd) +
  xlab(NULL) + 
  ylab("Centroid size") + 
  scale_x_discrete(limits = c("cue", "no_cue"),
                   labels = c("Reared\n w/ Cue", "Reared\n w/out Cue"), 
                   expand = c(0.2, 0.2)) + 
  scale_y_continuous() + 
  scale_shape_manual(values = c(21, 22, 23, 21, 22, 23), 
                     limits = c("GH", "LL", "UL", "CL", "TL", "TB")) +
  scale_fill_manual(values = cbPalette3,
                    limits = c("GH", "LL", "UL", "CL", "TL", "TB")) + 
  theme_bw(base_size = 20, base_family = "Helvetica") +
  ggtitle("2008 Introductions") +
  lab_theme
 
 plot.CS2.lab <- 
   ggplot(CS.lab.2, aes(x = trt , y = CS, group = pop)) + 
   geom_line(aes(type = 1, group = pop), colour = "gray", position = pd) +
   geom_errorbar(aes(ymax = CS + ci, ymin= CS - ci), 
                 position = pd, width = 0.0) +
   geom_point(aes(shape = pop, fill = pop), size = 5, position = pd) +
   xlab(NULL) + 
   ylab("Centroid size") + 
   scale_x_discrete(limits = c("cue", "no_cue"),
                    labels = c("Reared\n w/ Cue", "Reared\n w/out Cue"), 
                    expand = c(0.2, 0.2)) + 
   scale_y_continuous() + 
   scale_shape_manual(values = c(21, 24, 25, 21, 22, 23), 
                      limits = c("GH", "IC", "IT", "CL", "TL", "TB")) +
   scale_fill_manual(values = cbPalette3,
                     limits = c("GH", "IC", "IT", "CL", "TL", "TB")) + 
   theme_bw(base_size = 20, base_family = "Helvetica") +
   ggtitle("2009 Introductions") +
   lab_theme

 plot.PC1.1.lab <-
ggplot(PC1.lab.1, aes(x = trt , y = PC1, group = pop)) + 
  geom_errorbar(aes(ymax = PC1 + ci, ymin= PC1 - ci), 
                position = pd, width = 0.0) +
  geom_line(aes(type = 1, group = pop), colour = "gray", position = pd) +
  geom_point(aes(shape = pop, fill = pop), size = 5, position = pd) +
  xlab(NULL) + 
  ylab("PC1 (36.52%)") + 
  scale_x_discrete(limits = c("cue", "no_cue"),
                   labels = c("Reared\n w/ Cue", "Reared\n w/out Cue"), 
                   expand = c(0.2, 0.2)) + 
  scale_y_continuous(limits = c(min(PC1.lab.2$PC1 - PC1.lab.2$ci), 
                                max(PC1.lab.2$PC1 + PC1.lab.2$ci))) + 
  scale_shape_manual(values = c(21, 22, 23, 21, 22, 23), 
                     limits = c("GH", "LL", "UL", "CL", "TL", "TB")) +
  scale_fill_manual(values = cbPalette3,
                    limits = c("GH", "LL", "UL", "CL", "TL", "TB")) + 
  theme_bw(base_size = 20, base_family = "Helvetica") +
  ggtitle("2008 Introductions") +
  lab_theme
 
 plot.PC1.2.lab <-
   ggplot(PC1.lab.2, aes(x = trt , y = PC1, group = pop)) + 
   geom_errorbar(aes(ymax = PC1 + ci, ymin= PC1 - ci), 
                 position = pd, width = 0.0) +
   geom_line(aes(type = 1, group = pop), colour = "gray", position = pd) +
   geom_point(aes(shape = pop, fill = pop), size = 5, position = pd) +
   xlab(NULL) + 
   ylab("PC1 (36.52%)") + 
   scale_x_discrete(limits = c("cue", "no_cue"),
                    labels = c("Reared\n w/ Cue", "Reared\n w/out Cue"), 
                    expand = c(0.2, 0.2)) + 
   scale_y_continuous(limits = c(min(PC1.lab.2$PC1 - PC1.lab.2$ci), 
                                 max(PC1.lab.2$PC1 + PC1.lab.2$ci))) + 
   scale_shape_manual(values = c(21, 24, 25, 21, 22, 23), 
                      limits = c("GH", "IC", "IT", "CL", "TL", "TB")) +
   scale_fill_manual(values = cbPalette3,
                     limits = c("GH", "IC", "IT", "CL", "TL", "TB")) + 
   theme_bw(base_size = 20, base_family = "Helvetica") +
   ggtitle("2009 Introductions") +
   lab_theme

 plot.PC2.1.lab <- 
ggplot(PC2.lab.1, aes(x = trt , y = PC2, group = pop)) + 
  geom_errorbar(aes(ymax = PC2 + ci, ymin= PC2 - ci), 
                position = pd, width = 0.0) +
  geom_line(aes(type = 1, group = pop), colour = "gray", position = pd) +
  geom_point(aes(shape = pop, fill = pop), size = 5, position = pd) +
  xlab(NULL) + 
  ylab("PC2 (19.53%)") + 
  scale_x_discrete(limits = c("cue", "no_cue"),
                   labels = c("Reared\n w/ Cue", "Reared\n w/out Cue"), 
                   expand = c(0.2, 0.2)) + 
  scale_y_continuous(limits = c(min(PC2.lab.1$PC2 - PC2.lab.1$ci), 
                                max(PC2.lab.2$PC2 + PC2.lab.2$ci))) + 
  scale_shape_manual(values = c(21, 22, 23, 21, 22, 23), 
                     limits = c("GH", "LL", "UL", "CL", "TL", "TB")) +
  scale_fill_manual(values = cbPalette3,
                    limits = c("GH", "LL", "UL", "CL", "TL", "TB")) + 
  theme_bw(base_size = 20, base_family = "Helvetica") +
  ggtitle("2008 Introductions") +
  lab_theme
 
 plot.PC2.2.lab <- 
   ggplot(PC2.lab.2, aes(x = trt , y = PC2, group = pop)) + 
   geom_errorbar(aes(ymax = PC2 + ci, ymin= PC2 - ci), 
                 position = pd, width = 0.0) +
   geom_line(aes(type = 1, group = pop), colour = "gray", position = pd) +
   geom_point(aes(shape = pop, fill = pop), size = 5, position = pd) +
   xlab(NULL) + 
   ylab("PC2 (19.53%)") + 
   scale_x_discrete(limits = c("cue", "no_cue"),
                    labels = c("Reared\n w/ Cue", "Reared\n w/out Cue"), 
                    expand = c(0.2, 0.2)) + 
   scale_y_continuous(limits = c(min(PC2.lab.1$PC2 - PC2.lab.1$ci), 
                                 max(PC2.lab.2$PC2 + PC2.lab.2$ci))) + 
   scale_shape_manual(values = c(21, 24, 25, 21, 22, 23), 
                      limits = c("GH", "IC", "IT", "CL", "TL", "TB")) +
   scale_fill_manual(values = cbPalette3,
                     limits = c("GH", "IC", "IT", "CL", "TL", "TB")) + 
   theme_bw(base_size = 20, base_family = "Helvetica") +
   ggtitle("2009 Introductions") +
   lab_theme
 
 #########################
 # save plots as pdf files
 #########################
 
 pdf("figs/plot.CS1.lab.pdf", width=7, height=7, useDingbats=F)
 print(plot.CS1.lab)
 dev.off()
 
 pdf("figs/plot.CS2.lab.pdf", width=7, height=7, useDingbats=F)
 print(plot.CS2.lab)
 dev.off()
 
 pdf("figs/plot.PC1.1.lab.pdf", width=7, height=7, useDingbats=F)
 print(plot.PC1.1.lab)
 dev.off()
 
 pdf("figs/plot.PC1.2.lab.pdf", width=7, height=7, useDingbats=F)
 print(plot.PC1.2.lab)
 dev.off()
 
 pdf("figs/plot.PC2.1.lab.pdf", width=7, height=7, useDingbats=F)
 print(plot.PC2.1.lab)
 dev.off()
 
 pdf("figs/plot.PC2.2.lab.pdf", width=7, height=7, useDingbats=F)
 print(plot.PC2.2.lab)
 dev.off()