# Load data, remove specimens that never matured, 
# align configurations, define classifiers
source("R/clean_data.R")

# Principal component analysis
# PCA on field data
PCA.field <- plotTangentSpace(gpa$coords[,,1:1111], 
                              groups = class.field$pop, verbose = T)
PCA.field$pc.summary$importance

# PCA on lab data
PCA.lab <- plotTangentSpace(gpa$coords[,,1112:1387], 
                            groups = class.lab$pop, verbose = T)
PCA.lab$pc.summary$importance

# make data frame with centroid size & PC scores for field & lab
field.data <- data.frame(gpa$Csize[1:1111], PCA.field$pc.scores)
colnames(field.data)[1] <- "CS"
lab.data   <- data.frame(class.lab, gpa$Csize[1112:1387],PCA.lab$pc.scores)
colnames(lab.data)[4] <- "CS"

# save centroid size & PC scores as .csv files
# write.csv(field.data, "data/PCscores_field.csv")
# write.csv(lab.data, "data/PCscores_lab.csv")

##### Random plotting code #####
# plot principal components with deformation grids
# create dataframe containing PC scores for each dataset
PCA.f  <- PCA.field$pc.scores[1:1111,] # field data
PCA.np <- PCA.lab$pc.scores[1:138,]  # reared w/out cue
PCA.p  <- PCA.lab$pc.scores[139:276,]  # reared with cue

# make a factor of both population and treatment classifiers
gp.field  <- as.factor(paste(class.field$pop))
gp.no_cue <- as.factor(paste(class.nopred$pop))
gp.cue    <- as.factor(paste(class.pred$pop))

# generate a set of different color over the rainbow spectrum
col.gp.field  <- rainbow(length(levels(gp.field))) 
col.gp.no_cue <- rainbow(length(levels(gp.no_cue))) 
col.gp.cue    <- rainbow(length(levels(gp.cue))) 

#give dimensions to the vector
names(col.gp.field) <- levels(gp.field)
names(col.gp.no_cue) <- levels(gp.no_cue)
names(col.gp.cue) <- levels(gp.cue)

# Use match() to generate a vector of length(n) & assign a colour to each specimen
col.gp.field <- col.gp.field[match(gp.field, names(col.gp.field))]
col.gp.no_cue <- col.gp.no_cue[match(gp.no_cue, names(col.gp.no_cue))]
col.gp.cue <- col.gp.cue[match(gp.cue, names(col.gp.cue))]

# make axis labels for PCA plots
xlab <- paste("Principal Component 1 ", "(",
              round(PCA.lab$pc.summary$importance[2,1]*100, 1), "%)", sep="")
ylab <- paste("Principal Component 2 ", "(", 
              round(PCA.lab$pc.summary$importance[2,2]*100, 1), "%)", sep="")

# divide area to be plotted into 3x3 grid
mat <- matrix(c(4,5,0,1,1,2,1,1,3), 3)

# set the size of the rows and columns
layout(mat, widths=c(1,1,1), heights=c(1,1,0.6))

# set the margins
par(mar=c(4, 4, 1, 1)) 

# plot lab - no pred
plot(PCA.np[,1], PCA.np[,2], 
     pch=21, cex=1, bg=col.gp.no_cue,
     xlab=xlab, ylab=ylab, asp=T)
legend(-0.073, 0.0378, legend= unique(gp.no_cue), pch=19,  col=unique(col.gp.no_cue))

# # plot lab - pred cue
# plot(PCA.p[,1], PCA.p[,2], 
#      pch=21, cex=2, bg=col.gp,
#      xlab=xlab, ylab=ylab, asp=T)
# legend(-0.063, 0.0195, legend= unique(gp), pch=19,  col=unique(col.gp))
# 
# par(mar=c(4, 4, 1, 1)) 
# 
# # plot field
# plot(PCA.f[,1], PCA.f[,2], 
#      pch=21, cex=1, bg=col.gf,
#      xlab=xlab, ylab=ylab, asp=T)
# legend(-0.073, 0.0375, legend= unique(gf), pch=19,  col=unique(col.gf))

# add deformation grids to plot 
# assign mean shape for use with plotRefToTarget below
ref <- mshape(gpa$coords)

# Item 2 to plot, the first TPS grid; 
# here we use the outline option to add to the visualisation
par(mar = c(0,0,0,0)) # sets the margins
plotRefToTarget(ref,PCA.lab$pc.shapes$PC1min,outline=plethodon$outline)
# Item 3
plotRefToTarget(ref,PCA.lab$pc.shapes$PC1max,outline=plethodon$outline)
# Item 4
plotRefToTarget(ref,PCA.lab$pc.shapes$PC2min,outline=plethodon$outline)
# Item 5
plotRefToTarget(ref,PCA.lab$pc.shapes$PC2max,outline=plethodon$outline)


