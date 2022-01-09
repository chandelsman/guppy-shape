# load lib: geomorph
library("geomorph")

# load Cartesian coordinates for field & lab specimens
lmdata <- readland.tps("data/lm.all.TPS", specID = "imageID")

# make vector of field specimens to drop because they lack maturity data
omit <- c(128, 175, 344, 361, 366, 403, 437, 474, 484, 563, 697, 742, 774, 
          791, 792, 855, 868, 977, 986)
# drop specimens from data array
lmdata <- lmdata[,,-omit]

# load file with sliding landmarks
sliders <- as.matrix(read.table("data/curvepts.txt", header=T))

# perform generalized Procrustes analysis on all specimens
gpa <- gpagen(lmdata, curves=sliders, ProcD=TRUE)

# export table with ID and Csize to build classifiers in Excel
# write.csv(all.gpa$Csize, "data/all_Csize.csv")

# import classifer variables
classifier   <- read.csv("data/classifier.csv", header = T, row.names = 1)
class.field  <- subset(classifier, trt == "field")
class.lab    <- subset(classifier, env == "lab")
class.nopred <- subset(classifier, trt == "no_cue")
class.pred   <- subset(classifier, trt == "cue")
