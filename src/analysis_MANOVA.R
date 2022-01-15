library(geomorph)
library(dplyr)

# define variables
class <- subset(classifier, env == "lab")
class$pop <- revalue(class$pop, c("GH"="Anc", "LL"="Intro1", "UL"="Intro2",
                                  "IC"="Intro3", "IT"="Intro4", "CL"="LP1",
                                  "TB"="LP3","TL"="LP2"))
shape <- two.d.array(gpa$coords[,,1112:1387])
CS <- gpa$Csize[1112:1387]
population <- factor(class$pop)
type <- class.lab$type
treatment <- class.lab$trt

# nonparametric MANOVAs
# effects = intercept + size
f1 <- procD.lm(shape ~ log(CS), RRPP=TRUE, verbose = TRUE, iter = 9999)
# effects = intercept + size + population
f2 <- procD.lm(shape ~ log(CS) + population, RRPP=TRUE, verbose = TRUE, iter = 9999)
# effects = intercept + size + population + treatment
f3 <- procD.lm(shape ~ log(CS) + population + treatment, RRPP=TRUE, verbose = TRUE, iter = 9999)
# effects = intercept + size + population + treatment + population:treatment
f4 <- procD.lm(shape ~ log(CS) + population * treatment, RRPP=TRUE, verbose = TRUE, iter = 9999)
# effects = intercept + size + population + treatment + population:treatment + size:pop:trt
f5 <- procD.lm(shape ~ log(CS) * population * treatment, RRPP=TRUE, verbose = TRUE, iter = 9999)

fit.size.red  <- procD.lm(log(CS) ~ population + treatment, RRPP=T, verbose=T, iter=9999) 
fit.size.full <- procD.lm(log(CS) ~ population * treatment, RRPP=T, verbose=T, iter=9999)

# Model comparisons
fit1 <- advanced.procD.lm(shape ~ 1,
                  ~ log(CS),
                  groups = ~population,
                  slope  = ~log(CS), iter = 9999)

fit2 <- advanced.procD.lm(shape ~ log(CS),
                        ~ log(CS) + population,
                  groups = ~population,
                  slope  = ~log(CS), iter = 9999)

fit3 <- advanced.procD.lm(shape ~ log(CS) + population,
                        ~ log(CS) + population + treatment,
                  groups = ~population*treatment,
                  slope  = ~log(CS), iter = 9999)

# test for factor interaction accounting for common allometry
fit4 <- advanced.procD.lm(shape ~ log(CS) + population + treatment,
                        ~ log(CS) + population * treatment,
                  groups = ~population*treatment,
                  slope  = ~log(CS), iter = 9999)

# test homogeneity of slopes
fit5 <- advanced.procD.lm(shape ~ log(CS) + population * treatment,
                        ~ log(CS) * population * treatment,
                  groups = ~population*treatment,
                  slope  = ~log(CS), angle.type = "deg", iter = 9999) 
# not significant P=0.139

# model to calculate least squares means - null model = intercept + size 
fit6 <- advanced.procD.lm(shape ~ log(CS),
                          ~ log(CS) + population * treatment,
                          groups = ~population*treatment,
                          slope  = ~log(CS), angle.type = "deg", iter = 9999)

write.csv(as.matrix(f5$anova.table), file = "output/ANOVA.csv", na = "")

