library(car)
# separate data by rearing environment
test <- lab.data[1:138,2:7]
test2 <- lab.data[139:276,2:7]

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

plot(PCA.np[,1], PCA.np[,2], 
     pch=21, cex=0, bg=col.gnp,
     xlab=xlab, ylab=ylab, asp=T)

# bivariate plot PC1 vs PC2 with confidence ellipses
dataEllipse(x = test$PC1, y = test$PC2, 
            groups = test$pop,
            levels = 0.683, 
            center.pch = 19, 
            center.cex = 0, 
            draw = TRUE, 
            fill = FALSE, 
            plot.points = TRUE,
            segments = 51, 
            robust = FALSE, 
            grid = FALSE, 
            xlab = deparse(substitute(PC1)), 
            ylab = deparse(substitute(PC2)))

dataEllipse(x = test2$PC1, y = test2$PC2, 
            groups = test2$pop,
            levels = 0.683, 
            center.pch = 19, 
            center.cex = 0, 
            draw = TRUE, 
            fill = FALSE, 
            plot.points = TRUE,
            segments = 51, 
            robust = FALSE, 
            grid = FALSE, 
            xlab = deparse(substitute(PC1)), 
            ylab = deparse(substitute(PC2))) 
